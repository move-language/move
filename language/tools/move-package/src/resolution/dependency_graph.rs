// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::{bail, Context, Result};
use move_symbol_pool::Symbol;
use petgraph::{algo, prelude::DiGraphMap, Direction};
use std::{
    collections::{btree_map::Entry, BTreeMap, HashSet},
    fmt,
    fs::File,
    io::{BufWriter, Write},
    path::{Path, PathBuf},
};

use crate::{
    package_hooks,
    source_package::{
        manifest_parser::parse_dependency,
        parsed_manifest::{
            CustomDepInfo, Dependency, DependencyKind, GitInfo, NamedAddress, PackageName,
            SourceManifest, SubstOrRename, Substitution,
        },
    },
};

use super::{
    download_and_update_if_remote,
    lock_file::{schema, LockFile},
    parse_package_manifest,
};

/// A representation of the transitive dependency graph of a Move package.  If successfully created,
/// the resulting graph:
///
/// - is directed, acyclic and `BuildConfig` agnostic,
/// - mentions each package at most once (i.e. no duplicate packages), and
/// - contains information about the source of every package (excluding the root package).
///
/// It can be built by recursively exploring a package's dependencies, fetching their sources if
/// necessary, or by reading its serialized contents from a lock file.  Both these processes will
/// fail if any of the criteria above cannot be met (e.g. if the graph contains a cycle, the same
/// package is fetched multiple times from different sources, or information about a package's
/// source is not available).
///
/// In order to be `BuildConfig` agnostic, it contains `dev-dependencies` as well as `dependencies`
/// and labels edges in the graph accordingly, as `DevOnly`, or `Always` dependencies.
#[derive(Debug)]
pub struct DependencyGraph {
    /// Path to the root package and its name (according to its manifest)
    root_path: PathBuf,
    root_package: PackageName,

    /// Transitive dependency graph, with dependency edges `P -> Q` labelled according to whether Q
    /// is always a dependency of P or only in dev-mode.
    package_graph: DiGraphMap<PackageName, DependencyMode>,

    /// The dependency that each package (keyed by name) originates from.  The root package is the
    /// only node in `package_graph` that does not have an entry in `package_table`.
    package_table: BTreeMap<PackageName, Dependency>,

    /// Packages that are transitive dependencies regardless of mode (the transitive closure of
    /// `DependencyMode::Always` edges in `package_graph`).
    pub always_deps: HashSet<PackageName>,
}

/// Edge label indicating whether one package always depends on another, or only in dev-mode.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum DependencyMode {
    Always,
    DevOnly,
}

/// Wrapper struct to display a dependency as an inline table in the lock file (matching the
/// convention in the source manifest).  This is necessary becase the `toml` crate does not
/// currently support serializing types as inline tables.
struct DependencyTOML<'a>(&'a Dependency);
struct SubstTOML<'a>(&'a Substitution);

impl DependencyGraph {
    /// Build a graph from the transitive dependencies and dev-dependencies of `root_package`.
    ///
    /// `skip_fetch_latest_git_deps` controls whether package resolution will fetch the latest
    /// versions of remote dependencies, even if a version already exists locally.
    ///
    /// `progress_output` is an output stream that is written to while generating the graph, to
    /// provide human-readable progress updates.
    pub fn new<Progress: Write>(
        root_package: &SourceManifest,
        root_path: PathBuf,
        skip_fetch_latest_git_deps: bool,
        progress_output: &mut Progress,
    ) -> Result<DependencyGraph> {
        let mut graph = DependencyGraph {
            root_path,
            root_package: root_package.package.name,
            package_graph: DiGraphMap::new(),
            package_table: BTreeMap::new(),
            always_deps: HashSet::new(),
        };

        graph
            .extend_graph(
                DependencyKind::default(),
                root_package,
                skip_fetch_latest_git_deps,
                progress_output,
            )
            .with_context(|| {
                format!(
                    "Failed to resolve dependencies for package '{}'",
                    graph.root_package
                )
            })?;

        graph.discover_always_deps();

        Ok(graph)
    }

    /// Create a dependency graph by reading a package's manifest and lock file.
    ///
    /// The lock file is expected to contain a complete picture of the package's transitive
    /// dependency graph, which means it is not required to discover it through a recursive
    /// traversal.
    ///
    /// Expects the following pre-conditions:
    ///
    /// - The lock file conforms to the schema expected by this
    pub fn read_from_lock(
        root_path: PathBuf,
        root_package: SourceManifest,
        lock: &mut File,
    ) -> Result<DependencyGraph> {
        let mut package_graph = DiGraphMap::new();
        let mut package_table = BTreeMap::new();

        // Seed graph with edges from the root package
        let root = root_package.package.name;

        for dep in root_package.dependencies.keys() {
            package_graph.add_edge(root, *dep, DependencyMode::Always);
        }

        for dep in root_package.dev_dependencies.keys() {
            package_graph.add_edge(root, *dep, DependencyMode::DevOnly);
        }

        // Fill in the remaining dependencies, and the package source information from the lock
        // file.
        for schema::Dependency {
            name,
            source,
            dependencies,
            dev_dependencies,
        } in schema::Dependencies::read(lock)?
        {
            let package = PackageName::from(name.as_str());
            let source = parse_dependency(package.as_str(), source)
                .with_context(|| format!("Deserializing dependency {}", package))?;

            match package_table.entry(package) {
                Entry::Vacant(entry) => {
                    entry.insert(source);
                }
                Entry::Occupied(entry) => {
                    bail!(
                        "Duplicate dependency in lock file:\n{0} = {1}\n{0} = {2}\n",
                        package,
                        DependencyTOML(entry.get()),
                        DependencyTOML(&source),
                    );
                }
            };

            for dep in dependencies.iter().flatten() {
                let dep = PackageName::from(dep.as_str());
                package_graph.add_edge(package, dep, DependencyMode::Always);
            }

            for dep in dev_dependencies.iter().flatten() {
                let dep = PackageName::from(dep.as_str());
                package_graph.add_edge(package, dep, DependencyMode::DevOnly);
            }
        }

        let mut graph = DependencyGraph {
            root_path,
            root_package: root,
            package_graph,
            package_table,
            always_deps: HashSet::new(),
        };

        graph.check_consistency()?;
        graph.check_acyclic()?;
        graph.discover_always_deps();
        Ok(graph)
    }

    /// Serialize this dependency graph into a lock file, consuming it in the process.
    ///
    /// This operation fails, writing nothing, if the graph contains a cycle, and can fail with an
    /// undefined output if it cannot be represented in a TOML file.
    pub fn write_to_lock(self, lock: &mut LockFile) -> Result<()> {
        self.check_acyclic()?;

        let mut writer = BufWriter::new(&**lock);
        for (pkg, dep) in self.package_table {
            writeln!(writer, "\n[[move.dependency]]")?;

            writeln!(writer, "name = {}", str_escape(pkg.as_str())?)?;
            writeln!(writer, "source = {}", DependencyTOML(&dep))?;

            let mut deps: Vec<_> = self
                .package_graph
                .edges(pkg)
                .map(|(_, dep, kind)| (*kind, dep))
                .collect();

            // Sort by kind ("always" dependencies go first), and by name, to keep the output
            // stable.
            deps.sort();

            let mut deps = deps.into_iter().peekable();
            if let Some((DependencyMode::Always, _)) = deps.peek() {
                writeln!(writer, "dependencies = [")?;
                while let Some((DependencyMode::Always, dep)) = deps.peek() {
                    writeln!(writer, "  {},", str_escape(dep.as_str())?)?;
                    deps.next();
                }
                writeln!(writer, "]")?;
            }

            if let Some((DependencyMode::DevOnly, _)) = deps.peek() {
                writeln!(writer, "dev-dependencies = [")?;
                while let Some((DependencyMode::DevOnly, dep)) = deps.peek() {
                    writeln!(writer, "  {},", str_escape(dep.as_str())?)?;
                    deps.next();
                }
                writeln!(writer, "]")?;
            }
        }

        writer.flush()?;
        Ok(())
    }

    /// Add the transitive dependencies and dev-dependencies from `package` to the dependency graph.
    fn extend_graph<Progress: Write>(
        &mut self,
        parent: DependencyKind,
        package: &SourceManifest,
        skip_fetch_latest_git_deps: bool,
        progress_output: &mut Progress,
    ) -> Result<()> {
        let from = package.package.name;
        for (to, dep) in &package.dependencies {
            let mut dep = dep.clone();
            dep.kind.reroot(&parent)?;

            self.process_dependency(dep, *to, skip_fetch_latest_git_deps, progress_output)?;

            self.package_graph
                .add_edge(from, *to, DependencyMode::Always);
        }

        for (to, dep) in &package.dev_dependencies {
            let mut dep = dep.clone();
            dep.kind.reroot(&parent)?;

            self.process_dependency(dep, *to, skip_fetch_latest_git_deps, progress_output)?;

            self.package_graph
                .add_edge(from, *to, DependencyMode::DevOnly);
        }

        Ok(())
    }

    /// Ensures that package `dep_name` and all its transitive dependencies are present in the
    /// graph, all sourced from their respective `dep`endencies.  Fails if any of the packages in
    /// the dependency sub-graph rooted at `dep_name` are already present in `self` but sourced from
    /// a different dependency.
    fn process_dependency<Progress: Write>(
        &mut self,
        dep: Dependency,
        dep_name: PackageName,
        skip_fetch_latest_git_deps: bool,
        progress_output: &mut Progress,
    ) -> Result<()> {
        let dep = match self.package_table.entry(dep_name) {
            Entry::Vacant(entry) => entry.insert(dep),

            // Seeing the same package again, pointing to the same dependency: OK, return early.
            Entry::Occupied(entry) if entry.get().kind == dep.kind => {
                return Ok(());
            }

            // Seeing the same package again, but pointing to a different dependency: Not OK.
            Entry::Occupied(entry) => {
                bail!(
                    "Conflicting dependencies found:\n{0} = {1}\n{0} = {2}\n",
                    dep_name,
                    DependencyTOML(entry.get()),
                    DependencyTOML(&dep),
                );
            }
        };

        download_and_update_if_remote(dep_name, dep, skip_fetch_latest_git_deps, progress_output)
            .with_context(|| format!("Fetching '{}'", dep_name))?;

        let (manifest, _) = parse_package_manifest(dep, &dep_name, self.root_path.clone())
            .with_context(|| format!("Parsing manifest for '{}'", dep_name))?;

        if dep_name != manifest.package.name {
            bail!(
                "Name of dependency declared in package '{}' \
                 does not match dependency's package name '{}'",
                dep_name,
                manifest.package.name,
            )
        }

        let kind = dep.kind.clone();
        self.extend_graph(kind, &manifest, skip_fetch_latest_git_deps, progress_output)
            .with_context(|| format!("Resolving dependencies for package '{}'", dep_name))
    }

    /// Check that every dependency in the graph, excluding the root package, is present in the
    /// package table.
    fn check_consistency(&self) -> Result<()> {
        for package in self.package_graph.nodes() {
            if package == self.root_package {
                continue;
            }

            if self.package_table.contains_key(&package) {
                continue;
            }

            let dependees: Vec<_> = self
                .package_graph
                .neighbors_directed(package, Direction::Incoming)
                .map(|pkg| String::from(pkg.as_str()))
                .collect();

            bail!(
                "No source found for package {}, depended on by: {}",
                package,
                dependees.join(", "),
            );
        }

        Ok(())
    }

    /// Check that there isn't a cycle between packages in the dependency graph.  Returns `Ok(())`
    /// if there is not, or an error describing the cycle if there is.
    fn check_acyclic(&self) -> Result<()> {
        let mut cyclic_components = algo::kosaraju_scc(&self.package_graph)
            .into_iter()
            .filter(|scc| scc.len() != 1 || self.package_graph.contains_edge(scc[0], scc[0]));

        let Some(scc) = cyclic_components.next() else {
            return Ok(())
        };

        // Duplicate start of the node at end for display
        // SAFETY: Strongly connected components can't be empty
        let mut cycle: Vec<_> = scc.iter().map(Symbol::as_str).collect();
        cycle.push(cycle[0]);

        bail!("Found cycle between packages: {}", cycle.join(" -> "));
    }

    /// Add the transitive closure of `DependencyMode::Always` edges reachable from the root package
    /// to the `always_deps` set.  Assumes that if a package is already in the graph's `always_deps`
    /// set, then the sub-graph reachable from it has already been explored.
    fn discover_always_deps(&mut self) {
        let mut frontier = vec![self.root_package];
        while let Some(package) = frontier.pop() {
            let new_frontier = self.always_deps.insert(package);
            if !new_frontier {
                continue;
            }

            frontier.extend(
                self.package_graph
                    .edges(package)
                    .filter_map(|(_, dep, mode)| (mode == &DependencyMode::Always).then_some(dep)),
            );
        }
    }
}

impl<'a> fmt::Display for DependencyTOML<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Dependency {
            kind,
            subst,
            version,
            digest,
        } = self.0;

        f.write_str("{ ")?;

        match kind {
            DependencyKind::Local(local) => {
                write!(f, "local = ")?;
                f.write_str(&path_escape(local)?)?;
            }

            DependencyKind::Git(GitInfo {
                git_url,
                git_rev,
                subdir,
            }) => {
                write!(f, "git = ")?;
                f.write_str(&str_escape(git_url.as_str())?)?;

                write!(f, ", rev = ")?;
                f.write_str(&str_escape(git_rev.as_str())?)?;

                write!(f, ", subdir = ")?;
                f.write_str(&path_escape(subdir)?)?;
            }

            DependencyKind::Custom(CustomDepInfo {
                node_url,
                package_address,
                subdir,
                package_name: _,
            }) => {
                let custom_key = package_hooks::custom_dependency_key().ok_or(fmt::Error)?;

                f.write_str(&custom_key)?;
                write!(f, " = ")?;
                f.write_str(&str_escape(node_url.as_str())?)?;

                write!(f, ", address = ")?;
                f.write_str(&str_escape(package_address.as_str())?)?;

                write!(f, ", subdir = ")?;
                f.write_str(&path_escape(subdir)?)?;
            }
        }

        if let Some((major, minor, bugfix)) = version {
            write!(f, ", version = \"{}.{}.{}\"", major, minor, bugfix)?;
        }

        if let Some(digest) = digest {
            write!(f, ", digest = ")?;
            f.write_str(&str_escape(digest.as_str())?)?;
        }

        if let Some(subst) = subst {
            write!(f, ", addr_subst = {}", SubstTOML(subst))?;
        }

        f.write_str(" }")?;
        Ok(())
    }
}

impl<'a> fmt::Display for SubstTOML<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        /// Write an individual key value pair in the substitution.
        fn write_subst(
            f: &mut fmt::Formatter<'_>,
            addr: &NamedAddress,
            subst: &SubstOrRename,
        ) -> fmt::Result {
            f.write_str(&str_escape(addr.as_str())?)?;
            write!(f, " = ")?;

            match subst {
                SubstOrRename::RenameFrom(named) => {
                    f.write_str(&str_escape(named.as_str())?)?;
                }

                SubstOrRename::Assign(account) => {
                    f.write_str(&str_escape(&account.to_canonical_string())?)?;
                }
            }

            Ok(())
        }

        let mut substs = self.0.iter();

        let Some((addr, subst)) = substs.next() else {
            return f.write_str("{}")
        };

        f.write_str("{ ")?;

        write_subst(f, addr, subst)?;
        for (addr, subst) in substs {
            write!(f, ", ")?;
            write_subst(f, addr, subst)?;
        }

        f.write_str(" }")?;

        Ok(())
    }
}

/// Escape a string to output in a TOML file.
fn str_escape(s: &str) -> Result<String, fmt::Error> {
    toml::to_string(s).map_err(|_| fmt::Error)
}

/// Escape a path to output in a TOML file.
fn path_escape(p: &Path) -> Result<String, fmt::Error> {
    str_escape(p.to_str().ok_or(fmt::Error)?)
}
