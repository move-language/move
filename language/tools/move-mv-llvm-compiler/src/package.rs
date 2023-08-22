// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::{anyhow, bail, Context, Result};
use move_cli::base::reroot_path;
use move_command_line_common::env::MOVE_HOME;
use move_compiler::shared::{NumericalAddress, PackagePaths};
use move_core_types::account_address::AccountAddress;
use move_package::{
    source_package::{
        layout::SourcePackageLayout,
        manifest_parser,
        manifest_parser::{parse_move_manifest_string, parse_source_manifest},
        parsed_manifest::{
            CustomDepInfo, Dependencies, Dependency, DependencyKind, GitInfo, PackageName,
            SourceManifest,
        },
    },
    Architecture, BuildConfig,
};
use move_stdlib::{move_stdlib_files, move_stdlib_named_addresses};
use move_symbol_pool::Symbol;
use regex;
use std::{
    collections::{BTreeMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DependencyInfo {
    pub source_manifest: SourceManifest,
    pub path: PathBuf,
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DependencyAndAccountAddress {
    pub compilation_dependency: Vec<String>,
    pub account_addresses: Vec<(Symbol, AccountAddress)>,
}

pub fn build_dependency(
    move_package_path: Result<std::path::PathBuf, anyhow::Error>,
    target_path_string: &String,
    named_address_map: &mut BTreeMap<String, NumericalAddress>,
    stdlib: bool,
    dev: bool,
    test: bool,
) -> anyhow::Result<Vec<PackagePaths<String, String>>> {
    let mut deps = vec![];

    if stdlib {
        *named_address_map = move_stdlib_named_addresses();
        named_address_map.insert(
            "std".to_string(),
            NumericalAddress::parse_str("0x1").unwrap(),
        );

        let compilation_dependency = move_stdlib_files();

        deps.push(PackagePaths {
            name: None,
            paths: compilation_dependency,
            named_address_map: named_address_map.clone(),
        });
    }

    if let Ok(package) = move_package_path {
        let res = resolve_dependency(package, dev, test);
        if let Err(err) = &res {
            eprintln!("Error: {:#?}", &res);
            bail!("Error resolving dependency: {}", err);
        } else {
            let compilation_dependency: Vec<String> = res
                .as_ref()
                .unwrap()
                .compilation_dependency
                .iter()
                .cloned()
                .filter(|s| *s != *target_path_string)
                .collect();

            let account_addresses = res.unwrap().account_addresses;

            // Note: could not use a simple chaining iterator like
            // named_address_map.extend(account_addresses.iter().map(|(sym, acc)|
            //     (sym.as_str().to_string(), NumericalAddress::parse_str(&acc.to_string()).unwrap())).into_iter());
            // since need to check for possible reassignment, so making this in old fashion loop:
            for (symbol, account_address) in account_addresses {
                let name = symbol.as_str().to_string();
                let address_string = account_address.to_string();
                let address_string_hex = format!("0x{}", address_string);
                let address = NumericalAddress::parse_str(&address_string_hex)
                    .or_else(|err| {
                        bail!(
                            "Failed to parse numerical address '{}'. Got error: {}",
                            address_string_hex,
                            err
                        );
                    })
                    .unwrap();
                if let Some(value) = named_address_map.get(&name) {
                    if *value != address {
                        bail!("{} already has assigned address {}, cannot reassign with new address {}. Possibly an error in Move.toml.",
                              name, *value, address);
                    }
                }
                named_address_map.insert(name, address);
            }
            deps.push(PackagePaths {
                name: None,
                paths: compilation_dependency,
                named_address_map: named_address_map.clone(),
            });
        }
    }
    Ok(deps)
}

/// It reads Move manifest file (Move.toml), defined by `-p dir` option.
/// Dependency in this files should be relative paths `rel` and will be adjoined to path in `dir`,
/// then all *.move files in `dir/rel` and `dir/rel/sources` will be compiled together with source in `-c source`.
/// This function also assigns numerical addresses to all names listed in the manifest.
pub fn resolve_dependency(
    target_path: PathBuf,
    dev: bool,
    test: bool,
) -> anyhow::Result<DependencyAndAccountAddress> {
    let build_config = BuildConfig {
        dev_mode: dev,
        test_mode: test,
        generate_docs: false,
        generate_abis: false,
        install_dir: None, // Option<PathBuf>
        force_recompilation: false,
        lock_file: None, // Option<PathBuf>
        additional_named_addresses: BTreeMap::new(),
        architecture: Some(Architecture::Move),
        fetch_deps_only: true,
        skip_fetch_latest_git_deps: true,
        bytecode_version: None, // Option<u32>
    };

    let rerooted_path = reroot_path(Some(target_path))?;

    let dep_info = download_deps_for_package(&build_config, &rerooted_path)?;

    let mut compilation_dependency: Vec<String> = vec![];
    let mut account_addresses = Vec::<(Symbol, AccountAddress)>::new();

    for dep in dep_info {
        let manifest = dep.clone().source_manifest;

        // Collect named addresses.
        let acc_addr = if !build_config.dev_mode {
            manifest
                .addresses
                .unwrap_or_default()
                .into_iter()
                .filter_map(|(sym, op)| op.map(|v| (sym, v)))
                .collect::<Vec<_>>()
        } else {
            manifest
                .dev_address_assignments
                .unwrap_or_default()
                .into_iter()
                .collect::<Vec<(Symbol, AccountAddress)>>()
        };

        account_addresses.extend(&acc_addr);

        // Collect compilation_dependency
        let path = dep.path;
        let path_string = &path.to_string_lossy();
        if !path.exists() {
            bail!("No such file or directory '{}'", path_string)
        }

        compilation_dependency.extend(move_dep_files(path));
        continue;
    }

    let dep_and_names = DependencyAndAccountAddress {
        compilation_dependency,
        account_addresses,
    };
    Ok(dep_and_names)
}

use move_command_line_common::files::{extension_equals, find_filenames, MOVE_EXTENSION};
// Const below defined in `move-stdlib` but only as private.
// Since it is "standard" for stdlib, we follow the same scheme.
const MODULES_DIR: &str = "sources";
fn move_dep_files(path: PathBuf) -> Vec<String> {
    let mut dir = path;
    dir.push(MODULES_DIR);
    if !dir.exists() {
        return vec![];
    }
    find_filenames(&[dir], |p| extension_equals(p, MOVE_EXTENSION)).unwrap()
}

fn download_deps_for_package(
    build_config: &BuildConfig,
    path: &Path,
) -> anyhow::Result<Vec<DependencyInfo>> {
    let path = SourcePackageLayout::try_find_root(path)?;
    let toml_manifest = parse_toml_manifest(path.join(SourcePackageLayout::Manifest.path()))?;
    let manifest: SourceManifest = manifest_parser::parse_source_manifest(toml_manifest)?;

    let mut processed_deps: HashSet<String> = HashSet::new();
    let mut deps_for_pack: Vec<DependencyInfo> = vec![];

    download_dependency_repos(
        &manifest,
        build_config,
        path,
        &mut processed_deps,
        &mut deps_for_pack,
    )?;
    Ok(deps_for_pack)
}

fn parse_toml_manifest(path: PathBuf) -> anyhow::Result<toml::Value> {
    let manifest_string = std::fs::read_to_string(path)?;
    manifest_parser::parse_move_manifest_string(manifest_string)
}

pub fn download_dependency_repos(
    manifest: &SourceManifest,
    build_config: &BuildConfig,
    root_path: PathBuf,
    processed_deps: &mut HashSet<String>,
    deps_for_pack: &mut Vec<DependencyInfo>,
) -> anyhow::Result<()> {
    let manifest_name = manifest.package.name.as_str().to_string();
    if !processed_deps.insert(manifest_name.clone()) {
        println!("Dependency {} has been processed before", &manifest_name);
        return Ok(());
    }

    deps_for_pack.push(DependencyInfo {
        source_manifest: manifest.clone(),
        path: root_path.clone(),
    });

    // include dev dependencies if in dev mode
    let empty_deps;
    let additional_deps = if build_config.dev_mode {
        &manifest.dev_dependencies
    } else {
        empty_deps = Dependencies::new();
        &empty_deps
    };

    for (dep_name, dep) in manifest.dependencies.iter().chain(additional_deps.iter()) {
        download_and_update_if_remote(*dep_name, dep, build_config.skip_fetch_latest_git_deps)?;
        let (dep_manifest, root_path_from_manifest) =
            parse_package_manifest(dep, dep_name, root_path.clone())
                .with_context(|| format!("While processing dependency '{}'", *dep_name))?;

        download_dependency_repos(
            &dep_manifest,
            build_config,
            root_path_from_manifest.clone(),
            processed_deps,
            deps_for_pack,
        )?;
    }
    Ok(())
}

fn parse_package_manifest(
    dep: &Dependency,
    dep_name: &PackageName,
    mut root_path: PathBuf,
) -> Result<(SourceManifest, PathBuf)> {
    root_path.push(local_path(&dep.kind));
    let manifest_path = root_path.join(SourcePackageLayout::Manifest.path());

    let contents = fs::read_to_string(&manifest_path).with_context(|| {
        format!(
            "Unable to find package manifest for '{}' at {:?}",
            dep_name, manifest_path,
        )
    })?;

    let manifest_toml = parse_move_manifest_string(contents)?;
    let source_package = parse_source_manifest(manifest_toml)?;
    Ok((source_package, root_path))
}

// Note: for full dependency processing see same function in move-package
fn download_and_update_if_remote(
    _dep_name: PackageName,
    dep: &Dependency,
    _skip_fetch_latest_git_deps: bool,
) -> Result<()> {
    match &dep.kind {
        DependencyKind::Local(_) => Ok(()),
        _ => Err(anyhow::anyhow!(
            "Only local dependency allowed in manifest (.toml) file"
        )),
    }
}

// The local location of the repository containing the dependency of kind `kind` (and potentially
// other, related dependencies).
fn repository_path(kind: &DependencyKind) -> PathBuf {
    match kind {
        DependencyKind::Local(path) => path.clone(),

        // Note: non-local was restricted in `download_and_update_if_remote`,
        // but we keep the full functionality here, since it is an independent function.

        // Downloaded packages are of the form <sanitized_git_url>_<rev_name>
        DependencyKind::Git(GitInfo {
            git_url,
            git_rev,
            subdir: _,
        }) => [
            &*MOVE_HOME,
            &format!(
                "{}_{}",
                url_to_file_name(git_url.as_str()),
                git_rev.replace('/', "__"),
            ),
        ]
        .iter()
        .collect(),

        // Downloaded packages are of the form <sanitized_node_url>_<address>_<package>
        DependencyKind::Custom(CustomDepInfo {
            node_url,
            package_address,
            package_name,
            subdir: _,
        }) => [
            &*MOVE_HOME,
            &format!(
                "{}_{}_{}",
                url_to_file_name(node_url.as_str()),
                package_address.as_str(),
                package_name.as_str(),
            ),
        ]
        .iter()
        .collect(),
    }
}

// The path that the dependency of kind `kind` is found at locally, after it is fetched.
fn local_path(kind: &DependencyKind) -> PathBuf {
    let mut repo_path = repository_path(kind);

    if let DependencyKind::Git(GitInfo { subdir, .. })
    | DependencyKind::Custom(CustomDepInfo { subdir, .. }) = kind
    {
        repo_path.push(subdir);
    }

    repo_path
}

fn url_to_file_name(url: &str) -> String {
    regex::Regex::new(r"/|:|\.|@")
        .unwrap()
        .replace_all(url, "_")
        .to_string()
}

pub fn path_to_string(path: &Path) -> anyhow::Result<String> {
    match path.to_str() {
        Some(p) => Ok(p.to_string()),
        None => Err(anyhow!("non-Unicode file name")),
    }
}
