// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::Result;
use colored::Colorize;
use move_command_line_common::env::MOVE_HOME;
use std::{
    ffi::OsStr,
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

use crate::{
    package_hooks,
    source_package::parsed_manifest::{
        CustomDepInfo, DependencyKind, GitInfo, PackageName, SourceManifest,
    },
    BuildConfig,
};

use self::dependency_graph::DependencyGraph;

pub mod dependency_graph;
mod digest;
pub mod lock_file;
pub mod resolution_graph;
pub mod resolving_table;

pub fn download_dependency_repos<Progress: Write>(
    manifest: &SourceManifest,
    build_options: &BuildConfig,
    root_path: &Path,
    progress_output: &mut Progress,
) -> Result<()> {
    let graph = DependencyGraph::new(
        manifest,
        root_path.to_path_buf(),
        build_options.skip_fetch_latest_git_deps,
        progress_output,
    )?;

    for pkg_name in graph.topological_order() {
        if pkg_name == graph.root_package {
            continue;
        }

        if !(build_options.dev_mode || graph.always_deps.contains(&pkg_name)) {
            continue;
        }

        let package = graph
            .package_table
            .get(&pkg_name)
            .expect("Metadata for package");

        download_and_update_if_remote(
            pkg_name,
            &package.kind,
            build_options.skip_fetch_latest_git_deps,
            progress_output,
        )?;
    }

    Ok(())
}

fn download_and_update_if_remote<Progress: Write>(
    dep_name: PackageName,
    kind: &DependencyKind,
    skip_fetch_latest_git_deps: bool,
    progress_output: &mut Progress,
) -> Result<()> {
    match kind {
        DependencyKind::Local(_) => Ok(()),

        DependencyKind::Custom(node_info) => {
            package_hooks::resolve_custom_dependency(dep_name, node_info)
        }

        kind @ DependencyKind::Git(GitInfo {
            git_url,
            git_rev,
            subdir: _,
        }) => {
            let git_path = repository_path(kind);
            let os_git_url = OsStr::new(git_url.as_str());
            let os_git_rev = OsStr::new(git_rev.as_str());

            if !git_path.exists() {
                writeln!(
                    progress_output,
                    "{} {}",
                    "FETCHING GIT DEPENDENCY".bold().green(),
                    git_url,
                )?;

                // If the cached folder does not exist, download and clone accordingly
                Command::new("git")
                    .args([OsStr::new("clone"), os_git_url, git_path.as_os_str()])
                    .output()
                    .map_err(|_| {
                        anyhow::anyhow!("Failed to clone Git repository for package '{}'", dep_name)
                    })?;

                Command::new("git")
                    .args([
                        OsStr::new("-C"),
                        git_path.as_os_str(),
                        OsStr::new("checkout"),
                        os_git_rev,
                    ])
                    .output()
                    .map_err(|_| {
                        anyhow::anyhow!(
                            "Failed to checkout Git reference '{}' for package '{}'",
                            git_rev,
                            dep_name
                        )
                    })?;
            } else if !skip_fetch_latest_git_deps {
                // Update the git dependency
                // Check first that it isn't a git rev (if it doesn't work, just continue with the
                // fetch)
                if let Ok(rev) = Command::new("git")
                    .args([
                        OsStr::new("-C"),
                        git_path.as_os_str(),
                        OsStr::new("rev-parse"),
                        OsStr::new("--verify"),
                        os_git_rev,
                    ])
                    .output()
                {
                    if let Ok(parsable_version) = String::from_utf8(rev.stdout) {
                        // If it's exactly the same, then it's a git rev
                        if parsable_version.trim().starts_with(git_rev.as_str()) {
                            return Ok(());
                        }
                    }
                }

                let tag = Command::new("git")
                    .args([
                        OsStr::new("-C"),
                        git_path.as_os_str(),
                        OsStr::new("tag"),
                        OsStr::new("--list"),
                        os_git_rev,
                    ])
                    .output();

                if let Ok(tag) = tag {
                    if let Ok(parsable_version) = String::from_utf8(tag.stdout) {
                        // If it's exactly the same, then it's a git tag, for now tags won't be updated
                        // Tags don't easily update locally and you can't use reset --hard to cleanup
                        // any extra files
                        if parsable_version.trim().starts_with(git_rev.as_str()) {
                            return Ok(());
                        }
                    }
                }

                writeln!(
                    progress_output,
                    "{} {}",
                    "UPDATING GIT DEPENDENCY".bold().green(),
                    git_url,
                )?;

                // If the current folder exists, do a fetch and reset to ensure that the branch
                // is up to date.
                //
                // NOTE: this means that you must run the package system with a working network
                // connection.
                let status = Command::new("git")
                    .args([
                        OsStr::new("-C"),
                        git_path.as_os_str(),
                        OsStr::new("fetch"),
                        OsStr::new("origin"),
                    ])
                    .stdout(Stdio::null())
                    .stderr(Stdio::null())
                    .status()
                    .map_err(|_| {
                        anyhow::anyhow!(
                            "Failed to fetch latest Git state for package '{}', to skip set \
                             --skip-fetch-latest-git-deps",
                            dep_name
                        )
                    })?;

                if !status.success() {
                    return Err(anyhow::anyhow!(
                        "Failed to fetch to latest Git state for package '{}', to skip set \
                         --skip-fetch-latest-git-deps | Exit status: {}",
                        dep_name,
                        status
                    ));
                }

                let status = Command::new("git")
                    .args([
                        OsStr::new("-C"),
                        git_path.as_os_str(),
                        OsStr::new("reset"),
                        OsStr::new("--hard"),
                        OsStr::new(&format!("origin/{}", git_rev)),
                    ])
                    .stdout(Stdio::null())
                    .stderr(Stdio::null())
                    .status()
                    .map_err(|_| {
                        anyhow::anyhow!(
                            "Failed to reset to latest Git state '{}' for package '{}', to skip \
                             set --skip-fetch-latest-git-deps",
                            git_rev,
                            dep_name
                        )
                    })?;

                if !status.success() {
                    return Err(anyhow::anyhow!(
                        "Failed to reset to latest Git state '{}' for package '{}', to skip set \
                         --skip-fetch-latest-git-deps | Exit status: {}",
                        git_rev,
                        dep_name,
                        status
                    ));
                }
            }

            Ok(())
        }
    }
}

/// The local location of the repository containing the dependency of kind `kind` (and potentially
/// other, related dependencies).
pub fn repository_path(kind: &DependencyKind) -> PathBuf {
    match kind {
        DependencyKind::Local(path) => path.clone(),

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

/// The path that the dependency of kind `kind` is found at locally, after it is fetched.
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
