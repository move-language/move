// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

pub mod ast;
pub mod comments;
pub mod cst;
pub(crate) mod filter;
pub mod keywords;
pub mod lexer;
pub(crate) mod merge_spec_modules;
pub(crate) mod syntax;
pub mod token_range;
pub mod translate;

use crate::shared::{CompilationEnv, IndexedPackagePath};
use anyhow::anyhow;
use move_command_line_common::files::find_move_filenames;
use move_symbol_pool::Symbol;
use std::collections::BTreeSet;

pub fn find_move_filenames_with_address_mapping(
    paths_with_mapping: Vec<IndexedPackagePath>,
) -> anyhow::Result<Vec<IndexedPackagePath>> {
    let mut res = vec![];
    for IndexedPackagePath {
        package,
        path,
        named_address_map: named_address_mapping,
    } in paths_with_mapping
    {
        res.extend(
            find_move_filenames(&[path.as_str()], true)?
                .into_iter()
                .map(|s| IndexedPackagePath {
                    package,
                    path: Symbol::from(s),
                    named_address_map: named_address_mapping,
                }),
        );
    }
    // sort the filenames so errors about redefinitions, or other inter-file conflicts, are
    // deterministic
    res.sort_by(|p1, p2| p1.path.cmp(&p2.path));
    Ok(res)
}

fn ensure_targets_deps_dont_intersect(
    compilation_env: &CompilationEnv,
    targets: &[IndexedPackagePath],
    deps: &mut Vec<IndexedPackagePath>,
) -> anyhow::Result<()> {
    /// Canonicalize a file path.
    fn canonicalize(path: &Symbol) -> String {
        let p = path.as_str();
        match std::fs::canonicalize(p) {
            Ok(s) => s.to_string_lossy().to_string(),
            Err(_) => p.to_owned(),
        }
    }
    let target_set = targets
        .iter()
        .map(|p| canonicalize(&p.path))
        .collect::<BTreeSet<_>>();
    let dep_set = deps
        .iter()
        .map(|p| canonicalize(&p.path))
        .collect::<BTreeSet<_>>();
    let intersection = target_set.intersection(&dep_set).collect::<Vec<_>>();
    if intersection.is_empty() {
        return Ok(());
    }
    if compilation_env.flags().sources_shadow_deps() {
        deps.retain(|p| !intersection.contains(&&canonicalize(&p.path)));
        return Ok(());
    }
    let all_files = intersection
        .into_iter()
        .map(|s| format!("    {}", s))
        .collect::<Vec<_>>()
        .join("\n");
    Err(anyhow!(
        "The following files were marked as both targets and dependencies:\n{}",
        all_files
    ))
}
