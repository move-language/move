// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

pub mod build;
pub mod coverage;
pub mod disassemble;
pub mod errmap;
pub mod info;
pub mod new;
pub mod prove;
pub mod test;

use move_package::source_package::layout::SourcePackageLayout;
use std::path::{Path, PathBuf};

pub fn reroot_path(path: &Path) -> anyhow::Result<PathBuf> {
    // Always root ourselves to the package root, and then compile relative to that.
    let rooted_path = SourcePackageLayout::try_find_root(&path.canonicalize()?)?;
    std::env::set_current_dir(&rooted_path).unwrap();

    Ok(PathBuf::from("."))
}
