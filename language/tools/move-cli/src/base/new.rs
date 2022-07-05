// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use clap::*;
use move_package::source_package::layout::SourcePackageLayout;
use std::{fmt::Display, fs::create_dir_all, io::Write, path::Path};

pub const MOVE_STDLIB_PACKAGE_NAME: &str = "MoveStdlib";
pub const MOVE_STDLIB_PACKAGE_PATH: &str = "{ \
    git = \"https://github.com/move-language/move.git\", \
    subdir = \"language/move-stdlib\", rev = \"main\" \
}";
pub const MOVE_STDLIB_ADDR_NAME: &str = "std";
pub const MOVE_STDLIB_ADDR_VALUE: &str = "0x1";

/// Create a new Move package with name `name` at `path`. If `path` is not provided the package
/// will be created in the directory `name`.
#[derive(Parser)]
#[clap(name = "new")]
pub struct New {
    /// The name of the package to be created.
    name: String,
}

impl New {
    pub fn execute_with_defaults(self, path: &Path) -> anyhow::Result<()> {
        self.execute(
            path,
            "0.0.0",
            [(MOVE_STDLIB_PACKAGE_NAME, MOVE_STDLIB_PACKAGE_PATH)],
            [(MOVE_STDLIB_ADDR_NAME, MOVE_STDLIB_ADDR_VALUE)],
        )
    }

    pub fn execute(
        self,
        path: &Path,
        version: &str,
        deps: impl IntoIterator<Item = (impl Display, impl Display)>,
        addrs: impl IntoIterator<Item = (impl Display, impl Display)>,
    ) -> anyhow::Result<()> {
        // TODO warn on build config flags
        let Self { name } = self;
        create_dir_all(path.join(SourcePackageLayout::Sources.path()))?;
        let mut w = std::fs::File::create(path.join(SourcePackageLayout::Manifest.path()))?;
        writeln!(
            &mut w,
            "[package]
    name = \"{name}\"
    version = \"{version}\"

    [dependencies]
    "
        )?;
        for (dep_name, dep_val) in deps {
            writeln!(w, "{dep_name} = {dep_val}")?;
        }

        writeln!(
            w,
            "

    [addresses]
    "
        )?;
        for (addr_name, addr_val) in addrs {
            writeln!(w, "{addr_name} =  \"{addr_val}\"")?;
        }
        Ok(())
    }
}
