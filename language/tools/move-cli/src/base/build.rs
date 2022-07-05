// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use super::reroot_path;
use clap::*;
use move_package::{Architecture, BuildConfig};
use std::path::Path;

/// Build the package at `path`. If no path is provided defaults to current directory.
#[derive(Parser)]
#[clap(name = "build")]
pub struct Build;

impl Build {
    pub fn execute(self, path: &Path, config: BuildConfig) -> anyhow::Result<()> {
        let rerooted_path = reroot_path(path)?;
        let architecture = config.architecture.unwrap_or(Architecture::Move);

        match architecture {
            Architecture::Move | Architecture::AsyncMove => {
                config.compile_package(&rerooted_path, &mut std::io::stderr())?;
            }

            Architecture::Ethereum => {
                #[cfg(feature = "evm-backend")]
                config.compile_package_evm(&rerooted_path, &mut std::io::stderr())?;

                #[cfg(not(feature = "evm-backend"))]
                anyhow::bail!("The Ethereum architecture is not supported because move-cli was not compiled with feature flag `evm-backend`.");
            }
        }
        Ok(())
    }
}
