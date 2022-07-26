// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::base::prove::{Prove, ProverOptions};
use clap::*;
use move_package::BuildConfig;
use std::path::PathBuf;

/// Generating javadoc style documentation for packages using the Move Prover
#[derive(Parser)]
#[clap(name = "docgen")]
pub struct Docgen;

impl Docgen {
    /// Simply calling the Move Prover with "--docgen"
    pub fn execute(self, path: Option<PathBuf>, config: BuildConfig) -> anyhow::Result<()> {
        let options = Some(ProverOptions::Options(vec!["--docgen".to_string()]));
        Prove::execute(
            Prove {
                target_filter: None,
                for_test: false,
                options,
            },
            path,
            config,
        )
    }
}
