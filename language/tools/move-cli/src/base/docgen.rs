// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::base::prove::{Prove, ProverOptions};
use clap::*;
use move_package::BuildConfig;
use std::path::PathBuf;

/// Generating javadoc style documentation for packages using the Move Prover
#[derive(Parser)]
#[clap(name = "docgen")]
pub struct Docgen {
    /// Do not print out other than errors
    #[clap(long = "quiet", short = 'q')]
    pub quiet: bool,
    /// A template for documentation generation.
    #[clap(long = "template", short = 't', value_name = "FILE")]
    pub template: Option<String>,
}

impl Docgen {
    /// Simply calling the Move Prover with "--docgen"
    /// Forwarding `--docgen-template` and setting `--verbose` to `error` if applicable
    pub fn execute(self, path: Option<PathBuf>, config: BuildConfig) -> anyhow::Result<()> {
        let mut options_list = vec!["--docgen".to_string()];

        if self.quiet {
            options_list.push("--verbose".to_string());
            options_list.push("error".to_string());
        }
        if self.template.is_some() {
            options_list.push("--docgen-template".to_string());
            options_list.push(self.template.unwrap());
        }

        let options = Some(ProverOptions::Options(options_list));

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
