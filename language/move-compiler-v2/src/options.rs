// Copyright © Aptos Foundation
// Parts of the project are originally copyright © Meta Platforms, Inc.
// SPDX-License-Identifier: Apache-2.0

use clap::Parser;
use codespan_reporting::diagnostic::Severity;
use std::collections::BTreeSet;

/// Defines options for a run of the compiler.
#[derive(Parser, Clone, Debug)]
#[clap(author, version, about)]
pub struct Options {
    /// Directories where to lookup dependencies.
    #[clap(
        short,
        num_args = 0..
    )]
    pub dependencies: Vec<String>,
    /// Named address mapping.
    #[clap(
        short,
        num_args = 0..
    )]
    pub named_address_mapping: Vec<String>,
    /// Output directory.
    #[clap(short, long, default_value = "")]
    pub output_dir: String,
    /// Whether to dump intermediate bytecode for debugging.
    #[clap(long = "dump-bytecode")]
    pub dump_bytecode: bool,
    /// Do not complain about unknown attributes in Move code.
    #[clap(long, default_value = "false")]
    pub skip_attribute_checks: bool,
    /// Known attributes for this dialect of move; if empty, assumes third-party Move.
    /// Only used if skip_attribute_checks is false.
    #[clap(skip)]
    pub known_attributes: BTreeSet<String>,
    /// Whether we generate code for tests. This specifically guarantees stable output
    /// for baseline testing.
    #[clap(long)]
    pub testing: bool,
    /// Active experiments. Experiments alter default behavior of the compiler.
    /// See `Experiment` struct.
    #[clap(short)]
    #[clap(
        long = "experiment",
        num_args = 0..
    )]
    pub experiments: Vec<String>,
    /// Sources to compile (positional arg, therefore last)
    pub sources: Vec<String>,
}

impl Default for Options {
    fn default() -> Self {
        Parser::parse_from(std::iter::empty::<String>())
    }
}

impl Options {
    /// Returns the least severity of diagnosis which shall be reported.
    /// This is currently hardwired.
    pub fn report_severity(&self) -> Severity {
        Severity::Warning
    }

    /// Returns true if an experiment is on.
    pub fn experiment_on(&self, name: &str) -> bool {
        self.experiments.iter().any(|s| s == name)
    }
}
