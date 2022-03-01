// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::experiments::Experiment;
use codespan_reporting::diagnostic::Severity;
use structopt::StructOpt;

/// Options for a run of the compiler.
#[derive(StructOpt, Debug)]
#[structopt(name = "move-to-yul", about = "Move Solidity Generator")]
pub struct Options {
    /// Directories where to lookup dependencies.
    #[structopt(short)]
    pub dependencies: Vec<String>,
    /// Named address mapping.
    #[structopt(short)]
    pub named_address_mapping: Vec<String>,
    /// Output file name.
    #[structopt(short)]
    #[structopt(long, default_value = "output.yul")]
    pub output: String,
    /// Solc executable
    #[structopt(long, env = "SOLC_EXE", default_value = "solc")]
    pub solc_exe: String,
    /// Whether to dump bytecode to a file.
    #[structopt(long = "dump-bytecode")]
    pub dump_bytecode: bool,
    /// Whether we generate code for tests.
    #[structopt(long)]
    pub testing: bool,
    /// Active experiments.
    #[structopt(short)]
    #[structopt(long = "experiment")]
    pub experiments: Vec<String>,
    /// Sources to compile (positional arg)
    pub sources: Vec<String>,
}

impl Default for Options {
    fn default() -> Self {
        Options::from_iter(std::iter::empty::<String>())
    }
}

impl Options {
    /// Returns the least severity of diagnosis which shall be reported.
    pub fn report_severity(&self) -> Severity {
        Severity::Warning
    }

    /// Returns the version of the tool.
    pub fn version(&self) -> &str {
        "0.0"
    }

    /// Returns true if an experiment is on.
    pub fn experiment_on(&self, name: &str) -> bool {
        self.experiments.iter().any(|s| s == name)
    }

    /// Returns true if source info should be generated during tests.
    pub fn generate_source_info(&self) -> bool {
        !self.testing || self.experiment_on(Experiment::CAPTURE_SOURCE_INFO)
    }
}
