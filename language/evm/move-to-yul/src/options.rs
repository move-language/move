// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use codespan_reporting::diagnostic::Severity;
use structopt::StructOpt;

#[derive(StructOpt, Debug, Default)]
#[structopt(name = "move-to-yul", about = "Move Solidity Generator")]
pub struct Options {
    /// Directories where to lookup dependencies.
    #[structopt(short)]
    pub dependencies: Vec<String>,
    /// Named address mapping.
    #[structopt(short)]
    pub named_address_mapping: Vec<String>,
    /// Output file
    #[structopt(short, default_value = "output.yul")]
    pub output: String,
    /// Whether to dump bytecode to a file.
    #[structopt(long = "dump-bytecode")]
    pub dump_bytecode: bool,
    /// Sources to compile (positional arg)
    pub sources: Vec<String>,
}

impl Options {
    pub fn report_severity(&self) -> Severity {
        Severity::Warning
    }

    pub fn version(&self) -> &str {
        "0.0"
    }
}
