// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use codespan_reporting::diagnostic::Severity;
use move_command_line_common::env::read_env_var;
use structopt::StructOpt;

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
    #[structopt(long)]
    pub output: String,
    /// Solc executable
    #[structopt(long)]
    pub solc_exe: String,
    /// Whether to dump bytecode to a file.
    #[structopt(long = "dump-bytecode")]
    pub dump_bytecode: bool,
    /// Sources to compile (positional arg)
    pub sources: Vec<String>,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            dependencies: vec![],
            named_address_mapping: vec![],
            output: "output.yul".to_string(),
            solc_exe: read_env_var("SOLC_EXE"),
            dump_bytecode: false,
            sources: vec![],
        }
    }
}

impl Options {
    pub fn report_severity(&self) -> Severity {
        Severity::Warning
    }

    pub fn version(&self) -> &str {
        "0.0"
    }
}
