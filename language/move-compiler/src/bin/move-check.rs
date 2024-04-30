// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#![forbid(unsafe_code)]

use clap::*;
use move_command_line_common::files::verify_and_create_named_address_mapping;
use move_compiler::{
    command_line::{self as cli},
    shared::{self, known_attributes::KnownAttribute, Flags, NumericalAddress},
};

#[derive(Debug, Parser)]
#[clap(
    name = "move-check",
    about = "Check Move source code, without compiling to bytecode",
    author,
    version
)]
pub struct Options {
    /// The source files to check
    #[clap(
        name = "PATH_TO_SOURCE_FILE",
        num_args = 0..
    )]
    pub source_files: Vec<String>,

    /// The library files needed as dependencies
    #[clap(
        name = "PATH_TO_DEPENDENCY_FILE",
        short = cli::DEPENDENCY_SHORT,
        long = cli::DEPENDENCY,
    )]
    pub dependencies: Vec<String>,

    /// The output directory for saved artifacts, namely any 'move' interface files generated from
    /// 'mv' files
    #[clap(
        name = "PATH_TO_OUTPUT_DIRECTORY",
        short = cli::OUT_DIR_SHORT,
        long = cli::OUT_DIR,
    )]
    pub out_dir: Option<String>,

    /// Named address mapping
    #[clap(
        name = "NAMED_ADDRESSES",
        short = 'a',
        long = "addresses",
        value_parser = shared::parse_named_address
    )]
    pub named_addresses: Vec<(String, NumericalAddress)>,

    #[clap(flatten)]
    pub flags: Flags,
}

pub fn main() -> anyhow::Result<()> {
    let Options {
        source_files,
        dependencies,
        out_dir,
        flags,
        named_addresses,
    } = Options::parse();
    let named_addr_map = verify_and_create_named_address_mapping(named_addresses)?;

    let _files = move_compiler::Compiler::from_files(
        source_files,
        dependencies,
        named_addr_map,
        flags,
        KnownAttribute::get_all_attribute_names(),
    )
    .set_interface_files_dir_opt(out_dir)
    .check_and_report()?;
    Ok(())
}

#[test]
fn verify_tool() {
    use clap::CommandFactory;
    Options::command().debug_assert()
}
