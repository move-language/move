// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::Result;
use move_command_line_common::env::get_bytecode_version_from_env;
use move_compiler::{self, shared::NumericalAddress, Compiler, Flags};
use std::collections::BTreeMap;

/// Compile the user modules in `sources` against the dependencies in `interface_files`, placing
/// the resulting binaries in `output_dir`.
pub fn compile(
    interface_files: Vec<String>,
    output_dir: &str,
    sources_shadow_deps: bool,
    sources: Vec<String>,
    named_address_mapping: BTreeMap<String, NumericalAddress>,
    emit_source_map: bool,
    verbose: bool,
) -> Result<()> {
    if verbose {
        println!("Compiling Move files...");
    }
    let (files, compiled_units) =
        Compiler::from_files(sources, interface_files, named_address_mapping)
            .set_flags(Flags::empty().set_sources_shadow_deps(sources_shadow_deps))
            .build_and_report()?;
    move_compiler::output_compiled_units(
        get_bytecode_version_from_env(),
        emit_source_map,
        files,
        compiled_units,
        output_dir,
    )
}
