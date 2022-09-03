// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use std::collections::BTreeMap;

use anyhow::Result;
use move_compiler::{self, shared::NumericalAddress, Compiler, Flags};

/// Type-check the user modules in `files` and the dependencies in `interface_files`
pub fn check(
    interface_files: Vec<String>,
    sources_shadow_deps: bool,
    files: Vec<String>,
    named_addresses: BTreeMap<String, NumericalAddress>,
    verbose: bool,
) -> Result<()> {
    if verbose {
        println!("Checking Move files...");
    }
    Compiler::new(
        vec![(files, named_addresses.clone())],
        vec![(interface_files, named_addresses)],
    )
    .set_flags(Flags::empty().set_sources_shadow_deps(sources_shadow_deps))
    .check_and_report()?;
    Ok(())
}
