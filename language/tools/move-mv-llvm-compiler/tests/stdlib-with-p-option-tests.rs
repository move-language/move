// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! Tests of compilation from .move to LLVM IR with resolution against Move stdlib,
//! where stdlib is defined as a user package (option -p).
//!
//! # Usage
//!
//! These tests require `move-compiler` to be pre-built:
//!
//! ```
//! cargo build -p move-compiler
//! ```
//!
//! Running the tests:
//!
//! ```
//! cargo test -p move-mv-llvm-compiler --test stdlib-with-p-option-tests
//! ```
//!
//! Running a specific test:
//!
//! ```
//! cargo test -p move-mv-llvm-compiler --test stdlib-with-p-option-tests -- hash_tests.move
//! ```
//!
//! Promoting all results to expected results:
//!
//! ```
//! PROMOTE_LLVM_IR=1 cargo test -p move-mv-llvm-compiler --test stdlib-with-p-option-tests
//! ```
//!
//! # Details
//!
//! They do the following:
//!
//! - Create a test for every .move file in stdlib-with-p-option-tests/
//! - Run `move-mv-llvm-compiler` to compile Move source to LLVM IR.
//! - Compare the actual IR to an existing expected IR.
//!
//! If the `PROMOTE_LLVM_IR` env var is set, the actual IR is promoted to the
//! expected IR.
//!
//! MVIR files may contain "test directives" instructing the harness
//! how to behave. These are specially-interpreted comments of the form
//!
//! - `// ignore` - don't run the test

use std::path::{Path, PathBuf};

mod test_common;
use test_common as tc;

pub const TEST_DIR: &str = "tests/stdlib-with-p-option-tests";

datatest_stable::harness!(run_test, TEST_DIR, r".*\.move$");

fn run_test(test_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    tc::setup_logging_for_test();
    Ok(run_test_inner(test_path)?)
}

fn run_test_inner(test_path: &Path) -> anyhow::Result<()> {
    let test_plan = tc::get_test_plan(test_path)?;

    if test_plan.should_ignore() {
        eprintln!("ignoring {}", test_plan.name);
        return Ok(());
    }

    let harness_paths = tc::get_harness_paths("move-compiler")?;
    let move_compiler_path = harness_paths
        .dep
        .canonicalize()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();
    let stdlib_path = &move_compiler_path.join(PathBuf::from("../../language/move-stdlib"));
    let stdlib_string = stdlib_path
        .canonicalize()
        .unwrap()
        .as_os_str()
        .to_str()
        .unwrap()
        .to_string();

    tc::run_move_to_llvm_build(
        &harness_paths,
        &test_plan,
        vec![
            &"-p".to_string(),
            &stdlib_string,
            &"--test".to_string(),
            &"--dev".to_string(),
        ],
    )?;

    tc::compare_results(&test_plan)?;

    Ok(())
}
