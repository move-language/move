// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! Tests of compilation from .move to LLVM IR.
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
//! cargo test -p move-mv-llvm-compiler --test move_to_llvm_tests
//! ```
//!
//! Running a specific test:
//!
//! ```
//! cargo test -p move-mv-llvm-compiler --test move_to_llvm_tests -- struct01.move
//! ```
//!
//! Promoting all results to expected results:
//!
//! ```
//! PROMOTE_LLVM_IR=1 cargo test -p move-mv-llvm-compiler --test move_to_llvm_tests
//! ```
//!
//! # Details
//!
//! They do the following:
//!
//! - Create a test for every .move file in move_to_llvm_tests/
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

use std::path::Path;

mod test_common;
use test_common as tc;

pub const TEST_DIR: &str = "tests/move-to-llvm-tests";

datatest_stable::harness!(run_test, TEST_DIR, r".*\.move$");

fn run_test(test_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    tc::setup_logging_for_test();
    Ok(run_test_inner(test_path)?)
}

fn run_test_inner(test_path: &Path) -> anyhow::Result<()> {
    let harness_paths = tc::get_harness_paths("move-compiler")?;
    let test_plan = tc::get_test_plan(test_path)?;

    if test_plan.should_ignore() {
        eprintln!("ignoring {}", test_plan.name);
        return Ok(());
    }

    tc::run_move_to_llvm_build(&harness_paths, &test_plan, vec![])?;

    tc::compare_results(&test_plan)?;

    Ok(())
}
