// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! Tests of compilation from .move to LLVM IR with resolution against Move stdlib.
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
//! cargo test -p move-mv-llvm-compiler --test cli-tests
//! ```
//!
//! Running a specific test:
//!
//! ```
//! cargo test -p move-mv-llvm-compiler --test cli-tests -- basic-coin.move
//! ```
//!
//! Promoting all results to expected results:
//!
//! ```
//! PROMOTE_LLVM_IR=1 cargo test -p move-mv-llvm-compiler --test cli-tests
//! ```
//!
//! # Details
//!
//! They do the following:
//!
//! - Create a test for every .move file in cli-tests/, for example for test basic-coin.move
//! directort basic-coin-build is created.
//! - Run `move-mv-llvm-compiler` twice:
//! -- with dependency -p option set as relative path - sub-directory relative_path_results will be created;
//! -- with dependency -p option set as absolute path - sub-directory absolute_path_results will be created.
//! - Compare the actual IR to an existing expected IR in each directory.
//!
//! If the `PROMOTE_LLVM_IR` env var is set, the actual IR is promoted to the
//! expected IR.
//!

use std::{env, path::Path};

mod test_common;
use anyhow::bail;
use test_common as tc;

pub const TEST_DIR: &str = "tests/cli-tests";

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

    let current_dir = env::current_dir()
        .or_else(|err| bail!("Cannot get currecnt directory. Got error: {}", err))
        .unwrap();

    let test_name = &test_plan.name;

    let toml_dir: String;
    if let Some(pos) = test_name.rfind('.') {
        toml_dir = test_name[..pos].to_string();
    } else {
        bail!("No extension found in the filename {}", test_name);
    }

    let p_absolute_path = current_dir.join(&toml_dir).to_str().unwrap().to_owned();
    let p_relative_path = &toml_dir;

    /////////////////////
    // test relative path
    tc::run_move_to_llvm_build(
        &harness_paths,
        &test_plan,
        vec![&"-p".to_string(), &p_relative_path.to_string()],
    )?;

    tc::compare_results(&test_plan)?;

    let src = &test_plan.build_dir;
    let dst = &src.join("relative_path_results");
    tc::store_results(src, dst)?;

    /////////////////////
    // test absolute path
    tc::run_move_to_llvm_build(
        &harness_paths,
        &test_plan,
        vec![&"-p".to_string(), &p_absolute_path],
    )?;

    tc::compare_results(&test_plan)?;

    let src = &test_plan.build_dir;
    let dst = &src.join("absolute_path_results");
    tc::store_results(src, dst)?;

    tc::clean_results(src)?;

    Ok(())
}
