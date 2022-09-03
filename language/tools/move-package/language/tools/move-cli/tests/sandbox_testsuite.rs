// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use move_cli::sandbox::commands::test;

use std::path::{Path, PathBuf};

#[cfg(debug_assertions)]
const CLI_EXE: &str = "../../../target/debug/move";
#[cfg(not(debug_assertions))]
const CLI_EXE: &str = "../../../target/release/move";

fn run_all(args_path: &Path) -> datatest_stable::Result<()> {
    let use_temp_dir = !args_path.parent().unwrap().join("NO_TEMPDIR").exists();
    test::run_one(
        args_path,
        &PathBuf::from(CLI_EXE),
        /* use_temp_dir */ use_temp_dir,
        /* track_cov */ false,
    )?;
    Ok(())
}

// runs all the tests
datatest_stable::harness!(run_all, "tests/sandbox_tests", r"args\.txt$");
