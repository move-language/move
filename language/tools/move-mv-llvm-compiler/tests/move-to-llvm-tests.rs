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

use similar::{ChangeTag, TextDiff};
use std::{
    fs,
    path::{Path, PathBuf},
};

mod test_common;
use tc::TestPlan;
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

    tc::run_move_to_llvm_build(&harness_paths, &test_plan)?;

    compare_results(&test_plan)?;

    Ok(())
}

fn compare_results(test_plan: &TestPlan) -> anyhow::Result<()> {
    let move_file = &test_plan.move_file;
    let build_dir = &test_plan.build_dir;

    if move_file.exists() && !build_dir.exists() {
        return Err(anyhow::anyhow!("Building directory does not exist"));
    }

    match find_matching_files(build_dir, "actual", "expected") {
        Ok(results) => {
            for result in results {
                match compare_actual_to_expected(result, test_plan) {
                    Ok(_) => {}
                    Err(error) => {
                        anyhow::bail!(error);
                    }
                }
            }
        }
        Err(error) => {
            eprintln!("Compare results failed: {}", error);
            anyhow::bail!("Test failed");
        }
    }

    Ok(())
}

struct ActualExpectedPair {
    actual: PathBuf,
    expected: PathBuf,
}

fn find_matching_files(
    directory: &Path,
    ext_actual: &str,
    ext_expected: &str,
) -> Result<Vec<ActualExpectedPair>, std::io::Error> {
    let mut result = vec![];

    let entries: Vec<_> = fs::read_dir(directory)
        .expect("Error reading directory")
        .filter_map(|result| result.ok())
        .filter(|e| e.path().extension().is_some())
        .map(|e| e.path())
        .collect();

    let entries_actual: Vec<_> = entries
        .iter()
        .filter(|p| {
            p.extension()
                .expect("Must be extension")
                .to_str()
                .unwrap()
                .eq(ext_actual)
        })
        .collect();
    let base_name = directory.to_str().unwrap();

    if std::env::var("PROMOTE_LLVM_IR").is_ok() {
        let string = format!("Copy actual to expected in directory: {}", base_name);
        println!("On demand: {}", string);
        for actual in entries_actual.iter().copied() {
            let mut expected = actual.clone();
            expected.set_extension(ext_expected);
            if fs::copy(actual, &expected).is_err() {
                let err_string = format!("Error while: {}", string);
                return Err(std::io::Error::new(std::io::ErrorKind::Other, err_string));
            }
        }
    }

    let entries_expected: Vec<_> = entries
        .iter()
        .filter(|p| {
            p.extension()
                .expect("Must be extension")
                .to_str()
                .unwrap()
                .eq(ext_expected)
        })
        .collect();

    let (actual_num, expected_num) = (entries_actual.len(), entries_expected.len());
    if actual_num != expected_num {
        let err_string = format!(
            "Did not match number of expected {} and actual results {}",
            expected_num, actual_num
        );
        return Err(std::io::Error::new(std::io::ErrorKind::Other, err_string));
    }
    if actual_num == 0 {
        let err_string = format!("Did not find expected or actual results for {}", base_name);
        return Err(std::io::Error::new(std::io::ErrorKind::Other, err_string));
    }

    for actual in entries_actual {
        let mut expected = actual.clone();
        expected.set_extension(ext_expected);
        let pair = ActualExpectedPair {
            actual: actual.clone(),
            expected,
        };
        result.push(pair);
    }

    Ok(result)
}

fn compare_actual_to_expected(
    pair: ActualExpectedPair,
    test_plan: &TestPlan,
) -> anyhow::Result<()> {
    let mut diff_msg = String::new();
    let file_actual = fs::read_to_string(pair.actual.as_os_str().to_str().unwrap())?;
    let file_expected = fs::read_to_string(pair.expected.as_os_str().to_str().unwrap())?;

    let diff = TextDiff::from_lines(&file_expected, &file_actual);
    for change in diff.iter_all_changes() {
        if change.value().contains("source_filename") {
            // depends of running system, ignore this
            continue;
        }
        let sign = match change.tag() {
            ChangeTag::Delete => Some("-"),
            ChangeTag::Insert => Some("+"),
            ChangeTag::Equal => None,
        };

        if let Some(sign) = sign {
            diff_msg.push_str(&format!("{}{}", sign, change));
        }
    }

    if !diff_msg.is_empty() {
        return test_plan.test_msg(format!(
            "LLVM IR actual ({:?}) does not equal expected: \n\n{}",
            file_actual, diff_msg
        ));
    } else {
        // If the test was expected to fail but it passed, then issue an error.
        let xfail = test_plan.xfail_message();
        if let Some(x) = xfail {
            anyhow::bail!(format!("Test expected to fail with: {}", x));
        }
    }

    Ok(())
}
