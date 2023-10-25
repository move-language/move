// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! Tests of compilation generated debug information. Only *.dbg_info files are checked.
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
//! cargo test -p move-mv-llvm-compiler --test dwarf-tests
//! ```
//!
//! Running a specific test:
//!
//! ```
//! cargo test -p move-mv-llvm-compiler --test dwarf-tests -- basic-coin.move
//! ```
//!
//! # Details
//!
//! They do the following:
//!
//! - Create a test for every .move file in dwarf-tests/, for example for test basic-coin.move
//! directort basic-coin-build is created.
//! - Run `move-mv-llvm-compiler` with -g option. This will create *.dbg_info files.
//! - Compare the dbg_info.actual files with dbg_info.expected files.

use std::{env, path::Path};

mod test_common;
use anyhow::bail;
use test_common as tc;

pub const TEST_DIR: &str = "tests/dwarf-tests";

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

    let p_absolute_path = current_dir.join(toml_dir).to_str().unwrap().to_owned();

    let src = &test_plan.build_dir;
    let dst = &src.join("stored_results");

    tc::clean_results(src)?;
    std::fs::remove_dir_all(dst).ok();

    tc::run_move_to_llvm_build(
        &harness_paths,
        &test_plan,
        vec![&"-p".to_string(), &p_absolute_path, &"-g".to_string()],
    )?;

    // remove .actual files; this will not remove.dbg_info files
    tc::clean_results(src)?;

    let spot = current_dir
        .ancestors()
        .nth(3)
        .expect("Cannot go up in directory")
        .to_str()
        .unwrap();

    // dbg_info files contain lines with absolute path, since it is host dependent, remove prefix in all paths.
    match tc::list_files_with_extension(src, "debug_info") {
        Ok(files) => {
            for file in files {
                tc::filter_file(&file, spot, |line: &str, spot: &str| line.replace(spot, ""))?;
            }
        }
        Err(_err) => {}
    }

    rename_dwarf_files(&test_plan); // will rename *.actual.dbg_info to *.dbg_info.actual
    tc::compare_results(&test_plan)?;

    tc::store_results(src, dst)?;
    tc::clean_results(src)?;

    Ok(())
}

fn rename_dwarf_files(test_plan: &tc::TestPlan) {
    let build_dir = &test_plan.build_dir;

    match tc::list_files_with_extension(build_dir, "debug_info") {
        Ok(files) => {
            for file in files {
                tc::switch_last_two_extensions_and_rename(&file);
            }
        }
        Err(_err) => {}
    }
}
