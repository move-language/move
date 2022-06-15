// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use std::fs;
use move_cli::sandbox::commands::test;

use std::path::{Path, PathBuf};
use std::process::Command;

pub const CLI_METATEST_PATH: [&str; 3] = ["tests", "metatests", "args.txt"];

fn get_cli_binary_path() -> PathBuf {
    let cli_exe = env!("CARGO_BIN_EXE_move");
    PathBuf::from(cli_exe)
}

fn get_metatest_path() -> PathBuf {
    CLI_METATEST_PATH.iter().collect()
}

#[test]
fn run_metatest() {
    let path_cli_binary = get_cli_binary_path();
    let path_metatest = get_metatest_path();

    // local workspace + with coverage
    assert!(test::run_all(&path_metatest, path_cli_binary.as_path(), false, true).is_ok());

    // temp workspace + with coverage
    assert!(test::run_all(&path_metatest, &path_cli_binary, true, true).is_ok());

    // local workspace + without coverage
    assert!(test::run_all(&path_metatest, &path_cli_binary, false, false).is_ok());

    // temp workspace + without coverage
    assert!(test::run_all(&path_metatest, &path_cli_binary, true, false).is_ok());
}

#[test]
fn cross_process_locking_git_deps() {
    let cli_exe = env!("CARGO_BIN_EXE_move");
    let handle = std::thread::spawn(move || {
        std::process::Command::new(cli_exe)
            .current_dir("./tests/cross_process_tests/Package1")
            .args(["package", "build"])
            .output()
            .expect("Package1 failed");
    });
    let cli_exe = env!("CARGO_BIN_EXE_move").to_string();
    std::process::Command::new(cli_exe)
        .current_dir("./tests/cross_process_tests/Package2")
        .args(["package", "build"])
        .output()
        .expect("Package2 failed");
    handle.join().unwrap();
}

const PACKAGE_PATH: &str = "./tests/upload_tests/valid_package";
#[cfg(debug_assertions)]
const CLI_EXE: &str = "../../../../../../target/debug/move";
#[cfg(not(debug_assertions))]
const CLI_EXE: &str = "../../../../../../target/release/move";

#[test]
fn upload_package_to_movey_works() {
    init_git(PACKAGE_PATH, 0);
    let output = Command::new(CLI_EXE)
        .current_dir(PACKAGE_PATH)
        .args(["package", "upload", "--test"])
        .output()
        .unwrap();
    assert!(output.status.success());
    let res_path = format!("{}{}", PACKAGE_PATH, "/request-body.txt");
    let data = fs::read_to_string(&res_path).unwrap();
    assert!(data.contains("rev"));
    assert!(data.contains("\"github_repo_url\":\"https://github.com/diem/move\""));
    assert!(data.contains("\"description\":\"Description test\""));
    fs::remove_file(&res_path).unwrap();
}

#[test]
fn upload_package_to_movey_with_no_remote_should_panic() {
    init_git(PACKAGE_PATH, 1);
    let output = Command::new(CLI_EXE)
        .current_dir(PACKAGE_PATH)
        .args(["package", "upload", "--test"])
        .output()
        .unwrap();
    assert!(!output.status.success());
    let error = String::from_utf8_lossy(output.stderr.as_slice()).to_string();
    assert!(error.contains("invalid git repository"));
}

#[test]
fn upload_package_to_movey_with_no_head_commit_id_should_panic() {
    init_git(PACKAGE_PATH, 2);
    let output = Command::new(CLI_EXE)
        .current_dir(PACKAGE_PATH)
        .args(["package", "upload", "--test"])
        .output()
        .unwrap();
    assert!(!output.status.success());
    let error = String::from_utf8_lossy(output.stderr.as_slice()).to_string();
    assert!(error.contains("invalid HEAD commit id"));
}

// flag == 0: all git command are run
// flag == 1: missing git add remote command
// flag == 2: missing git commit command
fn init_git(package_path: &str, flag: i32) {
    let git_path = format!("{}{}", package_path, "/.git");
    if Path::new::<String>(&git_path).exists() {
        fs::remove_dir_all(&git_path).unwrap();
    }
    Command::new("git")
        .current_dir(package_path)
        .args(&["init"])
        .output().unwrap();
    if flag != 1 {
        Command::new("git")
            .current_dir(package_path)
            .args(&["remote", "add", "origin", "git@github.com:diem/move.git"])
            .output().unwrap();
    }
    Command::new("touch")
        .current_dir(package_path)
        .args(&["simplefile"])
        .output().unwrap();
    Command::new("git")
        .current_dir(package_path)
        .args(&["add", "simplefile"])
        .output().unwrap();
    if flag != 2{
        Command::new("git")
            .current_dir(package_path)
            .args(&["commit", "-m", "initial commit"])
            .output().unwrap();
    }
}
