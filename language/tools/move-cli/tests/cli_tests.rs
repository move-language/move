// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use httpmock::prelude::*;
use move_cli::{sandbox::commands::test, utils::movey_credential::MOVEY_API_KEY_PATH};
use move_command_line_common::files::path_to_string;
use serde_json::json;
use std::{fs, path::PathBuf, process::Command};

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
        Command::new(cli_exe)
            .current_dir("./tests/cross_process_tests/Package1")
            .args(["package", "build"])
            .output()
            .expect("Package1 failed");
    });
    let cli_exe = env!("CARGO_BIN_EXE_move").to_string();
    Command::new(cli_exe)
        .current_dir("./tests/cross_process_tests/Package2")
        .args(["package", "build"])
        .output()
        .expect("Package2 failed");
    handle.join().unwrap();
}

const UPLOAD_PACKAGE_PATH: &str = "./tests/upload_tests";

#[test]
fn upload_package_to_movey_works() {
    let package_path = format!("{}/valid_package1", UPLOAD_PACKAGE_PATH);
    init_git(&package_path, true);
    let server = MockServer::start();
    server.mock(|when, then| {
        when.method(POST)
            .path("/api/v1/post_package/")
            .header("content-type", "application/json")
            .json_body(json!({
            "github_repo_url":"https://github.com/move-language/move",
            "total_files":2,
            "token":"test-token",
            "subdir":"\n"
            }));
        then.status(200);
    });
    init_stub_registry_file(&package_path, &server.base_url());
    let relative_package_path = PathBuf::from(&package_path);
    let absolute_package_path =
        path_to_string(&relative_package_path.canonicalize().unwrap()).unwrap();

    let cli_exe = env!("CARGO_BIN_EXE_move");
    let output = Command::new(cli_exe)
        .env("MOVE_HOME", &absolute_package_path)
        .current_dir(&absolute_package_path)
        .args(["movey-upload"])
        .output()
        .unwrap();

    assert!(output.status.success());
    let output = String::from_utf8_lossy(output.stdout.as_slice()).to_string();
    assert!(
        output.contains("Your package has been successfully uploaded to Movey"),
        "{}",
        output
    );

    clean_up(&absolute_package_path);
}

#[test]
fn upload_package_to_movey_prints_error_message_if_server_respond_4xx() {
    let package_path = format!("{}/valid_package2", UPLOAD_PACKAGE_PATH);
    init_git(&package_path, true);

    let server = MockServer::start();
    server.mock(|when, then| {
        when.method(POST)
            .path("/api/v1/post_package/")
            .header("content-type", "application/json")
            .json_body(json!({
            "github_repo_url":"https://github.com/move-language/move",
            "total_files":2,
            "token":"test-token",
            "subdir":"\n"
            }));
        then.status(400).body("Invalid Api token");
    });
    init_stub_registry_file(&package_path, &server.base_url());
    let relative_package_path = PathBuf::from(&package_path);
    let absolute_package_path =
        path_to_string(&relative_package_path.canonicalize().unwrap()).unwrap();

    let cli_exe = env!("CARGO_BIN_EXE_move");
    let output = Command::new(cli_exe)
        .env("MOVE_HOME", &absolute_package_path)
        .current_dir(&absolute_package_path)
        .args(["movey-upload"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let output = String::from_utf8_lossy(output.stderr.as_slice()).to_string();
    assert!(output.contains("Error: Invalid Api token"), "{}", output);

    clean_up(&absolute_package_path);
}

#[test]
fn upload_package_to_movey_prints_hardcoded_error_message_if_server_respond_5xx() {
    let package_path = format!("{}/valid_package3", UPLOAD_PACKAGE_PATH);
    init_git(&package_path, true);

    let server = MockServer::start();
    server.mock(|when, then| {
        when.method(POST)
            .path("/api/v1/post_package/")
            .header("content-type", "application/json")
            .json_body(json!({
            "github_repo_url":"https://github.com/move-language/move",
            "total_files":2,
            "token":"test-token",
            "subdir":"\n"
            }));
        then.status(500).body("Invalid Api token");
    });
    init_stub_registry_file(&package_path, &server.base_url());
    let relative_package_path = PathBuf::from(&package_path);
    let absolute_package_path =
        path_to_string(&relative_package_path.canonicalize().unwrap()).unwrap();

    let cli_exe = env!("CARGO_BIN_EXE_move");
    let output = Command::new(cli_exe)
        .env("MOVE_HOME", &absolute_package_path)
        .current_dir(&absolute_package_path)
        .args(["movey-upload"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let output = String::from_utf8_lossy(output.stderr.as_slice()).to_string();
    assert!(
        output.contains("Error: An unexpected error occurred. Please try again later"),
        "{}",
        output
    );

    clean_up(&absolute_package_path);
}

#[test]
fn upload_package_to_movey_with_no_remote_should_panic() {
    let package_path = format!("{}/no_git_remote_package", UPLOAD_PACKAGE_PATH);
    init_git(&package_path, false);

    let cli_exe = env!("CARGO_BIN_EXE_move");
    let output = Command::new(cli_exe)
        .current_dir(&package_path)
        .args(["movey-upload"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let error = String::from_utf8_lossy(output.stderr.as_slice()).to_string();
    assert!(error.contains("invalid git repository"));

    clean_up(&package_path);
}

// is_valid == true: all git commands are run
// is_valid == false: missing git remote add command
fn init_git(package_path: &str, is_valid: bool) {
    Command::new("git")
        .current_dir(package_path)
        .args(&["init"])
        .output()
        .unwrap();
    Command::new("git")
        .current_dir(package_path)
        .args(&["add", "."])
        .output()
        .unwrap();
    if is_valid {
        Command::new("git")
            .current_dir(package_path)
            .args(&[
                "remote",
                "add",
                "test-origin",
                "git@github.com:move-language/move.git",
            ])
            .output()
            .unwrap();
    }
}

fn clean_up(package_path: &str) {
    let _ = fs::remove_file(format!("{}/simple_file", package_path));
    fs::remove_dir_all(format!("{}/.git", package_path)).unwrap();
    let credential_path = format!("{}{}", package_path, MOVEY_API_KEY_PATH);
    let _ = fs::remove_file(&credential_path);
}

fn init_stub_registry_file(package_path: &str, base_url: &str) {
    let credential_path = format!("{}{}", package_path, MOVEY_API_KEY_PATH);
    let content = format!(
        r#"
        [registry]
        token = "test-token"
        url = "{}"
        "#,
        base_url
    );
    fs::write(credential_path, content).expect("Unable to write file");
}
