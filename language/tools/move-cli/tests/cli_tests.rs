// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use move_cli::sandbox::commands::test;
#[cfg(unix)]
use std::fs::File;
use std::io::Write;
use std::{env, fs};

use home::home_dir;
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use std::path::PathBuf;
use std::process::Stdio;
use toml_edit::easy::Value;

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

#[test]
fn save_credential_works() {
    let cli_exe = env!("CARGO_BIN_EXE_move");
    let (move_home, credential_path) = setup_move_home("/save_credential_works");
    assert!(fs::read_to_string(&credential_path).is_err());

    match std::process::Command::new(cli_exe)
        .current_dir(".")
        .args(["login", "--test", "--test-path", "/save_credential_works"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
    {
        Ok(child) => {
            let token = "test_token";
            child
                .stdin
                .as_ref()
                .unwrap()
                .write(token.as_bytes())
                .unwrap();
            match child.wait_with_output() {
                Ok(output) => {
                    assert!(String::from_utf8_lossy(&output.stdout).contains(
                        "Please paste the API Token found on \
                                https://movey-app-staging.herokuapp.com/settings/tokens below"
                    ));
                    Ok(())
                }
                Err(error) => Err(error),
            }
        }
        Err(error) => Err(error),
    }
    .unwrap();

    let contents = fs::read_to_string(&credential_path).expect("Unable to read file");
    let mut toml: Value = contents.parse().unwrap();
    let registry = toml.as_table_mut().unwrap().get_mut("registry").unwrap();
    let token = registry.as_table_mut().unwrap().get_mut("token").unwrap();
    assert!(token.to_string().contains("test_token"));

    clean_up(&move_home)
}

#[cfg(unix)]
#[test]
fn save_credential_fails_if_undeletable_credential_file_exists() {
    let cli_exe = env!("CARGO_BIN_EXE_move");
    let (move_home, credential_path) =
        setup_move_home("/save_credential_fails_if_undeletable_credential_file_exists");
    let file = File::create(&credential_path).unwrap();
    let mut perms = file.metadata().unwrap().permissions();
    perms.set_mode(0o000);
    file.set_permissions(perms).unwrap();

    match std::process::Command::new(cli_exe)
        .current_dir(".")
        .args([
            "login",
            "--test",
            "--test-path",
            "/save_credential_fails_if_undeletable_credential_file_exists",
        ])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
    {
        Ok(child) => {
            let token = "test_token";
            child
                .stdin
                .as_ref()
                .unwrap()
                .write(token.as_bytes())
                .unwrap();
            match child.wait_with_output() {
                Ok(output) => {
                    assert!(String::from_utf8_lossy(&output.stdout).contains(
                        "Please paste the API Token found on \
                                    https://movey-app-staging.herokuapp.com/settings/tokens below"
                    ));
                    assert!(String::from_utf8_lossy(&output.stderr)
                        .contains("Error: Error reading input: Permission denied (os error 13)"));
                    Ok(())
                }
                Err(error) => Err(error),
            }
        }
        Err(error) => Err(error),
    }
    .unwrap();

    let mut perms = file.metadata().unwrap().permissions();
    perms.set_mode(0o600);
    file.set_permissions(perms).unwrap();
    let _ = fs::remove_file(&credential_path);

    clean_up(&move_home)
}

fn setup_move_home(test_path: &str) -> (String, String) {
    let move_home = home_dir().unwrap().to_string_lossy().to_string() + "/.move" + test_path;
    let _ = fs::remove_dir_all(&move_home);
    fs::create_dir_all(&move_home).unwrap();
    let credential_path = move_home.clone() + "/credential.toml";
    return (move_home, credential_path);
}

fn clean_up(move_home: &str) {
    let _ = fs::remove_dir_all(move_home);
}
