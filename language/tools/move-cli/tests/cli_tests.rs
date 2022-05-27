// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use std::{env, fs};
use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use move_cli::sandbox::commands::test;

use std::path::PathBuf;
use std::process::Stdio;
use home::home_dir;
use toml_edit::easy::Value;
use std::os::unix::fs::PermissionsExt;

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
    #[cfg(debug_assertions)]
    const CLI_EXE: &str = "../../../target/debug/move";
    #[cfg(not(debug_assertions))]
    const CLI_EXE: &str = "../../../target/release/move";
    let home = home_dir().unwrap().to_string_lossy().to_string() + "/.move/test";
    env::set_var("MOVE_HOME", &home);
    match std::process::Command::new(CLI_EXE)
        .current_dir(".")
        .args(["login"])
        .stdin(Stdio::piped()).spawn() {
            Ok(mut child) => {
                let token = "test_token";
                child.stdin.as_ref().unwrap().write(token.as_bytes()).unwrap();
                child.wait().unwrap();
                Ok(())
            }
            Err(error) => Err(error),
        }.unwrap();
    let contents = fs::read_to_string(home + "/credential.toml").expect("Unable to read file");
    let mut toml: Value = contents.parse().unwrap();
    let registry = toml.as_table_mut().unwrap().get_mut("registry").unwrap();
    let token = registry.as_table_mut().unwrap().get_mut("token").unwrap();
    assert!(token.to_string().contains("test_token"));
}

#[test]
fn save_credential_fails() {
    #[cfg(debug_assertions)]
    const CLI_EXE: &str = "../../../target/debug/move";
    #[cfg(not(debug_assertions))]
    const CLI_EXE: &str = "../../../target/release/move";
    let home = home_dir().unwrap().to_string_lossy().to_string() + "/.move/test";
    let credential_file = home.clone() + "/credential.toml";
    let _ = fs::remove_file(&credential_file);

    fs::create_dir_all(&home).unwrap();
    let file = File::create(&credential_file).unwrap();
    let mut perms = file.metadata().unwrap().permissions();
    perms.set_mode(0o000);
    file.set_permissions(perms).unwrap();

    env::set_var("MOVE_HOME", &home);
    match std::process::Command::new(CLI_EXE)
        .current_dir(".")
        .args(["login"])
        .stdin(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn() {
        Ok(mut child) => {
            let token = "test_token";
            child.stdin.as_ref().unwrap().write(token.as_bytes()).unwrap();
            child.wait().unwrap();
            let out = BufReader::new(child.stderr.take().unwrap());
            out.lines().for_each(|line|
                assert!(line.unwrap().contains("Error: Error reading input: Permission denied (os error 13)"))
            );
            Ok(())
        }
        Err(error) => Err(error),
    }.unwrap();

    let mut perms = file.metadata().unwrap().permissions();
    perms.set_mode(0o600);
    file.set_permissions(perms).unwrap();
    let _ = fs::remove_file(&credential_file);
}
