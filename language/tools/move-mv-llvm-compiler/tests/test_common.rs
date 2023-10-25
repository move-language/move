// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0
#![allow(dead_code)]
use anyhow::Context;
use core::result::Result::Ok;
use log::debug;
use rand::Rng;
use serde::{Deserialize, Serialize};
use serde_json::Result;
use std::{
    ffi::OsStr,
    fs::{self, File},
    io::{BufRead, BufReader, BufWriter, Write},
    path::{Path, PathBuf},
    process::Command,
};

pub fn setup_logging_for_test() {
    static LOGGER_INIT: std::sync::Once = std::sync::Once::new();
    LOGGER_INIT.call_once(|| {
        env_logger::init();
    });
}

#[derive(Debug)]
pub struct HarnessPaths {
    pub dep: PathBuf,
    pub move_mv_llvm_compiler: PathBuf,
    pub move_cli: PathBuf,
    /// The path to move-stdlib source code.
    pub stdlib_src_dir: PathBuf,
    pub move_native_source: PathBuf,
    pub move_native_archive: PathBuf,
}

pub fn get_harness_paths(dep: &str) -> anyhow::Result<HarnessPaths> {
    static BUILD_MOVE: std::sync::Once = std::sync::Once::new();
    BUILD_MOVE.call_once(|| {
        assert!(Command::new("cargo")
            .args([
                "build",
                "-p",
                "move-cli",
                "--bin",
                "move",
                "--features",
                "solana-backend"
            ])
            .status()
            .expect("Failed to build move-cli")
            .success());
    });
    static BUILD: std::sync::Once = std::sync::Once::new();
    BUILD.call_once(|| {
        build_crate(dep);
    });

    let build_name = if dep == "move-compiler" {
        "move-build"
    } else {
        dep
    };
    // Cargo will tell us the location of move-mv-llvm-compiler.
    let move_mv_llvm_compiler = env!("CARGO_BIN_EXE_move-mv-llvm-compiler");
    let move_mv_llvm_compiler = PathBuf::from(move_mv_llvm_compiler);

    // We have to guess where move-ir-compiler is
    let move_build = move_mv_llvm_compiler
        .with_file_name(build_name)
        .with_extension(std::env::consts::EXE_EXTENSION);

    if !move_build.exists() {
        // todo: can we build move-build automatically?

        let is_release = move_build.to_string_lossy().contains("release");
        let suggestion = if is_release {
            format!("try running `cargo build -p {dep} --release` first")
        } else {
            format!("try running `cargo build -p {dep}` first")
        };
        anyhow::bail!("{build_name} not built. {suggestion}");
    }

    let move_cli = move_mv_llvm_compiler
        .with_file_name("move")
        .with_extension(std::env::consts::EXE_EXTENSION);
    if !move_cli.exists() {
        // todo: can we build move-build automatically?

        let is_release = move_cli.to_string_lossy().contains("release");
        let suggestion = if is_release {
            "try running `cargo build -p move-cli --bin move --features solana-backend --release` first"
                .to_string()
        } else {
            "try running `cargo build -p move-cli --bin move --features solana-backend` first"
                .to_string()
        };
        anyhow::bail!("{build_name} not built. {suggestion}");
    }

    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").expect("cargo_manifest_dir");
    let manifest_dir = Path::new(&manifest_dir);
    let stdlib_src_dir = manifest_dir.join("../../../language/move-stdlib");
    let move_native_source = manifest_dir.join("../../../language/move-native");
    let move_native_archive = manifest_dir.join("../../../target");

    Ok(HarnessPaths {
        dep: move_build,
        move_mv_llvm_compiler,
        move_cli,
        stdlib_src_dir,
        move_native_source,
        move_native_archive,
    })
}

pub fn build_crate(crate_name: &str) {
    assert!(Command::new("cargo")
        .args(["build", "-p", crate_name])
        .status()
        .expect("Failed to build {crate_name}")
        .success());
}

#[derive(Debug)]
pub struct TestPlan {
    pub name: String,
    /// The move file to be compiled to LLVM IR
    pub move_file: PathBuf,
    /// The build directory, which contains bytecode for multiple modules and
    /// scripts.
    pub build_dir: PathBuf,
    /// Special commands embedded in the test file as comments
    pub directives: Vec<TestDirective>,
    /// The path to the move-stdlib bytecode build dir.
    /// This must be different for each test harness to avoid problems
    /// with concurrent file writes.
    pub stdlib_build_dir: PathBuf,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TestDirective {
    Ignore,          // Do not run the test.
    Signers(String), // List of signers to pass to compiler.
    Xfail(String), // The test is expected to fail with the `String` message. It is an error if test passes.
    Abort(u64),    // The test should abort.
    Log(String),   // Test should pass.
    Input(Input),
    UseStdlib, // Build and link the move stdlib as bytecode
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
pub struct Account {
    pub key: String,
    pub owner: Option<String>,
    pub is_signer: Option<bool>,
    pub is_writable: Option<bool>,
    pub lamports: Option<u64>,
    pub data: Option<Vec<u8>>,
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
pub struct Input {
    pub program_id: String,
    pub accounts: Vec<Account>,
    pub instruction_data: Vec<u8>,
}

impl TestPlan {
    pub fn should_ignore(&self) -> bool {
        self.directives.contains(&TestDirective::Ignore)
    }

    #[allow(unused)] // not used by all test harnesses
    pub fn abort_code(&self) -> Option<u64> {
        self.directives.iter().find_map(|d| match d {
            TestDirective::Abort(code) => Some(*code),
            _ => None,
        })
    }

    #[allow(unused)] // not used by all test harnesses
    pub fn signer_list(&self) -> Option<String> {
        self.directives.iter().find_map(|d| match d {
            TestDirective::Signers(s) => Some(s.clone()),
            _ => None,
        })
    }

    pub fn xfail_message(&self) -> Option<String> {
        self.directives.iter().find_map(|d| match d {
            TestDirective::Xfail(message) => Some(message.clone()),
            _ => None,
        })
    }

    pub fn test_msg(&self, msg: String) -> anyhow::Result<()> {
        let xfail = self.xfail_message();
        match xfail {
            Some(x) => {
                if x.contains(&msg) {
                    Ok(())
                } else {
                    anyhow::bail!(format!("XFAIL: expected: {}, got: {}", x, msg))
                }
            }
            _ => anyhow::bail!(msg),
        }
    }

    #[allow(unused)]
    pub fn expected_logs(&self) -> Vec<String> {
        self.directives
            .iter()
            .filter_map(|d| match d {
                TestDirective::Log(s) => Some(s.clone()),
                _ => None,
            })
            .collect()
    }
    pub fn test_root(&self) -> PathBuf {
        match std::env::current_dir() {
            Ok(path) => path,
            Err(_) => PathBuf::from("/"),
        }
    }

    pub fn use_stdlib(&self) -> bool {
        self.directives.contains(&TestDirective::UseStdlib)
    }
}

pub fn get_test_plan(test_path: &Path) -> anyhow::Result<TestPlan> {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").expect("cargo_manifest_dir");
    let move_file = Path::new(&manifest_dir).join(test_path);

    let name = test_path.to_string_lossy().to_string();
    let stem = move_file.file_stem().expect("stem").to_string_lossy();
    let build_dir = move_file.with_file_name(format!("{}-build", stem));
    let directives = load_directives(test_path)?;

    // For simplicity the stdlib is rebuilt for each test case that needs it.
    // Otherwise we would need to handle several different concurrency issues.
    let stdlib_build_dir = move_file.with_file_name(format!("{}-stdlib-build", stem));

    Ok(TestPlan {
        name,
        move_file,
        build_dir,
        directives,
        stdlib_build_dir,
    })
}

fn load_accounts(path: PathBuf) -> Result<Input> {
    debug!("Reading input file {path:?}");
    let file = fs::File::open(path).unwrap();
    let input: Input = serde_json::from_reader(file)?;
    debug!("Program input:");
    debug!("program_id: {}", &input.program_id);
    debug!("accounts {:?}", &input.accounts);
    debug!("instruction_data {:?}", &input.instruction_data);
    Ok(input)
}

fn load_directives(test_path: &Path) -> anyhow::Result<Vec<TestDirective>> {
    let mut directives = Vec::new();
    let source = std::fs::read_to_string(test_path)?;

    for line in source.lines() {
        let line = line.trim();
        let line_is_comment = line.starts_with("//");
        if !line_is_comment {
            continue;
        }
        let line = &line[2..].trim();
        if line.starts_with("ignore") {
            directives.push(TestDirective::Ignore);
        }
        if line.starts_with("xfail: ") {
            let msg = line["xfail:".len()..].trim();
            directives.push(TestDirective::Xfail(msg.to_string()));
        }
        if line.starts_with("abort ") {
            let code = line.split(' ').nth(1).expect("abort code");
            let code = code.parse().expect("u64");
            directives.push(TestDirective::Abort(code));
        }
        if line.starts_with("log ") {
            let s = line.strip_prefix("log ").unwrap().to_string();
            directives.push(TestDirective::Log(s.to_string()));
        }
        if line.starts_with("input ") {
            let filename = line.split(' ').nth(1).expect("input file name");
            let filename = test_path.parent().unwrap().join(filename);
            let input = load_accounts(filename).unwrap();
            directives.push(TestDirective::Input(input));
        }
        if line.starts_with("signers ") {
            let s = line.split(' ').nth(1).expect("signer list");
            directives.push(TestDirective::Signers(s.to_string()));
        }
        if line.starts_with("use-stdlib") {
            directives.push(TestDirective::UseStdlib);
        }
    }

    Ok(directives)
}

pub fn run_move_build(harness_paths: &HarnessPaths, test_plan: &TestPlan) -> anyhow::Result<()> {
    run_move_build_full(
        harness_paths,
        &[test_plan.move_file.clone()],
        &test_plan.build_dir,
        &[],
        &[],
    )
}

pub fn run_move_stdlib_build(harness_paths: &HarnessPaths, build_dir: &Path) -> anyhow::Result<()> {
    let stdlib_sources = {
        let stdlib_sources_dir = harness_paths.stdlib_src_dir.join("sources");

        fs::read_dir(stdlib_sources_dir)?
            .filter_map(|dentry| dentry.ok().map(|dentry| dentry.path()))
            .filter(|path| path.extension() == Some(OsStr::new("move")))
            .collect::<Vec<PathBuf>>()
    };

    run_move_build_full(
        harness_paths,
        &stdlib_sources,
        build_dir,
        &[("std", "0x01")],
        &[],
    )
}

pub fn run_move_build_with_deps(
    harness_paths: &HarnessPaths,
    test_plan: &TestPlan,
    deps: &[CompilationUnit],
) -> anyhow::Result<()> {
    let deps = deps
        .iter()
        .filter_map(|dep| {
            if dep.type_ == CompilationUnitType::Module {
                Some(dep.bytecode.clone())
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    run_move_build_full(
        harness_paths,
        &[test_plan.move_file.clone()],
        &test_plan.build_dir,
        &[],
        &deps,
    )
}

pub fn run_move_build_full(
    harness_paths: &HarnessPaths,
    move_files: &[PathBuf],
    build_dir: &Path,
    addresses: &[(&str, &str)],
    deps: &[PathBuf],
) -> anyhow::Result<()> {
    clean_build_dir(build_dir)?;
    let mut cmd = Command::new(&harness_paths.dep);
    cmd.args(move_files);
    cmd.args(["--flavor", "none"]);
    cmd.args(["--out-dir", build_dir.to_str().expect("utf-8")]);

    for (name, addr) in addresses {
        cmd.arg("--addresses");
        cmd.arg(&format!("{name}={addr}"));
    }

    for dep in deps {
        cmd.arg("--dependency");
        cmd.arg(dep);
    }

    let output = cmd.output()?;
    if !output.status.success() {
        anyhow::bail!(
            "move-build failed.\ncommand:\n\n{:?}\n\nstdout:\n{}\n\nstderr:\n{}",
            cmd,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(())
}

pub fn run_move_to_llvm_build(
    harness_paths: &HarnessPaths,
    test_plan: &TestPlan,
    extra_params: Vec<&String>,
) -> anyhow::Result<()> {
    clean_build_dir(&test_plan.build_dir)?;
    let mut cmd = Command::new(&harness_paths.move_mv_llvm_compiler);
    let test = test_plan.move_file.to_str().expect("utf-8");
    cmd.args(["-c", test]);
    cmd.args(["--extension", "ll.actual"]);

    for param in extra_params {
        cmd.arg(param);
    }
    cmd.arg("-S");

    fs::create_dir_all(test_plan.build_dir.to_str().unwrap()).expect("Directory does not exist");
    cmd.args(["-o", test_plan.build_dir.to_str().expect("utf-8")]);

    let output = cmd.output()?;

    if !output.status.success() {
        anyhow::bail!(
            "move-llvm-build failed.\ncommand:\n\n{:?}\n\nstdout:\n{}\n\nstderr:\n{}",
            cmd,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(())
}

pub fn run_move_build_to_solana(
    harness_paths: &HarnessPaths,
    test_plan: &TestPlan,
    extra_params: Vec<&String>,
) -> anyhow::Result<()> {
    clean_build_dir(&test_plan.build_dir)?;
    let mut cmd = Command::new(&harness_paths.move_cli);
    cmd.env("MOVE_NATIVE", &harness_paths.move_native_source);
    let test = test_plan.move_file.to_str().expect("utf-8");
    let test = Path::new(test).parent().unwrap().parent().unwrap();
    cmd.args(["build", "--arch", "solana", "-p", &test.to_string_lossy()]);

    for param in extra_params {
        cmd.arg(param);
    }

    debug!("Running {cmd:?}");
    let output = cmd.output()?;

    if !output.status.success() {
        anyhow::bail!(
            "move-solana-build failed.\ncommand:\n\n{:?}\n\nstdout:\n{}\n\nstderr:\n{}",
            cmd,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(())
}

#[derive(Debug)]
pub struct CompilationUnit {
    pub type_: CompilationUnitType,
    pub bytecode: PathBuf,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum CompilationUnitType {
    Script,
    Module,
}

/// Return all paths to all bytecode modules.
///
/// They are ordered topologically by dependency graph,
/// as required by the move model.
pub fn find_compilation_units(build_dir: &Path) -> anyhow::Result<Vec<CompilationUnit>> {
    let modules_dir = build_dir.join("modules");
    let scripts_dir = build_dir.join("scripts");

    let dirs = [
        (modules_dir, CompilationUnitType::Module),
        (scripts_dir, CompilationUnitType::Script),
    ];

    let mut units = vec![];

    for (dir, type_) in dirs {
        if !dir.exists() {
            continue;
        }

        let mut paths = vec![];

        for dirent in fs::read_dir(&dir)? {
            let dirent = dirent?;
            let path = dirent.path();
            if path.extension() != Some(OsStr::new("mv")) {
                continue;
            }

            paths.push(path);
        }

        // The move compiler conveniently outputs modules with topo-sorted names!
        // So we just have to sort the filenames and we've got them in the correct order.
        paths.sort();

        for path in paths {
            let bytecode = path;
            units.push(CompilationUnit { type_, bytecode });
        }
    }

    Ok(units)
}

fn clean_build_dir(build_dir: &Path) -> anyhow::Result<()> {
    let modules_dir = build_dir.join("modules");
    let scripts_dir = build_dir.join("scripts");

    for dir in [modules_dir, scripts_dir] {
        if !dir.exists() {
            continue;
        }

        for dirent in fs::read_dir(&dir)? {
            let dirent = dirent?;
            let path = dirent.path();
            if path.extension() == Some(OsStr::new("mv")) {
                fs::remove_file(&path)?;
            }
        }
    }

    Ok(())
}

pub fn compile_all_bytecode(
    harness_paths: &HarnessPaths,
    compilation_units: &[CompilationUnit],
    signers: Option<String>,
    outtype_flag: &str,
    outfile: &dyn Fn(&CompilationUnit) -> PathBuf,
) -> anyhow::Result<()> {
    // compilation_units is sorted by dependencies
    let compilation_units_with_deps: Vec<(&CompilationUnit, Vec<&CompilationUnit>)> =
        compilation_units
            .iter()
            .enumerate()
            .map(|(i, cu)| {
                let deps: Vec<_> = compilation_units.iter().take(i).collect();
                (cu, deps)
            })
            .collect();

    for (cu, deps) in compilation_units_with_deps {
        let mut cmd = Command::new(&harness_paths.move_mv_llvm_compiler);

        // Add signers for testing/debugging. These are gathered from the `signers` directive
        // in a particular testcase.
        if let Some(ref s) = signers {
            cmd.arg("--signers");
            cmd.arg(s);
        }

        cmd.arg("-b");
        cmd.arg(&cu.bytecode);
        cmd.arg("-o");
        cmd.arg(&outfile(cu));
        cmd.arg(outtype_flag);

        if cu.type_ == CompilationUnitType::Script {
            cmd.arg("-s");
        }

        for dep in deps {
            cmd.arg("-d");
            cmd.arg(&dep.bytecode);
        }

        debug!("Running {cmd:?}");
        if !std::env::args().any(|arg| arg == "--nocapture") {
            let output = cmd.output().context("run move-mv-llvm-compiler failed")?;
            if !output.status.success() {
                anyhow::bail!(
                    "move-mv-llvm-compiler failed. stderr:\n\n{}",
                    String::from_utf8_lossy(&output.stderr)
                );
            }
        } else {
            let status = cmd.status().context("run move-mv-llvm-compiler failed")?;
            if !status.success() {
                anyhow::bail!("move-mv-llvm-compiler failed");
            }
        };
    }

    Ok(())
}

//
// Common for tests of category move_to_llvm_
//
use similar::{ChangeTag, TextDiff};

pub fn compare_results(test_plan: &TestPlan) -> anyhow::Result<()> {
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
) -> anyhow::Result<Vec<ActualExpectedPair>, std::io::Error> {
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

pub fn store_results<P: AsRef<Path>>(source: P, destination: P) -> std::io::Result<()> {
    fs::create_dir_all(&destination)?;

    for entry in fs::read_dir(source)? {
        let entry = entry?;
        let entry_path = entry.path();

        let dest_path = destination.as_ref().join(entry_path.file_name().unwrap());

        if entry_path.is_file() {
            fs::copy(&entry_path, dest_path)?;
        }
    }

    Ok(())
}

// Removes files with extension "actual"
pub fn clean_results<P: AsRef<Path>>(source: P) -> std::io::Result<()> {
    for entry in fs::read_dir(source)? {
        let entry = entry?;
        let entry_path = entry.path();

        if entry_path.is_file() {
            if let Some(ext) = entry_path.extension() {
                if ext == "actual" {
                    fs::remove_file(&entry_path)?;
                }
            }
        }
    }

    Ok(())
}

pub fn switch_last_two_extensions_and_rename(path: &Path) {
    if let Some(filename) = path.file_name() {
        if let Some(filename_str) = filename.to_str() {
            let mut parts: Vec<&str> = filename_str.split('.').collect();

            if parts.len() >= 3 {
                // Swap the last two extensions
                let last_ext = parts.pop().unwrap();
                let second_last_ext = parts.pop().unwrap();
                parts.push(last_ext);
                parts.push(second_last_ext);

                let new_filename = parts.join(".");

                let mut new_path = path.to_path_buf();
                new_path.set_file_name(new_filename);

                fs::rename(path, new_path).expect("Error in renameing");
            }
        }
    }
}

use std::ffi::OsString;
pub fn list_files_with_extension(
    directory_path: &Path,
    extension: &str,
) -> std::io::Result<Vec<PathBuf>> {
    let dir = fs::read_dir(directory_path)?;
    let dummy = OsString::from(extension.to_owned() + "dummy");
    let dummy_ext = dummy.as_os_str();

    let file_names: Vec<PathBuf> = dir
        .filter_map(anyhow::Result::ok)
        .filter(|f| extension == f.path().extension().unwrap_or(dummy_ext))
        .map(|f| f.path())
        .collect();

    Ok(file_names)
}

pub fn filter_file<F>(file_path: &PathBuf, filter_key: &str, mut filter: F) -> std::io::Result<()>
where
    F: FnMut(&str, &str) -> String,
{
    // Open the original file for reading
    let input_file = File::open(file_path)?;
    let input_file_reader = BufReader::new(input_file);

    // Create a temporary file for writing the modified content
    let random_extension = generate_random_string();
    let tmp_file_path = file_path.with_extension(random_extension);
    let tmp_file = File::create(&tmp_file_path)?;
    let mut tmp_file_writer = BufWriter::new(tmp_file);

    for line in input_file_reader.lines() {
        let original_line = line?;
        let modified_line = filter(&original_line, filter_key);

        // Write the modified line to the temporary file
        writeln!(tmp_file_writer, "{}", modified_line)?;
    }

    // Replace the original file with the temporary file
    std::fs::rename(&tmp_file_path, file_path)?;

    Ok(())
}

fn generate_random_number() -> u32 {
    let mut rng = rand::thread_rng();
    rng.gen::<u32>()
}

fn generate_random_string() -> String {
    let rand = generate_random_number();
    let charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    let charset_length = charset.len();
    let length = rand as usize % charset_length;

    let mut rng = rand::thread_rng();
    let random_string: String = (0..length)
        .map(|_| {
            let index = rng.gen_range(0..charset_length);
            charset.chars().nth(index).unwrap()
        })
        .collect();

    random_string
}
