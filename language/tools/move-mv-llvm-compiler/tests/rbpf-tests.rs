// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::Context;
use extension_trait::extension_trait;
use log::*;
use solana_bpf_loader_program::{
    create_vm, load_program_from_bytes,
    serialization::serialize_parameters,
    syscalls::{create_program_runtime_environment_v1, SyscallError},
};
use solana_program_runtime::{
    invoke_context::InvokeContext,
    loaded_programs::{LoadProgramMetrics, LoadedProgramType},
    with_mock_invoke_context,
};
use solana_rbpf::{elf::Executable, static_analysis::Analysis};
use solana_sdk::{
    account::AccountSharedData,
    bpf_loader_upgradeable,
    pubkey::Pubkey,
    slot_history::Slot,
    transaction_context::{IndexOfAccount, InstructionAccount},
};
use std::{
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
};

mod test_common;
use test_common as tc;

pub const TEST_DIR: &str = "tests/rbpf-tests";

// The rbpf-tests directory contains multiple test formats,
// and build artifacts for those tests,
// and this regex is complicated by filtering tests from non-tests.
//
// There are two types of tests in `rbpf-tests`:
//
// - Single .move files under the rbpf-tests directory.
// - Move projects - these are direct subdirectories of rbpf-tests,
//   and identified by a single .move file under their own `sources` directory.
//
// Tests' build artifacts are emitted to directories that end in `-build`,
// some of those artifacts are also .move files,
// and we need to avoid datatest_stable picking up any files in them as tests.
//
// This regex says
//
// - `rbpf-tests/` - only paths somewhere under `rbpf-tests/`
// - `([^/]*/sources/)?` - maybe a single-level directory name followed by
//    a `sources` directory, to find the Move projects.
// - `[^/]*\.move$` - a file name without `/` in it that ends in `.move`.
pub const TEST_REGEX: &str = r"rbpf-tests/([^/]*/sources/)?[^/]*\.move$";

datatest_stable::harness!(run_test, TEST_DIR, TEST_REGEX);

fn run_test(test_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    tc::setup_logging_for_test();
    Ok(run_test_inner(test_path)?)
}

fn run_test_inner(test_path: &Path) -> anyhow::Result<()> {
    let sbf_tools = get_sbf_tools()?;
    let runtime = get_runtime(&sbf_tools)?;

    let harness_paths = tc::get_harness_paths("move-compiler")?;
    let test_plan = tc::get_test_plan(test_path)?;

    if test_plan.should_ignore() {
        eprintln!("ignoring {}", test_plan.name);
        return Ok(());
    }

    if test_path.parent().unwrap().ends_with("sources") {
        test_package(
            &harness_paths,
            &test_plan,
            test_path.parent().unwrap().parent().unwrap(),
        )?;
        return Ok(());
    }

    let compilation_units = if test_plan.use_stdlib() {
        tc::run_move_stdlib_build(&harness_paths, &test_plan.stdlib_build_dir)?;
        let stdlib_compilation_units = tc::find_compilation_units(&test_plan.stdlib_build_dir)?;

        tc::run_move_build_with_deps(&harness_paths, &test_plan, &stdlib_compilation_units)?;
        let test_compilation_units = tc::find_compilation_units(&test_plan.build_dir)?;

        stdlib_compilation_units
            .into_iter()
            .chain(test_compilation_units)
            .collect::<Vec<_>>()
    } else {
        tc::run_move_build(&harness_paths, &test_plan)?;
        tc::find_compilation_units(&test_plan.build_dir)?
    };

    let signers = test_plan.signer_list();
    compile_all_bytecode_to_object_files(&harness_paths, &compilation_units, signers)?;

    let exe = link_object_files(&test_plan, &sbf_tools, &compilation_units, &runtime)?;

    run_rbpf(&test_plan, &exe)?;

    Ok(())
}

#[extension_trait]
impl CompilationUnitExt for tc::CompilationUnit {
    fn object_file(&self) -> PathBuf {
        self.bytecode.with_extension("o")
    }
}

fn compile_all_bytecode_to_object_files(
    harness_paths: &tc::HarnessPaths,
    compilation_units: &[tc::CompilationUnit],
    signers: Option<String>,
) -> anyhow::Result<()> {
    tc::compile_all_bytecode(harness_paths, compilation_units, signers, "-O", &|cu| {
        cu.object_file()
    })
}

struct PlatformTools {
    _root: PathBuf,
    clang: PathBuf,
    rustc: PathBuf,
    cargo: PathBuf,
    lld: PathBuf,
}

fn get_sbf_tools() -> anyhow::Result<PlatformTools> {
    let sbf_tools_root =
        std::env::var("PLATFORM_TOOLS_ROOT").context("env var PLATFORM_TOOLS_ROOT not set")?;
    let sbf_tools_root = PathBuf::from(sbf_tools_root);

    let sbf_tools = PlatformTools {
        _root: sbf_tools_root.clone(),
        clang: sbf_tools_root
            .join("llvm/bin/clang")
            .with_extension(std::env::consts::EXE_EXTENSION),
        rustc: sbf_tools_root
            .join("rust/bin/rustc")
            .with_extension(std::env::consts::EXE_EXTENSION),
        cargo: sbf_tools_root
            .join("rust/bin/cargo")
            .with_extension(std::env::consts::EXE_EXTENSION),
        lld: sbf_tools_root.join("llvm/bin/ld.lld"),
    };

    if !sbf_tools.clang.exists() {
        anyhow::bail!("no clang bin at {}", sbf_tools.clang.display());
    }
    if !sbf_tools.rustc.exists() {
        anyhow::bail!("no rustc bin at {}", sbf_tools.rustc.display());
    }
    if !sbf_tools.cargo.exists() {
        anyhow::bail!("no cargo bin at {}", sbf_tools.cargo.display());
    }
    if !sbf_tools.lld.exists() {
        anyhow::bail!("no lld bin at {}", sbf_tools.lld.display());
    }

    Ok(sbf_tools)
}

struct Runtime {
    /// The path to the Rust staticlib (.a) file
    archive_file: PathBuf,
}

fn get_runtime(sbf_tools: &PlatformTools) -> anyhow::Result<Runtime> {
    static BUILD: std::sync::Once = std::sync::Once::new();

    BUILD.call_once(|| {
        eprintln!("building move-native runtime for sbf");

        let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").expect("cargo manifest dir");
        let manifest_dir = PathBuf::from(manifest_dir);
        let move_native = manifest_dir
            .join("../../../language/move-native/Cargo.toml")
            .to_string_lossy()
            .to_string();

        // Using `cargo rustc` to compile move-native as a staticlib.
        // See move-native documentation on `no-std` compatibilty for explanation.
        // Release mode is required to eliminate large stack frames.
        let res = sbf_tools.run_cargo(&[
            "rustc",
            "--crate-type=staticlib",
            "-p",
            "move-native",
            "--target",
            "sbf-solana-solana",
            "--manifest-path",
            &move_native,
            "--release",
            "--features",
            "solana",
        ]);

        if let Err(e) = res {
            panic!("{e}");
        }
    });

    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").expect("cargo manifest dir");
    let manifest_dir = PathBuf::from(manifest_dir);
    let archive_file = manifest_dir
        .join("tests/cargo-target-dir")
        .join("sbf-solana-solana/")
        .join("release/libmove_native.a");

    if !archive_file.exists() {
        anyhow::bail!("native runtime not found at {archive_file:?}. this is a bug");
    }

    Ok(Runtime { archive_file })
}

impl PlatformTools {
    fn run_cargo(&self, args: &[&str]) -> anyhow::Result<()> {
        let target_dir = {
            let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").expect("cargo manifest dir");
            let manifest_dir = PathBuf::from(manifest_dir);
            manifest_dir.join("tests/cargo-target-dir")
        };

        let mut cmd = Command::new(&self.cargo);
        cmd.env_remove("RUSTUP_TOOLCHAIN");
        cmd.env_remove("RUSTC_WRAPPER");
        cmd.env_remove("RUSTC_WORKSPACE_WRAPPER");
        cmd.env("CARGO_TARGET_DIR", &target_dir);
        cmd.env("CARGO", &self.cargo);
        cmd.env("RUSTC", &self.rustc);
        cmd.env("CARGO_PROFILE_DEV_PANIC", "abort");
        cmd.env("CARGO_PROFILE_RELEASE_PANIC", "abort");
        cmd.args(args);

        let status = cmd.status()?;
        if !status.success() {
            anyhow::bail!("running SBF cargo failed");
        }

        Ok(())
    }
}

fn link_object_files(
    test_plan: &tc::TestPlan,
    sbf_tools: &PlatformTools,
    compilation_units: &[tc::CompilationUnit],
    runtime: &Runtime,
) -> anyhow::Result<PathBuf> {
    let link_script = {
        let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").expect("cargo manifest dir");
        let manifest_dir = PathBuf::from(manifest_dir);
        let link_script = manifest_dir.join("tests/sbf-link-script.ld");
        link_script.to_string_lossy().to_string()
    };

    let output_dylib = test_plan.build_dir.join("output.so");

    let mut cmd = Command::new(&sbf_tools.lld);
    cmd.arg("--threads=1");
    cmd.arg("-znotext");
    cmd.arg("-znoexecstack");
    cmd.args(["--script", &link_script]);
    cmd.arg("--gc-sections");
    cmd.arg("--shared");
    cmd.arg("--Bstatic");
    cmd.arg("--strip-all");
    cmd.args(["--entry", "main"]);
    cmd.arg("-o");
    cmd.arg(&output_dylib);

    for cu in compilation_units {
        cmd.arg(&cu.object_file());
    }

    cmd.arg(&runtime.archive_file);

    let output = cmd.output()?;
    if !output.status.success() {
        anyhow::bail!(
            "linking with lld failed. stderr:\n\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(output_dylib)
}

fn load_program<'a>(
    filename: &Path,
    program_id: Pubkey,
    invoke_context: &InvokeContext<'a>,
) -> Executable<InvokeContext<'a>> {
    debug!("Load program {filename:?}, id {program_id}");
    let contents = &std::fs::read(filename).unwrap_or_else(|e| {
        eprintln!("Can't read the executable {:?}, error: {}", filename, e);
        std::process::exit(1);
    });
    let slot = Slot::default();
    let log_collector = invoke_context.get_log_collector();
    let loader_key = bpf_loader_upgradeable::id();
    let mut load_program_metrics = LoadProgramMetrics {
        program_id: program_id.to_string(),
        ..LoadProgramMetrics::default()
    };
    let account_size = contents.len();
    let program_runtime_environment = create_program_runtime_environment_v1(
        &invoke_context.feature_set,
        invoke_context.get_compute_budget(),
        false, /* deployment */
        true,  /* debugging_features */
    )
    .unwrap();
    let result = load_program_from_bytes(
        &invoke_context.feature_set,
        log_collector,
        &mut load_program_metrics,
        contents,
        &loader_key,
        account_size,
        slot,
        Arc::new(program_runtime_environment),
    );
    match result {
        Ok(loaded_program) => match loaded_program.program {
            LoadedProgramType::LegacyV1(program) => Ok(unsafe { std::mem::transmute(program) }),
            _ => unreachable!(),
        },
        Err(err) => Err(format!("Loading executable failed: {err:?}")),
    }
    .unwrap()
}

fn check_abort_code(expected_code: u64, message: String) {
    let codes = message
        .split(", ")
        .collect::<Vec<&str>>()
        .iter()
        .map(|x| {
            let y = x.trim_start_matches("0x");
            u64::from_str_radix(y, 16).unwrap()
        })
        .collect::<Vec<u64>>();
    assert!(codes.iter().all(|c| *c == codes[0]), "all abort codes same");
    if expected_code != codes[0] {
        panic!(
            "unexpected abort code {}, expected {expected_code}",
            codes[0]
        );
    }
}

struct LazyAnalysis<'a, 'b> {
    analysis: Option<Analysis<'a>>,
    executable: &'a Executable<InvokeContext<'b>>,
}

impl<'a, 'b> LazyAnalysis<'a, 'b> {
    fn new(executable: &'a Executable<InvokeContext<'b>>) -> Self {
        Self {
            analysis: None,
            executable,
        }
    }

    fn analyze(&mut self) -> &Analysis {
        if let Some(ref analysis) = self.analysis {
            return analysis;
        }
        self.analysis
            .insert(Analysis::from_executable(self.executable).unwrap())
    }
}

fn output_trace(filename: &str, trace: &[[u64; 12]], frame: usize, analysis: &mut LazyAnalysis) {
    use std::{fs::File, io::Write};
    if filename.is_empty() || filename == "stdout" {
        writeln!(&mut std::io::stdout(), "Frame {frame}").unwrap();
        analysis
            .analyze()
            .disassemble_trace_log(&mut std::io::stdout(), trace)
            .unwrap();
    } else {
        let mut fd = File::create(filename).unwrap();
        writeln!(&fd, "Frame {frame}").unwrap();
        analysis
            .analyze()
            .disassemble_trace_log(&mut fd, trace)
            .unwrap();
    }
}

fn run_rbpf(test_plan: &tc::TestPlan, exe: &Path) -> anyhow::Result<()> {
    let mut transaction_accounts = Vec::new();
    let mut instruction_accounts = Vec::new();
    let mut instruction_data = Vec::new();
    let mut program_id = Pubkey::new_unique();
    let loader_id = bpf_loader_upgradeable::id();
    let input_directive = test_plan
        .directives
        .iter()
        .find(|&x| matches!(x, tc::TestDirective::Input(_x)));
    if let Some(tc::TestDirective::Input(input)) = input_directive {
        instruction_data = input.instruction_data.clone();
        program_id = input.program_id.parse::<Pubkey>().unwrap_or_else(|err| {
            debug!(
                "Invalid program ID in input {}, error {}",
                input.program_id, err,
            );
            Pubkey::new_unique()
        });
        let accounts = input.accounts.clone();
        for (index, account_info) in accounts.into_iter().enumerate() {
            let pubkey = account_info.key.parse::<Pubkey>().unwrap_or_else(|err| {
                debug!("Invalid key in input {}, error {}", account_info.key, err);
                Pubkey::new_unique()
            });
            let data = account_info.data.unwrap_or_default();
            let space = data.len();
            let owner = account_info
                .owner
                .unwrap_or_else(|| Pubkey::new_unique().to_string());
            let owner = owner.parse::<Pubkey>().unwrap_or_else(|err| {
                eprintln!("Invalid owner key in input {owner}, error {err}");
                Pubkey::new_unique()
            });
            let lamports = account_info.lamports.unwrap_or(0);
            let mut account = AccountSharedData::new(lamports, space, &owner);
            account.set_data_from_slice(&data);
            transaction_accounts.push((pubkey, account));
            instruction_accounts.push(InstructionAccount {
                index_in_transaction: index as IndexOfAccount,
                index_in_caller: index as IndexOfAccount,
                index_in_callee: index as IndexOfAccount,
                is_signer: account_info.is_signer.unwrap_or(false),
                is_writable: account_info.is_writable.unwrap_or(false),
            });
        }
    }
    transaction_accounts.push((
        loader_id,
        AccountSharedData::new(0, 0, &solana_sdk::native_loader::id()),
    ));
    transaction_accounts.push((
        program_id, // ID of the loaded program. It can modify accounts with the same owner key
        AccountSharedData::new(0, 0, &loader_id),
    ));
    with_mock_invoke_context!(invoke_context, transaction_context, transaction_accounts);
    let program_index: u16 = instruction_accounts.len().try_into().unwrap();
    invoke_context
        .transaction_context
        .get_next_instruction_context()
        .unwrap()
        .configure(
            &[program_index, program_index.saturating_add(1)],
            &instruction_accounts,
            &instruction_data,
        );
    invoke_context.push().unwrap();
    #[allow(unused_mut)]
    let mut verified_executable = load_program(exe, program_id, &invoke_context);
    let (_parameter_bytes, regions, account_lengths) = serialize_parameters(
        invoke_context.transaction_context,
        invoke_context
            .transaction_context
            .get_current_instruction_context()
            .unwrap(),
        true, // should_cap_ix_accounts
        true, // copy_account_data
    )
    .unwrap();

    let mut analysis = LazyAnalysis::new(&verified_executable);

    create_vm!(
        vm,
        &verified_executable,
        regions,
        account_lengths,
        &mut invoke_context,
    );
    let mut vm = vm.unwrap();

    let (_instruction_count, result) = vm.execute_program(&verified_executable, true);

    let result = Result::from(result);

    let trace_var = std::env::var("TRACE");
    if let Ok(trace_filename) = trace_var {
        if let Some(Some(syscall_context)) = vm.context_object_pointer.syscall_context.last() {
            let trace = syscall_context.trace_log.as_slice();
            output_trace(&trace_filename, trace, 0, &mut analysis);

            // The remaining traces are saved in InvokeContext when
            // corresponding syscall_contexts are popped.
            let traces = vm.context_object_pointer.get_traces();
            for (frame, trace) in traces.iter().filter(|t| !t.is_empty()).enumerate() {
                output_trace(&trace_filename, trace, frame + 1, &mut analysis);
            }
        }
    }

    drop(vm);

    let mut all_logs = invoke_context
        .get_log_collector()
        .unwrap()
        .borrow()
        .get_recorded_content()
        .to_vec()
        .iter()
        .map(|x| {
            if x.starts_with("Program log: ") {
                x.strip_prefix("Program log: ").unwrap().to_string()
            } else {
                x.clone()
            }
        })
        .collect::<Vec<_>>();

    // If that test plan expected an abort, make sure an abort actually occurred.
    if test_plan.abort_code().is_some() && result.is_ok() {
        return test_plan.test_msg("test plan expected an abort, but it did not occur".to_string());
    }

    let r = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        match result {
            Ok(0) => {}
            Ok(_) => {
                // fixme rbpf expects a function that returns a status code, but we
                // currently emit a main function that returns void, so this value
                // is seemingly whatever happens to be in the return register.
            }
            Err(e) if e.is::<SyscallError>() => {
                if let Some(expected_code) = test_plan.abort_code() {
                    let syscall_error = *(e.downcast::<SyscallError>().unwrap());
                    match syscall_error {
                        SyscallError::Abort => {
                            check_abort_code(expected_code, all_logs.pop().unwrap())
                        }
                        _ => panic!("{syscall_error:?}"),
                    };
                } else {
                    panic!("test aborted unexpectedly");
                }
            }
            e => {
                panic!("{e:?}");
            }
        }
    }));

    let should_dump = r.is_err() || std::env::var("DUMP").is_ok();
    if should_dump {
        for (i, event) in all_logs.iter().enumerate() {
            eprintln!("event {i}: {event:?}");
        }
    }

    if r.is_ok() {
        let expected_logs = test_plan.expected_logs();
        assert_eq!(all_logs, expected_logs);
    }

    if r.is_err() {
        Err(anyhow::anyhow!("test failed"))
    } else {
        Ok(())
    }
}

fn test_package(
    harness_paths: &tc::HarnessPaths,
    test_plan: &tc::TestPlan,
    package_path: &Path,
) -> anyhow::Result<()> {
    debug!("Testing {package_path:?}");
    tc::run_move_build_to_solana(harness_paths, test_plan, vec![])?;
    let package_name = package_path.file_name().unwrap();
    let mut exe = package_path.join("build").join("solana").join(package_name);
    exe.set_extension("so");
    run_rbpf(test_plan, exe.as_path())?;
    Ok(())
}
