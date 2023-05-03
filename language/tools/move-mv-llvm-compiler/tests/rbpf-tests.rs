// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::Context;
use extension_trait::extension_trait;
use solana_rbpf as rbpf;
use std::{
    path::{Path, PathBuf},
    process::Command,
};

mod test_common;
use test_common as tc;

pub const TEST_DIR: &str = "tests/rbpf-tests";

datatest_stable::harness!(run_test, TEST_DIR, r".*\.move$");

fn run_test(test_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
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

    tc::run_move_build(&harness_paths, &test_plan)?;

    let compilation_units = tc::find_compilation_units(&test_plan)?;

    compile_all_bytecode_to_object_files(&harness_paths, &compilation_units)?;

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
) -> anyhow::Result<()> {
    tc::compile_all_bytecode(harness_paths, compilation_units, "-O", &|cu| {
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

        // release mode required to eliminate large stack frames
        let res = sbf_tools.run_cargo(&[
            "build",
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
    cmd.arg("-shared");
    cmd.arg("--Bstatic");
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

fn run_rbpf(test_plan: &tc::TestPlan, exe: &Path) -> anyhow::Result<()> {
    use rbpf::{
        ebpf, elf::Executable, error::EbpfError, memory_region::MemoryRegion,
        verifier::RequisiteVerifier, vm::*,
    };

    let loader = rbpf_setup::build_loader()?;

    let elf = &std::fs::read(exe)?;
    let mem = &mut vec![0; 1024];
    let executable = Executable::<rbpf_setup::Context>::from_elf(elf, loader).unwrap();
    let mem_region = MemoryRegion::new_writable(mem, ebpf::MM_INPUT_START);
    let verified_executable =
        VerifiedExecutable::<RequisiteVerifier, rbpf_setup::Context>::from_executable(executable)
            .unwrap();

    let mut heap = vec![0; 1024 * 32]; // Same size as Solana default.

    let mut context_object = rbpf_setup::Context::default();
    let mut vm = EbpfVm::new(
        &verified_executable,
        &mut context_object,
        &mut heap,
        vec![mem_region],
    )
    .unwrap();

    let (_instruction_count, result) = vm.execute_program(true);

    let result = Result::from(result);

    // If that test plan expected an abort, make sure an abort actually occurred.
    if test_plan.abort_code().is_some() && result.is_ok() {
        panic!("test plan expected an abort, but it did not occur.");
    }

    let events = vm.env.context_object_pointer.events.clone();

    let r = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        match result {
            Ok(0) => {}
            Ok(_) => {
                // fixme rbpf expects a function that returns a status code, but we
                // currently emit a main function that returns void, so this value
                // is seemingly whatever happens to be in the return register.
            }
            Err(EbpfError::UserError(e)) if e.is::<rbpf_setup::AbortError>() => {
                if let Some(expected_code) = test_plan.abort_code() {
                    let last_event = events.last();
                    match last_event {
                        Some(rbpf_setup::Event::LogU64(c1, c2, c3, c4, c5)) => {
                            assert!(
                                [c1, c2, c3, c4, c5].iter().all(|c| *c == c1),
                                "all abort codes same"
                            );
                            if *c1 != expected_code {
                                panic!("unexpected abort code {c1}, expected {expected_code}");
                            }
                        }
                        _ => {
                            panic!("abort without abort code?!");
                        }
                    }
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
        let mem = &vm.env.memory_mapping;
        eprintln!("memory mapping: {mem:#?}");

        for (i, event) in events.iter().enumerate() {
            eprintln!("event {i}: {event:?}");
        }
    }

    if r.is_ok() {
        let all_logs = events
            .iter()
            .filter_map(|e| match e {
                rbpf_setup::Event::Log(s) => Some(s.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();
        let expected_logs = test_plan.expected_logs();
        assert_eq!(all_logs, expected_logs);
    }

    if r.is_err() {
        Err(anyhow::anyhow!("test failed"))
    } else {
        Ok(())
    }
}

mod rbpf_setup {
    use super::rbpf;
    use anyhow::anyhow;
    use rbpf::{
        error::EbpfError,
        memory_region::{AccessType, MemoryMapping},
        vm::*,
    };
    use std::sync::Arc;

    #[derive(Default, Debug)]
    pub struct Context {
        pub events: Vec<Event>,
    }

    impl ContextObject for Context {
        fn trace(&mut self, _state: [u64; 12]) {}
        fn consume(&mut self, _amount: u64) {}
        fn get_remaining(&self) -> u64 {
            u64::max_value()
        }
    }

    #[derive(Debug, Clone)]
    pub enum Event {
        Log(String),
        LogU64(u64, u64, u64, u64, u64),
    }

    pub fn build_loader() -> anyhow::Result<Arc<BuiltInProgram<Context>>> {
        let config = Config {
            dynamic_stack_frames: false,
            enable_elf_vaddr: false,
            reject_rodata_stack_overlap: false,
            static_syscalls: false,
            enable_instruction_meter: false,
            ..Config::default()
        };
        let mut loader = BuiltInProgram::new_loader(config);

        loader
            .register_function_by_name("abort", SyscallAbort::call)
            .map_err(|e| anyhow!("{e}"))?;
        loader
            .register_function_by_name("sol_log_", SyscallLog::call)
            .map_err(|e| anyhow!("{e}"))?;
        loader
            .register_function_by_name("sol_log_64_", SyscallLogU64::call)
            .map_err(|e| anyhow!("{e}"))?;

        Ok(Arc::new(loader))
    }

    pub struct SyscallAbort;

    impl SyscallAbort {
        pub fn call(
            _invoke_context: &mut Context,
            _arg_a: u64,
            _arg_b: u64,
            _arg_c: u64,
            _arg_d: u64,
            _arg_e: u64,
            _memory_mapping: &mut MemoryMapping,
            result: &mut ProgramResult,
        ) {
            *result = ProgramResult::Err(EbpfError::UserError(Box::new(AbortError)));
        }
    }

    #[derive(thiserror::Error, Debug)]
    #[error("aborted")]
    pub struct AbortError;

    pub struct SyscallLog;

    impl SyscallLog {
        pub fn call(
            invoke_context: &mut Context,
            addr: u64,
            len: u64,
            _arg_c: u64,
            _arg_d: u64,
            _arg_e: u64,
            memory_mapping: &mut MemoryMapping,
            result: &mut ProgramResult,
        ) {
            unsafe {
                let host_addr = memory_mapping.map(AccessType::Load, addr, len, 0);
                let host_addr = Result::from(host_addr).expect("bad address");
                let msg_slice = std::slice::from_raw_parts(host_addr as *const u8, len as usize);
                let msg = std::str::from_utf8(msg_slice).expect("utf8");
                invoke_context.events.push(Event::Log(msg.to_string()));
                *result = ProgramResult::Ok(0);
            }
        }
    }

    pub struct SyscallLogU64;

    impl SyscallLogU64 {
        pub fn call(
            invoke_context: &mut Context,
            arg_a: u64,
            arg_b: u64,
            arg_c: u64,
            arg_d: u64,
            arg_e: u64,
            _memory_mapping: &mut MemoryMapping,
            result: &mut ProgramResult,
        ) {
            invoke_context
                .events
                .push(Event::LogU64(arg_a, arg_b, arg_c, arg_d, arg_e));
            *result = ProgramResult::Ok(0);
        }
    }
}
