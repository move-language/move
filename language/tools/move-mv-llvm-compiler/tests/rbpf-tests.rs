use anyhow::Context;
use extension_trait::extension_trait;
use solana_rbpf as rbpf;
use std::path::{Path, PathBuf};
use std::process::Command;

mod test_common;
use test_common as tc;

pub const TEST_DIR: &str = "tests/rbpf-tests";

datatest_stable::harness!(run_test, TEST_DIR, r".*\.move$");

fn run_test(test_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    Ok(run_test_inner(test_path)?)
}

fn run_test_inner(test_path: &Path) -> anyhow::Result<()> {
    let sbf_tools = get_sbf_tools()?;

    let harness_paths = tc::get_harness_paths()?;
    let test_plan = tc::get_test_plan(test_path)?;

    if test_plan.should_ignore() {
        eprintln!("ignoring {}", test_plan.name);
        return Ok(());
    }

    tc::run_move_build(&harness_paths, &test_plan)?;

    let compilation_units = tc::find_compilation_units(&test_plan)?;

    compile_all_bytecode_to_object_files(&harness_paths, &compilation_units)?;

    let exe = link_object_files(&test_plan, &sbf_tools, &compilation_units)?;

    run_rbpf(&exe)?;

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

struct SbfTools {
    _root: PathBuf,
    clang: PathBuf,
    rustc: PathBuf,
    lld: PathBuf,
}

fn get_sbf_tools() -> anyhow::Result<SbfTools> {
    let sbf_tools_root =
        std::env::var("SBF_TOOLS_ROOT").context("env var SBF_TOOLS_ROOT not set")?;
    let sbf_tools_root = PathBuf::from(sbf_tools_root);

    let sbf_tools = SbfTools {
        _root: sbf_tools_root.clone(),
        clang: sbf_tools_root
            .join("llvm/bin/clang")
            .with_extension(std::env::consts::EXE_EXTENSION),
        rustc: sbf_tools_root
            .join("rust/bin/rustc")
            .with_extension(std::env::consts::EXE_EXTENSION),
        lld: sbf_tools_root.join("llvm/bin/ld.lld"),
    };

    if !sbf_tools.clang.exists() {
        anyhow::bail!("no clang bin at {}", sbf_tools.clang.display());
    }
    if !sbf_tools.rustc.exists() {
        anyhow::bail!("no rustc bin at {}", sbf_tools.rustc.display());
    }
    if !sbf_tools.lld.exists() {
        anyhow::bail!("no lld bin at {}", sbf_tools.lld.display());
    }

    Ok(sbf_tools)
}

fn link_object_files(
    test_plan: &tc::TestPlan,
    sbf_tools: &SbfTools,
    compilation_units: &[tc::CompilationUnit],
) -> anyhow::Result<PathBuf> {
    let output_dylib = test_plan.build_dir.join("output.so");

    let mut cmd = Command::new(&sbf_tools.lld);
    cmd.args(["-z", "notext"]);
    cmd.arg("-shared");
    cmd.arg("--Bdynamic");
    cmd.args(["--entry", "main"]);
    cmd.arg("-o");
    cmd.arg(&output_dylib);

    for cu in compilation_units {
        cmd.arg(&cu.object_file());
    }

    let output = cmd.output()?;
    if !output.status.success() {
        anyhow::bail!(
            "linking with lld failed. stderr:\n\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(output_dylib)
}

fn run_rbpf(exe: &Path) -> anyhow::Result<()> {
    use rbpf::ebpf;
    use rbpf::elf::Executable;
    use rbpf::memory_region::MemoryRegion;
    use rbpf::verifier::RequisiteVerifier;
    use rbpf::vm::*;
    use std::sync::Arc;

    let elf = &std::fs::read(exe)?;
    let mem = &mut vec![0; 1024];

    let config = Config {
        dynamic_stack_frames: false,
        enable_elf_vaddr: false,
        reject_rodata_stack_overlap: false,
        static_syscalls: false,
        ..Config::default()
    };
    let loader = Arc::new(BuiltInProgram::new_loader(config));
    let executable = Executable::<TestContextObject>::from_elf(elf, loader).unwrap();
    let mem_region = MemoryRegion::new_writable(mem, ebpf::MM_INPUT_START);
    let verified_executable =
        VerifiedExecutable::<RequisiteVerifier, TestContextObject>::from_executable(executable)
            .unwrap();
    let mut context_object = TestContextObject::new(1);
    let mut vm = EbpfVm::new(
        &verified_executable,
        &mut context_object,
        &mut [],
        vec![mem_region],
    )
    .unwrap();

    let (_instruction_count, result) = vm.execute_program(true);

    let result = Result::from(result);

    match result {
        Ok(0) => {}
        e => {
            panic!("{e:?}");
        }
    }

    Ok(())
}
