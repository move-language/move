use anyhow::Context;
use similar::{ChangeTag, TextDiff};
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

pub const TEST_DIR: &str = "tests/move-ir-tests";

datatest_stable::harness!(run_test, TEST_DIR, r".*\.move$");

fn run_test(test_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    Ok(run_test_inner(test_path)?)
}

fn run_test_inner(test_path: &Path) -> anyhow::Result<()> {
    let harness_paths = get_harness_paths()?;
    let test_plan = get_test_plan(test_path)?;

    if test_plan.should_ignore() {
        eprintln!("ignoring {}", test_plan.name);
        return Ok(());
    }

    run_move_build(&harness_paths, &test_plan)?;
    let compilation_units = find_compilation_units(&test_plan)?;
    compile_all_bytecode_to_llvm_ir(&harness_paths, &compilation_units)?;
    maybe_promote_actual_llvm_ir_to_expected(&compilation_units)?;
    compare_all_actual_llvm_ir_to_expected(&compilation_units)?;

    Ok(())
}

#[derive(Debug)]
struct HarnessPaths {
    move_build: PathBuf,
    move_mv_llvm_compiler: PathBuf,
}

fn get_harness_paths() -> anyhow::Result<HarnessPaths> {
    // Cargo will tell us the location of move-mv-llvm-compiler.
    let move_mv_llvm_compiler = env!("CARGO_BIN_EXE_move-mv-llvm-compiler");
    let move_mv_llvm_compiler = PathBuf::from(move_mv_llvm_compiler);

    // We have to guess where move-ir-compiler is
    let move_build = if !cfg!(windows) {
        move_mv_llvm_compiler.with_file_name("move-build")
    } else {
        move_mv_llvm_compiler.with_file_name("move-build.exe")
    };

    if !move_build.exists() {
        // todo: can we build move-build automatically?

        let is_release = move_build.to_string_lossy().contains("release");
        let suggestion = if is_release {
            "try running `cargo build -p move-build --release` first"
        } else {
            "try running `cargo build -p move-build` first"
        };
        anyhow::bail!("move-build not built. {suggestion}");
    }

    Ok(HarnessPaths {
        move_build,
        move_mv_llvm_compiler,
    })
}

#[derive(Debug)]
struct TestPlan {
    name: String,
    /// The move file to be compiled to LLVM IR
    move_file: PathBuf,
    /// The build directory, which contains bytecode for multiple modules and
    /// scripts.
    build_dir: PathBuf,
    /// Special commands embedded in the test file as comments
    directives: Vec<TestDirective>,
}

#[derive(Debug, Eq, PartialEq)]
enum TestDirective {
    Ignore,
}

impl TestPlan {
    fn should_ignore(&self) -> bool {
        self.directives.contains(&TestDirective::Ignore)
    }
}

#[derive(Debug)]
struct CompilationUnit {
    type_: CompilationUnitType,
    bytecode: PathBuf,
    llvm_ir: PathBuf,
    llvm_ir_expected: PathBuf,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum CompilationUnitType {
    Script,
    Module,
}

fn get_test_plan(test_path: &Path) -> anyhow::Result<TestPlan> {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").expect("cargo_manifest_dir");
    let move_file = Path::new(&manifest_dir).join(test_path);

    let name = test_path.to_string_lossy().to_string();
    let move_file = move_file.to_owned();
    let stem = move_file.file_stem().expect("stem").to_string_lossy();
    let build_dir = move_file.with_file_name(format!("{}-build", stem));
    let directives = load_directives(test_path)?;

    Ok(TestPlan {
        name,
        move_file,
        build_dir,
        directives,
    })
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
    }

    Ok(directives)
}

fn run_move_build(harness_paths: &HarnessPaths, test_plan: &TestPlan) -> anyhow::Result<()> {
    let mut cmd = Command::new(&harness_paths.move_build);
    cmd.arg(&test_plan.move_file);
    cmd.args(["--flavor", "none"]);
    cmd.args(["--out-dir", &test_plan.build_dir.to_str().expect("utf-8")]);

    let output = cmd.output().context("run move-build failed")?;
    if !output.status.success() {
        anyhow::bail!(
            "move-build failed. stderr:\n\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(())
}

fn find_compilation_units(test_plan: &TestPlan) -> anyhow::Result<Vec<CompilationUnit>> {
    let modules_dir = test_plan.build_dir.join("modules");
    let scripts_dir = test_plan.build_dir.join("scripts");

    let dirs = [
        (modules_dir, CompilationUnitType::Module),
        (scripts_dir, CompilationUnitType::Script),
    ];

    let mut units = vec![];

    for (dir, type_) in dirs {
        if !dir.exists() {
            continue;
        }

        for dirent in fs::read_dir(&dir)? {
            let dirent = dirent?;
            let path = dirent.path();
            if path.extension() != Some(&OsStr::new("mv")) {
                continue;
            }

            let bytecode = path;
            let llvm_ir = bytecode.with_extension("actual.ll");
            let llvm_ir_expected = bytecode.with_extension("expected.ll");

            units.push(CompilationUnit {
                type_,
                bytecode,
                llvm_ir,
                llvm_ir_expected,
            });
        }
    }

    Ok(units)
}

fn compile_all_bytecode_to_llvm_ir(
    harness_paths: &HarnessPaths,
    compilation_units: &[CompilationUnit],
) -> anyhow::Result<()> {
    for cu in compilation_units {
        let mut cmd = Command::new(&harness_paths.move_mv_llvm_compiler);
        cmd.arg("-b");
        cmd.arg(&cu.bytecode);
        cmd.arg("-o");
        cmd.arg(&cu.llvm_ir);
        cmd.arg("-S");

        if cu.type_ == CompilationUnitType::Script {
            cmd.arg("-s");
        }

        let output = cmd.output().context("run move-mv-llvm-compiler failed")?;
        if !output.status.success() {
            anyhow::bail!(
                "move-mv-llvm-compiler failed. stderr:\n\n{}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
    }

    Ok(())
}

fn maybe_promote_actual_llvm_ir_to_expected(
    compilation_units: &[CompilationUnit],
) -> anyhow::Result<()> {
    if !std::env::var("PROMOTE_LLVM_IR").is_ok() {
        return Ok(());
    }

    for cu in compilation_units {
        fs::copy(&cu.llvm_ir, &cu.llvm_ir_expected)?;
    }

    Ok(())
}

fn compare_all_actual_llvm_ir_to_expected(
    compilation_units: &[CompilationUnit],
) -> anyhow::Result<()> {
    for cu in compilation_units {
        compare_actual_llvm_ir_to_expected(cu)?;
    }

    Ok(())
}

fn compare_actual_llvm_ir_to_expected(compilation_unit: &CompilationUnit) -> anyhow::Result<()> {
    if !compilation_unit.llvm_ir_expected.exists() {
        anyhow::bail!(
            "no expected.ll file: {:?}",
            compilation_unit.llvm_ir_expected
        );
    }

    let mut diff_msg = String::new();
    let file_actual = fs::read_to_string(&compilation_unit.llvm_ir)?;
    let file_expected = fs::read_to_string(&compilation_unit.llvm_ir_expected)?;

    let diff = TextDiff::from_lines(&file_expected, &file_actual);
    for change in diff.iter_all_changes() {
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
        anyhow::bail!(format!(
            "LLVM IR actual ({:?}) does not equal expected: \n\n{}",
            compilation_unit.llvm_ir, diff_msg
        ));
    }

    Ok(())
}
