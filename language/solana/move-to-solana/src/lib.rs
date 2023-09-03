// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

pub mod cstr;
pub mod options;
pub mod runner;
pub mod stackless;

use crate::{options::Options, runner::Input};

use anyhow::Context;
use codespan_reporting::{diagnostic::Severity, term::termcolor::WriteColor};
use llvm_sys::prelude::LLVMModuleRef;
use log::{debug, Level};
use move_binary_format::{
    binary_views::BinaryIndexedView,
    file_format::{CompiledModule, CompiledScript},
};
use move_bytecode_source_map::{mapping::SourceMapping, utils::source_map_from_file};
use move_command_line_common::files::{
    MOVE_COMPILED_EXTENSION, MOVE_EXTENSION, SOURCE_MAP_EXTENSION,
};
use move_compiler::{shared::PackagePaths, Flags};
use move_core_types::{identifier::IdentStr, language_storage::ModuleId, value::MoveValue};
use move_ir_types::location::Spanned;
use move_model::{
    model::GlobalEnv, options::ModelBuilderOptions, parse_addresses_from_options,
    run_model_builder_with_options_and_compilation_flags,
};
use std::{
    fs,
    io::Write,
    path::{Path, PathBuf},
    process::Command,
};

struct PlatformTools {
    clang: PathBuf,
    rustc: PathBuf,
    cargo: PathBuf,
    lld: PathBuf,
}

impl PlatformTools {
    fn run_cargo(&self, target_dir: &PathBuf, args: &[&str]) -> anyhow::Result<()> {
        let mut cmd = Command::new(&self.cargo);
        cmd.env_remove("RUSTUP_TOOLCHAIN");
        cmd.env_remove("RUSTC_WRAPPER");
        cmd.env_remove("RUSTC_WORKSPACE_WRAPPER");
        cmd.env("CARGO_TARGET_DIR", target_dir);
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

fn initialize_logger() {
    static LOGGER_INIT: std::sync::Once = std::sync::Once::new();
    LOGGER_INIT.call_once(|| {
        use env_logger::fmt::Color;
        env_logger::Builder::from_default_env()
            .format(|formatter, record| {
                let level = record.level();
                let mut style = formatter.style();
                match record.level() {
                    Level::Error => style.set_color(Color::Red),
                    Level::Warn => style.set_color(Color::Yellow),
                    Level::Info => style.set_color(Color::Green),
                    Level::Debug => style.set_color(Color::Blue),
                    Level::Trace => style.set_color(Color::Cyan),
                };
                writeln!(
                    formatter,
                    "[{} {}:{}] {}",
                    style.value(level),
                    record.file().unwrap_or("unknown"),
                    record.line().unwrap_or(0),
                    record.args()
                )
            })
            .init();
    });
}

fn get_sbf_tools() -> anyhow::Result<PlatformTools> {
    let sbf_tools_root =
        std::env::var("PLATFORM_TOOLS_ROOT").context("env var PLATFORM_TOOLS_ROOT not set")?;
    let sbf_tools_root = PathBuf::from(sbf_tools_root);

    let sbf_tools = PlatformTools {
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

fn llvm_write_to_file(
    module: LLVMModuleRef,
    llvm_ir: bool,
    output_file_name: &String,
) -> anyhow::Result<()> {
    use crate::cstr::SafeCStr;
    use llvm_sys::{
        bit_writer::LLVMWriteBitcodeToFD,
        core::{LLVMDisposeMessage, LLVMPrintModuleToFile, LLVMPrintModuleToString},
    };
    use std::{ffi::CStr, fs::File, os::unix::io::AsRawFd, ptr};

    unsafe {
        if llvm_ir {
            if output_file_name != "-" {
                let mut err_string = ptr::null_mut();
                let filename = output_file_name.cstr();
                let res = LLVMPrintModuleToFile(module, filename, &mut err_string);

                if res != 0 {
                    assert!(!err_string.is_null());
                    let msg = CStr::from_ptr(err_string).to_string_lossy();
                    LLVMDisposeMessage(err_string);
                    anyhow::bail!("{}", msg);
                }
            } else {
                let buf = LLVMPrintModuleToString(module);
                assert!(!buf.is_null());
                let cstr = CStr::from_ptr(buf);
                print!("{}", cstr.to_string_lossy());
                LLVMDisposeMessage(buf);
            }
        } else {
            if output_file_name == "-" {
                anyhow::bail!("Not writing bitcode to stdout");
            }
            let bc_file = File::create(output_file_name)?;
            let res = LLVMWriteBitcodeToFD(module, bc_file.as_raw_fd(), false as i32, true as i32);

            if res != 0 {
                anyhow::bail!("Failed to write bitcode to file");
            }
        }
    }

    Ok(())
}

fn get_runtime(out_path: &PathBuf, sbf_tools: &PlatformTools) -> anyhow::Result<PathBuf> {
    debug!("building move-native runtime for sbf in {out_path:?}");
    let archive_file = out_path
        .join("sbf-solana-solana")
        .join("release")
        .join("libmove_native.a");

    if archive_file.exists() {
        return Ok(archive_file);
    }

    let move_native = std::env::var("MOVE_NATIVE").expect("move native");
    let move_native = PathBuf::from(move_native);
    let move_native = move_native.join("Cargo.toml").to_string_lossy().to_string();

    // Using `cargo rustc` to compile move-native as a staticlib.
    // See move-native documentation on `no-std` compatibilty for explanation.
    // Release mode is required to eliminate large stack frames.
    let res = sbf_tools.run_cargo(
        out_path,
        &[
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
        ],
    );

    if let Err(e) = res {
        anyhow::bail!("{e}");
    }

    if !archive_file.exists() {
        anyhow::bail!("native runtime not found at {archive_file:?}. this is a bug");
    }

    Ok(archive_file)
}

fn link_object_files(
    out_path: PathBuf,
    objects: &[PathBuf],
    output_dylib: PathBuf,
    move_native: &Option<String>,
) -> anyhow::Result<PathBuf> {
    let script = r"
PHDRS
{
  text PT_LOAD ;
  rodata PT_LOAD ;
  data PT_LOAD ;
  dynamic PT_DYNAMIC ;
}

SECTIONS
{
  . = SIZEOF_HEADERS;
  .text : { *(.text*) } :text
  .rodata : { *(.rodata*) } :rodata
  .data.rel.ro : { *(.data.rel.ro*) } :rodata
  .dynamic : { *(.dynamic) } :dynamic
  .dynsym : { *(.dynsym) } :data
  .dynstr : { *(.dynstr) } :data
  .rel.dyn : { *(.rel.dyn) } :data
  /DISCARD/ : {
      *(.eh_frame*)
      *(.gnu.hash*)
      *(.hash*)
    }
}
";

    let tmpdir = tempfile::Builder::new()
        .prefix("move")
        .tempdir()
        .unwrap()
        .into_path();
    let file_name = "move-to-solana-linkfile.ld";
    let link_script_path = tmpdir.join(file_name);
    if let Err(error) = fs::write(&link_script_path, script) {
        anyhow::bail!("can't output linker script: {}", error);
    }
    let sbf_tools = get_sbf_tools()?;
    let mut cmd = Command::new(&sbf_tools.lld);
    cmd.arg("--threads=1");
    cmd.arg("-znotext");
    cmd.arg("-znoexecstack");
    cmd.args(["--script", &link_script_path.to_string_lossy()]);
    cmd.arg("--gc-sections");
    cmd.arg("--shared");
    cmd.arg("--Bstatic");
    cmd.arg("--strip-all");
    cmd.args(["--entry", "main"]);
    cmd.arg("-o");
    cmd.arg(&output_dylib);
    for obj in objects {
        cmd.arg(obj);
    }
    let move_native_known = move_native.is_some();
    let empty_path = String::from("");
    let move_native = move_native.as_ref().unwrap_or(&empty_path);
    let path = Path::new(move_native).to_path_buf();
    let move_native_path = if move_native_known { &path } else { &out_path };
    let runtime = get_runtime(move_native_path, &sbf_tools)?;
    cmd.arg(&runtime);
    debug!("Running {cmd:?}");
    let output = cmd.output()?;
    if !output.status.success() {
        anyhow::bail!(
            "linking with lld failed. stderr:\n\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
    Ok(output_dylib)
}

fn get_env_from_source<W: WriteColor>(
    error_writer: &mut W,
    options: &Options,
) -> anyhow::Result<GlobalEnv> {
    let addrs = parse_addresses_from_options(options.named_address_mapping.clone())?;
    debug!("Named addresses {:?}", addrs);

    let env = run_model_builder_with_options_and_compilation_flags(
        vec![PackagePaths {
            name: None,
            paths: options.sources.clone(),
            named_address_map: addrs.clone(),
        }],
        vec![PackagePaths {
            name: None,
            paths: options.dependencies.clone(),
            named_address_map: addrs,
        }],
        ModelBuilderOptions::default(),
        Flags::empty().set_flavor("async"),
    )?;

    env.report_diag(error_writer, Severity::Warning);
    if env.has_errors() {
        anyhow::bail!("Move source code errors")
    } else {
        Ok(env)
    }
}

fn get_env_from_bytecode(options: &Options) -> anyhow::Result<GlobalEnv> {
    let move_extension = MOVE_EXTENSION;
    let mv_bytecode_extension = MOVE_COMPILED_EXTENSION;
    let source_map_extension = SOURCE_MAP_EXTENSION;

    let bytecode_file_path = (options.bytecode_file_path.as_ref()).unwrap();
    let source_path = Path::new(&bytecode_file_path);
    let extension = source_path
        .extension()
        .context("Missing file extension for bytecode file")?;
    if extension != mv_bytecode_extension {
        anyhow::bail!(
            "Bad source file extension {:?}; expected {}",
            extension,
            mv_bytecode_extension
        );
    }

    let bytecode_bytes = fs::read(bytecode_file_path).context("Unable to read bytecode file")?;

    let mut dep_bytecode_bytes = vec![];
    for dep in &options.dependencies {
        let bytes = fs::read(dep).context("Unable to read dependency bytecode file {dep}")?;
        dep_bytecode_bytes.push(bytes);
    }

    let source_path = Path::new(&bytecode_file_path).with_extension(move_extension);
    let source = fs::read_to_string(&source_path).ok();
    let source_map =
        source_map_from_file(&Path::new(&bytecode_file_path).with_extension(source_map_extension));

    let no_loc = Spanned::unsafe_no_loc(()).loc;
    let module: CompiledModule;
    let script: CompiledScript;
    let bytecode = if options.is_script {
        script = CompiledScript::deserialize(&bytecode_bytes)
            .context("Script blob can't be deserialized")?;
        BinaryIndexedView::Script(&script)
    } else {
        module = CompiledModule::deserialize(&bytecode_bytes)
            .context("Module blob can't be deserialized")?;
        BinaryIndexedView::Module(&module)
    };

    let mut source_mapping = {
        if let Ok(s) = source_map {
            SourceMapping::new(s, bytecode)
        } else {
            SourceMapping::new_from_view(bytecode, no_loc)
                .context("Unable to build dummy source mapping")?
        }
    };

    if let Some(source_code) = source {
        source_mapping.with_source_code((source_path.to_str().unwrap().to_string(), source_code));
    }

    let main_move_module = if options.is_script {
        let script = CompiledScript::deserialize(&bytecode_bytes)
            .context("Script blob can't be deserialized")?;
        move_model::script_into_module(script)
    } else {
        CompiledModule::deserialize(&bytecode_bytes).context("Module blob can't be deserialized")?
    };

    let mut dep_move_modules = vec![];

    for bytes in &dep_bytecode_bytes {
        let dep_module = CompiledModule::deserialize(bytes)
            .context("Dependency module blob can't be deserialized")?;
        dep_move_modules.push(dep_module);
    }

    let modules = dep_move_modules
        .into_iter()
        .chain(Some(main_move_module))
        .collect::<Vec<_>>();

    move_model::run_bytecode_model_builder(&modules)
}

fn compile(global_env: &GlobalEnv, options: &Options) -> anyhow::Result<()> {
    use crate::stackless::{extensions::ModuleEnvExt, Target, *};

    let tgt_platform = TargetPlatform::Solana;
    tgt_platform.initialize_llvm();
    let lltarget = Target::from_triple(tgt_platform.triple())?;
    let llmachine = lltarget.create_target_machine(
        tgt_platform.triple(),
        tgt_platform.llvm_cpu(),
        tgt_platform.llvm_features(),
    );
    let global_cx = GlobalContext::new(global_env, tgt_platform, &llmachine);
    let output_file_path = options.output.clone();
    let file_stem = Path::new(&output_file_path).file_stem().unwrap();
    // If building a shared object library, then -o is the
    // directory to output the compiled modules, each module
    // 'mod' will get file name 'mod.o'
    let out_path = Path::new(&output_file_path)
        .parent()
        .unwrap()
        .to_path_buf()
        .join(file_stem);
    if !(options.compile || options.llvm_ir) {
        fs::create_dir_all(&out_path)
            .or_else(|err| anyhow::bail!("Error creating directory: {}", err))?;
    }
    let mut objects = vec![];
    for mod_id in global_env
        .get_modules()
        .collect::<Vec<_>>()
        .iter() // now the last is the first - use this in case of deserialization
        .rev()
        .map(|m| m.get_id())
    {
        let module = global_env.get_module(mod_id);
        let modname = module.llvm_module_name();
        debug!("Generating code for module {}", modname);
        let mut llmod = global_cx.llvm_cx.create_module(&modname);
        let mod_cx = global_cx.create_module_context(mod_id, &llmod, options);
        mod_cx.translate();

        let mut out_path = out_path.join(&modname);
        out_path.set_extension(&options.output_file_extension);
        let mut output_file = out_path.to_str().unwrap().to_string();
        if options.llvm_ir {
            output_file = options.output.clone();
            let path = Path::new(&output_file);
            if path.exists() && path.is_dir() {
                let mut path = path.join(modname);
                path.set_extension(&options.output_file_extension);
                output_file = path.to_string_lossy().to_string();
            }
            debug!("Output generated code to {}", output_file);
            llvm_write_to_file(llmod.as_mut(), options.llvm_ir, &output_file)?;
            drop(llmod);
        } else {
            if options.compile {
                output_file = options.output.clone();
            }
            debug!("Output generated code to {}", output_file);
            write_object_file(llmod, &llmachine, &output_file)?;
        }
        if !(options.compile || options.llvm_ir) {
            objects.push(Path::new(&output_file).to_path_buf());
        }
        // Deserialization is always for one module, and if global env returns many,
        // after reversing the list the subject of interest is the first one.
        // For Compilation we process all modules.
        if options.bytecode_file_path.is_some() {
            break;
        }
    }
    if !(options.compile || options.llvm_ir) {
        link_object_files(
            out_path,
            objects.as_slice(),
            Path::new(&output_file_path).to_path_buf(),
            &options.move_native_archive,
        )?;
    }
    // NB: context must outlive llvm module
    // fixme this should be handled with lifetimes
    drop(global_cx);
    Ok(())
}

/*
Generate input.json file with similar contents
    {
    "program_id": "DozgQiYtGbdyniV2T74xMdmjZJvYDzoRFFqw7UR5MwPK",
    "accounts": [
        {
            "key": "524HMdYYBy6TAn4dK5vCcjiTmT2sxV6Xoue5EXrz22Ca",
            "owner": "BPFLoaderUpgradeab1e11111111111111111111111",
            "is_signer": false,
            "is_writable": true,
            "lamports": 1000,
            "data": [0, 0, 0, 3]
        }
    ],
    "instruction_data": [
        9, 0, 0, 0, 0, 0, 0, 0,
        109, 97, 105, 110, 95, 95, 98, 97, 114]
    }
 */
fn generate_input_for_unit_test(
    module_id: &ModuleId,
    fun_name: &IdentStr,
    _args: &[MoveValue],
) -> anyhow::Result<()> {
    let program_id = bs58::encode(module_id.address().into_bytes()).into_string();
    let entry_point = format!("{}__{}", module_id.name(), fun_name);
    let mut instruction_data = entry_point.len().to_le_bytes().to_vec();
    let mut name_bytes = entry_point.as_bytes().to_vec();
    instruction_data.append(&mut name_bytes);
    let input = Input {
        program_id,
        accounts: vec![],
        instruction_data,
    };
    let content = serde_json::to_string_pretty(&input).unwrap();
    debug!("input.json {}", content);
    if let Err(error) = fs::write("input.json", content) {
        anyhow::bail!("can't output input.json: {}", error);
    }
    Ok(())
}

pub fn run_to_solana<W: WriteColor>(error_writer: &mut W, options: Options) -> anyhow::Result<()> {
    initialize_logger();
    // Normally the compiler is invoked on a package from `move build`
    // coomand, and builds an entire package as a .so file.  The test
    // harness is currently designed to invoke stand-alone compiler
    // tool on individual Move bytecode files, compiling each to a .o
    // file. To build a .so file loadable into a VM, it's necessary to
    // link the separate .o files into a .so file.  If all input files
    // are .o object files, the compiler assumes that it should link
    // them into an output .so file.
    if !options.llvm_ir
        && !options.compile
        && options.bytecode_file_path.is_none()
        && options.sources.iter().all(|s| s.ends_with(".o"))
    {
        let output = Path::new(&options.output).to_path_buf();
        let objects: Vec<PathBuf> = options
            .sources
            .iter()
            .map(|s| Path::new(s).to_path_buf())
            .collect();
        link_object_files(
            output.parent().unwrap().to_path_buf(),
            objects.as_slice(),
            output,
            &options.move_native_archive,
        )?;
        return Ok(());
    }
    match &*options.gen_dot_cfg {
        "write" | "view" | "" => {}
        _ => {
            eprintln!(
                "unexpected gen-dot-cfg option '{}', ignored.",
                &options.gen_dot_cfg
            );
        }
    };

    let global_env: GlobalEnv = if options.bytecode_file_path.is_some() {
        get_env_from_bytecode(&options)?
    } else {
        get_env_from_source(error_writer, &options)?
    };

    compile(&global_env, &options)?;

    Ok(())
}

pub fn run_for_unit_test(
    _options: &Options, // currently not used
    env: &GlobalEnv,
    module_id: &ModuleId,
    fun_name: &IdentStr,
    args: &[MoveValue],
) -> Result<String, String> {
    initialize_logger();
    debug!("module id {module_id:?}, fun_name {fun_name:?}, args {args:?}");
    let options = Options {
        unit_test_function: Some(format!("{}__{}", module_id.name(), fun_name)),
        ..Options::default()
    };
    if let Err(e) = generate_input_for_unit_test(module_id, fun_name, args) {
        return Err(e.to_string());
    }
    match compile(env, &options) {
        Ok(_) => Ok(options.output),
        Err(e) => Err(e.to_string()),
    }
}
