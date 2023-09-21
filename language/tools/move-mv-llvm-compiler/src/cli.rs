// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use clap::Parser;

#[derive(Debug, Parser)]
#[clap(author, version, about)]
pub struct Args {
    /// Skip printing of private functions.
    #[clap(long = "skip-private")]
    pub skip_private: bool,

    /// Do not print the disassembled bytecodes of each function.
    #[clap(long = "skip-code")]
    pub skip_code: bool,

    /// Do not print locals of each function.
    #[clap(long = "skip-locals")]
    pub skip_locals: bool,

    /// Do not print the basic blocks of each function.
    #[clap(long = "skip-basic-blocks")]
    pub skip_basic_blocks: bool,

    /// Treat input file as a script (default is to treat file as a module)
    #[clap(short = 's', long = "script")]
    pub is_script: bool,

    /// The path to the move bytecode file to compile.
    #[clap(short = 'b', long = "bytecode")]
    pub bytecode_file_path: Option<String>,

    /// Path to the move source direcory (containing Move.toml)
    #[clap(short = 'p', long = "package")]
    pub move_package_path: Option<String>,

    /// Call Move compiler and pass this option.
    #[clap(short = 'c', long = "compile")]
    pub compile: Option<String>,

    /// Use stdlib.
    #[clap(long = "stdlib")]
    pub stdlib: bool,

    /// Compile in test mode.
    #[clap(long = "test")]
    pub test: bool,

    /// Compile in dev mode.
    #[clap(long = "dev")]
    pub dev: bool,
    /// Output file extension. This is used with -c option.
    /// Each created in compilation module `mod` will be placed into file `mod.ll`
    /// by default, or extension may be changed by this option.
    #[clap(long = "extension", default_value = "ll")]
    pub output_file_extension: String,

    /// Bytecode dependencies, sorted.
    #[clap(short = 'd', long = "deps")]
    pub bytecode_dependency_paths: Vec<String>,

    /// Path to output file or if option `-c` is set to output directory.
    #[clap(short = 'o', default_value = "-")]
    pub output_file_path: String,

    /// Output llvm bitcode in a human readable text format.
    #[clap(short = 'S')]
    pub llvm_ir: bool,

    /// Output an object file
    #[clap(short = 'O')]
    pub obj: bool,

    /// Provide signers to a script (only for testing/debugging purposes).
    #[clap(long = "signers", use_value_delimiter = true, value_delimiter = ',')]
    pub test_signers: Vec<String>,

    /// Write or view GraphViz dot graph files for each CFG.
    /// ("write": gen dot files, "view": gen dot files and invoke xdot viewer)"
    #[clap(long = "gen-dot-cfg", default_value = "")]
    pub gen_dot_cfg: String,

    /// Path to GraphViz output files (defaults to current working directory).
    #[clap(long = "dot-out-dir", default_value = "")]
    pub dot_file_path: String,

    /// Print more diagnostics in compilation
    #[clap(long = "diagnostics")]
    pub diagnostics: bool,
}

use anyhow::{bail, Result};
use std::{fs::canonicalize, path::PathBuf};

pub fn absolute_existing_file(path: Option<String>, title: &str) -> Result<PathBuf> {
    absolute_path(path, title, ExistanceCheck::ExistingFile)
}
pub fn absolute_new_file(path: Option<String>, title: &str) -> Result<PathBuf> {
    absolute_path(path, title, ExistanceCheck::NewFile)
}
enum ExistanceCheck {
    ExistingFile,
    NewFile,
}
impl ExistanceCheck {
    fn should_exist(&self) -> bool {
        use ExistanceCheck::*;
        match self {
            ExistingFile => true,
            NewFile => false,
        }
    }
}
fn absolute_path(path: Option<String>, title: &str, check: ExistanceCheck) -> Result<PathBuf> {
    let Some(path) = path else {
        bail!("Option not set: {title}")
    };
    let path_str = path.clone();
    let path = PathBuf::from(path);
    if check.should_exist() && !path.exists() {
        bail!("File is not found: {path_str}");
    }
    if path.is_absolute() {
        return Ok(path);
    }
    match canonicalize(path) {
        Err(err) => bail!(
            "Cannot transform path {} into absolute. Got error: {}",
            path_str,
            err
        ),
        Ok(path) => Ok(path),
    }
}
