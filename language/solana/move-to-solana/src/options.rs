// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use clap::Parser;

#[derive(Debug, Parser)]
#[clap(author, version, about)]
pub struct Options {
    /// Directories where to lookup dependencies.
    #[clap(
        short,
        takes_value(true),
        multiple_values(true),
        multiple_occurrences(true)
    )]
    pub dependencies: Vec<String>,

    /// Named address mapping.
    #[clap(
        short,
        takes_value(true),
        multiple_values(true),
        multiple_occurrences(true)
    )]
    pub named_address_mapping: Vec<String>,

    /// Path to output file or if option `-c` is set to output directory.
    #[clap(short)]
    #[clap(long, default_value = "output.so")]
    pub output: String,

    /// Sources to compile (positional arg)
    pub sources: Vec<String>,

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

    /// Compile to object file.
    #[clap(short)]
    pub compile: bool,

    /// Location of precompiled move native library.
    #[clap(long = "move-native-archive")]
    pub move_native_archive: Option<String>,

    /// Output file extension. This is used with -c option.
    /// Each created in compilation module `mod` will be placed into file `mod.ll`
    /// by default, or extension may be changed by this option.
    #[clap(long = "extension", default_value = "ll")]
    pub output_file_extension: String,

    /// Output llvm bitcode in a human readable text format.
    #[clap(short = 'S')]
    pub llvm_ir: bool,

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
}

impl Default for Options {
    fn default() -> Self {
        Parser::parse_from(std::iter::empty::<String>())
    }
}
