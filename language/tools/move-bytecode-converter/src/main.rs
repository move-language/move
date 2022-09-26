// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#![forbid(unsafe_code)]

use anyhow::{bail, Result};
use clap::Parser;
use std::{
    path::{Path, PathBuf},
    str::FromStr,
};

#[derive(Debug, Parser)]
#[clap(author, version, about)]
struct Args {
    /// The path to the bytecode file to convert;
    /// if the file is xx.mv, the output file will be xx.json
    /// if the file is xx.json, the output file will be xx.mv
    #[clap(short = 'i', long = "input")]
    pub input_file_path: String,

    /// The path to the output file dir, if absent, use the current dir
    #[clap(short = 'o', long = "output")]
    pub output_file_dir: Option<String>,

    /// Should verify the module before convert.
    #[clap(long)]
    pub verify: bool,
}

fn run() -> Result<()> {
    let args = Args::parse();

    let input_path = Path::new(&args.input_file_path);
    if input_path.is_dir() {
        bail!("The input file path is a directory, please input a file path");
    }
    let input_source = move_bytecode_converter::InputSource::new(input_path.to_path_buf())?;
    let output_dir = args
        .output_file_dir
        .map(|s| PathBuf::from_str(&s).unwrap())
        .unwrap_or_else(|| std::env::current_dir().unwrap());
    if !output_dir.is_dir() {
        bail!("The output file path is a file, please input a directory path");
    }
    input_source.convert(&output_dir, args.verify)
}

fn main() {
    match run() {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}
