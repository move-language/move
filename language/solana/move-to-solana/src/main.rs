// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#![forbid(unsafe_code)]

use clap::Parser;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use move_to_solana::{options::Options, run_to_solana};

fn main() -> anyhow::Result<()> {
    let options = Options::parse();
    let color = if atty::is(atty::Stream::Stderr) && atty::is(atty::Stream::Stdout) {
        ColorChoice::Auto
    } else {
        ColorChoice::Never
    };
    let mut error_writer = StandardStream::stderr(color);
    run_to_solana(&mut error_writer, options)
}
