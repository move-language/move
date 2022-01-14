// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

#![forbid(unsafe_code)]

pub mod generator;
pub mod options;
mod yul_functions;

use crate::{generator::Generator, options::Options};
use anyhow::anyhow;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream, WriteColor};
use move_model::{
    model::GlobalEnv, options::ModelBuilderOptions, parse_addresses_from_options,
    run_model_builder_with_options,
};

/// Run move-to-yul compiler and print errors to stderr.
pub fn run_to_yul_errors_to_stderr(options: Options) -> anyhow::Result<()> {
    let mut error_writer = StandardStream::stderr(ColorChoice::Auto);
    run_to_yul(&mut error_writer, options)
}

/// Run move-to-yul compiler and print errors to given writer.
pub fn run_to_yul<W: WriteColor>(error_writer: &mut W, options: Options) -> anyhow::Result<()> {
    // Run the model builder.
    let env = run_model_builder_with_options(
        &options.sources,
        &options.dependencies,
        ModelBuilderOptions::default(),
        parse_addresses_from_options(options.named_address_mapping.clone())?,
    )?;
    // If the model contains any errors, report them now and exit.
    check_errors(
        &env,
        &options,
        error_writer,
        "exiting with Move build errors",
    )?;
    Generator::run(&options, &env)?;
    check_errors(
        &env,
        &options,
        error_writer,
        "exiting with Yul generation errors",
    )
}

pub fn check_errors<W: WriteColor>(
    env: &GlobalEnv,
    options: &Options,
    error_writer: &mut W,
    msg: &'static str,
) -> anyhow::Result<()> {
    env.report_diag(error_writer, options.report_severity());
    if env.has_errors() {
        Err(anyhow!(msg))
    } else {
        Ok(())
    }
}
