// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::anyhow;
use codespan_reporting::{diagnostic::Severity, term::termcolor::Buffer};
use move_command_line_common::testing::EXP_EXT;
use move_model::{options::ModelBuilderOptions, run_model_builder_with_options};
use move_prover_test_utils::{baseline_test::verify_or_update_baseline, extract_test_directives};
use move_to_yul::{generator::Generator, options::Options};
use std::{
    io::Write,
    path::Path,
    process::{Command, Stdio},
};

fn test_runner(path: &Path) -> datatest_stable::Result<()> {
    let mut sources = extract_test_directives(path, "// dep:")?;
    sources.push(path.to_string_lossy().to_string());
    let env = run_model_builder_with_options(
        &sources,
        &[],
        ModelBuilderOptions::default(),
        move_stdlib::move_stdlib_named_addresses(),
    )?;
    let options = Options::default();
    let mut out = Generator::run_to_string(&options, &env);
    if !env.has_errors() {
        out = format!("{}\n\n{}", out, compile_check(&options, &out));
    }
    let mut error_writer = Buffer::no_color();
    env.report_diag(&mut error_writer, Severity::Help);
    let diag = String::from_utf8_lossy(&error_writer.into_inner()).to_string();
    if !diag.is_empty() {
        out = format!("{}\n\n!! Move-To-Yul Diagnostics:\n {}", out, diag);
    }
    let baseline_path = path.with_extension(EXP_EXT);
    verify_or_update_baseline(baseline_path.as_path(), &out)?;
    Ok(())
}

fn compile_check(options: &Options, source: &str) -> String {
    let run = || -> anyhow::Result<String> {
        let mut child = Command::new(&options.solc_exe)
            .arg("--optimize")
            .arg("--strict-assembly")
            .arg("--ir-optimized")
            .arg("-")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;
        let pipe = child.stdin.as_mut().ok_or(anyhow!("cannot create pipe"))?;
        pipe.write_all(source.as_bytes())?;
        let out = child.wait_with_output()?;
        let result = if out.status.success() {
            let yul = String::from_utf8_lossy(&out.stdout).to_string();
            // Yul output contains markers which are mistaken for merge conflicts. Skip
            // them.
            let yul_slice = if let Some(p) = yul.find("Pretty printed source:") {
                &yul[p..]
            } else {
                yul.as_str()
            };
            format!("!! Optimized Yul\n\n{}", yul_slice)
        } else {
            format!(
                "!! Errors compiling Yul\n\n{}",
                String::from_utf8_lossy(&out.stderr).to_string()
            )
        };
        Ok(result)
    };
    run().unwrap_or_else(|e| {
        panic!(
            "cannot run solidity compiler`{}`: {}.\n Make sure SOLC_EXE is set.",
            options.solc_exe, e
        )
    })
}

datatest_stable::harness!(test_runner, "tests", r".*\.move$");
