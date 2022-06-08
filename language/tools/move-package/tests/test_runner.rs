// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use move_command_line_common::testing::{
    add_update_baseline_fix, format_diff, read_env_update_baseline, EXP_EXT,
};
use move_package::{
    compilation::{build_plan::BuildPlan, model_builder::ModelBuilder},
    resolution::resolution_graph as RG,
    source_package::{manifest_parser as MP, parsed_manifest::PackageDigest},
    BuildConfig, ModelConfig,
};
use std::{
    ffi::OsStr,
    fs,
    path::{Component, Path, PathBuf},
};
use tempfile::tempdir;

const COMPILE_EXT: &str = "compile";
const MODEL_EXT: &str = "model";

pub fn run_test(path: &Path) -> datatest_stable::Result<()> {
    let update_baseline = read_env_update_baseline();
    if path
        .components()
        .any(|component| component == Component::Normal(OsStr::new("deps_only")))
    {
        return Ok(());
    }
    let exp_path = path.with_extension(EXP_EXT);
    let should_compile = path.with_extension(COMPILE_EXT).is_file();
    let should_model = path.with_extension(MODEL_EXT).is_file();

    let exp_exists = exp_path.is_file();

    let contents = fs::read_to_string(path)?;
    let output = match MP::parse_move_manifest_string(contents)
        .and_then(MP::parse_source_manifest)
        .and_then(|parsed_manifest| {
            RG::ResolutionGraph::new(
                parsed_manifest,
                path.parent().unwrap().to_path_buf(),
                BuildConfig {
                    dev_mode: true,
                    test_mode: false,
                    generate_docs: false,
                    generate_abis: false,
                    install_dir: Some(tempdir().unwrap().path().to_path_buf()),
                    force_recompilation: false,
                    ..Default::default()
                },
            )
        })
        .and_then(|rg| rg.resolve())
    {
        Ok(mut resolved_package) => match (should_compile, should_model) {
            (true, true) => {
                return Err(anyhow::format_err!(
                    "Cannot have compile and model flags set for same package"
                )
                .into())
            }
            (true, _) => match BuildPlan::create(resolved_package)
                .and_then(|bp| bp.compile(&mut Vec::new()))
            {
                Ok(mut pkg) => {
                    pkg.compiled_package_info.source_digest =
                        Some(PackageDigest::from("ELIDED_FOR_TEST"));
                    pkg.compiled_package_info.build_flags.install_dir =
                        Some(PathBuf::from("ELIDED_FOR_TEST"));
                    format!("{:#?}\n", pkg.compiled_package_info)
                }
                Err(error) => format!("{:#}\n", error),
            },
            (_, true) => match ModelBuilder::create(
                resolved_package,
                ModelConfig {
                    all_files_as_targets: false,
                    target_filter: None,
                },
            )
            .build_model()
            {
                Ok(_) => "Built model".to_string(),
                Err(error) => format!("{:#}\n", error),
            },
            (_, _) => {
                for (_, package) in resolved_package.package_table.iter_mut() {
                    package.package_path = PathBuf::from("ELIDED_FOR_TEST");
                    package.source_digest = PackageDigest::from("ELIDED_FOR_TEST");
                }
                resolved_package.build_options.install_dir = Some(PathBuf::from("ELIDED_FOR_TEST"));
                format!("{:#?}\n", resolved_package)
            }
        },
        Err(error) => format!("{:#}\n", error),
    };

    if update_baseline {
        fs::write(&exp_path, &output)?;
        return Ok(());
    }

    if exp_exists {
        let expected = fs::read_to_string(&exp_path)?;
        if expected != output {
            let msg = format!(
                "Expected outputs differ for {:?}:\n{}",
                exp_path,
                format_diff(expected, output)
            );
            return Err(anyhow::format_err!(add_update_baseline_fix(msg)).into());
        }
    } else {
        return Err(anyhow::format_err!(
            "No expected output found for {:?}.\
                    You probably want to rerun with `env UPDATE_BASELINE=1`",
            path
        )
        .into());
    }
    Ok(())
}

datatest_stable::harness!(run_test, "tests/test_sources", r".*\.toml$");
