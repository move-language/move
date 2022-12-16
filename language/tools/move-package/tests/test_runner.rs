// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::{anyhow, bail};
use move_command_line_common::testing::{
    add_update_baseline_fix, format_diff, read_env_update_baseline,
};
use move_package::{
    compilation::{
        build_plan::BuildPlan, compiled_package::CompiledPackageInfo, model_builder::ModelBuilder,
    },
    package_hooks,
    package_hooks::PackageHooks,
    resolution::resolution_graph::ResolvedPackage,
    source_package::parsed_manifest::{CustomDepInfo, PackageDigest},
    BuildConfig, ModelConfig,
};
use move_symbol_pool::Symbol;
use std::{
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};
use tempfile::{tempdir, TempDir};

const EXTENSIONS: &[&str] = &["resolved", "compiled", "modeled"];

pub fn run_test(path: &Path) -> datatest_stable::Result<()> {
    package_hooks::register_package_hooks(Box::new(TestHooks()));
    let update_baseline = read_env_update_baseline();

    if path.iter().any(|part| part == "deps_only") {
        return Ok(());
    }

    let test = Test::from_path(path)?;
    let output = test.run().unwrap_or_else(|err| format!("{:#}\n", err));

    if update_baseline {
        fs::write(&test.expected, &output)?;
        return Ok(());
    }

    let expected = fs::read_to_string(&test.expected)?;
    if expected != output {
        return Err(anyhow!(add_update_baseline_fix(format!(
            "Expected outputs differ for {:?}:\n{}",
            test.expected,
            format_diff(expected, output),
        )))
        .into());
    }

    Ok(())
}

struct Test<'a> {
    toml_path: &'a Path,
    expected: PathBuf,
    output_dir: TempDir,
}

impl Test<'_> {
    fn from_path(toml_path: &Path) -> datatest_stable::Result<Test> {
        let mut candidates = EXTENSIONS
            .iter()
            .map(|ext| toml_path.with_extension(*ext))
            .filter(|p| p.is_file());

        let Some(expected) = candidates.next() else {
            return Err(anyhow!(
                "No snapshot file found for {:?}, please add a file with the same basename and one \
                 of the following extensions: {:#?}\n\n\
                 You probably want to re-run with `env UPDATE_BASELINE=1` after adding this file.",
                toml_path,
                EXTENSIONS,
            ).into());
        };

        let mut remaining: Vec<_> = candidates.collect();
        if !remaining.is_empty() {
            remaining.push(expected);
            return Err(anyhow!(
                "Multiple snapshot files found for {:?}, please supply only one of: {:#?}",
                toml_path,
                remaining,
            )
            .into());
        }

        Ok(Test {
            toml_path,
            expected,
            output_dir: tempdir()?,
        })
    }

    fn run(&self) -> anyhow::Result<String> {
        let config = BuildConfig {
            dev_mode: true,
            test_mode: false,
            generate_docs: false,
            generate_abis: false,
            install_dir: Some(self.output_dir.path().to_path_buf()),
            force_recompilation: false,
            ..Default::default()
        };

        let mut sink = std::io::sink();
        let mut resolved_package =
            config.resolution_graph_for_package(self.toml_path, &mut sink)?;

        let Some(ext) = self.expected.extension().and_then(OsStr::to_str) else {
            bail!("Unexpected snapshot file extension: {:?}", self.expected.extension());
        };

        Ok(match ext {
            "compiled" => {
                let mut pkg = BuildPlan::create(resolved_package)?.compile(&mut sink)?;
                scrub_compiled_package(&mut pkg.compiled_package_info);
                format!("{:#?}\n", pkg.compiled_package_info)
            }

            "modeled" => {
                ModelBuilder::create(
                    resolved_package,
                    ModelConfig {
                        all_files_as_targets: false,
                        target_filter: None,
                    },
                )
                .build_model()?;
                "Built model\n".to_string()
            }

            "resolved" => {
                for package in resolved_package.package_table.values_mut() {
                    scrub_resolved_package(package)
                }

                scrub_build_config(&mut resolved_package.build_options);
                format!("{:#?}\n", resolved_package)
            }

            ext => bail!("Unrecognised snapshot type: '{ext}'"),
        })
    }
}

fn scrub_build_config(config: &mut BuildConfig) {
    config.install_dir = Some(PathBuf::from("ELIDED_FOR_TEST"));
}

fn scrub_compiled_package(pkg: &mut CompiledPackageInfo) {
    pkg.source_digest = Some(PackageDigest::from("ELIDED_FOR_TEST"));
    scrub_build_config(&mut pkg.build_flags);
}

fn scrub_resolved_package(pkg: &mut ResolvedPackage) {
    pkg.package_path = PathBuf::from("ELIDED_FOR_TEST");
    pkg.source_digest = PackageDigest::from("ELIDED_FOR_TEST");
}

/// Some dummy hooks for testing the hook mechanism
struct TestHooks();

impl PackageHooks for TestHooks {
    fn custom_package_info_fields(&self) -> Vec<String> {
        vec!["test_hooks_field".to_owned()]
    }

    fn custom_dependency_key(&self) -> Option<String> {
        Some("custom".to_owned())
    }

    fn resolve_custom_dependency(
        &self,
        dep_name: Symbol,
        info: &CustomDepInfo,
    ) -> anyhow::Result<()> {
        bail!(
            "TestHooks resolve dep {:?} = {:?} {:?} {:?} {:?}",
            dep_name,
            info.node_url,
            info.package_name,
            info.package_address,
            info.subdir.to_string_lossy(),
        )
    }
}

datatest_stable::harness!(run_test, "tests/test_sources", r".*\.toml$");
