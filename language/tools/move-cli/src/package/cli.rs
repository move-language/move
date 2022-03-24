// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use std::{
    collections::HashMap,
    fmt::Display,
    fs::{create_dir_all, read_to_string},
    io::Write,
    path::{Path, PathBuf},
    process::ExitStatus,
};

// if windows
#[cfg(target_family = "windows")]
use std::os::windows::process::ExitStatusExt;
// if unix
#[cfg(any(target_family = "unix"))]
use std::os::unix::prelude::ExitStatusExt;
// if not windows nor unix
#[cfg(not(any(target_family = "windows", target_family = "unix")))]
compile_error!("Unsupported OS, currently we only support windows and unix family");

use anyhow::{bail, Result};

use clap::Parser;
use move_command_line_common::files::{FileHash, MOVE_COVERAGE_MAP_EXTENSION};
use move_compiler::{
    compiled_unit::{CompiledUnit, NamedCompiledModule},
    diagnostics::{self, codes::Severity},
    shared::{NumberFormat, NumericalAddress},
    unit_test::{plan_builder::construct_test_plan, TestPlan},
    PASS_CFGIR,
};
use move_core_types::account_address::AccountAddress;
use move_coverage::{
    coverage_map::{output_map_to_file, CoverageMap},
    format_csv_summary, format_human_summary,
    source_coverage::SourceCoverageBuilder,
    summary::summarize_inst_cov,
};
use move_disassembler::disassembler::Disassembler;
use move_package::{
    compilation::{build_plan::BuildPlan, compiled_package::CompiledUnitWithSource},
    resolution::resolution_graph::ResolutionGraph,
    source_package::layout::SourcePackageLayout,
    ModelConfig,
};
use move_unit_test::UnitTestingConfig;

use crate::{package::prover::run_move_prover, NativeFunctionRecord};

#[derive(Parser)]
pub enum CoverageSummaryOptions {
    /// Display a coverage summary for all modules in this package
    #[clap(name = "summary")]
    Summary {
        /// Whether function coverage summaries should be displayed
        #[clap(long = "summarize-functions")]
        functions: bool,
        /// Output CSV data of coverage
        #[clap(long = "csv")]
        output_csv: bool,
    },
    /// Display coverage information about the module against source code
    #[clap(name = "source")]
    Source {
        #[clap(long = "module")]
        module_name: String,
    },
    /// Display coverage information about the module against disassembled bytecode
    #[clap(name = "bytecode")]
    Bytecode {
        #[clap(long = "module")]
        module_name: String,
    },
}

#[derive(Parser)]
pub enum PackageCommand {
    /// Create a new Move package with name `name` at `path`. If `path` is not provided the package
    /// will be created in the directory `name`.
    #[clap(name = "new")]
    New {
        /// The name of the package to be created.
        name: String,
    },
    /// Build the package at `path`. If no path is provided defaults to current directory.
    #[clap(name = "build")]
    Build,
    /// Print address information.
    #[clap(name = "info")]
    Info,
    /// Generate error map for the package and its dependencies at `path` for use by the Move
    /// explanation tool.
    #[clap(name = "errmap")]
    ErrMapGen {
        /// The prefix that all error reasons within modules will be prefixed with, e.g., "E" if
        /// all error reasons are "E_CANNOT_PERFORM_OPERATION", "E_CANNOT_ACCESS", etc.
        #[clap(long)]
        error_prefix: Option<String>,
        /// The file to serialize the generated error map to.
        #[clap(long, default_value = "error_map", parse(from_os_str))]
        output_file: PathBuf,
    },
    /// Run the Move Prover on the package at `path`. If no path is provided defaults to current
    /// directory. Use `.. prove .. -- <options>` to pass on options to the prover.
    #[clap(name = "prove")]
    Prove {
        /// The target filter used to prune the modules to verify. Modules with a name that contains
        /// this string will be part of verification.
        #[clap(short = 't', long = "target")]
        target_filter: Option<String>,
        /// Internal field indicating that this prover run is for a test.
        #[clap(skip)]
        for_test: bool,
        /// Any options passed to the prover.
        #[clap(subcommand)]
        options: Option<ProverOptions>,
    },
    /// Inspect test coverage for this package. A previous test run with the `--coverage` flag must
    /// have previously been run.
    #[clap(name = "coverage")]
    CoverageReport {
        #[clap(subcommand)]
        options: CoverageSummaryOptions,
    },
    /// Run Move unit tests in this package.
    #[clap(name = "test")]
    UnitTest {
        /// Bound the number of instructions that can be executed by any one test.
        #[clap(
            name = "instructions",
            default_value = "5000",
            short = 'i',
            long = "instructions"
        )]
        instruction_execution_bound: u64,
        /// A filter string to determine which unit tests to run. A unit test will be run only if it
        /// contains this string in its fully qualified (<addr>::<module_name>::<fn_name>) name.
        #[clap(name = "filter", short = 'f', long = "filter")]
        filter: Option<String>,
        /// List all tests
        #[clap(name = "list", short = 'l', long = "list")]
        list: bool,
        /// Number of threads to use for running tests.
        #[clap(
            name = "num_threads",
            default_value = "8",
            short = 't',
            long = "threads"
        )]
        num_threads: usize,
        /// Report test statistics at the end of testing
        #[clap(name = "report_statistics", short = 's', long = "statistics")]
        report_statistics: bool,
        /// Show the storage state at the end of execution of a failing test
        #[clap(name = "global_state_on_error", short = 'g', long = "state_on_error")]
        report_storage_on_error: bool,
        /// Use the stackless bytecode interpreter to run the tests and cross check its results with
        /// the execution result from Move VM.
        #[clap(long = "stackless")]
        check_stackless_vm: bool,
        /// Verbose mode
        #[clap(long = "verbose")]
        verbose_mode: bool,
        /// Collect coverage information for later use with the various `package coverage` subcommands
        #[clap(long = "coverage")]
        compute_coverage: bool,

        /// Use the EVM-based execution backend.
        /// Does not work with --stackless.
        #[cfg(feature = "evm-backend")]
        #[structopt(long = "evm")]
        evm: bool,
    },
    /// Disassemble the Move bytecode pointed to
    #[clap(name = "disassemble")]
    BytecodeView {
        /// Start a disassembled bytecode-to-source explorer
        #[clap(long = "interactive")]
        interactive: bool,
        /// The package name. If not provided defaults to current package modules only
        #[clap(long = "package")]
        package_name: Option<String>,
        /// The name of the module or script in the package to disassemble
        #[clap(long = "name")]
        module_or_script_name: String,
    },
}

#[derive(Parser, Debug)]
pub enum ProverOptions {
    // Pass through unknown commands to the prover Clap parser
    #[clap(
        external_subcommand,
        takes_value(true),
        multiple_values(true),
        multiple_occurrences(true)
    )]
    Options(Vec<String>),
}

/// Encapsulates the possible returned states when running unit tests on a move package.
#[derive(PartialEq)]
pub enum UnitTestResult {
    Success,
    Failure,
}

impl From<UnitTestResult> for ExitStatus {
    fn from(result: UnitTestResult) -> Self {
        match result {
            UnitTestResult::Success => ExitStatus::from_raw(0),
            UnitTestResult::Failure => ExitStatus::from_raw(1),
        }
    }
}

impl CoverageSummaryOptions {
    pub fn handle_command(&self, config: move_package::BuildConfig, path: &Path) -> Result<()> {
        let coverage_map = CoverageMap::from_binary_file(path.join(".coverage_map.mvcov"))?;
        let package = config.compile_package(path, &mut Vec::new())?;
        let modules: Vec<_> = package
            .modules()?
            .filter_map(|unit| match &unit.unit {
                CompiledUnit::Module(NamedCompiledModule { module, .. }) => Some(module.clone()),
                _ => None,
            })
            .collect();
        match self {
            CoverageSummaryOptions::Source { module_name } => {
                let unit = package.get_module_by_name(module_name)?;
                let source_path = &unit.source_path;
                let (module, source_map) = match &unit.unit {
                    CompiledUnit::Module(NamedCompiledModule {
                        module, source_map, ..
                    }) => (module, source_map),
                    _ => panic!("Should all be modules"),
                };
                let source_coverage = SourceCoverageBuilder::new(module, &coverage_map, source_map);
                source_coverage
                    .compute_source_coverage(source_path)
                    .output_source_coverage(&mut std::io::stdout())
                    .unwrap();
            }
            CoverageSummaryOptions::Summary {
                functions,
                output_csv,
                ..
            } => {
                let coverage_map = coverage_map.to_unified_exec_map();
                if *output_csv {
                    format_csv_summary(
                        modules.as_slice(),
                        &coverage_map,
                        summarize_inst_cov,
                        &mut std::io::stdout(),
                    )
                } else {
                    format_human_summary(
                        modules.as_slice(),
                        &coverage_map,
                        summarize_inst_cov,
                        &mut std::io::stdout(),
                        *functions,
                    )
                }
            }
            CoverageSummaryOptions::Bytecode { module_name } => {
                let unit = package.get_module_by_name(module_name)?;
                let mut disassembler = Disassembler::from_unit(&unit.unit);
                disassembler.add_coverage_map(coverage_map.to_unified_exec_map());
                println!("{}", disassembler.disassemble()?);
            }
        }
        Ok(())
    }
}

pub fn handle_package_commands(
    path: &Path,
    config: move_package::BuildConfig,
    cmd: &PackageCommand,
    natives: Vec<NativeFunctionRecord>,
) -> Result<()> {
    // This is the exceptional command as it doesn't need a package to run, so we can't count on
    // being able to root ourselves.
    if let PackageCommand::New { name } = cmd {
        let creation_path = Path::new(&path).join(name);
        create_move_package(name, &creation_path)?;
        return Ok(());
    }

    // Always root ourselves to the package root, and then compile relative to that.
    let rooted_path = SourcePackageLayout::try_find_root(&path.canonicalize()?)?;
    std::env::set_current_dir(&rooted_path).unwrap();

    let rerooted_path = PathBuf::from(".");

    match cmd {
        PackageCommand::Build => {
            config.compile_package(&rerooted_path, &mut std::io::stdout())?;
        }
        PackageCommand::Info => {
            config
                .resolution_graph_for_package(&rerooted_path)?
                .print_info()?;
        }
        PackageCommand::BytecodeView {
            interactive,
            package_name,
            module_or_script_name,
        } => {
            // Make sure the package is built
            let package = config.compile_package(&rerooted_path, &mut Vec::new())?;
            // Find the package we're interested in looking at, we default to the root package.
            let needle_package = match package_name {
                Some(package_name) => {
                    if package_name == package.compiled_package_info.package_name.as_str() {
                        &package
                    } else {
                        package.get_dependency_by_name(package_name)?
                    }
                }
                None => &package,
            };
            match needle_package
                .get_module_by_name(module_or_script_name)
                .ok()
            {
                None => bail!(
                    "Unable to find module or script with name '{}' in package '{}'",
                    module_or_script_name,
                    needle_package.compiled_package_info.package_name
                ),
                Some(unit) => {
                    // Once we find the compiled bytecode we're interested in, startup the bytecode
                    // viewer, or run the disassembler depending on if we need to run interactively
                    // or not.
                    if *interactive {
                        match unit {
                            CompiledUnitWithSource {
                                unit:
                                    CompiledUnit::Module(NamedCompiledModule {
                                        module, source_map, ..
                                    }),
                                source_path,
                            } => move_bytecode_viewer::start_viewer_in_memory(
                                module.clone(),
                                source_map.clone(),
                                source_path,
                            ),
                            _ => bail!("Interactive disassembler not supported for scripts"),
                        }
                    } else {
                        println!("{}", Disassembler::from_unit(&unit.unit).disassemble()?);
                    }
                }
            }
        }
        PackageCommand::Prove {
            target_filter,
            for_test,
            options,
        } => {
            if let Some(ProverOptions::Options(opts)) = options {
                run_move_prover(config, &rerooted_path, target_filter, *for_test, opts)?
            } else {
                run_move_prover(config, &rerooted_path, target_filter, *for_test, &[])?
            }
        }
        PackageCommand::ErrMapGen {
            error_prefix,
            output_file,
        } => {
            let mut errmap_options = move_errmapgen::ErrmapOptions::default();
            if let Some(err_prefix) = error_prefix {
                errmap_options.error_prefix = err_prefix.to_string();
            }
            errmap_options.output_file = output_file
                .with_extension(move_command_line_common::files::MOVE_ERROR_DESC_EXTENSION)
                .to_string_lossy()
                .to_string();
            let model = config.move_model_for_package(
                &rerooted_path,
                ModelConfig {
                    all_files_as_targets: true,
                    target_filter: None,
                },
            )?;
            let mut errmap_gen = move_errmapgen::ErrmapGen::new(&model, &errmap_options);
            errmap_gen.gen();
            errmap_gen.save_result();
        }
        PackageCommand::UnitTest {
            instruction_execution_bound,
            filter,
            list,
            num_threads,
            report_statistics,
            report_storage_on_error,
            check_stackless_vm,
            verbose_mode,
            compute_coverage,

            #[cfg(feature = "evm-backend")]
            evm,
        } => {
            let unit_test_config = UnitTestingConfig {
                instruction_execution_bound: *instruction_execution_bound,
                filter: filter.clone(),
                list: *list,
                num_threads: *num_threads,
                report_statistics: *report_statistics,
                report_storage_on_error: *report_storage_on_error,
                check_stackless_vm: *check_stackless_vm,
                verbose: *verbose_mode,

                #[cfg(feature = "evm-backend")]
                evm: *evm,

                ..UnitTestingConfig::default_with_bound(None)
            };
            let result = run_move_unit_tests(
                &rerooted_path,
                config,
                unit_test_config,
                natives,
                *compute_coverage,
            )?;

            // Return a non-zero exit code if any test failed
            if let UnitTestResult::Failure = result {
                std::process::exit(1)
            }
        }
        PackageCommand::CoverageReport { options } => {
            options.handle_command(config, &rerooted_path)?;
        }
        PackageCommand::New { .. } => {
            panic!("Hit a package new command after it should have been handled -- this should never happen")
        }
    };
    Ok(())
}

fn extract_named_address_values(
    resolution_graph: &ResolutionGraph<AccountAddress>,
) -> Vec<(String, NumericalAddress)> {
    let rooot_package_name = &resolution_graph.root_package.package.name;
    let root_package = resolution_graph
        .package_table
        .get(rooot_package_name)
        .expect("Failed to find root package in package table -- this should never happen");

    root_package
        .resolution_table
        .iter()
        .map(|(name, addr)| {
            (
                name.to_string(),
                NumericalAddress::new(addr.into_bytes(), NumberFormat::Hex),
            )
        })
        .collect()
}

pub fn run_move_unit_tests(
    pkg_path: &Path,
    mut build_config: move_package::BuildConfig,
    mut unit_test_config: UnitTestingConfig,
    natives: Vec<NativeFunctionRecord>,
    compute_coverage: bool,
) -> Result<UnitTestResult> {
    let mut test_plan = None;
    build_config.test_mode = true;
    build_config.dev_mode = true;

    // Build the resolution graph
    let resolution_graph = build_config.resolution_graph_for_package(pkg_path)?;

    // Note: unit_test_config.named_address_values is always set to vec![] (the default value) before
    // being passed in.
    unit_test_config.named_address_values = extract_named_address_values(&resolution_graph);

    // Get the source files for all modules. We need this in order to report source-mapped error
    // messages.
    let dep_file_map: HashMap<_, _> = resolution_graph
        .package_table
        .iter()
        .flat_map(|(_, rpkg)| {
            rpkg.get_sources(&resolution_graph.build_options)
                .unwrap()
                .iter()
                .map(|fname| {
                    let contents = read_to_string(Path::new(fname.as_str())).unwrap();
                    let fhash = FileHash::new(&contents);
                    (fhash, (*fname, contents))
                })
                .collect::<HashMap<_, _>>()
        })
        .collect();
    let build_plan = BuildPlan::create(resolution_graph)?;
    // Compile the package now. We need to treat the root package differently since we need to
    // construct the test plan. This means that we can't rely on the cached version of the root
    // package even if it is consistent. Additionally, we need to intercede in the compilation
    // process being performed by the Move package system, to first grab the compilation env,
    // construct the test plan from it, and then save it, before resuming the rest of the
    // compilation and returning the results and control back to the Move package system.
    let pkg = build_plan.compile_with_driver(&mut std::io::stdout(), |compiler, is_root| {
        if !is_root {
            compiler.build_and_report()
        } else {
            let (files, comments_and_compiler_res) = compiler.run::<PASS_CFGIR>().unwrap();
            let (_, compiler) =
                diagnostics::unwrap_or_report_diagnostics(&files, comments_and_compiler_res);
            let (mut compiler, cfgir) = compiler.into_ast();
            let compilation_env = compiler.compilation_env();
            let built_test_plan = construct_test_plan(compilation_env, &cfgir);

            if let Err(diags) = compilation_env.check_diags_at_or_above_severity(Severity::Warning)
            {
                diagnostics::report_diagnostics(&files, diags);
            }

            let compilation_result = compiler.at_cfgir(cfgir).build();

            let (units, _) = diagnostics::unwrap_or_report_diagnostics(&files, compilation_result);

            test_plan = Some((built_test_plan, files.clone(), units.clone()));
            Ok((files, units))
        }
    })?;

    let (test_plan, mut files, units) = test_plan.unwrap();
    files.extend(dep_file_map);
    let test_plan = test_plan.unwrap();
    let no_tests = test_plan.is_empty();
    let mut test_plan = TestPlan::new(test_plan, files, units);
    // Insert all of the package's transitive dependencies into the set of modules that
    // need to be published for unit testing.
    for pkg in pkg.0.transitive_dependencies() {
        for unit in &pkg.compiled_units {
            match &unit.unit {
                CompiledUnit::Script(_) => (),
                CompiledUnit::Module(module) => {
                    test_plan
                        .module_info
                        .insert(module.module.self_id(), module.clone());
                }
            }
        }
    }

    let trace_path = pkg_path.join(".trace");
    let coverage_map_path = pkg_path
        .join(".coverage_map")
        .with_extension(MOVE_COVERAGE_MAP_EXTENSION);
    let cleanup_trace = || {
        if compute_coverage && trace_path.exists() {
            std::fs::remove_file(&trace_path).unwrap();
        }
    };

    cleanup_trace();

    // If we need to compute test coverage set the VM tracking environment variable since we will
    // need this trace to construct the coverage information.
    if compute_coverage {
        std::env::set_var("MOVE_VM_TRACE", &trace_path);
    }

    // Run the tests. If any of the tests fail, then we don't produce a coverage report, so cleanup
    // the trace files.
    if !unit_test_config
        .run_and_report_unit_tests(test_plan, Some(natives), std::io::stdout())
        .unwrap()
        .1
    {
        cleanup_trace();
        return Ok(UnitTestResult::Failure);
    }

    // Compute the coverage map. This will be used by other commands after this.
    if compute_coverage && !no_tests {
        let coverage_map = CoverageMap::from_trace_file(trace_path);
        output_map_to_file(&coverage_map_path, &coverage_map).unwrap();
    }
    Ok(UnitTestResult::Success)
}

pub fn create_move_package<S: AsRef<str> + Display>(name: S, creation_path: &Path) -> Result<()> {
    create_dir_all(creation_path.join(SourcePackageLayout::Sources.path()))?;
    let mut w = std::fs::File::create(creation_path.join(SourcePackageLayout::Manifest.path()))?;
    writeln!(
        &mut w,
        "[package]\nname = \"{}\"\nversion = \"0.0.0\"",
        name
    )?;
    Ok(())
}
