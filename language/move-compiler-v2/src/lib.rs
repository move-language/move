// Copyright © Aptos Foundation
// Parts of the project are originally copyright © Meta Platforms, Inc.
// SPDX-License-Identifier: Apache-2.0

mod bytecode_generator;
mod experiments;
mod file_format_generator;
pub mod flow_insensitive_checkers;
pub mod function_checker;
pub mod inliner;
pub mod logging;
mod options;
pub mod pipeline;

use crate::pipeline::{
    ability_checker::AbilityChecker, avail_copies_analysis::AvailCopiesAnalysisProcessor,
    copy_propagation::CopyPropagation, dead_store_elimination::DeadStoreElimination,
    exit_state_analysis::ExitStateAnalysisProcessor, explicit_drop::ExplicitDrop,
    livevar_analysis_processor::LiveVarAnalysisProcessor,
    reference_safety_processor::ReferenceSafetyProcessor,
    uninitialized_use_checker::UninitializedUseChecker,
    unreachable_code_analysis::UnreachableCodeProcessor,
    unreachable_code_remover::UnreachableCodeRemover,
};
use anyhow::bail;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream, WriteColor};
pub use experiments::*;
use log::{debug, info, log_enabled, trace, Level};
use move_binary_format::binary_views::BinaryIndexedView;
use move_command_line_common::files::FileHash;
use move_compiler::{
    compiled_unit::{
        verify_units, AnnotatedCompiledModule, AnnotatedCompiledScript, AnnotatedCompiledUnit,
        CompiledUnit, FunctionInfo,
    },
    diagnostics::FilesSourceText,
    shared::{known_attributes::KnownAttribute, unique_map::UniqueMap},
};
use move_disassembler::disassembler::Disassembler;
use move_ir_types::location;
use move_model::{add_move_lang_diagnostics, model::GlobalEnv, PackageInfo};
use move_stackless_bytecode::function_target_pipeline::{
    FunctionTargetPipeline, FunctionTargetsHolder, FunctionVariant,
};
use move_symbol_pool::Symbol;
pub use options::*;
use std::{collections::BTreeSet, path::Path};

/// Run Move compiler and print errors to stderr.
pub fn run_move_compiler_to_stderr(
    options: Options,
) -> anyhow::Result<(GlobalEnv, Vec<AnnotatedCompiledUnit>)> {
    let mut error_writer = StandardStream::stderr(ColorChoice::Auto);
    run_move_compiler(&mut error_writer, options)
}

/// Run move compiler and print errors to given writer.
pub fn run_move_compiler(
    error_writer: &mut impl WriteColor,
    options: Options,
) -> anyhow::Result<(GlobalEnv, Vec<AnnotatedCompiledUnit>)> {
    logging::setup_logging();
    info!("Move Compiler v2");
    // Run context check.
    let mut env = run_checker(options.clone())?;
    check_errors(&env, error_writer, "checking errors")?;

    trace!("After context check, GlobalEnv=\n{}", env.dump_env());

    // Flow-insensitive checks on AST
    flow_insensitive_checkers::check_for_unused_vars_and_params(&mut env);
    function_checker::check_for_function_typed_parameters(&mut env);
    function_checker::check_access_and_use(&mut env, true);
    check_errors(&env, error_writer, "checking errors")?;

    trace!(
        "After flow-insensitive checks, GlobalEnv=\n{}",
        env.dump_env()
    );

    // Run inlining.
    inliner::run_inlining(&mut env);
    check_errors(&env, error_writer, "inlining")?;

    debug!("After inlining, GlobalEnv=\n{}", env.dump_env());

    function_checker::check_access_and_use(&mut env, false);
    check_errors(&env, error_writer, "post-inlining access checks")?;

    // Run code generator
    let mut targets = run_bytecode_gen(&env);
    check_errors(&env, error_writer, "code generation errors")?;

    // Run transformation pipeline
    let pipeline = bytecode_pipeline(&env);
    if log_enabled!(Level::Debug) {
        // Dump bytecode, providing a name for the target derived from the first input file.
        let dump_base_name = options
            .sources
            .first()
            .and_then(|f| {
                Path::new(f)
                    .file_name()
                    .map(|f| f.to_string_lossy().as_ref().to_owned())
            })
            .unwrap_or_else(|| "dump".to_owned());
        pipeline.run_with_dump(
            &env,
            &mut targets,
            &dump_base_name,
            false,
            &pipeline::register_formatters,
        )
    } else {
        pipeline.run(&env, &mut targets)
    }
    check_errors(&env, error_writer, "stackless-bytecode analysis errors")?;

    let modules_and_scripts = run_file_format_gen(&env, &targets);
    check_errors(&env, error_writer, "assembling errors")?;

    debug!(
        "File format bytecode:\n{}",
        disassemble_compiled_units(&modules_and_scripts)?
    );

    let annotated_units = annotate_units(modules_and_scripts);
    run_bytecode_verifier(&annotated_units, &mut env);
    check_errors(&env, error_writer, "bytecode verification errors")?;

    Ok((env, annotated_units))
}

/// Run the type checker and return the global env (with errors if encountered). The result
/// fails not on context checking errors, but possibly on i/o errors.
pub fn run_checker(options: Options) -> anyhow::Result<GlobalEnv> {
    info!("Type Checking");
    // Run the model builder, which performs context checking.
    let addrs = move_model::parse_addresses_from_options(options.named_address_mapping.clone())?;
    let mut env = move_model::run_model_builder_in_compiler_mode(
        PackageInfo {
            sources: options.sources.clone(),
            address_map: addrs.clone(),
        },
        vec![PackageInfo {
            sources: options.dependencies.clone(),
            address_map: addrs.clone(),
        }],
        options.skip_attribute_checks,
        if !options.skip_attribute_checks && options.known_attributes.is_empty() {
            KnownAttribute::get_all_attribute_names()
        } else {
            &options.known_attributes
        },
    )?;
    // Store address aliases
    let map = addrs
        .into_iter()
        .map(|(s, a)| (env.symbol_pool().make(&s), a.into_inner()))
        .collect();
    env.set_address_alias_map(map);
    // Store options in env, for later access
    env.set_extension(options);
    Ok(env)
}

// Run the (stackless) bytecode generator. For each function which is target of the
// compilation, create an entry in the functions target holder which encapsulate info
// like the generated bytecode.
pub fn run_bytecode_gen(env: &GlobalEnv) -> FunctionTargetsHolder {
    info!("Stackless bytecode Generation");
    let mut targets = FunctionTargetsHolder::default();
    let mut todo = BTreeSet::new();
    let mut done = BTreeSet::new();
    for module in env.get_modules() {
        if module.is_target() {
            for fun in module.get_functions() {
                let id = fun.get_qualified_id();
                todo.insert(id);
            }
        }
    }
    while let Some(id) = todo.pop_first() {
        done.insert(id);
        let func_env = env.get_function(id);
        let data = bytecode_generator::generate_bytecode(env, id);
        targets.insert_target_data(&id, FunctionVariant::Baseline, data);
        for callee in func_env
            .get_called_functions()
            .expect("called functions available")
        {
            if !done.contains(callee) {
                todo.insert(*callee);
            }
        }
    }
    targets
}

pub fn run_file_format_gen(env: &GlobalEnv, targets: &FunctionTargetsHolder) -> Vec<CompiledUnit> {
    info!("File Format Generation");
    file_format_generator::generate_file_format(env, targets)
}

/// Returns the bytecode processing pipeline.
pub fn bytecode_pipeline(env: &GlobalEnv) -> FunctionTargetPipeline {
    let options = env.get_extension::<Options>().expect("options");
    let safety_on = !options.experiment_on(Experiment::NO_SAFETY);
    let mut pipeline = FunctionTargetPipeline::default();
    if safety_on {
        pipeline.add_processor(Box::new(UninitializedUseChecker {}));
    }
    pipeline.add_processor(Box::new(LiveVarAnalysisProcessor {
        with_copy_inference: true,
    }));
    pipeline.add_processor(Box::new(ReferenceSafetyProcessor {}));
    pipeline.add_processor(Box::new(ExplicitDrop {}));
    if safety_on {
        // only used for ability checking
        pipeline.add_processor(Box::new(ExitStateAnalysisProcessor {}));
        // Ability checker is functionally not relevant so can be completely skipped if safety is off
        pipeline.add_processor(Box::new(AbilityChecker {}));
    }
    // The default optimization pipeline is currently always run by the compiler.
    add_default_optimization_pipeline(&mut pipeline);
    // Run live var analysis again because it could be invalidated by previous pipeline steps,
    // but it is needed by file format generator.
    pipeline.add_processor(Box::new(LiveVarAnalysisProcessor {
        with_copy_inference: false,
    }));
    pipeline
}

/// Add the default optimization pipeline to the given function target pipeline.
///
/// Any compiler errors or warnings should be reported before running this section, as we can
/// potentially delete or change code through these optimizations.
/// While this section of the pipeline is optional, some code that used to previously compile
/// may no longer compile without this section because of using too many local (temp) variables.
fn add_default_optimization_pipeline(pipeline: &mut FunctionTargetPipeline) {
    // Available copies analysis is needed by copy propagation.
    pipeline.add_processor(Box::new(AvailCopiesAnalysisProcessor {}));
    pipeline.add_processor(Box::new(CopyPropagation {}));
    // Live var analysis is needed by dead store elimination.
    pipeline.add_processor(Box::new(LiveVarAnalysisProcessor {
        with_copy_inference: false,
    }));
    pipeline.add_processor(Box::new(DeadStoreElimination {}));
    pipeline.add_processor(Box::new(UnreachableCodeProcessor {}));
    pipeline.add_processor(Box::new(UnreachableCodeRemover {}));
}

/// Disassemble the given compiled units and return the disassembled code as a string.
pub fn disassemble_compiled_units(units: &[CompiledUnit]) -> anyhow::Result<String> {
    let disassembled_units: anyhow::Result<Vec<_>> = units
        .iter()
        .map(|unit| {
            let view = match unit {
                CompiledUnit::Module(module) => BinaryIndexedView::Module(&module.module),
                CompiledUnit::Script(script) => BinaryIndexedView::Script(&script.script),
            };
            Disassembler::from_view(view, location::Loc::new(FileHash::empty(), 0, 0))
                .and_then(|d| d.disassemble())
        })
        .collect();
    Ok(disassembled_units?.concat())
}

/// Run the bytecode verifier on the given compiled units and add any diagnostics to the global env.
pub fn run_bytecode_verifier(units: &[AnnotatedCompiledUnit], env: &mut GlobalEnv) {
    let diags = verify_units(units);
    if !diags.is_empty() {
        add_move_lang_diagnostics(env, diags);
    }
}

/// Report any diags in the env to the writer and fail if there are errors.
pub fn check_errors<W: WriteColor>(
    env: &GlobalEnv,
    error_writer: &mut W,
    msg: &'static str,
) -> anyhow::Result<()> {
    let options = env.get_extension::<Options>().unwrap_or_default();
    env.report_diag(error_writer, options.report_severity());
    if env.has_errors() {
        bail!("exiting with {}", msg);
    } else {
        Ok(())
    }
}

/// Annotate the given compiled units.
/// TODO: this currently only fills in defaults. The annotations are only used in
/// the prover, and compiler v2 is not yet connected to the prover.
pub fn annotate_units(units: Vec<CompiledUnit>) -> Vec<AnnotatedCompiledUnit> {
    units
        .into_iter()
        .map(|u| match u {
            CompiledUnit::Module(named_module) => {
                let loc = named_module.source_map.definition_location;
                AnnotatedCompiledUnit::Module(AnnotatedCompiledModule {
                    loc,
                    module_name_loc: loc,
                    address_name: None,
                    named_module,
                    function_infos: UniqueMap::new(),
                })
            },
            CompiledUnit::Script(named_script) => {
                AnnotatedCompiledUnit::Script(AnnotatedCompiledScript {
                    loc: named_script.source_map.definition_location,
                    named_script,
                    function_info: FunctionInfo {
                        spec_info: Default::default(),
                    },
                })
            },
        })
        .collect()
}

/// Computes the `FilesSourceText` from the global environment, which maps IR loc file hashes
/// into files and sources. This value is used for the package system only.
pub fn make_files_source_text(env: &GlobalEnv) -> FilesSourceText {
    let mut result = FilesSourceText::new();
    for fid in env.get_source_file_ids() {
        if let Some(hash) = env.get_file_hash(fid) {
            let file_name = Symbol::from(env.get_file(fid).to_string_lossy().to_string());
            let file_content = env.get_file_source(fid).to_owned();
            result.insert(hash, (file_name, file_content));
        }
    }
    result
}
