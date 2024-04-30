// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#![forbid(unsafe_code)]

use crate::{
    tasks::{
        taskify, InitCommand, PrintBytecodeCommand, PrintBytecodeInputChoice, PublishCommand,
        RunCommand, SyntaxChoice, TaskCommand, TaskInput, ViewCommand,
    },
    vm_test_harness::TestRunConfig,
};
use anyhow::{anyhow, Result};
use clap::Parser;
use move_binary_format::{
    binary_views::BinaryIndexedView,
    file_format::{CompiledModule, CompiledScript},
};
use move_bytecode_source_map::mapping::SourceMapping;
use move_command_line_common::{
    address::ParsedAddress,
    env::read_bool_env_var,
    files::{MOVE_EXTENSION, MOVE_IR_EXTENSION},
    testing::{
        add_update_baseline_fix, format_diff, format_diff_no_color, read_env_update_baseline,
        EXP_EXT,
    },
    types::ParsedType,
    values::{ParsableValue, ParsedValue},
};
use move_compiler::{
    compiled_unit::AnnotatedCompiledUnit,
    diagnostics::{Diagnostics, FilesSourceText},
    shared::NumericalAddress,
    FullyCompiledProgram,
};
use move_core_types::{
    account_address::AccountAddress,
    identifier::{IdentStr, Identifier},
    language_storage::{ModuleId, StructTag, TypeTag},
};
use move_disassembler::disassembler::{Disassembler, DisassemblerOptions};
use move_ir_types::location::Spanned;
use move_symbol_pool::Symbol;
use move_vm_runtime::session::SerializedReturnValues;
use once_cell::sync::Lazy;
use regex::Regex;
use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    fmt::{Debug, Write as FmtWrite},
    io::Write,
    path::Path,
};
use tempfile::NamedTempFile;

pub struct ProcessedModule {
    module: CompiledModule,
    source_file: Option<(String, NamedTempFile)>,
}

pub struct CompiledState<'a> {
    pre_compiled_deps: Option<&'a FullyCompiledProgram>,
    pre_compiled_ids: BTreeSet<(AccountAddress, String)>,
    compiled_module_named_address_mapping: BTreeMap<ModuleId, Symbol>,
    pub named_address_mapping: BTreeMap<String, NumericalAddress>,
    default_named_address_mapping: Option<NumericalAddress>,
    modules: BTreeMap<ModuleId, ProcessedModule>,
}

impl<'a> CompiledState<'a> {
    pub fn resolve_named_address(&self, s: &str) -> AccountAddress {
        if let Some(addr) = self
            .named_address_mapping
            .get(s)
            .or(self.default_named_address_mapping.as_ref())
        {
            return AccountAddress::new(addr.into_bytes());
        }
        panic!("Failed to resolve named address '{}'", s)
    }

    pub fn resolve_address(&self, addr: &ParsedAddress) -> AccountAddress {
        match addr {
            ParsedAddress::Named(named_addr) => self.resolve_named_address(named_addr.as_str()),
            ParsedAddress::Numerical(addr) => addr.into_inner(),
        }
    }

    pub fn resolve_args<Extra: ParsableValue>(
        &self,
        args: Vec<ParsedValue<Extra>>,
    ) -> Result<Vec<Extra::ConcreteValue>> {
        args.into_iter()
            .map(|arg| arg.into_concrete_value(&|s| Some(self.resolve_named_address(s))))
            .collect()
    }

    pub fn resolve_type_args(&self, type_args: Vec<ParsedType>) -> Result<Vec<TypeTag>> {
        type_args
            .into_iter()
            .map(|arg| arg.into_type_tag(&|s| Some(self.resolve_named_address(s))))
            .collect()
    }
}

fn merge_output(left: Option<String>, right: Option<String>) -> Option<String> {
    match (left, right) {
        (None, right) => right,
        (left, None) => left,
        (Some(mut left), Some(right)) => {
            left.push_str(&right);
            Some(left)
        },
    }
}

pub trait MoveTestAdapter<'a>: Sized {
    type ExtraPublishArgs: Parser;
    type ExtraValueArgs: ParsableValue;
    type ExtraRunArgs: Parser;
    type Subcommand: Parser;
    type ExtraInitArgs: Parser;

    fn compiled_state(&mut self) -> &mut CompiledState<'a>;
    fn default_syntax(&self) -> SyntaxChoice;
    fn known_attributes(&self) -> &BTreeSet<String>;
    fn run_config(&self) -> TestRunConfig {
        TestRunConfig::CompilerV1
    }
    fn init(
        default_syntax: SyntaxChoice,
        comparison_mode: bool,
        run_config: TestRunConfig,
        option: Option<&'a FullyCompiledProgram>,
        init_data: Option<TaskInput<(InitCommand, Self::ExtraInitArgs)>>,
    ) -> (Self, Option<String>);
    fn publish_module(
        &mut self,
        module: CompiledModule,
        named_addr_opt: Option<Identifier>,
        gas_budget: Option<u64>,
        extra: Self::ExtraPublishArgs,
    ) -> Result<(Option<String>, CompiledModule)>;
    fn execute_script(
        &mut self,
        script: CompiledScript,
        type_args: Vec<TypeTag>,
        signers: Vec<ParsedAddress>,
        args: Vec<<<Self as MoveTestAdapter<'a>>::ExtraValueArgs as ParsableValue>::ConcreteValue>,
        gas_budget: Option<u64>,
        extra: Self::ExtraRunArgs,
    ) -> Result<(Option<String>, SerializedReturnValues)>;
    fn call_function(
        &mut self,
        module: &ModuleId,
        function: &IdentStr,
        type_args: Vec<TypeTag>,
        signers: Vec<ParsedAddress>,
        args: Vec<<<Self as MoveTestAdapter<'a>>::ExtraValueArgs as ParsableValue>::ConcreteValue>,
        gas_budget: Option<u64>,
        extra: Self::ExtraRunArgs,
    ) -> Result<(Option<String>, SerializedReturnValues)>;
    fn view_data(
        &mut self,
        address: AccountAddress,
        module: &ModuleId,
        resource: &IdentStr,
        type_args: Vec<TypeTag>,
    ) -> Result<String>;

    fn handle_subcommand(
        &mut self,
        subcommand: TaskInput<Self::Subcommand>,
    ) -> Result<Option<String>>;

    fn compile_module(
        &mut self,
        syntax: SyntaxChoice,
        data: Option<NamedTempFile>,
        start_line: usize,
        command_lines_stop: usize,
    ) -> Result<(
        NamedTempFile,
        Option<Symbol>,
        CompiledModule,
        Option<String>,
    )> {
        let data = match data {
            Some(f) => f,
            None => panic!(
                "Expected a module text block following 'publish' starting on lines {}-{}",
                start_line, command_lines_stop
            ),
        };
        let data_path = data.path().to_str().unwrap();
        let run_config = self.run_config();
        let state = self.compiled_state();
        let (named_addr_opt, module, warnings_opt) = match syntax {
            // Run the V2 compiler if requested
            SyntaxChoice::Source if run_config == TestRunConfig::CompilerV2 => {
                let ((module, _), warning_opt) = compile_source_unit_v2(
                    state.pre_compiled_deps,
                    state.named_address_mapping.clone(),
                    &state.source_files().cloned().collect::<Vec<_>>(),
                    data_path.to_owned(),
                    self.known_attributes(),
                )?;
                if let Some(module) = module {
                    (None, module, warning_opt)
                } else {
                    anyhow::bail!("expected a module but found a script")
                }
            },
            // In all other cases, run V1
            SyntaxChoice::Source => {
                let (unit, warnings_opt) = compile_source_unit(
                    state.pre_compiled_deps,
                    state.named_address_mapping.clone(),
                    &state.source_files().cloned().collect::<Vec<_>>(),
                    data_path.to_owned(),
                    self.known_attributes(),
                )?;
                let (named_addr_opt, module) = match unit {
                    AnnotatedCompiledUnit::Module(annot_module) => {
                        let (named_addr_opt, _id) = annot_module.module_id();
                        (
                            named_addr_opt.map(|n| n.value),
                            annot_module.named_module.module,
                        )
                    },
                    AnnotatedCompiledUnit::Script(_) => panic!(
                        "Expected a module text block, not a script, following 'publish' \
                         starting on lines {}-{}",
                        start_line, command_lines_stop
                    ),
                };
                (named_addr_opt, module, warnings_opt)
            },
            SyntaxChoice::IR => {
                let module = compile_ir_module(state.dep_modules(), data_path)?;
                (None, module, None)
            },
        };
        Ok((data, named_addr_opt, module, warnings_opt))
    }

    fn compile_script(
        &mut self,
        syntax: SyntaxChoice,
        data: Option<NamedTempFile>,
        start_line: usize,
        command_lines_stop: usize,
    ) -> Result<(CompiledScript, Option<String>)> {
        let data = match data {
            Some(f) => f,
            None => panic!(
                "Expected a script text block following 'run' starting on lines {}-{}",
                start_line, command_lines_stop
            ),
        };
        let data_path = data.path().to_str().unwrap();
        let run_config = self.run_config();
        let state = self.compiled_state();
        let (script, warning_opt) = match syntax {
            // Run the V2 compiler if requested.
            SyntaxChoice::Source if run_config == TestRunConfig::CompilerV2 => {
                let ((_, script), warning_opt) = compile_source_unit_v2(
                    state.pre_compiled_deps,
                    state.named_address_mapping.clone(),
                    &state.source_files().cloned().collect::<Vec<_>>(),
                    data_path.to_owned(),
                    self.known_attributes(),
                )?;
                if let Some(script) = script {
                    (script, warning_opt)
                } else {
                    anyhow::bail!("expected a script but found a module")
                }
            },
            // In all other Source cases, run the V1 compiler
            SyntaxChoice::Source => {
                let (unit, warning_opt) = compile_source_unit(
                    state.pre_compiled_deps,
                    state.named_address_mapping.clone(),
                    &state.source_files().cloned().collect::<Vec<_>>(),
                    data_path.to_owned(),
                    self.known_attributes(),
                )?;
                match unit {
                    AnnotatedCompiledUnit::Script(annot_script) => (annot_script.named_script.script, warning_opt),
                    AnnotatedCompiledUnit::Module(_) => panic!(
                        "Expected a script text block, not a module, following 'run' starting on lines {}-{}",
                        start_line, command_lines_stop
                    ),
                }
            },
            SyntaxChoice::IR => (compile_ir_script(state.dep_modules(), data_path)?, None),
        };
        Ok((script, warning_opt))
    }

    fn handle_command(
        &mut self,
        task: TaskInput<
            TaskCommand<
                Self::ExtraInitArgs,
                Self::ExtraPublishArgs,
                Self::ExtraValueArgs,
                Self::ExtraRunArgs,
                Self::Subcommand,
            >,
        >,
    ) -> Result<Option<String>> {
        let TaskInput {
            command,
            name,
            number,
            start_line,
            command_lines_stop,
            stop_line,
            data,
        } = task;
        match command {
            TaskCommand::Init { .. } => {
                panic!("The 'init' command is optional. But if used, it must be the first command")
            },
            TaskCommand::PrintBytecode(PrintBytecodeCommand { input, syntax }) => {
                let syntax = syntax.unwrap_or_else(|| self.default_syntax());
                let result = match input {
                    PrintBytecodeInputChoice::Script => {
                        let (script, _warning_opt) =
                            self.compile_script(syntax, data, start_line, command_lines_stop)?;
                        disassembler_for_view(BinaryIndexedView::Script(&script)).disassemble()?
                    },
                    PrintBytecodeInputChoice::Module => {
                        let (_data, _named_addr_opt, module, _warnings_opt) =
                            self.compile_module(syntax, data, start_line, command_lines_stop)?;
                        disassembler_for_view(BinaryIndexedView::Module(&module)).disassemble()?
                    },
                };
                Ok(Some(result))
            },
            TaskCommand::Publish(
                PublishCommand {
                    gas_budget,
                    syntax,
                    print_bytecode,
                },
                extra_args,
            ) => {
                let syntax = syntax.unwrap_or_else(|| self.default_syntax());
                let (data, named_addr_opt, module, warnings_opt) =
                    self.compile_module(syntax, data, start_line, command_lines_stop)?;
                let printed = if print_bytecode {
                    let disassembler = disassembler_for_view(BinaryIndexedView::Module(&module));
                    Some(format!(
                        "\n== BEGIN Bytecode ==\n{}\n== END Bytecode ==",
                        disassembler.disassemble()?
                    ))
                } else {
                    None
                };
                let (mut output, module) = self.publish_module(
                    module,
                    named_addr_opt.map(|s| Identifier::new(s.as_str()).unwrap()),
                    gas_budget,
                    extra_args,
                )?;
                if print_bytecode {
                    output = merge_output(output, printed);
                }
                let data_path = data.path().to_str().unwrap();
                match syntax {
                    SyntaxChoice::Source => self.compiled_state().add_with_source_file(
                        named_addr_opt,
                        module,
                        (data_path.to_owned(), data),
                    ),
                    SyntaxChoice::IR => {
                        self.compiled_state()
                            .add_and_generate_interface_file(module);
                    },
                };
                Ok(merge_output(warnings_opt, output))
            },
            TaskCommand::Run(
                RunCommand {
                    signers,
                    args,
                    type_args,
                    gas_budget,
                    syntax,
                    name: None,
                    print_bytecode,
                },
                extra_args,
            ) => {
                let syntax = syntax.unwrap_or_else(|| self.default_syntax());
                let (script, warning_opt) =
                    self.compile_script(syntax, data, start_line, command_lines_stop)?;
                let printed = if print_bytecode {
                    let disassembler = disassembler_for_view(BinaryIndexedView::Script(&script));
                    Some(format!(
                        "\n== BEGIN Bytecode ==\n{}\n== END Bytecode ==",
                        disassembler.disassemble()?
                    ))
                } else {
                    None
                };
                let args = self.compiled_state().resolve_args(args)?;
                let type_args = self.compiled_state().resolve_type_args(type_args)?;
                let (mut output, return_values) =
                    self.execute_script(script, type_args, signers, args, gas_budget, extra_args)?;
                let rendered_return_value = display_return_values(return_values);
                output = merge_output(output, rendered_return_value);
                if print_bytecode {
                    output = merge_output(output, printed);
                }
                Ok(merge_output(warning_opt, output))
            },
            TaskCommand::Run(
                RunCommand {
                    signers,
                    args,
                    type_args,
                    gas_budget,
                    syntax,
                    name: Some((raw_addr, module_name, name)),
                    print_bytecode: _,
                },
                extra_args,
            ) => {
                assert!(
                    syntax.is_none(),
                    "syntax flag meaningless with function execution"
                );
                let addr = self.compiled_state().resolve_address(&raw_addr);
                let module_id = ModuleId::new(addr, module_name);
                let type_args = self.compiled_state().resolve_type_args(type_args)?;
                let args = self.compiled_state().resolve_args(args)?;
                let (output, return_values) = self.call_function(
                    &module_id,
                    name.as_ident_str(),
                    type_args,
                    signers,
                    args,
                    gas_budget,
                    extra_args,
                )?;
                let rendered_return_value = display_return_values(return_values);
                Ok(merge_output(output, rendered_return_value))
            },
            TaskCommand::View(ViewCommand { address, resource }) => {
                let state: &CompiledState = self.compiled_state();
                let StructTag {
                    address: module_addr,
                    module,
                    name,
                    type_params: type_arguments,
                } = resource
                    .into_struct_tag(&|s| Some(state.resolve_named_address(s)))
                    .unwrap();
                let module_id = ModuleId::new(module_addr, module);
                let address = self.compiled_state().resolve_address(&address);
                Ok(Some(self.view_data(
                    address,
                    &module_id,
                    name.as_ident_str(),
                    type_arguments,
                )?))
            },
            TaskCommand::Subcommand(c) => self.handle_subcommand(TaskInput {
                command: c,
                name,
                number,
                start_line,
                command_lines_stop,
                stop_line,
                data,
            }),
        }
    }
}

fn disassembler_for_view(view: BinaryIndexedView) -> Disassembler {
    let source_mapping =
        SourceMapping::new_from_view(view, Spanned::unsafe_no_loc(()).loc).expect("source mapping");
    Disassembler::new(source_mapping, DisassemblerOptions::new())
}

fn display_return_values(return_values: SerializedReturnValues) -> Option<String> {
    let SerializedReturnValues {
        mutable_reference_outputs,
        return_values,
    } = return_values;
    let mut output = vec![];
    if !mutable_reference_outputs.is_empty() {
        let values = mutable_reference_outputs
            .iter()
            .map(|(idx, bytes, layout)| {
                let value =
                    move_vm_types::values::Value::simple_deserialize(bytes, layout).unwrap();
                (idx, value)
            })
            .collect::<Vec<_>>();
        let printed = values
            .iter()
            .map(|(idx, v)| {
                let mut buf = String::new();
                move_vm_types::values::debug::print_value(&mut buf, v).unwrap();
                format!("local#{}: {}", idx, buf)
            })
            .collect::<Vec<_>>()
            .join(", ");
        output.push(format!("mutable inputs after call: {}", printed))
    };
    if !return_values.is_empty() {
        let values = return_values
            .iter()
            .map(|(bytes, layout)| {
                move_vm_types::values::Value::simple_deserialize(bytes, layout).unwrap()
            })
            .collect::<Vec<_>>();
        let printed = values
            .iter()
            .map(|v| {
                let mut buf = String::new();
                move_vm_types::values::debug::print_value(&mut buf, v).unwrap();
                buf
            })
            .collect::<Vec<_>>()
            .join(", ");
        output.push(format!("return values: {}", printed))
    };
    if output.is_empty() {
        None
    } else {
        Some(output.join("\n"))
    }
}

impl<'a> CompiledState<'a> {
    pub fn new(
        named_address_mapping: BTreeMap<String, NumericalAddress>,
        pre_compiled_deps: Option<&'a FullyCompiledProgram>,
        default_named_address_mapping: Option<NumericalAddress>,
    ) -> Self {
        let pre_compiled_ids = match pre_compiled_deps {
            None => BTreeSet::new(),
            Some(pre_compiled) => pre_compiled
                .cfgir
                .modules
                .key_cloned_iter()
                .map(|(ident, _)| {
                    (
                        ident.value.address.into_addr_bytes().into_inner(),
                        ident.value.module.to_string(),
                    )
                })
                .collect(),
        };
        let mut state = Self {
            pre_compiled_deps,
            pre_compiled_ids,
            modules: BTreeMap::new(),
            compiled_module_named_address_mapping: BTreeMap::new(),
            named_address_mapping,
            default_named_address_mapping,
        };
        if let Some(pcd) = pre_compiled_deps {
            for unit in &pcd.compiled {
                if let AnnotatedCompiledUnit::Module(annot_module) = unit {
                    let (named_addr_opt, _id) = annot_module.module_id();
                    state.add_precompiled(
                        named_addr_opt.map(|n| n.value),
                        annot_module.named_module.module.clone(),
                    );
                }
            }
        }
        state
    }

    pub fn dep_modules(&self) -> impl Iterator<Item = &CompiledModule> {
        self.modules.values().map(|pmod| &pmod.module)
    }

    pub fn source_files(&self) -> impl Iterator<Item = &String> {
        self.modules
            .iter()
            .filter_map(|(_, pmod)| Some(&pmod.source_file.as_ref()?.0))
    }

    pub fn add_with_source_file(
        &mut self,
        named_addr_opt: Option<Symbol>,
        module: CompiledModule,
        source_file: (String, NamedTempFile),
    ) {
        let id = module.self_id();
        self.check_not_precompiled(&id);
        if let Some(named_addr) = named_addr_opt {
            self.compiled_module_named_address_mapping
                .insert(id.clone(), named_addr);
        }

        let processed = ProcessedModule {
            module,
            source_file: Some(source_file),
        };
        self.modules.insert(id, processed);
    }

    pub fn add_and_generate_interface_file(&mut self, module: CompiledModule) {
        let id = module.self_id();
        self.check_not_precompiled(&id);
        let interface_file = NamedTempFile::new().unwrap();
        let path = interface_file.path().to_str().unwrap().to_owned();
        let (_id, interface_text) = move_compiler::interface_generator::write_module_to_string(
            &self.compiled_module_named_address_mapping,
            &module,
        )
        .unwrap();
        interface_file
            .reopen()
            .unwrap()
            .write_all(interface_text.as_bytes())
            .unwrap();
        let source_file = Some((path, interface_file));
        let processed = ProcessedModule {
            module,
            source_file,
        };
        self.modules.insert(id, processed);
    }

    fn add_precompiled(&mut self, named_addr_opt: Option<Symbol>, module: CompiledModule) {
        let id = module.self_id();
        if let Some(named_addr) = named_addr_opt {
            self.compiled_module_named_address_mapping
                .insert(id.clone(), named_addr);
        }
        let processed = ProcessedModule {
            module,
            source_file: None,
        };
        self.modules.insert(id, processed);
    }

    pub fn is_precompiled_dep(&self, id: &ModuleId) -> bool {
        let addr = *id.address();
        let name = id.name().to_string();
        self.pre_compiled_ids.contains(&(addr, name))
    }

    fn check_not_precompiled(&self, id: &ModuleId) {
        assert!(
            !self.is_precompiled_dep(id),
            "Error publishing module: '{}'. \
             Re-publishing modules in pre-compiled lib is not yet supported",
            id
        )
    }
}

fn compile_source_unit_v2(
    pre_compiled_deps: Option<&FullyCompiledProgram>,
    named_address_mapping: BTreeMap<String, NumericalAddress>,
    deps: &[String],
    path: String,
    known_attributes: &BTreeSet<String>,
) -> Result<(
    (Option<CompiledModule>, Option<CompiledScript>),
    Option<String>,
)> {
    let deps = if let Some(p) = pre_compiled_deps {
        // The v2 compiler does not (and perhaps never) supports precompiled programs, so
        // compile from the sources again, computing the directories where they are found.
        let mut dirs: BTreeSet<_> = p
            .files
            .iter()
            .filter_map(|(_, (file_name, _))| {
                Path::new(file_name.as_str())
                    .parent()
                    .map(|p| p.to_string_lossy().to_string())
            })
            .collect();
        dirs.extend(deps.iter().cloned());
        dirs.into_iter().collect()
    } else {
        deps.to_vec()
    };
    let options = move_compiler_v2::Options {
        sources: vec![path],
        dependencies: deps,
        named_address_mapping: named_address_mapping
            .into_iter()
            .map(|(alias, addr)| format!("{}={}", alias, addr))
            .collect(),
        known_attributes: known_attributes.clone(),
        ..move_compiler_v2::Options::default()
    };
    let mut error_writer = termcolor::Buffer::no_color();
    let result = move_compiler_v2::run_move_compiler(&mut error_writer, options);
    let error_str = String::from_utf8_lossy(&error_writer.into_inner()).to_string();
    let (_, mut units) =
        result.map_err(|_| anyhow::anyhow!("compilation errors:\n {}", error_str))?;
    let unit = if units.len() != 1 {
        anyhow::bail!("expected either one script or one module")
    } else {
        match units.pop().unwrap() {
            AnnotatedCompiledUnit::Module(m) => (Some(m.named_module.module), None),
            AnnotatedCompiledUnit::Script(s) => (None, Some(s.named_script.script)),
        }
    };
    if error_str.is_empty() {
        Ok((unit, None))
    } else {
        Ok((unit, Some(error_str)))
    }
}

fn compile_source_unit(
    pre_compiled_deps: Option<&FullyCompiledProgram>,
    named_address_mapping: BTreeMap<String, NumericalAddress>,
    deps: &[String],
    path: String,
    known_attributes: &BTreeSet<String>,
) -> Result<(AnnotatedCompiledUnit, Option<String>)> {
    fn rendered_diags(files: &FilesSourceText, diags: Diagnostics) -> Option<String> {
        if diags.is_empty() {
            return None;
        }

        let error_buffer = if read_bool_env_var(move_command_line_common::testing::PRETTY) {
            move_compiler::diagnostics::report_diagnostics_to_color_buffer(files, diags)
        } else {
            move_compiler::diagnostics::report_diagnostics_to_buffer(files, diags)
        };
        Some(String::from_utf8(error_buffer).unwrap())
    }

    use move_compiler::PASS_COMPILATION;
    let (mut files, comments_and_compiler_res) = move_compiler::Compiler::from_files(
        vec![path],
        deps.to_vec(),
        named_address_mapping,
        move_compiler::Flags::empty()
            .set_sources_shadow_deps(true)
            .set_skip_attribute_checks(false), // In case of bugs in transactional test code.
        known_attributes,
    )
    .set_pre_compiled_lib_opt(pre_compiled_deps)
    .run::<PASS_COMPILATION>()?;
    let units_or_diags = comments_and_compiler_res
        .map(|(_comments, move_compiler)| move_compiler.into_compiled_units());

    match units_or_diags {
        Err(diags) => {
            if let Some(pcd) = pre_compiled_deps {
                for (file_name, text) in &pcd.files {
                    // TODO This is bad. Rethink this when errors are redone
                    if !files.contains_key(file_name) {
                        files.insert(*file_name, text.clone());
                    }
                }
            }

            Err(anyhow!(rendered_diags(&files, diags).unwrap()))
        },
        Ok((mut units, warnings)) => {
            let warnings = rendered_diags(&files, warnings);
            let len = units.len();
            if len != 1 {
                panic!("Invalid input. Expected 1 compiled unit but got {}", len)
            }
            let unit = units.pop().unwrap();
            Ok((unit, warnings))
        },
    }
}

fn compile_ir_module<'a>(
    deps: impl Iterator<Item = &'a CompiledModule>,
    file_name: &str,
) -> Result<CompiledModule> {
    use move_ir_compiler::Compiler as IRCompiler;
    let code = std::fs::read_to_string(file_name).unwrap();
    IRCompiler::new(deps.collect()).into_compiled_module(&code)
}

fn compile_ir_script<'a>(
    deps: impl Iterator<Item = &'a CompiledModule>,
    file_name: &str,
) -> Result<CompiledScript> {
    use move_ir_compiler::Compiler as IRCompiler;
    let code = std::fs::read_to_string(file_name).unwrap();
    let (script, _) = IRCompiler::new(deps.collect()).into_compiled_script_and_source_map(&code)?;
    Ok(script)
}

pub fn run_test_impl<'a, Adapter>(
    config: TestRunConfig,
    path: &Path,
    fully_compiled_program_opt: Option<&'a FullyCompiledProgram>,
) -> Result<(), Box<dyn std::error::Error>>
where
    Adapter: MoveTestAdapter<'a>,
    Adapter::ExtraInitArgs: Debug,
    Adapter::ExtraPublishArgs: Debug,
    Adapter::ExtraValueArgs: Debug,
    Adapter::ExtraRunArgs: Debug,
    Adapter::Subcommand: Debug,
{
    let extension = path.extension().unwrap().to_str().unwrap();
    let default_syntax = if extension == MOVE_IR_EXTENSION {
        SyntaxChoice::IR
    } else {
        assert!(extension == MOVE_EXTENSION);
        SyntaxChoice::Source
    };

    // Construct a sequence of compiler runs based on the given config.
    let (runs, comparison_mode) = if config == TestRunConfig::ComparisonV1V2 {
        (
            vec![TestRunConfig::CompilerV1, TestRunConfig::CompilerV2],
            true,
        )
    } else {
        (vec![config], false) // either V1 or V2
    };
    let mut last_output = String::new();
    let mut bytecode_print_output = BTreeMap::<TestRunConfig, String>::new();
    for run_config in runs {
        let mut output = String::new();
        let mut tasks = taskify::<
            TaskCommand<
                Adapter::ExtraInitArgs,
                Adapter::ExtraPublishArgs,
                Adapter::ExtraValueArgs,
                Adapter::ExtraRunArgs,
                Adapter::Subcommand,
            >,
        >(path)?
        .into_iter()
        .collect::<VecDeque<_>>();
        assert!(!tasks.is_empty());
        let num_tasks = tasks.len();
        writeln!(
            &mut output,
            "processed {} task{}",
            num_tasks,
            if num_tasks > 1 { "s" } else { "" }
        )
        .unwrap();
        let first_task = tasks.pop_front().unwrap();
        let init_opt = match &first_task.command {
            TaskCommand::Init(_, _) => Some(first_task.map(|known| match known {
                TaskCommand::Init(command, extra_args) => (command, extra_args),
                _ => unreachable!(),
            })),
            _ => {
                tasks.push_front(first_task);
                None
            },
        };
        let (mut adapter, result_opt) = Adapter::init(
            default_syntax,
            comparison_mode,
            run_config,
            fully_compiled_program_opt,
            init_opt,
        );
        if let Some(result) = result_opt {
            writeln!(output, "\ninit:\n{}", result)?;
        }
        for task in tasks {
            handle_known_task(&mut output, &mut adapter, task);
        }
        // Extract any bytecode outputs, they should not be part of the diff.
        static BYTECODE_REX: Lazy<Regex> = Lazy::new(|| {
            Regex::new("(?m)== BEGIN Bytecode ==(.|\n|\r)*== END Bytecode ==").unwrap()
        });
        while let Some(m) = BYTECODE_REX.find(&output) {
            bytecode_print_output
                .entry(run_config)
                .or_default()
                .push_str(&output.drain(m.range()).collect::<String>());
        }

        // If there is a previous output, compare to that one
        if !last_output.is_empty() && last_output != output {
            let diff = format_diff_no_color(&last_output, &output);
            let output = format!("comparison between v1 and v2 failed:\n{}", diff);
            handle_expected_output(path, output)?;
            return Ok(());
        }
        last_output = output
    }
    if config == TestRunConfig::ComparisonV1V2 {
        // Indicate in output that we passed comparison test
        last_output += "\n==> Compiler v2 delivered same results!\n"
    }
    // Dump printed bytecode at last
    for (config, out) in bytecode_print_output {
        last_output += &format!(
            "\n>>> {} {{\n{}\n}}\n",
            match config {
                TestRunConfig::CompilerV1 => "V1 Compiler",
                TestRunConfig::CompilerV2 => "V2 Compiler",
                _ => panic!("unexpected test config"),
            },
            out
        );
    }
    handle_expected_output(path, last_output)?;
    Ok(())
}

fn handle_known_task<'a, Adapter: MoveTestAdapter<'a>>(
    output: &mut String,
    adapter: &mut Adapter,
    task: TaskInput<
        TaskCommand<
            Adapter::ExtraInitArgs,
            Adapter::ExtraPublishArgs,
            Adapter::ExtraValueArgs,
            Adapter::ExtraRunArgs,
            Adapter::Subcommand,
        >,
    >,
) {
    let task_number = task.number;
    let task_name = task.name.to_owned();
    let start_line = task.start_line;
    let stop_line = task.stop_line;
    let data_path = match &task.data {
        Some(f) => f.path().to_str().unwrap().to_string(),
        None => "".to_string(),
    };
    let result = adapter.handle_command(task);
    let mut result_string = match result {
        Ok(None) => return,
        Ok(Some(s)) => s,
        Err(e) => format!("Error: {}", e),
    };
    if !data_path.is_empty() {
        result_string = result_string.replace(&data_path, "TEMPFILE");
    }
    assert!(!result_string.is_empty());
    writeln!(
        output,
        "\ntask {} '{}'. lines {}-{}:\n{}",
        task_number, task_name, start_line, stop_line, result_string
    )
    .unwrap();
}

fn handle_expected_output(test_path: &Path, output: impl AsRef<str>) -> Result<()> {
    let output = output.as_ref();
    assert!(!output.is_empty());
    let exp_path = test_path.with_extension(EXP_EXT);

    if read_env_update_baseline() {
        std::fs::write(exp_path, output).unwrap();
        return Ok(());
    }

    if !exp_path.exists() {
        std::fs::write(&exp_path, "").unwrap();
    }
    let expected_output = std::fs::read_to_string(&exp_path)
        .unwrap()
        .replace("\r\n", "\n")
        .replace('\r', "\n");
    if output != expected_output {
        let msg = format!(
            "Expected errors differ from actual errors:\n{}",
            format_diff(expected_output, output),
        );
        anyhow::bail!(add_update_baseline_fix(msg))
    } else {
        Ok(())
    }
}
