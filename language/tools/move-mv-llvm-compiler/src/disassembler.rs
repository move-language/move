// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::{anyhow, bail, format_err, Error, Result};
use clap::Parser;
use colored::*;
use move_binary_format::{
    binary_views::BinaryIndexedView,
    control_flow_graph::{ControlFlowGraph, VMControlFlowGraph},
    file_format::{
        Ability, AbilitySet, Bytecode, CodeUnit, FieldHandleIndex, FunctionDefinition,
        FunctionDefinitionIndex, FunctionHandle, Signature, SignatureIndex, SignatureToken,
        StructDefinition, StructDefinitionIndex, StructFieldInformation, StructTypeParameter,
        TableIndex, TypeSignature, Visibility,
    },
};
use move_bytecode_source_map::{
    mapping::SourceMapping,
    source_map::{FunctionSourceMap, SourceName},
};
use move_compiler::compiled_unit::{CompiledUnit, NamedCompiledModule, NamedCompiledScript};
use move_core_types::identifier::IdentStr;
use move_coverage::coverage_map::{ExecCoverageMap, FunctionCoverage};
use move_ir_types::location::Loc;

use inkwell::targets::{TargetTriple, TargetMachine};
use inkwell::targets::{CodeModel, RelocMode};
use std::{fs::File};

/// Holds the various options that we support while disassembling code.
#[derive(Debug, Default, Parser)]
pub struct DisassemblerOptions {
    /// Only print non-private functions
    #[clap(long = "only-public")]
    pub only_externally_visible: bool,

    /// Print the bytecode for the instructions within the function.
    #[clap(long = "print-code")]
    pub print_code: bool,

    /// Print the basic blocks of the bytecode.
    #[clap(long = "print-basic-blocks")]
    pub print_basic_blocks: bool,

    /// Print the locals inside each function body.
    #[clap(long = "print-locals")]
    pub print_locals: bool,
}

impl DisassemblerOptions {
    pub fn new() -> Self {
        Self {
            only_externally_visible: false,
            print_code: true,
            print_basic_blocks: true,
            print_locals: true,
        }
    }
}

pub struct Disassembler<'a> {
    source_mapper: SourceMapping<'a>,
    // The various options that we can set for disassembly.
    options: DisassemblerOptions,
    // Optional coverage map for use in displaying code coverage
    coverage_map: Option<ExecCoverageMap>,
}

impl<'a> Disassembler<'a> {
    pub fn new(source_mapper: SourceMapping<'a>, options: DisassemblerOptions) -> Self {
        Self {
            source_mapper,
            options,
            coverage_map: None,
        }
    }

    pub fn from_view(view: BinaryIndexedView<'a>, default_loc: Loc) -> Result<Self> {
        let mut options = DisassemblerOptions::new();
        options.print_code = true;
        Ok(Self {
            source_mapper: SourceMapping::new_from_view(view, default_loc)?,
            options,
            coverage_map: None,
        })
    }

    pub fn from_unit(unit: &'a CompiledUnit) -> Self {
        let options = DisassemblerOptions::new();
        let source_map = unit.source_map().clone();
        let index_view = match unit {
            CompiledUnit::Module(NamedCompiledModule { module, .. }) => {
                BinaryIndexedView::Module(module)
            }
            CompiledUnit::Script(NamedCompiledScript { script, .. }) => {
                BinaryIndexedView::Script(script)
            }
        };

        let source_mapping = SourceMapping::new(source_map, index_view);
        Disassembler::new(source_mapping, options)
    }

    pub fn add_coverage_map(&mut self, coverage_map: ExecCoverageMap) {
        self.coverage_map = Some(coverage_map);
    }

    //***************************************************************************
    // Helpers
    //***************************************************************************

    fn get_function_def(
        &self,
        function_definition_index: FunctionDefinitionIndex,
    ) -> Result<&FunctionDefinition> {
        if function_definition_index.0 as usize
            >= self
                .source_mapper
                .bytecode
                .function_defs()
                .map_or(0, |f| f.len())
        {
            bail!("Invalid function definition index supplied when marking function")
        }
        match self
            .source_mapper
            .bytecode
            .function_def_at(function_definition_index)
        {
            Ok(definition) => Ok(definition),
            Err(err) => Err(Error::new(err)),
        }
    }

    fn get_struct_def(
        &self,
        struct_definition_index: StructDefinitionIndex,
    ) -> Result<&StructDefinition> {
        if struct_definition_index.0 as usize
            >= self
                .source_mapper
                .bytecode
                .struct_defs()
                .map_or(0, |d| d.len())
        {
            bail!("Invalid struct definition index supplied when marking struct")
        }
        match self
            .source_mapper
            .bytecode
            .struct_def_at(struct_definition_index)
        {
            Ok(definition) => Ok(definition),
            Err(err) => Err(Error::new(err)),
        }
    }

    //***************************************************************************
    // Code Coverage Helpers
    //***************************************************************************

    fn get_function_coverage(&self, function_name: &IdentStr) -> Option<&FunctionCoverage> {
        self.source_mapper
            .source_map
            .module_name_opt
            .as_ref()
            .and_then(|module| {
                self.coverage_map.as_ref().and_then(|coverage_map| {
                    coverage_map
                        .module_maps
                        .get(module)
                        .and_then(|module_map| module_map.get_function_coverage(function_name))
                })
            })
    }

    fn is_function_called(&self, function_name: &IdentStr) -> bool {
        self.get_function_coverage(function_name).is_some()
    }

    fn format_function_coverage(&self, name: &IdentStr, function_body: String) -> String {
        if self.coverage_map.is_none() {
            return function_body;
        }
        if self.is_function_called(name) {
            function_body.green()
        } else {
            function_body.red()
        }
        .to_string()
    }

    fn format_with_instruction_coverage(
        &self,
        pc: usize,
        function_coverage_map: Option<&FunctionCoverage>,
        instruction: String,
    ) -> String {
        if self.coverage_map.is_none() {
            return format!("\t{}: {}", pc, instruction);
        }
        let coverage = function_coverage_map.and_then(|map| map.get(&(pc as u64)));
        match coverage {
            Some(coverage) => format!("[{}]\t{}: {}", coverage, pc, instruction).green(),
            None => format!("\t{}: {}", pc, instruction).red(),
        }
        .to_string()
    }

    //***************************************************************************
    // Formatting Helpers
    //***************************************************************************

    fn name_for_field(&self, field_idx: FieldHandleIndex) -> Result<String> {
        let field_handle = self.source_mapper.bytecode.field_handle_at(field_idx)?;
        let struct_def = self
            .source_mapper
            .bytecode
            .struct_def_at(field_handle.owner)?;
        let field_def = match &struct_def.field_information {
            StructFieldInformation::Native => {
                return Err(format_err!("Attempt to access field on a native struct"));
            }
            StructFieldInformation::Declared(fields) => fields
                .get(field_handle.field as usize)
                .ok_or_else(|| format_err!("Bad field index"))?,
        };
        let field_name = self
            .source_mapper
            .bytecode
            .identifier_at(field_def.name)
            .to_string();
        let struct_handle = self
            .source_mapper
            .bytecode
            .struct_handle_at(struct_def.struct_handle);
        let struct_name = self
            .source_mapper
            .bytecode
            .identifier_at(struct_handle.name)
            .to_string();
        Ok(format!("{}.{}", struct_name, field_name))
    }

    fn type_for_field(&self, field_idx: FieldHandleIndex) -> Result<String> {
        let field_handle = self.source_mapper.bytecode.field_handle_at(field_idx)?;
        let struct_def = self
            .source_mapper
            .bytecode
            .struct_def_at(field_handle.owner)?;
        let field_def = match &struct_def.field_information {
            StructFieldInformation::Native => {
                return Err(format_err!("Attempt to access field on a native struct"));
            }
            StructFieldInformation::Declared(fields) => fields
                .get(field_handle.field as usize)
                .ok_or_else(|| format_err!("Bad field index"))?,
        };
        let struct_source_info = self
            .source_mapper
            .source_map
            .get_struct_source_map(field_handle.owner)?;
        let field_type_sig = field_def.signature.0.clone();
        let ty = self.disassemble_sig_tok(field_type_sig, &struct_source_info.type_parameters)?;
        Ok(ty)
    }

    fn struct_type_info(
        &self,
        struct_idx: StructDefinitionIndex,
        signature: &Signature,
        type_param_context: &[SourceName],
    ) -> Result<(String, String)> {
        let struct_definition = self.get_struct_def(struct_idx)?;
        let type_arguments = signature
            .0
            .iter()
            .map(|sig_tok| self.disassemble_sig_tok(sig_tok.clone(), type_param_context))
            .collect::<Result<Vec<String>>>()?;

        let struct_handle = self
            .source_mapper
            .bytecode
            .struct_handle_at(struct_definition.struct_handle);
        let name = self
            .source_mapper
            .bytecode
            .identifier_at(struct_handle.name)
            .to_string();
        Ok((name, Self::format_type_params(&type_arguments)))
    }

    fn name_for_parameter_or_local(
        &self,
        local_idx: usize,
        function_source_map: &FunctionSourceMap,
    ) -> Result<String> {
        let name = function_source_map
                .get_parameter_or_local_name(local_idx as u64)
                .ok_or_else(|| {
                    format_err!(
                        "Unable to get local name at index {} while disassembling location-based instruction", local_idx
                    )
                })?
                .0;
        Ok(name)
    }

    fn type_for_parameter_or_local(
        &self,
        idx: usize,
        parameters: &Signature,
        locals: &Signature,
        function_source_map: &FunctionSourceMap,
    ) -> Result<String> {
        let sig_tok = if idx < parameters.len() {
            &parameters.0[idx]
        } else if idx < parameters.len() + locals.len() {
            &locals.0[idx - parameters.len()]
        } else {
            bail!("Unable to get type for parameter or local at index {}", idx)
        };
        self.disassemble_sig_tok(sig_tok.clone(), &function_source_map.type_parameters)
    }

    fn type_for_local(
        &self,
        local_idx: usize,
        locals: &Signature,
        function_source_map: &FunctionSourceMap,
    ) -> Result<String> {
        let sig_tok = locals
            .0
            .get(local_idx as usize)
            .ok_or_else(|| format_err!("Unable to get type for local at index {}", local_idx))?;
        self.disassemble_sig_tok(sig_tok.clone(), &function_source_map.type_parameters)
    }

    fn format_ability(a: Ability) -> String {
        match a {
            Ability::Copy => "copy",
            Ability::Drop => "drop",
            Ability::Store => "store",
            Ability::Key => "key",
        }
        .to_string()
    }

    fn format_type_params(ty_params: &[String]) -> String {
        if ty_params.is_empty() {
            "".to_string()
        } else {
            format!("<{}>", ty_params.join(", "))
        }
    }

    fn format_ret_type(ty_rets: &[String]) -> String {
        if ty_rets.is_empty() {
            "".to_string()
        } else {
            format!(": {}", ty_rets.join(" * "))
        }
    }

    fn format_function_body(locals: Vec<String>, bytecode: Vec<String>) -> String {
        if locals.is_empty() && bytecode.is_empty() {
            "".to_string()
        } else {
            let body_iter: Vec<String> = locals
                .into_iter()
                .enumerate()
                .map(|(local_idx, local)| format!("L{}:\t{}", local_idx, local))
                .chain(bytecode.into_iter())
                .collect();
            format!(" {{\n{}\n}}", body_iter.join("\n"))
        }
    }

    //***************************************************************************
    // Disassemblers
    //***************************************************************************

    // These need to be in the context of a function or a struct definition since type parameters
    // can refer to function/struct type parameters.
    fn disassemble_sig_tok(
        &self,
        sig_tok: SignatureToken,
        type_param_context: &[SourceName],
    ) -> Result<String> {
        Ok(match sig_tok {
            SignatureToken::Bool => "bool".to_string(),
            SignatureToken::U8 => "u8".to_string(),
            SignatureToken::U64 => "u64".to_string(),
            SignatureToken::U128 => "u128".to_string(),
            SignatureToken::Address => "address".to_string(),
            SignatureToken::Signer => "signer".to_string(),
            SignatureToken::Struct(struct_handle_idx) => self
                .source_mapper
                .bytecode
                .identifier_at(
                    self.source_mapper
                        .bytecode
                        .struct_handle_at(struct_handle_idx)
                        .name,
                )
                .to_string(),
            SignatureToken::StructInstantiation(struct_handle_idx, instantiation) => {
                let instantiation = instantiation
                    .into_iter()
                    .map(|tok| self.disassemble_sig_tok(tok, type_param_context))
                    .collect::<Result<Vec<_>>>()?;
                let formatted_instantiation = Self::format_type_params(&instantiation);
                let name = self
                    .source_mapper
                    .bytecode
                    .identifier_at(
                        self.source_mapper
                            .bytecode
                            .struct_handle_at(struct_handle_idx)
                            .name,
                    )
                    .to_string();
                format!("{}{}", name, formatted_instantiation)
            }
            SignatureToken::Vector(sig_tok) => format!(
                "vector<{}>",
                self.disassemble_sig_tok(*sig_tok, type_param_context)?
            ),
            SignatureToken::Reference(sig_tok) => format!(
                "&{}",
                self.disassemble_sig_tok(*sig_tok, type_param_context)?
            ),
            SignatureToken::MutableReference(sig_tok) => format!(
                "&mut {}",
                self.disassemble_sig_tok(*sig_tok, type_param_context)?
            ),
            SignatureToken::TypeParameter(ty_param_index) => type_param_context
                .get(ty_param_index as usize)
                .ok_or_else(|| {
                    format_err!(
                        "Type parameter index {} out of bounds while disassembling type signature",
                        ty_param_index
                    )
                })?
                .0
                .to_string(),
        })
    }

    fn disassemble_instruction(
        &self,
        parameters: &Signature,
        instruction: &Bytecode,
        locals_sigs: &Signature,
        function_source_map: &FunctionSourceMap,
        default_location: &Loc,
    ) -> Result<String> {
        match instruction {
            Bytecode::Ret => Ok("Ret".to_string()),
            Bytecode::LdU64(a) => Ok(format!("LdU64({})", a)),
            x => Err(anyhow!(format!("Unhandled move instruction: {:#?}", x)))
        }
    }

    fn disassemble_bytecode(
        &self,
        function_source_map: &FunctionSourceMap,
        function_name: &IdentStr,
        parameters: SignatureIndex,
        code: &CodeUnit,
    ) -> Result<Vec<String>> {
        if !self.options.print_code {
            return Ok(vec!["".to_string()]);
        }

        let parameters = self.source_mapper.bytecode.signature_at(parameters);
        let locals_sigs = self.source_mapper.bytecode.signature_at(code.locals);

        let function_code_coverage_map = self.get_function_coverage(function_name);

        let decl_location = &function_source_map.definition_location;
        let instrs: Vec<String> = code
            .code
            .iter()
            .map(|instruction| {
                self.disassemble_instruction(
                    parameters,
                    instruction,
                    locals_sigs,
                    function_source_map,
                    decl_location,
                )
            })
            .collect::<Result<Vec<String>>>()?;

        let mut instrs: Vec<String> = instrs
            .into_iter()
            .enumerate()
            .map(|(instr_index, dis_instr)| {
                self.format_with_instruction_coverage(
                    instr_index,
                    function_code_coverage_map,
                    dis_instr,
                )
            })
            .collect();

        if self.options.print_basic_blocks {
            let cfg = VMControlFlowGraph::new(&code.code);
            for (block_number, block_id) in cfg.blocks().iter().enumerate() {
                instrs.insert(
                    *block_id as usize + block_number,
                    format!("B{}:", block_number),
                );
            }
        }

        Ok(instrs)
    }

    fn disassemble_struct_type_formals(
        source_map_ty_params: &[SourceName],
        type_parameters: &[StructTypeParameter],
    ) -> String {
        let ty_params: Vec<String> = source_map_ty_params
            .iter()
            .zip(type_parameters)
            .map(|((name, _), ty_param)| {
                let abilities_str = if ty_param.constraints == AbilitySet::EMPTY {
                    "".to_string()
                } else {
                    let ability_vec: Vec<_> = ty_param
                        .constraints
                        .into_iter()
                        .map(Self::format_ability)
                        .collect();
                    format!(": {}", ability_vec.join(" + "))
                };
                format!(
                    "{}{}{}",
                    if ty_param.is_phantom { "phantom " } else { "" },
                    name.as_str(),
                    abilities_str
                )
            })
            .collect();
        Self::format_type_params(&ty_params)
    }

    fn disassemble_fun_type_formals(
        source_map_ty_params: &[SourceName],
        ablities: &[AbilitySet],
    ) -> String {
        let ty_params: Vec<String> = source_map_ty_params
            .iter()
            .zip(ablities)
            .map(|((name, _), abs)| {
                let abilities_str = if *abs == AbilitySet::EMPTY {
                    "".to_string()
                } else {
                    let ability_vec: Vec<_> = abs.into_iter().map(Self::format_ability).collect();
                    format!(": {}", ability_vec.join(" + "))
                };
                format!("{}{}", name.as_str(), abilities_str)
            })
            .collect();
        Self::format_type_params(&ty_params)
    }

    fn disassemble_locals(
        &self,
        function_source_map: &FunctionSourceMap,
        locals_idx: SignatureIndex,
        parameter_len: usize,
    ) -> Result<Vec<String>> {
        if !self.options.print_locals {
            return Ok(vec![]);
        }

        let signature = self.source_mapper.bytecode.signature_at(locals_idx);
        let locals_names_tys = function_source_map
            .locals
            .iter()
            .skip(parameter_len)
            .enumerate()
            .map(|(local_idx, (name, _))| {
                let ty =
                    self.type_for_local(parameter_len + local_idx, signature, function_source_map)?;
                Ok(format!("{}: {}", name, ty))
            })
            .collect::<Result<Vec<String>>>()?;
        Ok(locals_names_tys)
    }

    /// Translates a compiled "function definition" into a disassembled bytecode string.
    ///
    /// Because a "function definition" can refer to either a function defined in a module or to a
    /// script's "main" function (which is not represented by a function definition in the binary
    /// format), this method takes a function definition and handle as optional arguments. These are
    /// `None` when disassembling a script's "main" function.
    pub fn disassemble_function_def(
        &self,
        function_source_map: &FunctionSourceMap,
        function: Option<(&FunctionDefinition, &FunctionHandle)>,
        name: &IdentStr,
        type_parameters: &[AbilitySet],
        parameters: SignatureIndex,
        code: Option<&CodeUnit>,
    ) -> Result<String> {
        debug_assert_eq!(
            function_source_map.parameters.len(),
            self.source_mapper.bytecode.signature_at(parameters).len(),
            "Arity mismatch between function source map and bytecode for function {}",
            name
        );

        let entry_modifier = if function.map(|(f, _)| f.is_entry).unwrap_or(false) {
            "entry "
        } else {
            ""
        };
        let visibility_modifier = match function {
            Some(function) => match function.0.visibility {
                Visibility::Private => {
                    if self.options.only_externally_visible {
                        return Ok("".to_string());
                    } else {
                        ""
                    }
                }
                Visibility::Friend => "public(friend) ",
                Visibility::Public => "public ",
            },
            None => "",
        };

        let native_modifier = match function {
            Some(function) if function.0.is_native() => "native ",
            _ => "",
        };

        let ty_params = Self::disassemble_fun_type_formals(
            &function_source_map.type_parameters,
            type_parameters,
        );
        let params = &self
            .source_mapper
            .bytecode
            .signature_at(parameters)
            .0
            .iter()
            .zip(function_source_map.parameters.iter())
            .map(|(tok, (name, _))| {
                Ok(format!(
                    "{}: {}",
                    name,
                    self.disassemble_sig_tok(tok.clone(), &function_source_map.type_parameters)?
                ))
            })
            .collect::<Result<Vec<_>>>()?;

        let ret_type = match function {
            Some(function) => self
                .source_mapper
                .bytecode
                .signature_at(function.1.return_)
                .0
                .iter()
                .cloned()
                .map(|sig_token| {
                    let sig_tok_str =
                        self.disassemble_sig_tok(sig_token, &function_source_map.type_parameters)?;
                    Ok(sig_tok_str)
                })
                .collect::<Result<Vec<String>>>()?,
            None => vec![],
        };

        let body = match code {
            Some(code) => {
                let locals =
                    self.disassemble_locals(function_source_map, code.locals, params.len())?;
                let bytecode =
                    self.disassemble_bytecode(function_source_map, name, parameters, code)?;
                Self::format_function_body(locals, bytecode)
            }
            None => "".to_string(),
        };
        Ok(self.format_function_coverage(
            name,
            format!(
                "{entry_modifier}{native_modifier}{visibility_modifier}{name}{ty_params}({params}){ret_type}{body}",
                params = &params.join(", "),
                ret_type = Self::format_ret_type(&ret_type),
            ),
        ))
    }

    // The struct defs will filter out the structs that we print to only be the ones that are
    // defined in the module in question.
    pub fn disassemble_struct_def(&self, struct_def_idx: StructDefinitionIndex) -> Result<String> {
        let struct_definition = self.get_struct_def(struct_def_idx)?;
        let struct_handle = self
            .source_mapper
            .bytecode
            .struct_handle_at(struct_definition.struct_handle);
        let struct_source_map = self
            .source_mapper
            .source_map
            .get_struct_source_map(struct_def_idx)?;

        let field_info: Option<Vec<(&IdentStr, &TypeSignature)>> =
            match &struct_definition.field_information {
                StructFieldInformation::Native => None,
                StructFieldInformation::Declared(fields) => Some(
                    fields
                        .iter()
                        .map(|field_definition| {
                            let type_sig = &field_definition.signature;
                            let field_name = self
                                .source_mapper
                                .bytecode
                                .identifier_at(field_definition.name);
                            (field_name, type_sig)
                        })
                        .collect(),
                ),
            };

        let native = if field_info.is_none() { "native " } else { "" };

        let abilities = if struct_handle.abilities == AbilitySet::EMPTY {
            String::new()
        } else {
            let ability_vec: Vec<_> = struct_handle
                .abilities
                .into_iter()
                .map(Self::format_ability)
                .collect();
            format!(" has {}", ability_vec.join(", "))
        };

        let name = self
            .source_mapper
            .bytecode
            .identifier_at(struct_handle.name)
            .to_string();

        let ty_params = Self::disassemble_struct_type_formals(
            &struct_source_map.type_parameters,
            &struct_handle.type_parameters,
        );
        let mut fields = match field_info {
            None => vec![],
            Some(field_info) => field_info
                .iter()
                .map(|(name, ty)| {
                    let ty_str =
                        self.disassemble_sig_tok(ty.0.clone(), &struct_source_map.type_parameters)?;
                    Ok(format!("{}: {}", name, ty_str))
                })
                .collect::<Result<Vec<String>>>()?,
        };

        if let Some(first_elem) = fields.first_mut() {
            first_elem.insert_str(0, "{\n\t");
        }

        if let Some(last_elem) = fields.last_mut() {
            last_elem.push_str("\n}");
        }

        Ok(format!(
            "{native}struct {name}{ty_params}{abilities} {fields}",
            native = native,
            name = name,
            ty_params = ty_params,
            abilities = abilities,
            fields = &fields.join(",\n\t"),
        ))
    }


    fn llvm_target_triple(&self) -> TargetTriple {
        TargetTriple::create("bpfel-unknown-unknown")
    }

    fn llvm_target_name(&self) -> &'static str {
        "bpfel" // bpf little endian.
    }

    fn llvm_features(&self) -> &'static str {
        "" // no additional target specific features.
    }

    pub fn get_target_machine(&self) -> Option<TargetMachine> {
        use inkwell::{OptimizationLevel, targets::Target, targets::InitializationConfig};
        Target::initialize_bpf(&InitializationConfig::default());

        let opt = OptimizationLevel::None; // TODO: Add optimization based on command line flag.
        let reloc = RelocMode::Default;
        let model = CodeModel::Default;
        let target = Target::from_name(self.llvm_target_name()).unwrap();

        return target.create_target_machine(
            &self.llvm_target_triple(),
            "v2",
            self.llvm_features(),
            opt,
            reloc,
            model
        );
    }

    pub fn disassemble(&self) -> Result<String> {
        let name_opt = self.source_mapper.source_map.module_name_opt.as_ref();
        let name = name_opt.map(|(addr, n)| format!("{}.{}", addr.short_str_lossless(), n));
        let version = format!("{}", self.source_mapper.bytecode.version());
        let llvm_module_name : String;
        let header = match name {
            Some(s) => {
                llvm_module_name = String::clone(&s) + ".bc";
                format!("module {}", s)
            },
            None => {
                llvm_module_name = "script.bc".to_string();
                "script".to_owned()
            }
        };
        use inkwell::context::Context;
        let llvm_context = Context::create();

        let llvm_module = llvm_context.create_module(&header);
        llvm_module.print_to_stderr();
        let _target_machine = self.get_target_machine().unwrap();
        println!("Disassembling: {}, with target: {}", header, self.llvm_target_triple());

        let bc_file = File::create(&llvm_module_name).unwrap();
        llvm_module.write_bitcode_to_file(&bc_file, true, true);
        let struct_defs: Vec<String> = (0..self
            .source_mapper
            .bytecode
            .struct_defs()
            .map_or(0, |d| d.len()))
            .map(|i| self.disassemble_struct_def(StructDefinitionIndex(i as TableIndex)))
            .collect::<Result<Vec<String>>>()?;

        println!("Struct defs: {:?}", struct_defs);

        let function_defs: Vec<String> = match self.source_mapper.bytecode {
            BinaryIndexedView::Script(script) => {
                vec![self.disassemble_function_def(
                    self.source_mapper
                        .source_map
                        .get_function_source_map(FunctionDefinitionIndex(0_u16))?,
                    None,
                    IdentStr::new("main")?,
                    &script.type_parameters,
                    script.parameters,
                    Some(&script.code),
                )?]
            }
            BinaryIndexedView::Module(module) => (0..module.function_defs.len())
                .map(|i| {
                    let function_definition_index = FunctionDefinitionIndex(i as TableIndex);
                    let function_definition = self.get_function_def(function_definition_index)?;
                    let function_handle = self
                        .source_mapper
                        .bytecode
                        .function_handle_at(function_definition.function);
                    self.disassemble_function_def(
                        self.source_mapper
                            .source_map
                            .get_function_source_map(function_definition_index)?,
                        Some((function_definition, function_handle)),
                        self.source_mapper
                            .bytecode
                            .identifier_at(function_handle.name),
                        &function_handle.type_parameters,
                        function_handle.parameters,
                        function_definition.code.as_ref(),
                    )
                })
                .collect::<Result<Vec<String>>>()?,
        };
        println!("Function defs: {:?}", function_defs);

        Ok(format!(
            "// Move bytecode v{version}\n{header} {{\n{struct_defs}\n\n{function_defs}\n}}",
            version = version,
            header = header,
            struct_defs = &struct_defs.join("\n"),
            function_defs = &function_defs.join("\n")
        ))
    }
}
