// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#![allow(unused_variables)]
#![allow(unused_must_use)]
#![allow(unused_imports)]

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
        TableIndex, TypeSignature, Visibility, StructHandleIndex,
    },
};
use move_bytecode_source_map::{
    mapping::SourceMapping,
    source_map::{FunctionSourceMap, SourceName},
};

use move_core_types::identifier::IdentStr;
use move_coverage::coverage_map::{ExecCoverageMap, FunctionCoverage};
use move_ir_types::location::Loc;

use llvm_sys::prelude::{
    LLVMBuilderRef, LLVMContextRef as LLVMContext, LLVMDIBuilderRef, LLVMMetadataRef,
    LLVMModuleRef, LLVMTypeRef, LLVMValueRef,
};
use llvm_sys::{
    core::{LLVMDumpModule, LLVMFunctionType, LLVMModuleCreateWithNameInContext, LLVMPrintModuleToFile},
    target_machine::LLVMCodeGenOptLevel,
};

//use llvm_sys::{target_machine::{LLVMCodeGenOptLevel}, core::{LLVMModuleCreateWithNameInContext, LLVMDumpModule, LLVMFunctionType, LLVMPrintModuleToFile}};

use std::{fs::File, mem::MaybeUninit};

//use llvm_sys::prelude::{LLVMBuilderRef, LLVMContextRef as LLVMContext, LLVMValueRef, LLVMMetadataRef, LLVMModuleRef, LLVMDIBuilderRef, LLVMTypeRef};

use crate::{move_bpf_module::MoveBPFModule, support::to_c_str};

use crate::errors::DisassemblerError;

use std::collections::HashMap;

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
    llvm_context: LLVMContext,
}

impl<'a> Disassembler<'a> {
    pub fn new(
        source_mapper: SourceMapping<'a>,
        options: DisassemblerOptions,
        llvm_context: LLVMContext,
    ) -> Self {
        Self {
            source_mapper: source_mapper,
            options: options,
            coverage_map: None,
            llvm_context: llvm_context,
        }
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
            x => Err(anyhow!(format!("Unhandled move instruction: {:#?}", x))),
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
        "".to_string()
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

    /// Returns type of llvm function with `ty` as return type and `param_types` as parameters.
    fn fn_type(
        ty: LLVMTypeRef,
        param_types: &mut Vec<LLVMTypeRef>,
        is_var_args: bool,
    ) -> LLVMTypeRef {
        assert!(!is_var_args, "Varargs not supported");
        //let mut param_types: Vec<LLVMTypeRef> = param_types.iter().map(|val| val.as_type_ref()).collect();
        unsafe {
            LLVMFunctionType(
                ty,
                param_types.as_mut_ptr(),
                param_types.len() as u32,
                is_var_args as i32,
            )
        }
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
        move_module: &mut MoveBPFModule,
    ) -> Result<String> {
        debug_assert_eq!(
            function_source_map.parameters.len(),
            self.source_mapper.bytecode.signature_at(parameters).len(),
            "Arity mismatch between function source map and bytecode for function {}",
            name
        );

        let parameter_list = &self.source_mapper.bytecode.signature_at(parameters).0;

        let ret_type = match function {
            Some(function) => self
                .source_mapper
                .bytecode
                .signature_at(function.1.return_)
                .0
                .clone(),
            None => vec![],
        };

        let llvm_return_type = move_module.llvm_type_for_sig_tokens(ret_type);
        // TODO: Account for signedness. Or maybe the signedness is incorporated as part of the use cases.
        let mut llvm_type_parameters =
            move_module.llvm_type_for_sig_tokens(parameter_list.to_vec());

        let fn_value = unsafe {
            llvm_sys::core::LLVMAddFunction(
                move_module.module,
                to_c_str(name.as_str()).as_ptr(),
                Self::fn_type(llvm_return_type[0], &mut llvm_type_parameters, false),
            )
        };

        let entry_block = move_module.append_basic_block(fn_value, "entry");
        move_module.position_at_end(entry_block);
        move_module.build_return(move_module.llvm_constant(0));

        let body = match code {
            Some(code) => {
                let locals = self.disassemble_locals(function_source_map, code.locals, 10)?;
                let bytecode =
                    self.disassemble_bytecode(function_source_map, name, parameters, code)?;
                Self::format_function_body(locals, bytecode)
            }
            None => "".to_string(),
        };
        Ok("".to_string())
    }

    pub fn process_struct_def(&self,
                              struct_def: &StructDefinition,
                              move_module: &mut MoveBPFModule,
    ) -> Result<LLVMTypeRef> {
        let llvm_struct = move_module.llvm_struct_from_index(&struct_def.struct_handle);
        let mut llvm_elem_types : Vec<LLVMTypeRef> = Vec::new();
        match &struct_def.field_information {
            StructFieldInformation::Native => return Ok(llvm_struct),
            StructFieldInformation::Declared(fields) => Some(
                for field_definition in fields {
                    let x = move_module.llvm_type_for_sig_tok(&field_definition.signature.0);
                    llvm_elem_types.push(x);
                }),
        };
        move_module.llvm_set_struct_body(llvm_struct, &mut llvm_elem_types);
        Ok(llvm_struct)
    }

    pub fn disassemble(&self, bitcode_or_text: bool) -> Result<String> {
        let name_opt = self.source_mapper.source_map.module_name_opt.as_ref();
        let name = name_opt.map(|(addr, n)| format!("{}.{}", addr.short_str_lossless(), n));
        let version = format!("{}", self.source_mapper.bytecode.version());
        let llvm_module_name: String;
        let header = match name {
            Some(s) => {
                llvm_module_name = String::clone(&s) + ".bc";
                format!("module {}", s)
            }
            None => {
                llvm_module_name = "script.bc".to_string();
                "script".to_owned()
            }
        };

        //let llvm_module = self.llvm_context.create_module(&header);
        let c_string = to_c_str(&header);

        let llvm_module =
            unsafe { LLVMModuleCreateWithNameInContext(c_string.as_ptr(), self.llvm_context) };

        //llvm_module.print_to_stderr();
        unsafe {
            LLVMDumpModule(llvm_module);
        }
        println!("Disassembling: {}", header);

        let bc_file = File::create(&llvm_module_name).unwrap();
        let opt = LLVMCodeGenOptLevel::LLVMCodeGenLevelNone; // TODO: Add optimization based on command line flag.
        let mut move_module = MoveBPFModule::new(&self.llvm_context, &header, &*llvm_module_name, opt, &self.source_mapper);

        let process_struct : bool = true;
        if process_struct {
            for i in &self.source_mapper.bytecode.struct_defs() {
                self.process_struct_def(&i[0], &mut move_module);
            }
        }

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
                    &mut move_module,
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
                        &mut move_module,
                    )
                })
                .collect::<Result<Vec<String>>>()?,
        };
        println!("Function defs: {:?}", function_defs);
        //move_module.module.write_bitcode_to_file(&bc_file, true, true);
        use llvm_sys::bit_writer::LLVMWriteBitcodeToFD;
        use std::os::unix::io::AsRawFd;

        unsafe {
            if bitcode_or_text {
                LLVMWriteBitcodeToFD(
                    move_module.module,
                    bc_file.as_raw_fd(),
                    true as i32,
                    true as i32,
                );
            } else {
                let mut err_string = MaybeUninit::uninit();
                LLVMPrintModuleToFile(move_module.module,
                    to_c_str(&(llvm_module_name + ".ll")).as_ptr(),
                    err_string.as_mut_ptr(),
                );
            }
        }

        Ok(format!(
            "// Move bytecode v{version}\n{header} {{\n{function_defs}\n}}",
            version = version,
            header = header,
            function_defs = &function_defs.join("\n")
        ))
    }
}
