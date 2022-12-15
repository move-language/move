// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::{bail, Error, Result};
use clap::Parser;
use move_binary_format::{
    binary_views::BinaryIndexedView,
    file_format::{
        FunctionDefinition, FunctionDefinitionIndex, FunctionHandle,
        StructDefinition, StructDefinitionIndex, StructFieldInformation,
        TableIndex, SignatureIndex,
    },
};
use move_bytecode_source_map::{
    mapping::SourceMapping,
};

use move_core_types::identifier::IdentStr;

use llvm_sys::prelude::{
    LLVMContextRef as LLVMContext, LLVMTypeRef, LLVMValueRef, LLVMModuleRef,
};
use llvm_sys::{
    core::{LLVMDumpModule, LLVMModuleCreateWithNameInContext, LLVMAddFunction, LLVMFunctionType,
    LLVMAddGlobal, LLVMGetStructName},
    target_machine::LLVMCodeGenOptLevel,
};
use std::{fs::File, mem::MaybeUninit};

use crate::{move_bpf_module::MoveBPFModule, support::to_c_str};

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
    llvm_context: LLVMContext,
}

impl<'a> Disassembler<'a> {
    pub fn new(
        source_mapper: SourceMapping<'a>,
        llvm_context: LLVMContext,
    ) -> Self {
        Self {
            source_mapper: source_mapper,
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

    pub fn process_function_def(
        &self,
        function: Option<(&FunctionDefinition, &FunctionHandle)>,
        name: &IdentStr,
        parameters: SignatureIndex,
        move_module: &mut MoveBPFModule,
    ) -> LLVMValueRef {
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
        let ts = move_module.llvm_type_for_sig_tokens(&ret_type);
        let llvm_type_return = move_module.llvm_make_single_return_type(ts);
        
        let mut llvm_type_parameters =
            move_module.llvm_type_for_sig_tokens(&parameter_list.to_vec());

        let fn_value = unsafe {
            LLVMAddFunction(
                move_module.module,
                to_c_str(name.as_str()).as_ptr(),
                LLVMFunctionType(llvm_type_return, llvm_type_parameters.as_mut_ptr(),
                                 llvm_type_parameters.len() as u32, false as i32),
            )
        };

        let entry_block = move_module.append_basic_block(fn_value, "entry");
        move_module.position_at_end(entry_block);
        move_module.build_return(move_module.llvm_constant(0));

        fn_value
    }

    pub fn process_struct_def(&self,
                              struct_def_idx: StructDefinitionIndex,
                              move_module: &mut MoveBPFModule,
    ) -> Result<LLVMTypeRef> {
        let struct_def = self.get_struct_def(struct_def_idx)?;
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
        let name = unsafe{LLVMGetStructName(llvm_struct)};
        if !name.is_null() {
            unsafe{LLVMAddGlobal(move_module.module, llvm_struct, name)};
        };
        Ok(llvm_struct)
    }

    pub fn disassemble(&mut self) -> Result<LLVMModuleRef> {
        let name_opt = self.source_mapper.source_map.module_name_opt.as_ref();
        let name = name_opt.map(|(addr, n)| format!("{}.{}", addr.short_str_lossless(), n));
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

        let c_string = to_c_str(&header);

        let opt = LLVMCodeGenOptLevel::LLVMCodeGenLevelNone; // TODO: Add optimization based on command line flag.
        let mut move_module = MoveBPFModule::new(&self.llvm_context, &header, &*llvm_module_name, opt, &self.source_mapper);

        (0 .. self.source_mapper.bytecode.struct_defs().map_or(0, |d| d.len()))
            .map(|i| self.process_struct_def(StructDefinitionIndex(i as TableIndex), &mut move_module))
            .collect::<Vec<Result<LLVMTypeRef>>>();

        match self.source_mapper.bytecode {
            BinaryIndexedView::Script(script) => {
                self.process_function_def(
                    None,
                    IdentStr::new("main")?,
                    script.parameters,
                    &mut move_module);
            }
            BinaryIndexedView::Module(module) => for i in 0..module.function_defs.len(){
                let function_definition_index = FunctionDefinitionIndex(i as TableIndex);
                let function_definition = self.get_function_def(function_definition_index)?;
                let function_handle = self
                    .source_mapper
                    .bytecode
                    .function_handle_at(function_definition.function);
                self.process_function_def(
                    Some((function_definition, function_handle)),
                    self.source_mapper
                        .bytecode
                        .identifier_at(function_handle.name),
                    function_handle.parameters,
                    &mut move_module);
            }
        };

        Ok(move_module.module)
    }

    pub fn llvm_write_to_file(&self, module: LLVMModuleRef, llvm_ir: bool, output_file_name: &String) {
        use llvm_sys::bit_writer::LLVMWriteBitcodeToFD;
        use llvm_sys::core::LLVMPrintModuleToFile;
        use std::os::unix::io::AsRawFd;

        unsafe {
            if llvm_ir {
                let mut err_string = MaybeUninit::uninit();
                LLVMPrintModuleToFile(module,
                                      to_c_str(&output_file_name).as_ptr(),
                                      err_string.as_mut_ptr(),
                );
            } else {
                let bc_file = File::create(&output_file_name).unwrap();
                LLVMWriteBitcodeToFD(
                    module,
                    bc_file.as_raw_fd(),
                    true as i32,
                    true as i32,
                );
            }
        }
    }
}
