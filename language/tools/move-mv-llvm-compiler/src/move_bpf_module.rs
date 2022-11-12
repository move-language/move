use llvm_sys::prelude::{LLVMBuilderRef, LLVMContextRef, LLVMValueRef, LLVMMetadataRef, LLVMModuleRef, LLVMDIBuilderRef};
//use inkwell::builder::Builder;
//use inkwell::context::Context;
/*use inkwell::debug_info::DICompileUnit;
use inkwell::debug_info::DebugInfoBuilder;
use inkwell::module::{Linkage, Module};
use inkwell::OptimizationLevel;
use inkwell::{targets::{TargetTriple, TargetMachine}};
use inkwell::targets::{CodeModel, RelocMode};
use inkwell::{targets::Target, targets::InitializationConfig};
use inkwell::types::{
    ArrayType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, IntType, StringRadix,
};*/
use llvm_sys::target_machine::{LLVMCodeGenOptLevel, LLVMCodeModel, LLVMTargetMachineRef, LLVMCreateTargetMachine, LLVMTargetRef, LLVMRelocMode};

use crate::support::{to_c_str, LLVMString};
use std::borrow::Cow;
use std::error::Error;
use std::ffi::{CStr, CString};
use std::fmt::{self, Debug, Display, Formatter};
use std::marker::PhantomData;
use std::ops::Deref;
use once_cell::sync::Lazy;
use parking_lot::RwLock;

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

use move_bytecode_source_map::source_map::SourceName;
use once_cell::sync::OnceCell;

static LLVM_INIT: OnceCell<()> = OnceCell::new();

/// Source file scope for debug info
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct DIFile<'ctx> {
    pub(crate) metadata_ref: LLVMMetadataRef,
    _marker: PhantomData<&'ctx LLVMContextRef>,
}

/// Compilation unit scope for debug info
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct DICompileUnit<'ctx> {
    file: DIFile<'ctx>,
    pub(crate) metadata_ref: LLVMMetadataRef,
    _marker: PhantomData<&'ctx LLVMContextRef>,
}

impl<'ctx> DICompileUnit<'ctx> {
    pub fn get_file(&self) -> DIFile<'ctx> {
        self.file
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct InitializationConfig {
    pub asm_parser: bool,
    pub asm_printer: bool,
    pub base: bool,
    pub disassembler: bool,
    pub info: bool,
    pub machine_code: bool,
}

impl Default for InitializationConfig {
    fn default() -> Self {
        InitializationConfig {
            asm_parser: true,
            asm_printer: true,
            base: true,
            disassembler: true,
            info: true,
            machine_code: true,
        }
    }
}

static TARGET_LOCK: Lazy<RwLock<()>> = Lazy::new(|| RwLock::new(()));

#[derive(Eq)]
pub struct TargetTriple {
    pub(crate) triple: LLVMString,
}

impl TargetTriple {
    pub(crate) fn new(triple: LLVMString) -> TargetTriple {
        TargetTriple { triple }
    }

    pub fn create(triple: &str) -> TargetTriple {
        let c_string = to_c_str(triple);

        TargetTriple {
            triple: LLVMString::create_from_c_str(&c_string),
        }
    }

    pub fn as_str(&self) -> &CStr {
        unsafe { CStr::from_ptr(self.as_ptr()) }
    }

    pub fn as_ptr(&self) -> *const ::libc::c_char {
        self.triple.as_ptr()
    }
}

impl PartialEq for TargetTriple {
    fn eq(&self, other: &TargetTriple) -> bool {
        self.triple == other.triple
    }
}

impl fmt::Debug for TargetTriple {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "TargetTriple({:?})", self.triple)
    }
}

impl fmt::Display for TargetTriple {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "TargetTriple({:?})", self.triple)
    }
}

pub struct MoveBPFModule<'a> {
    pub name: String,
    pub module: LLVMModuleRef, // Some things in inkwell are good. like Module which takes lifetime parameters. That might help getting around borrow checker issues down the road.
    pub builder: LLVMBuilderRef,
    pub dibuilder: LLVMDIBuilderRef,
    pub di_compile_unit: DICompileUnit<'a>,
    pub(crate) context: &'a LLVMContextRef,
    pub(crate) opt: LLVMCodeGenOptLevel,
}

impl<'a> MoveBPFModule<'a> {
    fn llvm_target_triple() -> TargetTriple {
        TargetTriple::create("bpfel-unknown-unknown")
    }

    fn llvm_target_name() -> &'static str {
        "bpfel" // bpf little endian.
    }

    fn llvm_features() -> &'static str {
        "" // no additional target specific features.
    }

    pub fn initialize_bpf(config: &InitializationConfig) {
        use llvm_sys::target::{
            LLVMInitializeBPFAsmPrinter, LLVMInitializeBPFTarget, LLVMInitializeBPFTargetInfo,
            LLVMInitializeBPFTargetMC,
        };

        if config.base {
            let _guard = TARGET_LOCK.write();
            unsafe { LLVMInitializeBPFTarget() };
        }

        if config.info {
            let _guard = TARGET_LOCK.write();
            unsafe { LLVMInitializeBPFTargetInfo() };
        }

        if config.asm_printer {
            let _guard = TARGET_LOCK.write();
            unsafe { LLVMInitializeBPFAsmPrinter() };
        }

        // No asm parser

        if config.disassembler {
            use llvm_sys::target::LLVMInitializeBPFDisassembler;

            let _guard = TARGET_LOCK.write();
            unsafe { LLVMInitializeBPFDisassembler() };
        }

        if config.machine_code {
            let _guard = TARGET_LOCK.write();
            unsafe { LLVMInitializeBPFTargetMC() };
        }
    }

    pub fn get_target_machine(&self) -> Option<LLVMTargetMachineRef> {
        Self::initialize_bpf(&InitializationConfig::default());

        let opt_level = LLVMCodeGenOptLevel::LLVMCodeGenLevelNone; // TODO: Add optimization based on command line flag.
        let reloc_mode = LLVMRelocMode::LLVMRelocDefault;
        let code_model = LLVMCodeModel::LLVMCodeModelDefault;
        let target:LLVMTargetRef;
        let cpu = "v2";

        let target_machine = unsafe {
            LLVMCreateTargetMachine(
                target,
                MoveBPFModule::llvm_target_triple().as_ptr(),
                to_c_str(cpu).as_ptr(),
                to_c_str(MoveBPFModule::llvm_features()).as_ptr(),
                opt_level,
                reloc_mode,
                code_model,
            )
        };

        assert!(!target_machine.is_null());
        return Some(target_machine);
    }

    pub fn new(
        context: &'a LLVMContextRef,
        name: &str,
        filename: &str,
        opt: LLVMCodeGenOptLevel,
    ) -> Self {
        LLVM_INIT.get_or_init(|| {
            inkwell::targets::Target::initialize_bpf(&Default::default());
        });

        let triple = MoveBPFModule::llvm_target_triple();
        let module = context.create_module(name);

        let debug_metadata_version = context.i32_type().const_int(3, false);
        module.add_basic_value_flag(
            "Debug Info Version",
            inkwell::module::FlagBehavior::Warning,
            debug_metadata_version,
        );

        let builder = context.create_builder();
        let (dibuilder, di_compile_unit) = module.create_debug_info_builder(
            true,
            inkwell::debug_info::DWARFSourceLanguage::C,
            filename,
            ".",
            "Move",
            false,
            "",
            0,
            "",
            inkwell::debug_info::DWARFEmissionKind::Full,
            0,
            false,
            false,
            "",
            "",
        );

        module.set_triple(&triple);
        module.set_source_file_name(filename);

        MoveBPFModule {
            name: name.to_owned(),
            module,
            builder,
            dibuilder,
            di_compile_unit,
            context,
            opt,
        }
    }

    pub(crate) fn llvm_type_for_sig_tok(&self, sig_tok: &SignatureToken, _type_parameters: &[AbilitySet]) -> BasicTypeEnum<'a> {
        match sig_tok {
            SignatureToken::Bool => BasicTypeEnum::IntType(self.context.bool_type()),
            SignatureToken::U8 => BasicTypeEnum::IntType(self.context.custom_width_int_type(8)), // FIXME: The signedness
            SignatureToken::U64 => BasicTypeEnum::IntType(self.context.custom_width_int_type(64)), // FIXME: The signedness
            _ => unimplemented!("Remaining Signature tokens to be implemented"),
        }
    }
    pub fn llvm_type_for_sig_tokens(&self, sig_tokens: Vec<SignatureToken>, type_parameters: &[AbilitySet],) -> Vec<BasicTypeEnum<'a>> {
        let mut vec = Vec::new();
        for v in sig_tokens {
            vec.push(self.llvm_type_for_sig_tok(&v, type_parameters));
        }
        return vec;
    }
    pub fn llvm_constant() {
        // TODO: Return a constant value corresponding to the input type.
    }
}