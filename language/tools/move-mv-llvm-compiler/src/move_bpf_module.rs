use llvm_sys::core::{LLVMInt1TypeInContext, LLVMInt8TypeInContext, LLVMInt64TypeInContext, LLVMModuleCreateWithNameInContext, LLVMAddModuleFlag, LLVMConstInt, LLVMCreateBuilderInContext, LLVMSetTarget, LLVMAppendBasicBlockInContext, LLVMGetNextBasicBlock, LLVMInsertBasicBlockInContext, LLVMGetBasicBlockParent, LLVMPositionBuilderAtEnd, LLVMBuildRetVoid, LLVMBuildRet, LLVMGetTypeKind, LLVMTypeOf, LLVMInt64Type};

use llvm_sys::prelude::{LLVMBuilderRef, LLVMContextRef, LLVMValueRef, LLVMMetadataRef, LLVMModuleRef, LLVMDIBuilderRef, LLVMTypeRef, LLVMBasicBlockRef};
use llvm_sys::target_machine::{LLVMCodeGenOptLevel, LLVMCodeModel, LLVMTargetMachineRef, LLVMCreateTargetMachine, LLVMTargetRef, LLVMRelocMode, LLVMGetTargetFromName};
use llvm_sys::{LLVMModuleFlagBehavior, LLVMTypeKind};
use llvm_sys::debuginfo::{LLVMDWARFEmissionKind, LLVMDWARFSourceLanguage, LLVMDIBuilderCreateCompileUnit, LLVMCreateDIBuilder, LLVMCreateDIBuilderDisallowUnresolved, LLVMDIBuilderCreateFile};

use crate::support::{to_c_str, LLVMString};
use std::any::Any;
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

/// Since LLVM types do not have signs, the signedness is encoded in the operations like udiv etc.
/// It becomes very hard to work with this setup at a higher level so we need to keep the signedness
/// along with the type.
pub struct LLVMSignedType {
    pub llvm_type : LLVMTypeRef,
    pub is_signed : bool // True = signed, False = unsigned
}

impl LLVMSignedType {
    pub(crate) fn new(t : LLVMTypeRef, s : bool) -> LLVMSignedType {
        LLVMSignedType { llvm_type : t, is_signed : s }
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
        let llvm_target_name_ptr = to_c_str(Self::llvm_target_name()).as_ptr();
        let target:LLVMTargetRef = unsafe { LLVMGetTargetFromName(llvm_target_name_ptr) };
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

    pub fn add_basic_value_flag(module: LLVMModuleRef, key: &str, behavior: LLVMModuleFlagBehavior, flag: LLVMValueRef) {
        use llvm_sys::core::LLVMValueAsMetadata;

        let md = unsafe { LLVMValueAsMetadata(flag) };

        unsafe {
            LLVMAddModuleFlag(
                module,
                behavior.into(),
                key.as_ptr() as *mut ::libc::c_char,
                key.len(),
                md,
            )
        }
    }

    pub fn set_source_file_name(module : LLVMModuleRef, file_name: &str) {
        use llvm_sys::core::LLVMSetSourceFileName;

        unsafe {
            LLVMSetSourceFileName(
                module,
                file_name.as_ptr() as *const ::libc::c_char,
                file_name.len(),
            )
        }
    }

    pub fn new(
        context: &'a LLVMContextRef,
        name: &str,
        filename: &str,
        opt: LLVMCodeGenOptLevel,
    ) -> Self {
        LLVM_INIT.get_or_init(|| {
            Self::initialize_bpf(&InitializationConfig::default());
        });

        let triple = MoveBPFModule::llvm_target_triple();
        let c_string = to_c_str(name);

        let module = unsafe { LLVMModuleCreateWithNameInContext(c_string.as_ptr(), *context) };

        let debug_metadata_version = unsafe { LLVMConstInt(LLVMInt64TypeInContext(*context), 3, false as i32) };
        Self::add_basic_value_flag(
            module,
            "Debug Info Version",
            LLVMModuleFlagBehavior::LLVMModuleFlagBehaviorWarning,
            debug_metadata_version,
        );

        let builder = unsafe { LLVMCreateBuilderInContext(*context) };

        let dibuilder = unsafe { LLVMCreateDIBuilder(module) };

        //let dibuilder = DebugInfoBuilder {
        //    builder,
        //    _marker: PhantomData,
        //};

        let directory = ".";
        //let file = builder.create_file(filename, directory);

        let file_metadata_ref = unsafe {
            LLVMDIBuilderCreateFile(
                dibuilder,
                filename.as_ptr() as _,
                filename.len(),
                directory.as_ptr() as _,
                directory.len(),
            )
        };

        let producer = "Move";
        let is_optimized = false;
        let flags = "";
        let runtime_ver = 0;
        let split_name = "";
        let kind = LLVMDWARFEmissionKind::LLVMDWARFEmissionKindFull;
        let dwo_id = 0;
        let split_debug_inlining= false;
        let debug_info_for_profiling= false;
        let sysroot = "";
        let sdk = "";

        let di_compile_unit = unsafe { LLVMDIBuilderCreateCompileUnit(
            dibuilder,
            LLVMDWARFSourceLanguage::LLVMDWARFSourceLanguageC,
            file_metadata_ref,
            producer.as_ptr() as _,
            producer.len(),
            is_optimized as _,
            flags.as_ptr() as _,
            flags.len(),
            runtime_ver,
            split_name.as_ptr() as _,
            split_name.len(),
            kind.into(),
            dwo_id,
            split_debug_inlining as _,
            debug_info_for_profiling as _,
            sysroot.as_ptr() as _,
            sysroot.len(),
            sdk.as_ptr() as _,
            sdk.len(),
        ) };

        let di_compile_unit = DICompileUnit {
            file: DIFile { metadata_ref: file_metadata_ref, _marker: PhantomData},
            metadata_ref: file_metadata_ref,
            _marker: PhantomData,
        };

        /*let (dibuilder, di_compile_unit) = module.create_debug_info_builder(
            true,
            LLVMDWARFSourceLanguage::LLVMDWARFSourceLanguageC,
            filename,
            ".",
            "Move",
            false,
            "",
            0,
            "",
            LLVMDWARFEmissionKind::LLVMDWARFEmissionKindFull,
            0,
            false,
            false,
            "",
            "",
        );*/

        // module.set_triple(&triple);
        unsafe { LLVMSetTarget(module, triple.as_ptr()) }
        Self::set_source_file_name(module, filename);

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

    pub(crate) fn llvm_signed_type_for_sig_tok(&self, sig_tok: &SignatureToken, _type_parameters: &[AbilitySet]) -> LLVMTypeRef {
        match sig_tok {
            SignatureToken::Bool => unsafe{LLVMInt1TypeInContext(*self.context)},
            SignatureToken::U8 => unsafe{LLVMInt8TypeInContext(*self.context)}, // FIXME: In llvm signedness is achieved with `cast` operation.
            SignatureToken::U64 => unsafe{LLVMInt64TypeInContext(*self.context)}, // FIXME: The signedness
            _ => unimplemented!("Remaining Signature tokens to be implemented"),
        }
    }
    pub fn llvm_signed_type_for_sig_tokens(&self, sig_tokens: Vec<SignatureToken>, type_parameters: &[AbilitySet],) -> Vec<LLVMTypeRef> {
        let mut vec = Vec::new();
        for v in sig_tokens {
            vec.push(self.llvm_signed_type_for_sig_tok(&v, type_parameters));
        }
        return vec;
    }

    pub(crate) fn llvm_type_for_sig_tok(&self, sig_tok: &SignatureToken, _type_parameters: &[AbilitySet]) -> LLVMSignedType {
        match sig_tok {
            SignatureToken::Bool => LLVMSignedType::new(unsafe{LLVMInt1TypeInContext(*self.context)}, false),
            SignatureToken::U8 => LLVMSignedType::new(unsafe{LLVMInt8TypeInContext(*self.context)}, true), // FIXME: In llvm signedness is achieved with `cast` operation.
            SignatureToken::U64 => LLVMSignedType::new(unsafe{LLVMInt64TypeInContext(*self.context)}, true), // FIXME: The signedness
            _ => unimplemented!("Remaining Signature tokens to be implemented"),
        }
    }
    pub fn llvm_type_for_sig_tokens(&self, sig_tokens: Vec<SignatureToken>, type_parameters: &[AbilitySet],) -> Vec<LLVMSignedType> {
        let mut vec = Vec::new();
        for v in sig_tokens {
            vec.push(self.llvm_type_for_sig_tok(&v, type_parameters));
        }
        return vec;
    }
    pub fn llvm_constant(&self, value: u64) -> LLVMValueRef {
        // TODO: Return a constant value corresponding to the input type.
        unsafe { LLVMConstInt(LLVMInt64TypeInContext(*self.context), value, false as i32) }
    }

    pub fn get_next_basic_block(&self, basic_block: LLVMBasicBlockRef) -> Option<LLVMBasicBlockRef> {
        let next_bb = unsafe { LLVMGetNextBasicBlock(basic_block) };
        if next_bb.is_null() {
            return None;
        }
        Some(next_bb)
    }

    pub fn append_basic_block<'ctx>(&self, function: LLVMValueRef, name: &str) -> LLVMBasicBlockRef {
        let c_string = to_c_str(name);
        unsafe {
            LLVMAppendBasicBlockInContext(
                *self.context,
                function,
                c_string.as_ptr(),
            )
        }
    }

    pub fn prepend_basic_block(&self, basic_block: LLVMBasicBlockRef, name: &str) -> LLVMBasicBlockRef {
        let c_string = to_c_str(name);
        unsafe {
            LLVMInsertBasicBlockInContext(
                *self.context,
                basic_block,
                c_string.as_ptr(),
            )
        }
    }

    pub fn insert_basic_block_after(&self, basic_block: LLVMBasicBlockRef, name: &str) -> LLVMBasicBlockRef {
        //let next_basic_block = &self.get_next_basic_block(basic_block);
        match self.get_next_basic_block(basic_block) {
            Some(bb) => self.prepend_basic_block(bb, name),
            None => unsafe { self.append_basic_block(LLVMGetBasicBlockParent(basic_block), name) }
        }
    }

    pub fn position_at_end(&self, basic_block: LLVMBasicBlockRef) {
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder, basic_block);
        }
    }
    pub fn build_return(&self, value: LLVMValueRef) {
        unsafe {
            match LLVMGetTypeKind(LLVMTypeOf(value)) {
                LLVMTypeKind::LLVMVoidTypeKind => LLVMBuildRetVoid(self.builder),
                default => LLVMBuildRet(self.builder, value)
            }
     }  ;
    }
    pub fn llvm_return_type_for(&self, ret_type : &Vec<SignatureToken>) -> LLVMSignedType {
        match ret_type[0] {
            SignatureToken::Bool => LLVMSignedType::new(unsafe{LLVMInt1TypeInContext(*self.context)}, false),
            SignatureToken::U8 => LLVMSignedType::new(unsafe{LLVMInt8TypeInContext(*self.context)}, true), // FIXME: In llvm signedness is achieved with `cast` operation.
            SignatureToken::U64 => LLVMSignedType::new(unsafe{LLVMInt64TypeInContext(*self.context)}, true), // FIXME: The signedness
            _ => unimplemented!("Remaining Signature tokens to be implemented"),
        }
    }

}