//! LLVM wrappers.
//!
//! The stackless code generator accesses llvm only through this mod.
//!
//! It:
//!
//! - Runs dtors
//! - Encapsulates unsafety, though making LLVM fully memsafe is hard.
//! - Hides weirdly mutable array pointers.
//! - Provides high-level instruction builders compatible with the stackless bytecode model.

use llvm_extra_sys::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;

use crate::cstr::SafeCStr;

use std::ptr;
use std::ffi::{CStr, CString};

pub use llvm_extra_sys::AttributeKind;
pub use llvm_sys::LLVMIntPredicate;

pub fn initialize_bpf() {
    unsafe {
        LLVMInitializeBPFTargetInfo();
        LLVMInitializeBPFTarget();
        LLVMInitializeBPFTargetMC();
        LLVMInitializeBPFAsmPrinter();
    }
}

pub struct Context(LLVMContextRef);

impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            LLVMContextDispose(self.0);
        }
    }
}

impl Context {
    pub fn new() -> Context {
        unsafe { Context(LLVMContextCreate()) }
    }

    pub fn create_module(&self, name: &str) -> Module {
        unsafe { Module(LLVMModuleCreateWithNameInContext(name.cstr(), self.0)) }
    }

    pub fn create_builder(&self) -> Builder {
        unsafe { Builder(LLVMCreateBuilderInContext(self.0)) }
    }

    pub fn void_type(&self) -> Type {
        unsafe { Type(LLVMVoidTypeInContext(self.0)) }
    }

    pub fn int1_type(&self) -> Type {
        unsafe { Type(LLVMInt1TypeInContext(self.0)) }
    }

    pub fn int8_type(&self) -> Type {
        unsafe { Type(LLVMInt8TypeInContext(self.0)) }
    }

    pub fn int64_type(&self) -> Type {
        unsafe { Type(LLVMInt64TypeInContext(self.0)) }
    }
}

pub struct Module(LLVMModuleRef);

impl Drop for Module {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeModule(self.0);
        }
    }
}

impl AsMut<llvm_sys::LLVMModule> for Module {
    fn as_mut(&mut self) -> &mut llvm_sys::LLVMModule {
        unsafe { &mut *self.0 }
    }
}

impl Module {
    pub fn set_target(&self, triple: &str) {
        unsafe {
            LLVMSetTarget(self.0, triple.cstr());
        }
    }

    pub fn set_source_file_name(&self, name: &str) {
        unsafe { LLVMSetSourceFileName(self.0, name.as_ptr() as *const libc::c_char, name.len()) }
    }

    pub fn add_function(&self, name: &str, ty: FunctionType) -> Function {
        unsafe { Function(LLVMAddFunction(self.0, name.cstr(), ty.0)) }
    }

    pub fn add_function_with_attrs(
        &self,
        name: &str,
        ty: FunctionType,
        attrs: &[AttributeKind],
    ) -> Function {
        unsafe {
            let cx = LLVMGetModuleContext(self.0);
            let attrs = attrs
                .iter()
                .map(|a| LLVMRustCreateAttrNoValue(cx, *a))
                .collect::<Vec<_>>();

            let llfn = self.add_function(name, ty);

            AddFunctionAttributes(llfn.0, AttributePlace::Function, &attrs);

            llfn
        }
    }

    pub fn get_named_function(&self, name: &str) -> Option<Function> {
        unsafe {
            let llfn = LLVMGetNamedFunction(self.0, name.cstr());
            if !llfn.is_null() {
                Some(Function(llfn))
            } else {
                None
            }
        }
    }

    pub fn verify(&self) {
        use llvm_sys::analysis::*;
        unsafe {
            LLVMVerifyModule(
                self.0,
                LLVMVerifierFailureAction::LLVMAbortProcessAction,
                ptr::null_mut(),
            );
        }
    }

    pub fn set_data_layout(&self, machine: &TargetMachine) {
        unsafe {
            let target_data = LLVMCreateTargetDataLayout(machine.0);
            let layout_str = LLVMCopyStringRepOfTargetData(target_data);
            LLVMSetDataLayout(self.0, layout_str);
            LLVMDisposeMessage(layout_str);
            LLVMDisposeTargetData(target_data);
        }
    }
}

pub struct Builder(LLVMBuilderRef);

impl Drop for Builder {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.0);
        }
    }
}

impl Builder {
    pub fn position_at_end(&self, bb: BasicBlock) {
        unsafe {
            LLVMPositionBuilderAtEnd(self.0, bb.0);
        }
    }

    pub fn build_alloca(&self, ty: Type, name: Name) -> Alloca {
        unsafe { Alloca(LLVMBuildAlloca(self.0, ty.0, name.0.cstr())) }
    }

    pub fn store_param_to_alloca(&self, param: Parameter, alloca: Alloca) {
        unsafe {
            LLVMBuildStore(self.0, param.0, alloca.0);
        }
    }

    pub fn load_store(&self, ty: Type, src: Alloca, dst: Alloca) {
        unsafe {
            let tmp_name = Name::kind("load_store_tmp");
            let tmp_reg = LLVMBuildLoad2(self.0, ty.0, src.0, tmp_name.0.cstr());
            LLVMBuildStore(self.0, tmp_reg, dst.0);
        }
    }

    pub fn build_return_void(&self) {
        unsafe {
            LLVMBuildRetVoid(self.0);
        }
    }

    pub fn load_return(&self, ty: Type, val: Alloca) {
        unsafe {
            let tmp_name = Name::kind("retval");
            let tmp_reg = LLVMBuildLoad2(self.0, ty.0, val.0, tmp_name.0.cstr());
            LLVMBuildRet(self.0, tmp_reg);
        }
    }

    pub fn store_const(&self, src: Constant, dst: Alloca) {
        unsafe {
            LLVMBuildStore(self.0, src.0, dst.0);
        }
    }

    pub fn build_br(&self, bb: BasicBlock) {
        unsafe {
            LLVMBuildBr(self.0, bb.0);
        }
    }

    pub fn load_cond_br(&self, ty: Type, val: Alloca, bb0: BasicBlock, bb1: BasicBlock) {
        unsafe {
            let cnd_name = Name::kind("cnd");
            let cnd_reg = LLVMBuildLoad2(self.0, ty.0, val.0, cnd_name.0.cstr());
            LLVMBuildCondBr(self.0, cnd_reg, bb0.0, bb1.0);
        }
    }

    pub fn load_call(&self, fnty: FunctionType, fnval: Function, args: &[(Type, Alloca)]) {
        unsafe {
            let mut args = args
                .iter()
                .enumerate()
                .map(|(i, (ty, val))| {
                    let name = Name::new("call_arg", i);
                    LLVMBuildLoad2(self.0, ty.0, val.0, name.0.cstr())
                })
                .collect::<Vec<_>>();
            LLVMBuildCall2(
                self.0,
                fnty.0,
                fnval.0,
                args.as_mut_ptr(),
                args.len() as libc::c_uint,
                "".cstr(),
            );
        }
    }

    pub fn build_unreachable(&self) {
        unsafe {
            LLVMBuildUnreachable(self.0);
        }
    }

    pub fn load_add_store(&self, ty: Type, src0: Alloca, src1: Alloca, dst: Alloca) {
        unsafe {
            let src0_name = Name::new("add_src", 0);
            let src1_name = Name::new("add_src", 1);
            let dst_name = Name::kind("add_dst");
            let src0_reg = LLVMBuildLoad2(self.0, ty.0, src0.0, src0_name.0.cstr());
            let src1_reg = LLVMBuildLoad2(self.0, ty.0, src1.0, src1_name.0.cstr());
            let dst_reg = LLVMBuildAdd(self.0, src0_reg, src1_reg, dst_name.0.cstr());
            LLVMBuildStore(self.0, dst_reg, dst.0);
        }
    }

    pub fn load_icmp_store(
        &self,
        ty: Type,
        src0: Alloca,
        src1: Alloca,
        dst: Alloca,
        pred: LLVMIntPredicate,
    ) {
        unsafe {
            let src0_name = Name::new("icmp_src", 0);
            let src1_name = Name::new("icmp_src", 1);
            let dst_name = Name::kind("icmp_dst");
            let src0_reg = LLVMBuildLoad2(self.0, ty.0, src0.0, src0_name.0.cstr());
            let src1_reg = LLVMBuildLoad2(self.0, ty.0, src1.0, src1_name.0.cstr());
            let dst_reg = LLVMBuildICmp(self.0, pred, src0_reg, src1_reg, dst_name.0.cstr());
            LLVMBuildStore(self.0, dst_reg, dst.0);
        }
    }
}

#[derive(Copy, Clone)]
pub struct Type(LLVMTypeRef);

impl Type {}

#[derive(Copy, Clone)]
pub struct FunctionType(LLVMTypeRef);

impl FunctionType {
    pub fn new(return_type: Type, parameter_types: &[Type]) -> FunctionType {
        let mut parameter_types: Vec<_> = parameter_types.iter().map(|t| t.0).collect();
        unsafe {
            FunctionType(LLVMFunctionType(
                return_type.0,
                parameter_types.as_mut_ptr(),
                parameter_types.len() as libc::c_uint,
                false as LLVMBool,
            ))
        }
    }
}

pub struct Function(LLVMValueRef);

impl Function {
    pub fn append_basic_block<N>(&self, name: N) -> BasicBlock
    where
        N: Into<Name>,
    {
        unsafe { BasicBlock(LLVMAppendBasicBlock(self.0, name.into().0.cstr())) }
    }

    pub fn get_param(&self, i: usize) -> Parameter {
        unsafe { Parameter(LLVMGetParam(self.0, i as u32)) }
    }

    pub fn llvm_type(&self) -> FunctionType {
        unsafe { FunctionType(LLVMTypeOf(self.0)) }
    }

    pub fn verify(&self) {
        use llvm_sys::analysis::*;
        unsafe {
            LLVMVerifyFunction(self.0, LLVMVerifierFailureAction::LLVMAbortProcessAction);
        }
    }
}

#[derive(Copy, Clone)]
pub struct BasicBlock(LLVMBasicBlockRef);

impl BasicBlock {}

/// A local identifier, variable, block, etc.
///
/// The trailing underscore is to visually distinguish cases where llvm renames
/// them with an appended number.
pub struct Name(String);

impl Name {
    pub fn new<S>(kind: &str, ident: S) -> Name
    where
        S: std::fmt::Display,
    {
        Name(format!("{kind}_{ident}_"))
    }

    pub fn kind(kind: &str) -> Name {
        Name(format!("{kind}_"))
    }
}

impl<'a> Into<Name> for &'a str {
    fn into(self) -> Name {
        Name::kind(self)
    }
}

#[derive(Copy, Clone)]
pub struct Alloca(LLVMValueRef);

impl Alloca {}

pub struct Parameter(LLVMValueRef);

impl Parameter {}

pub struct Constant(LLVMValueRef);

impl Constant {
    pub fn int(ty: Type, v: u64) -> Constant {
        unsafe { Constant(LLVMConstInt(ty.0, v, false as LLVMBool)) }
    }
}

pub struct Target(LLVMTargetRef);

impl Target {
    pub fn from_triple(triple: &str) -> anyhow::Result<Target> {
        unsafe {
            let target: &mut LLVMTargetRef = &mut ptr::null_mut();
            let error: &mut *mut libc::c_char = &mut ptr::null_mut();
            let result = LLVMGetTargetFromTriple(
                triple.cstr(),
                target,
                error,
            );

            if result == 0  {
                assert!((*error).is_null());
                Ok(Target(*target))
            } else {
                assert!(!(*error).is_null());
                let rust_error = CStr::from_ptr(*error)
                    .to_str()?.to_string();
                LLVMDisposeMessage(*error);
                anyhow::bail!("{rust_error}");
            }
        }
    }

    pub fn create_target_machine(
        &self,
        triple: &str,
        cpu: &str,
        features: &str,
    ) -> TargetMachine {
        unsafe {
            // fixme some of these should be params
            let level = LLVMCodeGenOptLevel::LLVMCodeGenLevelNone;
            let reloc = LLVMRelocMode::LLVMRelocPIC;
            let code_model = LLVMCodeModel::LLVMCodeModelDefault;

            let machine = LLVMCreateTargetMachine(
                self.0,
                triple.cstr(),
                cpu.cstr(),
                features.cstr(),
                level,
                reloc,
                code_model,
            );

            TargetMachine(machine)
        }
    }
}

pub struct TargetMachine(LLVMTargetMachineRef);

impl Drop for TargetMachine {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeTargetMachine(self.0);
        }
    }
}

impl TargetMachine {
    pub fn emit_to_obj_file(
        &self,
        module: &Module,
        filename: &str,
    ) -> anyhow::Result<()> {
        unsafe {
            // nb: llvm-sys seemingly-incorrectly wants
            // a mutable c-string for the filename.
            let filename = CString::new(filename.to_string()).expect("interior nul byte");
            let mut filename = filename.into_bytes_with_nul();
            let filename: *mut u8 = filename.as_mut_ptr();
            let filename = filename as *mut libc::c_char;

            let error: &mut *mut libc::c_char = &mut ptr::null_mut();
            let result = LLVMTargetMachineEmitToFile(
                self.0,
                module.0,
                filename,
                LLVMCodeGenFileType::LLVMObjectFile,
                error,
            );

            if result == 0  {
                assert!((*error).is_null());
                Ok(())
            } else {
                assert!(!(*error).is_null());
                let rust_error = CStr::from_ptr(*error)
                    .to_str()?.to_string();
                LLVMDisposeMessage(*error);
                anyhow::bail!("{rust_error}");
            }
        }
    }
}
