// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! Translation from stackless Move bytecode to LLVM.
//!
//! Move is a stack machine and challenging to translate directly to LLVM. The
//! `move_model` crate provides a translation of Move bytecode to "stackless
//! bytecode", which is well-suited to further translation to LLVM.
//!
//! The structure of this module naturally mirrors both the Move model and LLVM
//! sys, with a `GlobalContext` holding the Move `GlobalEnv` and the LLVM
//! `Context`. Modules are translated through a `ModuleContext`, and functions a
//! `FunctionContext`, each of which may accessed cached information from the
//! parent context, all linked through lifetimes.
//!
//!
//! # Lifetimes
//!
//! This module attempts to keep distinct lifetimes distinct to avoid
//! a situation where they have be disentangled later. The structures
//! contain two named lifetimes:
//!
//! - `'mm` - the lifetime of types stored inside the `move_model` `GlobalEnv`
//! - `'up` - reference up the callstack to the higher-level context struct
//!
//! When constructing a new context the local lifetime that becomes `'up`
//! is named `'this`.
//!
//! In general though this compiler does not need to be efficient at compile time -
//! we can clone things when it makes managing lifetimes easier.

use crate::stackless::{extensions::*, llvm};
use llvm_sys::prelude::LLVMValueRef;
use move_model::{ast as mast, model as mm, ty as mty};
use move_stackless_bytecode::{
    stackless_bytecode as sbc, stackless_bytecode_generator::StacklessBytecodeGenerator,
};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Copy, Clone)]
pub enum Target {
    Solana,
}

impl Target {
    fn triple(&self) -> &'static str {
        match self {
            Target::Solana => "sbf-solana-solana",
        }
    }

    fn llvm_cpu(&self) -> &'static str {
        match self {
            Target::Solana => "generic",
        }
    }

    fn llvm_features(&self) -> &'static str {
        match self {
            Target::Solana => "+solana",
        }
    }

    fn initialize_llvm(&self) {
        match self {
            Target::Solana => {
                llvm::initialize_sbf();
            }
        }
    }
}

pub struct GlobalContext<'up> {
    env: &'up mm::GlobalEnv,
    llvm_cx: llvm::Context,
    target: Target,
}

impl<'up> GlobalContext<'up> {
    pub fn new(env: &'up mm::GlobalEnv, target: Target) -> GlobalContext {
        target.initialize_llvm();

        GlobalContext {
            env,
            llvm_cx: llvm::Context::new(),
            target,
        }
    }

    pub fn create_module_context<'this>(
        &'this self,
        id: mm::ModuleId,
    ) -> ModuleContext<'up, 'this> {
        let env = self.env.get_module(id);
        let name = env.llvm_module_name();
        ModuleContext {
            env,
            llvm_cx: &self.llvm_cx,
            llvm_module: self.llvm_cx.create_module(&name),
            llvm_builder: self.llvm_cx.create_builder(),
            fn_decls: BTreeMap::new(),
            _target: self.target,
        }
    }
}

pub struct ModuleContext<'mm, 'up> {
    env: mm::ModuleEnv<'mm>,
    llvm_cx: &'up llvm::Context,
    llvm_module: llvm::Module,
    llvm_builder: llvm::Builder,
    /// A map of move function id's to llvm function ids
    ///
    /// All non-generic functions that might be called are declared prior to function translation.
    /// This includes local functions and dependencies.
    fn_decls: BTreeMap<mm::QualifiedId<mm::FunId>, llvm::Function>,
    _target: Target,
}

impl<'mm, 'up> ModuleContext<'mm, 'up> {
    pub fn translate(mut self) -> llvm::Module {
        let filename = self.env.get_source_path().to_str().expect("utf-8");
        self.llvm_module.set_source_file_name(filename);

        self.declare_functions();

        for fn_env in self.env.get_functions() {
            let fn_cx = self.create_fn_context(fn_env);
            fn_cx.translate();
        }

        self.llvm_module.verify();

        self.llvm_module
    }

    /// Create LLVM function decls for all local functions and
    /// all extern functions that might be called.
    ///
    /// Non-generic functions only. Generic handling todo.
    fn declare_functions(&mut self) {
        let mod_env = self.env.clone(); // fixme bad clone

        let mut foreign_fns = BTreeSet::new();

        for fn_env in mod_env.get_functions() {
            self.declare_function(&fn_env);

            for called_fn in fn_env.get_called_functions() {
                let is_foreign_mod = called_fn.module_id != mod_env.get_id();
                if is_foreign_mod {
                    foreign_fns.insert(called_fn);
                }
            }
        }

        for fn_id in foreign_fns {
            let global_env = &self.env.env;
            let called_fn_env = global_env.get_function(fn_id);
            self.declare_function(&called_fn_env);
        }
    }

    fn declare_function(&mut self, fn_env: &mm::FunctionEnv) {
        let fn_data = StacklessBytecodeGenerator::new(&fn_env).generate_function();

        let ll_fn = {
            let ll_fnty = {
                let ll_rty = match fn_data.return_types.len() {
                    0 => self.llvm_cx.void_type(),
                    1 => self.llvm_type(&fn_data.return_types[0]),
                    _ => {
                        todo!()
                    }
                };

                let ll_parm_tys = fn_env
                    .get_parameter_types()
                    .iter()
                    .map(|mty| self.llvm_type(mty))
                    .collect::<Vec<_>>();

                llvm::FunctionType::new(ll_rty, &ll_parm_tys)
            };

            self.llvm_module
                .add_function(&fn_env.llvm_symbol_name(), ll_fnty)
        };

        let id = fn_env.get_qualified_id();
        self.fn_decls.insert(id, ll_fn);
    }

    fn llvm_type(&self, mty: &mty::Type) -> llvm::Type {
        use mty::{PrimitiveType, Type};

        match mty {
            Type::Primitive(PrimitiveType::Bool) => self.llvm_cx.int1_type(),
            Type::Primitive(PrimitiveType::U8) => self.llvm_cx.int8_type(),
            Type::Primitive(PrimitiveType::U32) => self.llvm_cx.int32_type(),
            Type::Primitive(PrimitiveType::U64) => self.llvm_cx.int64_type(),
            Type::Primitive(PrimitiveType::U128) => self.llvm_cx.int128_type(),
            Type::Reference(_, referent_mty) => {
                let referent_llty = self.llvm_type(referent_mty);
                let llty = referent_llty.ptr_type();
                llty
            }
            _ => {
                todo!("{mty:?}")
            }
        }
    }
    // Primitive type :: number width
    fn get_bitwidth(&self, mty: &mty::Type) -> u64 {
        use mty::{PrimitiveType, Type};

        match mty {
            Type::Primitive(PrimitiveType::Bool) => 1,
            Type::Primitive(PrimitiveType::U8) => 8,
            Type::Primitive(PrimitiveType::U32) => 32,
            Type::Primitive(PrimitiveType::U64) => 64,
            Type::Primitive(PrimitiveType::U128) => 128,
            Type::Primitive(PrimitiveType::U256) => 256,
            _ => {
                todo!("{mty:?}")
            }
        }
    }
    fn create_fn_context<'this>(
        &'this self,
        fn_env: mm::FunctionEnv<'mm>,
    ) -> FunctionContext<'mm, 'this> {
        let locals = Vec::with_capacity(fn_env.get_local_count());
        FunctionContext {
            env: fn_env,
            llvm_cx: &self.llvm_cx,
            llvm_module: &self.llvm_module,
            llvm_builder: &self.llvm_builder,
            llvm_type: Box::new(|ty| self.llvm_type(ty)),
            get_bitwidth: Box::new(|ty| self.get_bitwidth(ty)),
            fn_decls: &self.fn_decls,
            label_blocks: BTreeMap::new(),
            locals,
        }
    }
}

struct FunctionContext<'mm, 'up> {
    env: mm::FunctionEnv<'mm>,
    llvm_cx: &'up llvm::Context,
    llvm_module: &'up llvm::Module,
    llvm_builder: &'up llvm::Builder,
    /// A function to get llvm types from move types.
    ///
    /// The implementation lives on ModuleContext, and this
    /// ugly declaration exists to avoid passing the entire module
    /// context to the function context. It may end up not worth
    /// the effort.
    llvm_type: Box<dyn (Fn(&mty::Type) -> llvm::Type) + 'up>,
    get_bitwidth: Box<dyn (Fn(&mty::Type) -> u64) + 'up>,
    fn_decls: &'up BTreeMap<mm::QualifiedId<mm::FunId>, llvm::Function>,
    label_blocks: BTreeMap<sbc::Label, llvm::BasicBlock>,
    /// Corresponds to FunctionData:local_types
    locals: Vec<Local>,
}

/// A stackless move local variable, translated as an llvm alloca
struct Local {
    mty: mty::Type,
    llty: llvm::Type,
    llval: llvm::Alloca,
}

impl<'mm, 'up> FunctionContext<'mm, 'up> {
    fn translate(mut self) {
        let fn_data = StacklessBytecodeGenerator::new(&self.env).generate_function();

        dbg!(&fn_data);

        let ll_fn = &self.fn_decls[&self.env.get_qualified_id()];

        // Create basic blocks and position builder at entry block
        {
            let entry_block = ll_fn.append_basic_block("entry");

            // Create basic blocks for move labels
            for instr in &fn_data.code {
                match instr {
                    sbc::Bytecode::Label(_, label) => {
                        let name = format!("bb_{}", label.as_usize());
                        let llbb = ll_fn.append_basic_block(&name);
                        self.label_blocks.insert(*label, llbb);
                    }
                    _ => {}
                }
            }

            self.llvm_builder.position_at_end(entry_block);
        }

        // Declare all the locals as allocas
        {
            for (i, mty) in fn_data.local_types.iter().enumerate() {
                let llty = self.llvm_type(mty);
                let name = format!("local_{}", i);
                let llval = self.llvm_builder.build_alloca(llty, &name);
                self.locals.push(Local {
                    mty: mty.clone(), // fixme bad clone
                    llty,
                    llval,
                });
            }
        }

        // Store params into locals
        {
            let param_count = self.env.get_parameter_count();
            let ll_params = (0..param_count).map(|i| ll_fn.get_param(i));

            for (ll_param, local) in ll_params.zip(self.locals.iter()) {
                self.llvm_builder
                    .store_param_to_alloca(ll_param, local.llval);
            }
        }

        // Translate instructions
        for instr in &fn_data.code {
            self.translate_instruction(instr);
        }

        ll_fn.verify();
    }

    fn llvm_type(&self, mty: &mty::Type) -> llvm::Type {
        (self.llvm_type)(mty)
    }

    fn get_bitwidth(&self, mty: &mty::Type) -> u64 {
        (self.get_bitwidth)(mty)
    }

    fn translate_instruction(&self, instr: &sbc::Bytecode) {
        match instr {
            sbc::Bytecode::Assign(_, dst, src, sbc::AssignKind::Move) => {
                let mty = &self.locals[*dst].mty;
                let llty = self.locals[*dst].llty;
                let dst_llval = self.locals[*dst].llval;
                let src_llval = self.locals[*src].llval;
                match mty {
                    mty::Type::Primitive(
                        mty::PrimitiveType::Bool
                        | mty::PrimitiveType::U8
                        | mty::PrimitiveType::U32
                        | mty::PrimitiveType::U64
                        | mty::PrimitiveType::U128,
                    ) => {
                        self.llvm_builder.load_store(llty, src_llval, dst_llval);
                    }
                    mty::Type::Reference(_, _) => {
                        self.llvm_builder.load_store(llty, src_llval, dst_llval);
                    }
                    _ => todo!(),
                }
            }
            sbc::Bytecode::Assign(_, dst, src, sbc::AssignKind::Copy) => {
                let mty = &self.locals[*dst].mty;
                let llty = self.locals[*dst].llty;
                let dst_llval = self.locals[*dst].llval;
                let src_llval = self.locals[*src].llval;
                match mty {
                    mty::Type::Primitive(
                        mty::PrimitiveType::Bool
                        | mty::PrimitiveType::U8
                        | mty::PrimitiveType::U32
                        | mty::PrimitiveType::U64
                        | mty::PrimitiveType::U128,
                    ) => {
                        self.llvm_builder.load_store(llty, src_llval, dst_llval);
                    }
                    _ => todo!(),
                }
            }
            sbc::Bytecode::Assign(_, dst, src, sbc::AssignKind::Store) => {
                let mty = &self.locals[*dst].mty;
                let llty = self.locals[*dst].llty;
                let dst_llval = self.locals[*dst].llval;
                let src_llval = self.locals[*src].llval;
                match mty {
                    mty::Type::Primitive(
                        mty::PrimitiveType::Bool
                        | mty::PrimitiveType::U8
                        | mty::PrimitiveType::U32
                        | mty::PrimitiveType::U64
                        | mty::PrimitiveType::U128,
                    ) => {
                        self.llvm_builder.load_store(llty, src_llval, dst_llval);
                    }
                    _ => todo!(),
                }
            }
            sbc::Bytecode::Call(_, dst, op, src, None) => {
                self.translate_call(dst, op, src);
            }
            sbc::Bytecode::Ret(_, vals) => match vals.len() {
                0 => {
                    self.llvm_builder.build_return_void();
                }
                1 => {
                    let idx = vals[0];
                    let llval = self.locals[idx].llval;
                    let llty = self.locals[idx].llty;
                    self.llvm_builder.load_return(llty, llval);
                }
                _ => todo!(),
            },
            sbc::Bytecode::Load(_, idx, val) => {
                let local_llval = self.locals[*idx].llval;
                let const_llval = self.constant(val);
                self.llvm_builder.store_const(const_llval, local_llval);
            }
            sbc::Bytecode::Branch(_, label0, label1, cnd_idx) => {
                let cnd_llval = self.locals[*cnd_idx].llval;
                let cnd_llty = self.locals[*cnd_idx].llty;
                let bb0 = self.label_blocks[label0];
                let bb1 = self.label_blocks[label1];
                self.llvm_builder
                    .load_cond_br(cnd_llty, cnd_llval, bb0, bb1);
            }
            sbc::Bytecode::Jump(_, label) => {
                let llbb = self.label_blocks[label];
                self.llvm_builder.build_br(llbb);
            }
            sbc::Bytecode::Label(_, label) => {
                let llbb = self.label_blocks[label];
                self.llvm_builder.position_at_end(llbb);
            }
            sbc::Bytecode::Abort(_, local) => {
                self.emit_rtcall(RtCall::Abort(*local));
            }
            _ => {
                todo!("{instr:?}")
            }
        }
    }

    fn load_reg(&self, src_idx: mast::TempIndex, name: &str) -> LLVMValueRef {
        let src_llval = self.locals[src_idx].llval;
        let src_ty = self.locals[src_idx].llty;
        self.llvm_builder.build_load(src_ty, src_llval, name)
    }

    fn store_reg(&self, dst_idx: mast::TempIndex, dst_reg: LLVMValueRef) {
        let dst_llval = self.locals[dst_idx].llval;
        self.llvm_builder.build_store(dst_reg, dst_llval);
    }

    fn translate_arithm_impl(
        &self,
        dst: &[mast::TempIndex],
        src: &[mast::TempIndex],
        name: &str,
        op: llvm_sys::LLVMOpcode,
    ) {
        assert_eq!(dst.len(), 1);
        assert_eq!(src.len(), 2);
        let src0_reg = self.load_reg(src[0], &format!("{name}_src_0"));
        let src1_reg = self.load_reg(src[1], &format!("{name}_src_1"));
        let dst_reg = self
            .llvm_builder
            .build_binop(op, src0_reg, src1_reg, &format!("{name}_dst"));
        self.store_reg(dst[0], dst_reg);
    }

    fn translate_call(
        &self,
        dst: &[mast::TempIndex],
        op: &sbc::Operation,
        src: &[mast::TempIndex],
    ) {
        use sbc::Operation;
        match op {
            Operation::Function(mod_id, fun_id, types) => {
                self.translate_fun_call(*mod_id, *fun_id, types, dst, src);
            }
            Operation::BorrowLoc => {
                assert_eq!(src.len(), 1);
                assert_eq!(dst.len(), 1);
                let src_idx = src[0];
                let dst_idx = dst[0];
                let src_llval = self.locals[src_idx].llval;
                let dst_llval = self.locals[dst_idx].llval;
                self.llvm_builder.ref_store(src_llval, dst_llval);
            }
            Operation::Destroy => {
                assert!(dst.is_empty());
                assert_eq!(src.len(), 1);
                let idx = src[0];
                let mty = &self.locals[idx].mty;
                match mty {
                    mty::Type::Primitive(_) => ( /* nop */ ),
                    _ => todo!(),
                }
            }
            Operation::ReadRef => {
                assert_eq!(src.len(), 1);
                assert_eq!(dst.len(), 1);
                let src_idx = src[0];
                let dst_idx = dst[0];
                let dst_llty = self.locals[dst_idx].llty;
                let src_llval = self.locals[src_idx].llval;
                let dst_llval = self.locals[dst_idx].llval;
                self.llvm_builder
                    .load_deref_store(dst_llty, src_llval, dst_llval);
            }
            Operation::WriteRef => {
                // nb: both operands are from the "src" vector.
                // "src" and "dst" might be the wrong names, maybe
                // "ops" and "returns", since these operations are all
                // expressed in stackless bytecode as function calls.
                assert_eq!(src.len(), 2);
                assert_eq!(dst.len(), 0);
                let src_idx = src[1];
                let dst_idx = src[0];
                let src_llty = self.locals[src_idx].llty;
                let src_llval = self.locals[src_idx].llval;
                let dst_llval = self.locals[dst_idx].llval;
                self.llvm_builder
                    .load_store_ref(src_llty, src_llval, dst_llval);
            }
            Operation::FreezeRef => {
                assert_eq!(dst.len(), 1);
                assert_eq!(src.len(), 1);
                let src_idx = src[0];
                let dst_idx = dst[0];
                let src_llty = self.locals[src_idx].llty;
                let src_llval = self.locals[src_idx].llval;
                let dst_llval = self.locals[dst_idx].llval;
                self.llvm_builder.load_store(src_llty, src_llval, dst_llval);
            }
            Operation::Add => {
                self.translate_arithm_impl(dst, src, "add", llvm_sys::LLVMOpcode::LLVMAdd);
            }
            Operation::Sub => {
                self.translate_arithm_impl(dst, src, "sub", llvm_sys::LLVMOpcode::LLVMSub);
            }
            Operation::Mul => {
                self.translate_arithm_impl(dst, src, "mul", llvm_sys::LLVMOpcode::LLVMMul);
            }
            Operation::Div => {
                self.translate_arithm_impl(dst, src, "div", llvm_sys::LLVMOpcode::LLVMSDiv);
            }
            Operation::Mod => {
                self.translate_arithm_impl(dst, src, "mod", llvm_sys::LLVMOpcode::LLVMSRem);
            }
            Operation::BitOr => {
                self.translate_arithm_impl(dst, src, "or", llvm_sys::LLVMOpcode::LLVMOr);
            }
            Operation::BitAnd => {
                self.translate_arithm_impl(dst, src, "and", llvm_sys::LLVMOpcode::LLVMAnd);
            }
            Operation::Xor => {
                self.translate_arithm_impl(dst, src, "xor", llvm_sys::LLVMOpcode::LLVMXor);
            }
            Operation::Shl => {
                self.translate_arithm_impl(dst, src, "shl", llvm_sys::LLVMOpcode::LLVMShl);
            }
            Operation::Shr => {
                // fixme is this an arithmetic or logical shift?
                self.translate_arithm_impl(dst, src, "shr", llvm_sys::LLVMOpcode::LLVMAShr);
            }
            Operation::Lt => {
                assert_eq!(dst.len(), 1);
                assert_eq!(src.len(), 2);
                let src0_reg = self.load_reg(src[0], "lt_src_0");
                let src1_reg = self.load_reg(src[1], "lt_src_1");
                // FIXME: All comparisons are unsigned. Is this correct?
                let dst_reg = self.llvm_builder.build_compare(
                    llvm::LLVMIntPredicate::LLVMIntULT,
                    src0_reg,
                    src1_reg,
                    "lt_dst",
                );
                self.store_reg(dst[0], dst_reg);
            }
            Operation::Gt => {
                assert_eq!(dst.len(), 1);
                assert_eq!(src.len(), 2);
                let src0_reg = self.load_reg(src[0], "gt_src_0");
                let src1_reg = self.load_reg(src[1], "gt_src_1");
                // FIXME: All comparisons are unsigned. Is this correct?
                let dst_reg = self.llvm_builder.build_compare(
                    llvm::LLVMIntPredicate::LLVMIntUGT,
                    src0_reg,
                    src1_reg,
                    "gt_dst",
                );
                self.store_reg(dst[0], dst_reg);
            }
            Operation::Le => {
                assert_eq!(dst.len(), 1);
                assert_eq!(src.len(), 2);
                let src0_reg = self.load_reg(src[0], "le_src_0");
                let src1_reg = self.load_reg(src[1], "le_src_1");
                // FIXME: All comparisons are unsigned. Is this correct?
                let dst_reg = self.llvm_builder.build_compare(
                    llvm::LLVMIntPredicate::LLVMIntULE,
                    src0_reg,
                    src1_reg,
                    "le_dst",
                );
                self.store_reg(dst[0], dst_reg);
            }
            Operation::Ge => {
                assert_eq!(dst.len(), 1);
                assert_eq!(src.len(), 2);
                let src0_reg = self.load_reg(src[0], "ge_src_0");
                let src1_reg = self.load_reg(src[1], "ge_src_1");
                // FIXME: All comparisons are unsigned. Is this correct?
                let dst_reg = self.llvm_builder.build_compare(
                    llvm::LLVMIntPredicate::LLVMIntUGE,
                    src0_reg,
                    src1_reg,
                    "ge_dst",
                );
                self.store_reg(dst[0], dst_reg);
            }
            Operation::Eq => {
                assert_eq!(dst.len(), 1);
                assert_eq!(src.len(), 2);
                let src0_reg = self.load_reg(src[0], "eq_src_0");
                let src1_reg = self.load_reg(src[1], "eq_src_1");
                // FIXME: All comparisons are unsigned. Is this correct?
                let dst_reg = self.llvm_builder.build_compare(
                    llvm::LLVMIntPredicate::LLVMIntEQ,
                    src0_reg,
                    src1_reg,
                    "eq_dst",
                );
                self.store_reg(dst[0], dst_reg);
            }
            Operation::Neq => {
                assert_eq!(dst.len(), 1);
                assert_eq!(src.len(), 2);
                let src0_reg = self.load_reg(src[0], "ne_src_0");
                let src1_reg = self.load_reg(src[1], "ne_src_1");
                // FIXME: All comparisons are unsigned. Is this correct?
                let dst_reg = self.llvm_builder.build_compare(
                    llvm::LLVMIntPredicate::LLVMIntNE,
                    src0_reg,
                    src1_reg,
                    "ne_dst",
                );
                self.store_reg(dst[0], dst_reg);
            }
            Operation::CastU32 => {
                assert_eq!(dst.len(), 1);
                assert_eq!(src.len(), 1);
                let src_idx = src[0];
                let src_mty = &self.locals[src_idx].mty;
                assert!(src_mty.is_number());
                let src_width = self.get_bitwidth(src_mty);
                let src_reg = self.load_reg(src_idx, "cast_src");
                let dst_reg = if src_width < 32 {
                    // Widen
                    self.llvm_builder
                        .build_zext(src_reg, self.llvm_type(src_mty).0, "zext_dst")
                } else {
                    // Truncate
                    self.llvm_builder
                        .build_trunc(src_reg, self.llvm_type(src_mty).0, "trunc_dst")
                };
                self.store_reg(dst[0], dst_reg);
            }
            Operation::CastU8 => {
                assert_eq!(dst.len(), 1);
                assert_eq!(src.len(), 1);
                let src_idx = src[0];
                let src_mty = &self.locals[src_idx].mty;
                assert!(src_mty.is_number());
                let src_width = self.get_bitwidth(src_mty);
                let src_reg = self.load_reg(src_idx, "cast_src");
                let dst_reg = if src_width < 8 {
                    // Widen
                    self.llvm_builder
                        .build_zext(src_reg, self.llvm_type(src_mty).0, "zext_dst")
                } else {
                    // Truncate
                    self.llvm_builder
                        .build_trunc(src_reg, self.llvm_type(src_mty).0, "trunc_dst")
                };
                self.store_reg(dst[0], dst_reg);
            }
            Operation::CastU64 => {
                assert_eq!(dst.len(), 1);
                assert_eq!(src.len(), 1);
                let src_idx = src[0];
                let src_mty = &self.locals[src_idx].mty;
                assert!(src_mty.is_number());
                let src_width = self.get_bitwidth(src_mty);
                let src_reg = self.load_reg(src_idx, "cast_src");
                let dst_reg = if src_width < 64 {
                    // Widen
                    self.llvm_builder
                        .build_zext(src_reg, self.llvm_type(src_mty).0, "zext_dst")
                } else {
                    // Truncate
                    self.llvm_builder
                        .build_trunc(src_reg, self.llvm_type(src_mty).0, "trunc_dst")
                };
                self.store_reg(dst[0], dst_reg);
            }
            _ => todo!("{op:?}"),
        }
    }

    fn translate_fun_call(
        &self,
        mod_id: mm::ModuleId,
        fun_id: mm::FunId,
        types: &[mty::Type],
        dst: &[mast::TempIndex],
        src: &[mast::TempIndex],
    ) {
        dbg!((mod_id, fun_id, types, dst, src));

        let dst_locals = dst.iter().map(|i| &self.locals[*i]).collect::<Vec<_>>();
        let src_locals = src.iter().map(|i| &self.locals[*i]).collect::<Vec<_>>();

        let ll_fn = self.fn_decls[&fun_id.qualified(mod_id)];

        if dst_locals.len() > 1 {
            todo!()
        }

        let dst = dst_locals.get(0);

        match dst {
            None => {
                let src = src_locals
                    .iter()
                    .map(|l| (l.llty, l.llval))
                    .collect::<Vec<_>>();
                self.llvm_builder.load_call(ll_fn, &src);
            }
            Some(dst) => {
                let dst = (dst.llty, dst.llval);
                let src = src_locals
                    .iter()
                    .map(|l| (l.llty, l.llval))
                    .collect::<Vec<_>>();
                self.llvm_builder.load_call_store(ll_fn, &src, dst);
            }
        }
    }

    fn constant(&self, mc: &sbc::Constant) -> llvm::Constant {
        use sbc::Constant;
        match mc {
            Constant::U8(val) => {
                let llty = self.llvm_cx.int8_type();
                llvm::Constant::int(llty, *val as u64)
            }
            Constant::U32(val) => {
                let llty = self.llvm_cx.int32_type();
                llvm::Constant::int(llty, *val as u64)
            }
            Constant::U64(val) => {
                let llty = self.llvm_cx.int64_type();
                llvm::Constant::int(llty, *val)
            }
            Constant::U128(val) => {
                let llty = self.llvm_cx.int128_type();
                llvm::Constant::int128(llty, *val)
            }
            _ => todo!(),
        }
    }

    fn emit_rtcall(&self, rtcall: RtCall) {
        match &rtcall {
            RtCall::Abort(local_idx) => {
                let llfn = self.get_runtime_function(&rtcall);
                let local_llval = self.locals[*local_idx].llval;
                let local_llty = self.locals[*local_idx].llty;
                self.llvm_builder
                    .load_call(llfn, &[(local_llty, local_llval)]);
                self.llvm_builder.build_unreachable();
            }
        }
    }

    fn get_runtime_function(&self, rtcall: &RtCall) -> llvm::Function {
        let name = match rtcall {
            RtCall::Abort(..) => "abort",
        };
        let name = format!("move_rt_{name}");
        let llfn = self.llvm_module.get_named_function(&name);
        if let Some(llfn) = llfn {
            llfn
        } else {
            let (llty, attrs) = match rtcall {
                RtCall::Abort(..) => {
                    let ret_ty = self.llvm_cx.void_type();
                    let param_tys = &[self.llvm_cx.int64_type()];
                    let llty = llvm::FunctionType::new(ret_ty, param_tys);
                    let attrs = vec![llvm::AttributeKind::NoReturn];
                    (llty, attrs)
                }
            };

            let llfn = self
                .llvm_module
                .add_function_with_attrs(&name, llty, &attrs);
            llfn
        }
    }
}

pub enum RtCall {
    Abort(mast::TempIndex),
}

/// Compile the module to object file.
///
/// This takes the module by value because it would otherwise have
/// side effects, mutating target-specific properties.
pub fn write_object_file(llmod: llvm::Module, target: Target, outpath: &str) -> anyhow::Result<()> {
    let lltarget = llvm::Target::from_triple(target.triple())?;
    let llmachine =
        lltarget.create_target_machine(target.triple(), target.llvm_cpu(), target.llvm_features());

    llmod.set_target(target.triple());
    llmod.set_data_layout(&llmachine);

    llmod.verify();

    llmachine.emit_to_obj_file(&llmod, outpath)?;

    Ok(())
}
