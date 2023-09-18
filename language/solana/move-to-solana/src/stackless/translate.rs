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

use crate::{
    options::Options,
    stackless::{
        entrypoint::EntrypointGenerator, extensions::*, llvm, module_context::ModuleContext,
        rttydesc::RttyContext,
    },
};
use log::debug;
use move_core_types::{account_address, u256::U256, vm_status::StatusCode::ARITHMETIC_ERROR};
use move_model::{ast as mast, model as mm, ty as mty};
use move_stackless_bytecode::{
    function_target::FunctionData, stackless_bytecode as sbc,
    stackless_bytecode_generator::StacklessBytecodeGenerator,
    stackless_control_flow_graph::generate_cfg_in_dot_format,
};
use num::BigUint;
use std::collections::BTreeMap;

#[derive(Copy, Clone)]
pub enum TargetPlatform {
    Solana,
}

impl TargetPlatform {
    pub fn triple(&self) -> &'static str {
        match self {
            TargetPlatform::Solana => "sbf-solana-solana",
        }
    }

    pub fn llvm_cpu(&self) -> &'static str {
        match self {
            TargetPlatform::Solana => "generic",
        }
    }

    pub fn llvm_features(&self) -> &'static str {
        match self {
            TargetPlatform::Solana => "",
        }
    }

    pub fn initialize_llvm(&self) {
        match self {
            TargetPlatform::Solana => {
                llvm::initialize_sbf();
            }
        }
    }
}

pub struct GlobalContext<'up> {
    pub env: &'up mm::GlobalEnv,
    pub llvm_cx: llvm::Context,
    target: TargetPlatform,
    target_machine: &'up llvm::TargetMachine,
}

impl<'up> GlobalContext<'up> {
    pub fn new(
        env: &'up mm::GlobalEnv,
        target: TargetPlatform,
        target_machine: &'up llvm::TargetMachine,
    ) -> GlobalContext<'up> {
        // Sanity/consistency check that the world was built with the target platform's account
        // address size. The various Move components we depend on, this compiler, and the native
        // runtime must all agree on the account length, otherwise bizarre behavior occurs.
        //
        // Now ideally we would just reference move_native::target_defs::ACCOUNT_ADDRESS_LENGTH
        // instead of hardcoding 32 below. Unfortunately, that is not currently possible because
        // move-native is built two different ways. For the runtime scenario, it is built with
        // the "solana" feature by the platform tools and therefore gets the proper target_defs
        // (e.g., account address length).
        //
        // On the other hand, when it is built for move-to-solana, it uses the Move-blessed
        // Rust version. That would ordinarily be fine except that we can't enable feature "solana"
        // with that toolchain (and recall, we need feature "solana" to get the proper target_defs
        // compiled in). The move-native crate is no_std, so it interferes with std on the compiler
        // build (e.g,, duplicate panic_impl). Also, in the "solana" config, the crate requires
        // feature(default_alloc_error_handler) which is rejected by the Move-blessed Rust.
        //
        // As near as I can tell, it's a catch-22 and will require a bit of refactoring in
        // move-native. Since we need one simple constant, I've avoided that rat's nest and simply
        // test for feature "solana" here. Needless to say, the compiler-build of move-native has
        // been getting non-Solana target_defs all along.
        #[cfg(feature = "solana")]
        assert!(account_address::AccountAddress::ZERO.len() == 32);

        debug!(target: "globalenv", "{:#?}", env);

        GlobalContext {
            env,
            llvm_cx: llvm::Context::new(),
            target,
            target_machine,
        }
    }

    pub fn create_module_context<'this: 'up>(
        &'this self,
        id: mm::ModuleId,
        llmod: &'this llvm::Module,
        entrypoint_generator: &'this EntrypointGenerator<'this, 'up>,
        options: &'this Options,
    ) -> ModuleContext<'up, 'this> {
        let rtty_cx = RttyContext::new(self.env, &self.llvm_cx, llmod);
        ModuleContext {
            env: self.env.get_module(id),
            entrypoint_generator,
            llvm_cx: &self.llvm_cx,
            llvm_module: llmod,
            llvm_builder: self.llvm_cx.create_builder(),
            fn_decls: BTreeMap::new(),
            expanded_functions: Vec::new(),
            target: self.target,
            target_machine: self.target_machine,
            options,
            rtty_cx,
        }
    }
}

pub struct FunctionContext<'mm, 'up> {
    pub env: mm::FunctionEnv<'mm>,
    pub module_cx: &'up ModuleContext<'mm, 'up>,
    pub label_blocks: BTreeMap<sbc::Label, llvm::BasicBlock>,
    /// Corresponds to FunctionData:local_types
    pub locals: Vec<Local>,
    pub type_params: &'mm [mty::Type],
}

/// A stackless move local variable, translated as an llvm alloca
#[derive(Clone, Debug)]
pub struct Local {
    mty: mty::Type,
    llty: llvm::Type,
    llval: llvm::Alloca,
}

#[derive(Eq, PartialEq)]
pub enum EmitterFnKind {
    PreCheck,
    PostCheck,
}
type CheckEmitterFn<'mm, 'up> = (
    fn(&FunctionContext<'mm, 'up>, &[Option<(mast::TempIndex, llvm::AnyValue)>]) -> (),
    EmitterFnKind,
);

impl<'mm, 'up> FunctionContext<'mm, 'up> {
    fn get_global_env(&self) -> &'mm mm::GlobalEnv {
        self.env.module_env.env
    }

    pub fn translate(mut self) {
        let fn_data = StacklessBytecodeGenerator::new(&self.env).generate_function();
        let func_target =
            move_stackless_bytecode::function_target::FunctionTarget::new(&self.env, &fn_data);
        debug!(target: "sbc", "\n{}", func_target);

        // Write the control flow graph to a .dot file for viewing.
        let options = &self.module_cx.options;
        let action = (*options.gen_dot_cfg).to_owned();
        if action == "write" || action == "view" {
            let fname = &self.env.llvm_symbol_name(self.type_params);
            let dot_graph = generate_cfg_in_dot_format(&func_target);
            let graph_label = format!("digraph {{ label=\"Function: {}\"\n", fname);
            let dgraph2 = dot_graph.replacen("digraph {", &graph_label, 1);
            let output_path = (*options.dot_file_path).to_owned();
            let path_sep = match &*output_path {
                "" => "",
                _ => "/",
            };
            let dot_file = format!("{}{}{}_cfg.dot", output_path, path_sep, fname);
            std::fs::write(&dot_file, dgraph2).expect("generating dot file for CFG");
            // If requested by user, also invoke the xdot viewer.
            if action == "view" {
                std::process::Command::new("xdot")
                    .arg(dot_file)
                    .status()
                    .expect("failed to execute 'xdot'");
            }
        }

        let ll_fn = self
            .module_cx
            .lookup_move_fn_decl(self.env.get_qualified_inst_id(self.type_params.to_vec()));

        // Create basic blocks and position builder at entry block
        {
            let entry_block = ll_fn.append_basic_block("entry");

            // Create basic blocks for move labels
            for instr in &fn_data.code {
                if let sbc::Bytecode::Label(_, label) = instr {
                    let name = format!("bb_{}", label.as_usize());
                    let llbb = ll_fn.append_basic_block(&name);
                    self.label_blocks.insert(*label, llbb);
                }
            }

            self.module_cx.llvm_builder.position_at_end(entry_block);
        }

        // Collect some local names from various structure field references.
        let mut named_locals = BTreeMap::new();
        self.collect_local_names(&fn_data, &mut named_locals);

        // Declare all the locals as allocas
        {
            for (i, mty) in fn_data.local_types.iter().enumerate() {
                let llty = self.module_cx.to_llvm_type(mty, self.type_params).unwrap();
                let mut name = format!("local_{}", i);
                if let Some(s) = named_locals.get(&i) {
                    name = format!("local_{}__{}", i, s);
                }
                let llval = self.module_cx.llvm_builder.build_alloca(llty, &name);
                self.locals.push(Local {
                    mty: mty.instantiate(self.type_params),
                    llty,
                    llval,
                });
            }
        }

        // Store params into locals.
        //
        // To support testing of scripts that require signers, inject signers that were provided
        // on the command line into all script function arguments of type `signer`. Each `signer`
        // argument is assigned in order from the command line signer list.
        {
            let param_count = self.env.get_parameter_count();
            let ll_params = (0..param_count).map(|i| ll_fn.get_param(i));
            let is_script = self.env.module_env.is_script_module();
            let mut curr_signer = 0;

            for (ll_param, local) in ll_params.zip(self.locals.iter()) {
                if is_script && local.mty.is_signer() {
                    let signer = self
                        .module_cx
                        .options
                        .test_signers
                        .get(curr_signer)
                        .expect("too few `--signer` arguments provided")
                        .strip_prefix("0x");
                    curr_signer += 1;
                    let addr_val = BigUint::parse_bytes(signer.unwrap().as_bytes(), 16);
                    let c = self.constant(&sbc::Constant::Address(addr_val.unwrap()), None);
                    self.module_cx
                        .llvm_builder
                        .build_store(c.as_any_value(), local.llval);
                } else {
                    self.module_cx
                        .llvm_builder
                        .store_param_to_alloca(ll_param, local.llval);
                }
            }
        }

        // Translate instructions
        for instr in &fn_data.code {
            self.translate_instruction(instr);
        }

        ll_fn.verify();
    }

    fn translate_instruction(&mut self, instr: &sbc::Bytecode) {
        let builder = &self.module_cx.llvm_builder;
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
                        | mty::PrimitiveType::U16
                        | mty::PrimitiveType::U32
                        | mty::PrimitiveType::U64
                        | mty::PrimitiveType::U128
                        | mty::PrimitiveType::U256,
                    ) => {
                        builder.load_store(llty, src_llval, dst_llval);
                    }
                    mty::Type::Reference(_, _) => {
                        builder.load_store(llty, src_llval, dst_llval);
                    }
                    mty::Type::Struct(_, _, _) => {
                        // A move renders the source location inaccessible, but the storage is
                        // to be reused for the target. We simply replace the dest local's slot
                        // with the source, so that all later references to dest use the original
                        // space (the alloca) of the source. If the input IR is correct, then
                        // src local slot should not be accessed again.
                        self.locals[*dst] = self.locals[*src].clone();
                    }
                    mty::Type::Primitive(mty::PrimitiveType::Address) => {
                        self.locals[*dst] = self.locals[*src].clone();
                    }
                    mty::Type::Primitive(mty::PrimitiveType::Signer) => {
                        self.locals[*dst] = self.locals[*src].clone();
                    }
                    mty::Type::Vector(_) => {
                        self.module_cx
                            .llvm_builder
                            .load_store(llty, src_llval, dst_llval);
                    }
                    _ => todo!("{mty:?}"),
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
                        | mty::PrimitiveType::U16
                        | mty::PrimitiveType::U32
                        | mty::PrimitiveType::U64
                        | mty::PrimitiveType::U128
                        | mty::PrimitiveType::U256,
                    ) => {
                        builder.load_store(llty, src_llval, dst_llval);
                    }
                    mty::Type::Struct(_, _, _) => {
                        builder.load_store(llty, src_llval, dst_llval);
                    }
                    mty::Type::Primitive(mty::PrimitiveType::Address) => {
                        builder.load_store(llty, src_llval, dst_llval);
                    }
                    mty::Type::Vector(elt_mty) => {
                        self.module_cx.emit_rtcall_with_retval(RtCall::VecCopy(
                            dst_llval.as_any_value(),
                            src_llval.as_any_value(),
                            (**elt_mty).clone(),
                        ));
                    }
                    mty::Type::Reference(_, referent) => match **referent {
                        mty::Type::Struct(_, _, _) => {
                            builder.load_store(llty, src_llval, dst_llval);
                        }
                        _ => {
                            builder.load_store(llty, src_llval, dst_llval);
                        }
                    },
                    _ => todo!("{mty:?}"),
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
                        | mty::PrimitiveType::U16
                        | mty::PrimitiveType::U32
                        | mty::PrimitiveType::U64
                        | mty::PrimitiveType::U128
                        | mty::PrimitiveType::U256
                        | mty::PrimitiveType::Address
                        | mty::PrimitiveType::Signer,
                    ) => {
                        builder.load_store(llty, src_llval, dst_llval);
                    }
                    mty::Type::Reference(_, _) => {
                        builder.load_store(llty, src_llval, dst_llval);
                    }
                    mty::Type::Struct(_, _, _) => {
                        builder.load_store(llty, src_llval, dst_llval);
                    }
                    mty::Type::Vector(_) => {
                        self.module_cx
                            .llvm_builder
                            .load_store(llty, src_llval, dst_llval);
                    }
                    _ => todo!("{mty:#?}"),
                }
            }
            sbc::Bytecode::Call(_, dst, op, src, None) => {
                self.translate_call(dst, op, src);
            }
            sbc::Bytecode::Ret(_, vals) => match vals.len() {
                0 => {
                    builder.build_return_void();
                }
                1 => {
                    let idx = vals[0];
                    let llval = self.locals[idx].llval;
                    let llty = self.locals[idx].llty;
                    builder.load_return(llty, llval);
                }
                _ => {
                    // Multiple return values are wrapped in a struct.
                    let nvals = vals
                        .iter()
                        .map(|i| (self.locals[*i].llty, self.locals[*i].llval))
                        .collect::<Vec<_>>();

                    let ll_fn = self.module_cx.lookup_move_fn_decl(
                        self.env.get_qualified_inst_id(self.type_params.to_vec()),
                    );
                    let ret_ty = ll_fn.llvm_return_type();
                    builder.load_multi_return(ret_ty, &nvals);
                }
            },
            sbc::Bytecode::Load(_, idx, val) => {
                let local_llval = self.locals[*idx].llval;
                let const_llval = self.constant(val, Some(&self.locals[*idx].mty));
                builder.store_const(const_llval, local_llval);
            }
            sbc::Bytecode::Branch(_, label0, label1, cnd_idx) => {
                let cnd_llval = self.locals[*cnd_idx].llval;
                let cnd_llty = self.locals[*cnd_idx].llty;
                let bb0 = self.label_blocks[label0];
                let bb1 = self.label_blocks[label1];
                builder.load_cond_br(cnd_llty, cnd_llval, bb0, bb1);
            }
            sbc::Bytecode::Jump(_, label) => {
                let llbb = self.label_blocks[label];
                builder.build_br(llbb);
            }
            sbc::Bytecode::Label(_, label) => {
                let llbb = self.label_blocks[label];
                builder.position_at_end(llbb);
            }
            sbc::Bytecode::Abort(_, local) => {
                self.emit_rtcall(RtCall::Abort(*local));
            }
            sbc::Bytecode::Nop(_) => {}
            _ => {
                let tmp = &self.locals[0];
                builder.load(tmp.llval.as_any_value(), tmp.llty, "nop");
            }
        }
    }

    fn collect_local_names(
        &self,
        fn_data: &FunctionData,
        named_locals: &mut BTreeMap<mast::TempIndex, String>,
    ) {
        // Most locals in stackless bytecode are anonymous. We attempt here to collect and
        // assign meaningful local names when they can be easily ascertained from the bytecode.
        //
        // A common and easly analyzable case is a where a local is involved in a structure
        // operation. Direct examination of those operations yield field names which can
        // then be mapped to the corresponding local. Consider:
        //
        // 0: $t1 := move($t0)
        // 1: ($t2, $t3, $t4) := unpack Country::Country($t1)
        // ...
        //
        // Above, $tN are anonymous names representing each local. By examining the unpack, it
        // is straightforward to assign names to each local in turn from the structure referenced
        // in the unpack operation. Similarly for other structure operations:
        // - Locals targeted by Operation::Unpack.
        // - Locals consumed by Operation::Pack.
        // - Local extracted by Operation::BorrowField.
        //
        for instr in &fn_data.code {
            use sbc::Operation;
            if let sbc::Bytecode::Call(_, dst, op, src, None) = instr {
                match op {
                    Operation::BorrowField(mod_id, struct_id, _types, offset) => {
                        assert_eq!(src.len(), 1);
                        assert_eq!(dst.len(), 1);
                        let senv = self
                            .get_global_env()
                            .get_module(*mod_id)
                            .into_struct(*struct_id);
                        let tmp_idx = dst[0];
                        let fenv = senv.get_field_by_offset(*offset);
                        let name = fenv.get_name().display(senv.symbol_pool()).to_string();
                        named_locals.insert(tmp_idx, name);
                    }
                    Operation::Pack(mod_id, struct_id, _types) => {
                        let senv = self
                            .get_global_env()
                            .get_module(*mod_id)
                            .into_struct(*struct_id);
                        assert_eq!(dst.len(), 1);
                        assert_eq!(src.len(), senv.get_field_count());
                        for (offset, tmp_idx) in src.iter().enumerate() {
                            let fenv = senv.get_field_by_offset(offset);
                            let name = fenv.get_name().display(senv.symbol_pool()).to_string();
                            named_locals.insert(*tmp_idx, name);
                        }
                    }
                    Operation::Unpack(mod_id, struct_id, _types) => {
                        let senv = self
                            .get_global_env()
                            .get_module(*mod_id)
                            .into_struct(*struct_id);
                        assert_eq!(src.len(), 1);
                        assert_eq!(dst.len(), senv.get_field_count());
                        for (offset, tmp_idx) in dst.iter().enumerate() {
                            let fenv = senv.get_field_by_offset(offset);
                            let name = fenv.get_name().display(senv.symbol_pool()).to_string();
                            named_locals.insert(*tmp_idx, name);
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    fn load_reg(&self, src_idx: mast::TempIndex, name: &str) -> llvm::AnyValue {
        let src_llval = self.locals[src_idx].llval;
        let src_ty = self.locals[src_idx].llty;
        self.module_cx
            .llvm_builder
            .build_load(src_ty, src_llval, name)
    }

    fn store_reg(&self, dst_idx: mast::TempIndex, dst_reg: llvm::AnyValue) {
        let dst_llval = self.locals[dst_idx].llval;
        self.module_cx.llvm_builder.build_store(dst_reg, dst_llval);
    }

    fn emit_prepost_new_blocks_with_abort(&self, cond_reg: llvm::AnyValue) {
        // All pre- and post-condition emitters generate the same conditional structure.

        // Generate and insert the two new basic blocks.
        let builder = &self.module_cx.llvm_builder;
        let curr_bb = builder.get_insert_block();
        let parent_func = curr_bb.get_basic_block_parent();
        let then_bb = parent_func.insert_basic_block_after(curr_bb, "then_bb");
        let join_bb = parent_func.insert_basic_block_after(then_bb, "join_bb");

        // Generate the conditional branch and call to abort.
        builder.build_cond_br(cond_reg, then_bb, join_bb);
        builder.position_at_end(then_bb);

        ModuleContext::emit_rtcall_abort_raw(
            self.module_cx.llvm_cx,
            &self.module_cx.llvm_builder,
            self.module_cx.llvm_module,
            &self.module_cx.rtty_cx,
            ARITHMETIC_ERROR as u64,
        );
        builder.position_at_end(join_bb);
    }

    fn emit_precond_for_shift(
        &self,
        args: &[Option<(mast::TempIndex, llvm::AnyValue)>], // src0, src1, dst.
    ) {
        // Generate the following LLVM IR to pre-check that the shift count is in range.
        //
        // Note that u256 shift count is always legal today in Move since count is restricted
        // to u8-- don't generate test in that case.
        //
        //   ...
        //   %rangecond = icmp uge i8 %n_bits, srco_width{8/32/64/128}
        //   br i1 %rangecond, %then_bb, %join_bb
        // then_bb:
        //   call void @move_rt_abort(i64 ARITHMETIC_ERROR)
        //   unreachable
        // join_bb:
        //  ...
        //

        // Generate the range check compare.
        let src0 = args[0].unwrap();
        let src0_llty = &self.locals[src0.0].llty;
        let src0_width = src0_llty.get_int_type_width();
        if src0_width == 256 {
            return;
        }

        let src1 = args[1].unwrap();
        let src1_llty = &self.locals[src1.0].llty;
        assert!(src1_llty.get_int_type_width() == 8);
        let const_llval = llvm::Constant::int(*src1_llty, U256::from(src0_width));
        let cond_reg = self.module_cx.llvm_builder.build_compare(
            llvm::LLVMIntPredicate::LLVMIntUGE,
            src1.1,
            const_llval.as_any_value(),
            "rangecond",
        );

        self.emit_prepost_new_blocks_with_abort(cond_reg);
    }

    fn emit_postcond_for_add(
        &self,
        args: &[Option<(mast::TempIndex, llvm::AnyValue)>], // src0, src1, dst.
    ) {
        // Generate the following LLVM IR to check that unsigned addition did not overflow.
        // This is indicated when the unsigned sum is less than the first input.
        //   ...
        //   %ovfcond = icmp ult {i8/32/64/128} %add_dst, %add_src0
        //   br i1 %ovfcond, %then_bb, %join_bb
        // then_bb:
        //   call void @move_rt_abort(i64 ARITHMETIC_ERROR)
        //   unreachable
        // join_bb:
        //  ...
        //

        // Generate the overflow check compare.
        let src0 = args[0].unwrap();
        let dst = args[2].unwrap();
        let cond_reg = self.module_cx.llvm_builder.build_compare(
            llvm::LLVMIntPredicate::LLVMIntULT,
            dst.1,
            src0.1,
            "ovfcond",
        );

        self.emit_prepost_new_blocks_with_abort(cond_reg);
    }

    fn emit_postcond_for_sub(
        &self,
        args: &[Option<(mast::TempIndex, llvm::AnyValue)>], // src0, src1, dst.
    ) {
        // Generate the following LLVM IR to check that unsigned subtraction did not overflow.
        // This is indicated when the unsigned difference is greater than the first input.
        //   ...
        //   %ovfcond = icmp ugt {i8/32/64/128} %sub_dst, %sub_src0
        //   br i1 %ovfcond, %then_bb, %join_bb
        // then_bb:
        //   call void @move_rt_abort(i64 ARITHMETIC_ERROR)
        //   unreachable
        // join_bb:
        //  ...
        //

        // Generate the overflow check compare.
        let src0 = args[0].unwrap();
        let dst = args[2].unwrap();
        let cond_reg = self.module_cx.llvm_builder.build_compare(
            llvm::LLVMIntPredicate::LLVMIntUGT,
            dst.1,
            src0.1,
            "ovfcond",
        );

        self.emit_prepost_new_blocks_with_abort(cond_reg);
    }

    fn emit_postcond_for_mul(
        &self,
        args: &[Option<(mast::TempIndex, llvm::AnyValue)>], // src0, src1, dst.
    ) {
        // Generate the following LLVM IR to check that unsigned multiplication did not overflow.
        //   ...
        //   %mul_ovf = extractvalue {<prod_dst_ty>, i1} %res, 1
        //   br i1 %mul_ovf, %then_bb, %join_bb
        // then_bb:
        //   call void @move_rt_abort(i64 ARITHMETIC_ERROR)
        //   unreachable
        // join_bb:
        //  ...
        //

        let dst = args[2].unwrap();
        let cond_reg = self
            .module_cx
            .llvm_builder
            .build_extract_value(dst.1, 1, "mul_ovf");
        self.emit_prepost_new_blocks_with_abort(cond_reg);
    }

    fn emit_precond_for_div(
        &self,
        args: &[Option<(mast::TempIndex, llvm::AnyValue)>], // src0, src1, dst.
    ) {
        // Generate the following LLVM IR to check that the divisor is not zero.
        //   ...
        //   %zerocond = icmp eq {i8/32/64/128} %div_src1, 0
        //   br i1 %zerocond, %then_bb, %join_bb
        // then_bb:
        //   call void @move_rt_abort(i64 ARITHMETIC_ERROR)
        //   unreachable
        // join_bb:
        //  ...
        //

        // Generate the zero check compare.
        let src1 = args[1].unwrap();
        let src1_llty = &self.locals[src1.0].llty;
        let const_llval = llvm::Constant::int(*src1_llty, U256::zero());
        let cond_reg = self.module_cx.llvm_builder.build_compare(
            llvm::LLVMIntPredicate::LLVMIntEQ,
            src1.1,
            const_llval.as_any_value(),
            "zerocond",
        );

        self.emit_prepost_new_blocks_with_abort(cond_reg);
    }

    fn translate_address_comparison_impl(
        &self,
        dst: &[mast::TempIndex],
        src: &[mast::TempIndex],
        name: &str,
        pred: llvm::LLVMIntPredicate,
    ) {
        // Generate the following LLVM IR to compare `address` types.
        // Note that only eq/ne apply to these.
        //
        // The incoming sources are allocas or global values of array type [N x i8],
        // where N = account_address::AccountAddress::LENGTH (typically 16, 20, or 32 bytes,
        // according to target platform/chain). Use memcmp to do the comparison.
        //    ...
        //    %t = call i32 @memcmp(ptr %local_0, ptr %local_1, i64 N)
        //    %{eq,ne}_dst = icmp {eq,ne} %t, 0
        //    ...

        assert_eq!(dst.len(), 1);
        assert_eq!(src.len(), 2);

        let mut src0_reg = self.locals[src[0]].llval.as_any_value();
        let mut src1_reg = self.locals[src[1]].llval.as_any_value();

        let src_mty = &self.locals[src[0]].mty;
        let cmp_mty = if src_mty.is_reference() {
            src0_reg = self.load_reg(src[0], &format!("{name}_indsrc_0"));
            src1_reg = self.load_reg(src[1], &format!("{name}_indsrc_1"));
            src_mty.skip_reference()
        } else {
            src_mty
        };
        assert!(cmp_mty.is_signer_or_address());

        let num_elts = account_address::AccountAddress::LENGTH as u64;
        let builder = &self.module_cx.llvm_builder;
        let llcx = &self.module_cx.llvm_cx;
        let memcmp = self
            .module_cx
            .llvm_module
            .get_named_function("memcmp")
            .expect("memcmp not found");

        let args = vec![
            src0_reg,
            src1_reg,
            llvm::Constant::int(llcx.int_type(64), U256::from(num_elts)).as_any_value(),
        ];
        let cmp_val = builder.call(memcmp, &args);

        let zero_val = llvm::Constant::get_const_null(llcx.int_type(32)).as_any_value();
        let dst_reg = builder.build_compare(pred, cmp_val, zero_val, &format!("{name}_dst"));
        self.store_reg(dst[0], dst_reg);
    }

    fn translate_vector_comparison_impl(
        &self,
        dst: &[mast::TempIndex],
        src: &[mast::TempIndex],
        name: &str,
        pred: llvm::LLVMIntPredicate,
    ) {
        // Generate the following LLVM IR to compare vector types.
        // Note that only eq/ne apply to these.
        //
        // The incoming sources are allocas of vector type (or references to those).
        //    ...
        //    %t = call void @move_rt_vec_cmp_eq(ptr @__move_rttydesc_{T}, ptr %vsrc0, ptr %vsrc1)
        //    ...
        let mut src0_reg = self.locals[src[0]].llval.as_any_value();
        let mut src1_reg = self.locals[src[1]].llval.as_any_value();

        let src_mty = &self.locals[src[0]].mty;
        let cmp_mty = if src_mty.is_reference() {
            src0_reg = self.load_reg(src[0], &format!("{name}_indsrc_0"));
            src1_reg = self.load_reg(src[1], &format!("{name}_indsrc_1"));
            src_mty.skip_reference()
        } else {
            src_mty
        };

        let vec_elt_cmp_mty = match cmp_mty {
            mty::Type::Vector(ety) => &**ety,
            _ => unreachable!(),
        };
        assert!(
            pred == llvm::LLVMIntPredicate::LLVMIntEQ || pred == llvm::LLVMIntPredicate::LLVMIntNE
        );

        let mut dst_reg = self.module_cx.emit_rtcall_with_retval(RtCall::VecCmpEq(
            src0_reg,
            src1_reg,
            vec_elt_cmp_mty.clone(),
        ));

        // The above produces equality, so invert if this is a not-equal comparison.
        if pred == llvm::LLVMIntPredicate::LLVMIntNE {
            let cval =
                llvm::Constant::int(self.module_cx.llvm_cx.int_type(1), U256::one()).as_any_value();
            dst_reg = self.module_cx.llvm_builder.build_binop(
                llvm_sys::LLVMOpcode::LLVMXor,
                dst_reg,
                cval,
                "invert_dst",
            );
        }

        self.store_reg(dst[0], dst_reg);
    }

    fn translate_struct_comparison_impl(
        &self,
        dst: &[mast::TempIndex],
        src: &[mast::TempIndex],
        name: &str,
        pred: llvm::LLVMIntPredicate,
    ) {
        // Generate the following LLVM IR to compare struct types.
        // Note that only eq/ne apply to these.
        //
        // The incoming sources are allocas of struct type (or references to those).
        //    ...
        //    %t = call void @move_rt_struct_cmp_eq(ptr @__move_rttydesc_{T}, ptr %src0, ptr %src1)
        //    ...
        let mut src0_reg = self.locals[src[0]].llval.as_any_value();
        let mut src1_reg = self.locals[src[1]].llval.as_any_value();

        let src_mty = &self.locals[src[0]].mty;
        let cmp_mty = if src_mty.is_reference() {
            src0_reg = self.load_reg(src[0], &format!("{name}_indsrc_0"));
            src1_reg = self.load_reg(src[1], &format!("{name}_indsrc_1"));
            src_mty.skip_reference()
        } else {
            src_mty
        };

        assert!(cmp_mty.is_struct());
        assert!(
            pred == llvm::LLVMIntPredicate::LLVMIntEQ || pred == llvm::LLVMIntPredicate::LLVMIntNE
        );

        let mut dst_reg = self.module_cx.emit_rtcall_with_retval(RtCall::StructCmpEq(
            src0_reg,
            src1_reg,
            cmp_mty.clone(),
        ));

        // The above produces equality, so invert if this is a not-equal comparison.
        if pred == llvm::LLVMIntPredicate::LLVMIntNE {
            let cval =
                llvm::Constant::int(self.module_cx.llvm_cx.int_type(1), U256::one()).as_any_value();
            dst_reg = self.module_cx.llvm_builder.build_binop(
                llvm_sys::LLVMOpcode::LLVMXor,
                dst_reg,
                cval,
                "invert_dst",
            );
        }

        self.store_reg(dst[0], dst_reg);
    }

    fn translate_comparison_impl(
        &self,
        dst: &[mast::TempIndex],
        src: &[mast::TempIndex],
        name: &str,
        pred: llvm::LLVMIntPredicate,
    ) {
        assert_eq!(dst.len(), 1);
        assert_eq!(src.len(), 2);

        let src_mty = &self.locals[src[0]].mty;

        let referent_mty = if src_mty.is_reference() {
            Some(src_mty.skip_reference())
        } else {
            None
        };

        if src_mty.is_signer_or_address()
            || referent_mty
                .unwrap_or(&mty::Type::Error)
                .is_signer_or_address()
        {
            self.translate_address_comparison_impl(dst, src, name, pred);
            return;
        }

        if src_mty.is_vector() || referent_mty.unwrap_or(&mty::Type::Error).is_vector() {
            self.translate_vector_comparison_impl(dst, src, name, pred);
            return;
        }

        if src_mty.is_struct() || referent_mty.unwrap_or(&mty::Type::Error).is_struct() {
            self.translate_struct_comparison_impl(dst, src, name, pred);
            return;
        }

        let cmp_mty = if let Some(rty) = referent_mty {
            rty
        } else {
            src_mty
        };

        assert!(cmp_mty.is_number() || cmp_mty.is_bool());

        let mut src0_reg = self.load_reg(src[0], &format!("{name}_src_0"));
        let mut src1_reg = self.load_reg(src[1], &format!("{name}_src_1"));

        if src_mty.is_reference() {
            let src_llty = self.module_cx.to_llvm_type(cmp_mty, &[]).unwrap();
            src0_reg = self.module_cx.llvm_builder.build_load_from_valref(
                src_llty,
                src0_reg,
                &format!("{name}_indsrc_0"),
            );
            src1_reg = self.module_cx.llvm_builder.build_load_from_valref(
                src_llty,
                src1_reg,
                &format!("{name}_indsrc_1"),
            );
        }

        let dst_reg = self.module_cx.llvm_builder.build_compare(
            pred,
            src0_reg,
            src1_reg,
            &format!("{name}_dst"),
        );
        self.store_reg(dst[0], dst_reg);
    }

    fn translate_arithm_impl(
        &self,
        dst: &[mast::TempIndex],
        src: &[mast::TempIndex],
        name: &str,
        op: llvm_sys::LLVMOpcode,
        dyncheck_emitter_fn: CheckEmitterFn<'mm, 'up>,
    ) {
        assert_eq!(dst.len(), 1);
        assert_eq!(src.len(), 2);
        let src0_reg = self.load_reg(src[0], &format!("{name}_src_0"));
        let mut src1_reg = self.load_reg(src[1], &format!("{name}_src_1"));

        // Emit any dynamic pre-condition checking code.
        if dyncheck_emitter_fn.1 == EmitterFnKind::PreCheck {
            let args = [Some((src[0], src0_reg)), Some((src[1], src1_reg)), None];
            dyncheck_emitter_fn.0(self, &args);
        }

        // LLVM IR requires binary operators to have the same type. On the other hand, the Move language
        // insists that shift operators only take u8 for the shift count. Extend src1 when its type does
        // not match src0 to meet LLVM IR requirements. This will be optimized away later by LLVM.
        if op == llvm_sys::LLVMOpcode::LLVMShl || op == llvm_sys::LLVMOpcode::LLVMLShr {
            let src0_mty = &self.locals[src[0]].mty;
            let src1_mty = &self.locals[src[1]].mty;
            assert_eq!(src1_mty.get_bitwidth(), 8);
            let src0_width = src0_mty.get_bitwidth();
            if src0_width > 8 {
                src1_reg = self.module_cx.llvm_builder.build_zext(
                    src1_reg,
                    self.module_cx.to_llvm_type(src0_mty, &[]).unwrap(),
                    "zext_dst",
                );
            }
        }

        let dst_reg =
            self.module_cx
                .llvm_builder
                .build_binop(op, src0_reg, src1_reg, &format!("{name}_dst"));

        // Emit any dynamic post-condition checking code.
        if dyncheck_emitter_fn.1 == EmitterFnKind::PostCheck {
            let args = [Some((src[0], src0_reg)), None, Some((dst[0], dst_reg))];
            dyncheck_emitter_fn.0(self, &args);
        }

        self.store_reg(dst[0], dst_reg);
    }

    fn emit_precond_for_cast(
        &self,
        src_reg: llvm::AnyValue,
        src_width: u64,
        dst_width: u64,
        src_llty: llvm::Type,
    ) {
        // Generate the following LLVM IR to abort if the result is too large for the target type.
        // (https://move-language.github.io/move/integers.html#casting).
        //   ...
        //   %castcond = icmp ugt {i8/16/32/64/128} %cast_src, (2**dest_bitwidth-1)
        //   br i1 %castcond, %then_bb, %join_bb
        // then_bb:
        //   call void @move_rt_abort(i64 ARITHMETIC_ERROR)
        //   unreachable
        // join_bb:
        //  ...
        //

        // This check only needs to be emitted with the source type is larger than the dest type.
        if src_width <= dst_width {
            return;
        }
        assert!(dst_width <= 128);
        let dst_maxval = (U256::one().checked_shl(dst_width as u32)).unwrap() - U256::one();
        let const_llval = llvm::Constant::int(src_llty, dst_maxval).as_any_value();
        let cond_reg = self.module_cx.llvm_builder.build_compare(
            llvm::LLVMIntPredicate::LLVMIntUGT,
            src_reg,
            const_llval,
            "castcond",
        );

        self.emit_prepost_new_blocks_with_abort(cond_reg);
    }

    fn translate_cast_impl(&self, dst: &[mast::TempIndex], src: &[mast::TempIndex]) {
        assert_eq!(dst.len(), 1);
        assert_eq!(src.len(), 1);
        let src_idx = src[0];
        let src_mty = &self.locals[src_idx].mty;
        let dst_idx = dst[0];
        let dst_mty = &self.locals[dst_idx].mty;
        assert!(src_mty.is_number());
        assert!(dst_mty.is_number());
        let src_width = src_mty.get_bitwidth();
        let dst_width = dst_mty.get_bitwidth();
        let src_reg = self.load_reg(src_idx, "cast_src");

        self.emit_precond_for_cast(
            src_reg,
            src_width,
            dst_width,
            self.module_cx.to_llvm_type(src_mty, &[]).unwrap(),
        );

        let dst_reg = if src_width < dst_width {
            // Widen
            self.module_cx.llvm_builder.build_zext(
                src_reg,
                self.module_cx.to_llvm_type(dst_mty, &[]).unwrap(),
                "zext_dst",
            )
        } else {
            // Truncate
            self.module_cx.llvm_builder.build_trunc(
                src_reg,
                self.module_cx.to_llvm_type(dst_mty, &[]).unwrap(),
                "trunc_dst",
            )
        };
        self.store_reg(dst[0], dst_reg);
    }

    fn translate_call(
        &self,
        dst: &[mast::TempIndex],
        op: &sbc::Operation,
        src: &[mast::TempIndex],
    ) {
        use sbc::Operation;
        let emitter_nop: CheckEmitterFn = (|_, _| (), EmitterFnKind::PreCheck);
        let builder = &self.module_cx.llvm_builder;
        match op {
            Operation::Function(mod_id, fun_id, types) => {
                let types = mty::Type::instantiate_vec(types.to_vec(), self.type_params);
                self.translate_fun_call(*mod_id, *fun_id, &types, dst, src);
            }
            Operation::BorrowLoc => {
                assert_eq!(src.len(), 1);
                assert_eq!(dst.len(), 1);
                let src_idx = src[0];
                let dst_idx = dst[0];
                let src_llval = self.locals[src_idx].llval;
                let dst_llval = self.locals[dst_idx].llval;
                builder.ref_store(src_llval, dst_llval);
            }
            Operation::BorrowField(mod_id, struct_id, types, offset) => {
                let types = mty::Type::instantiate_vec(types.to_vec(), self.type_params);
                assert_eq!(src.len(), 1);
                assert_eq!(dst.len(), 1);
                let src_llval = self.locals[src[0]].llval;
                let dst_llval = self.locals[dst[0]].llval;
                let struct_env = self
                    .get_global_env()
                    .get_module(*mod_id)
                    .into_struct(*struct_id);
                let struct_name = struct_env.ll_struct_name_from_raw_name(&types);
                let stype = self
                    .module_cx
                    .llvm_cx
                    .named_struct_type(&struct_name)
                    .expect("no struct type");
                builder.field_ref_store(src_llval, dst_llval, stype, *offset);
            }
            Operation::Pack(mod_id, struct_id, types) => {
                let types = mty::Type::instantiate_vec(types.to_vec(), self.type_params);
                let struct_env = self
                    .get_global_env()
                    .get_module(*mod_id)
                    .into_struct(*struct_id);
                assert_eq!(dst.len(), 1);
                assert_eq!(src.len(), struct_env.get_field_count());
                let struct_name = struct_env.ll_struct_name_from_raw_name(&types);
                let stype = self
                    .module_cx
                    .llvm_cx
                    .named_struct_type(&struct_name)
                    .expect("no struct type");
                let fvals = src
                    .iter()
                    .map(|i| (self.locals[*i].llty, self.locals[*i].llval))
                    .collect::<Vec<_>>();
                let dst_idx = dst[0];
                let ldst = (self.locals[dst_idx].llty, self.locals[dst_idx].llval);
                builder.insert_fields_and_store(&fvals, ldst, stype);
            }
            Operation::Unpack(mod_id, struct_id, types) => {
                let types = mty::Type::instantiate_vec(types.to_vec(), self.type_params);
                let struct_env = self
                    .get_global_env()
                    .get_module(*mod_id)
                    .into_struct(*struct_id);
                assert_eq!(src.len(), 1);
                assert_eq!(dst.len(), struct_env.get_field_count());
                let struct_name = struct_env.ll_struct_name_from_raw_name(&types);
                let stype = self
                    .module_cx
                    .llvm_cx
                    .named_struct_type(&struct_name)
                    .expect("no struct type");
                let fdstvals = dst
                    .iter()
                    .map(|i| (self.locals[*i].llty, self.locals[*i].llval))
                    .collect::<Vec<_>>();
                let src_idx = src[0];
                let lsrc = (self.locals[src_idx].llty, self.locals[src_idx].llval);
                builder.load_and_extract_fields(lsrc, &fdstvals, stype);
            }
            Operation::Destroy => {
                assert!(dst.is_empty());
                assert_eq!(src.len(), 1);
                let idx = src[0];
                let mty = &self.locals[idx].mty;
                match mty {
                    mty::Type::Primitive(_) => ( /* nop */ ),
                    mty::Type::Struct(_, _, _) => ( /* nop */ ),
                    mty::Type::Reference(_, _) => { /* nop */ }
                    mty::Type::Vector(elt_mty) => {
                        self.emit_rtcall(RtCall::VecDestroy(idx, (**elt_mty).clone()));
                    }
                    _ => todo!("{mty:?}"),
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
                builder.load_deref_store(dst_llty, src_llval, dst_llval);
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
                builder.load_store_ref(src_llty, src_llval, dst_llval);
            }
            Operation::FreezeRef => {
                assert_eq!(dst.len(), 1);
                assert_eq!(src.len(), 1);
                let src_idx = src[0];
                let dst_idx = dst[0];
                let src_llty = self.locals[src_idx].llty;
                let src_llval = self.locals[src_idx].llval;
                let dst_llval = self.locals[dst_idx].llval;
                builder.load_store(src_llty, src_llval, dst_llval);
            }
            Operation::Add => {
                self.translate_arithm_impl(
                    dst,
                    src,
                    "add",
                    llvm_sys::LLVMOpcode::LLVMAdd,
                    (Self::emit_postcond_for_add, EmitterFnKind::PostCheck),
                );
            }
            Operation::Sub => {
                self.translate_arithm_impl(
                    dst,
                    src,
                    "sub",
                    llvm_sys::LLVMOpcode::LLVMSub,
                    (Self::emit_postcond_for_sub, EmitterFnKind::PostCheck),
                );
            }
            Operation::Mul => {
                let src0_reg = self.load_reg(src[0], "mul_src_0");
                let src1_reg = self.load_reg(src[1], "mul_src_1");
                let src0_llty = &self.locals[src[0]].llty;
                let dst_val = builder.build_intrinsic_call(
                    self.module_cx.llvm_module,
                    "llvm.umul.with.overflow",
                    &[*src0_llty],
                    &[src0_reg, src1_reg],
                    "mul_val",
                );
                let prod_reg = builder.build_extract_value(dst_val, 0, "mul_dst");
                let args = [None, None, Some((mast::TempIndex::MAX, dst_val))];
                self.emit_postcond_for_mul(&args);

                self.store_reg(dst[0], prod_reg);
            }
            Operation::Div => {
                self.translate_arithm_impl(
                    dst,
                    src,
                    "div",
                    llvm_sys::LLVMOpcode::LLVMUDiv,
                    (Self::emit_precond_for_div, EmitterFnKind::PreCheck),
                );
            }
            Operation::Mod => {
                self.translate_arithm_impl(
                    dst,
                    src,
                    "mod",
                    llvm_sys::LLVMOpcode::LLVMURem,
                    (Self::emit_precond_for_div, EmitterFnKind::PreCheck),
                );
            }
            Operation::BitOr => {
                self.translate_arithm_impl(
                    dst,
                    src,
                    "or",
                    llvm_sys::LLVMOpcode::LLVMOr,
                    emitter_nop,
                );
            }
            Operation::BitAnd => {
                self.translate_arithm_impl(
                    dst,
                    src,
                    "and",
                    llvm_sys::LLVMOpcode::LLVMAnd,
                    emitter_nop,
                );
            }
            Operation::Xor => {
                self.translate_arithm_impl(
                    dst,
                    src,
                    "xor",
                    llvm_sys::LLVMOpcode::LLVMXor,
                    emitter_nop,
                );
            }
            Operation::Shl => {
                self.translate_arithm_impl(
                    dst,
                    src,
                    "shl",
                    llvm_sys::LLVMOpcode::LLVMShl,
                    (Self::emit_precond_for_shift, EmitterFnKind::PreCheck),
                );
            }
            Operation::Shr => {
                self.translate_arithm_impl(
                    dst,
                    src,
                    "shr",
                    llvm_sys::LLVMOpcode::LLVMLShr,
                    (Self::emit_precond_for_shift, EmitterFnKind::PreCheck),
                );
            }
            Operation::Lt => {
                self.translate_comparison_impl(dst, src, "lt", llvm::LLVMIntPredicate::LLVMIntULT);
            }
            Operation::Gt => {
                self.translate_comparison_impl(dst, src, "gt", llvm::LLVMIntPredicate::LLVMIntUGT);
            }
            Operation::Le => {
                self.translate_comparison_impl(dst, src, "le", llvm::LLVMIntPredicate::LLVMIntULE);
            }
            Operation::Ge => {
                self.translate_comparison_impl(dst, src, "ge", llvm::LLVMIntPredicate::LLVMIntUGE);
            }
            Operation::Or => {
                // Logical Or
                self.translate_arithm_impl(
                    dst,
                    src,
                    "or",
                    llvm_sys::LLVMOpcode::LLVMOr,
                    emitter_nop,
                );
            }
            Operation::And => {
                // Logical And
                self.translate_arithm_impl(
                    dst,
                    src,
                    "and",
                    llvm_sys::LLVMOpcode::LLVMAnd,
                    emitter_nop,
                );
            }
            Operation::Eq => {
                self.translate_comparison_impl(dst, src, "eq", llvm::LLVMIntPredicate::LLVMIntEQ);
            }
            Operation::Neq => {
                self.translate_comparison_impl(dst, src, "ne", llvm::LLVMIntPredicate::LLVMIntNE);
            }
            Operation::Not => {
                assert_eq!(dst.len(), 1);
                assert_eq!(src.len(), 1);
                let src_idx = src[0];
                let src_mty = &self.locals[src_idx].mty;
                let dst_idx = dst[0];
                let dst_mty = &self.locals[dst_idx].mty;
                assert!(src_mty.is_bool());
                assert!(dst_mty.is_bool());
                let src_reg = self.load_reg(src_idx, "not_src");
                let constval = llvm::Constant::int(
                    self.module_cx.to_llvm_type(src_mty, &[]).unwrap(),
                    U256::one(),
                );
                let dst_reg = builder.build_binop(
                    llvm_sys::LLVMOpcode::LLVMXor,
                    src_reg,
                    constval.as_any_value(),
                    "not_dst",
                );
                self.store_reg(dst_idx, dst_reg);
            }
            Operation::CastU8
            | Operation::CastU16
            | Operation::CastU32
            | Operation::CastU64
            | Operation::CastU128
            | Operation::CastU256 => {
                self.translate_cast_impl(dst, src);
            }
            // Ignore specification-related (Move Prover, etc) operations.
            Operation::GetField(_, _, _, _)
            | Operation::GetGlobal(_, _, _)
            | Operation::IsParent(_, _)
            | Operation::WriteBack(_, _)
            | Operation::UnpackRef
            | Operation::PackRef
            | Operation::UnpackRefDeep
            | Operation::PackRefDeep
            | Operation::TraceLocal(_)
            | Operation::TraceReturn(_)
            | Operation::TraceAbort
            | Operation::TraceExp(_, _)
            | Operation::TraceGlobalMem(_)
            | Operation::EmitEvent
            | Operation::EventStoreDiverge
            | Operation::OpaqueCallBegin(_, _, _)
            | Operation::OpaqueCallEnd(_, _, _)
            | Operation::Uninit
            | Operation::Havoc(_)
            | Operation::Stop => {}
            _ => todo!("{op:?}"),
        }
    }

    /// Translation of calls to native functions.
    ///
    /// Native functions are unlike Move functions in that they
    /// pass type descriptors for generics, and they follow
    /// the C ABI.
    fn translate_native_fun_call(
        &self,
        mod_id: mm::ModuleId,
        fun_id: mm::FunId,
        types: &[mty::Type],
        dst: &[mast::TempIndex],
        src: &[mast::TempIndex],
    ) {
        let types = mty::Type::instantiate_vec(types.to_vec(), self.type_params);
        let typarams = self.module_cx.get_rttydesc_ptrs(&types);

        let dst_locals = dst.iter().map(|i| &self.locals[*i]).collect::<Vec<_>>();
        let src_locals = src.iter().map(|i| &self.locals[*i]).collect::<Vec<_>>();

        let ll_fn = self
            .module_cx
            .lookup_native_fn_decl(mod_id.qualified(fun_id));

        // Get information from the possibly-generic callee function declaration
        // in order to make calling-convention adjustments for generics.
        let (callee_arg_types, return_val_is_generic) = {
            let global_env = &self.env.module_env.env;
            let fn_id = fun_id.qualified(mod_id);
            let fn_env = global_env.get_function(fn_id);
            let arg_types = fn_env.get_parameter_types();
            let ret_types = fn_env.get_return_types();
            let return_val_is_generic = match ret_types.len() {
                0 => false,
                1 => matches!(ret_types[0], mty::Type::TypeParameter(_)),
                _ => {
                    todo!()
                }
            };
            (arg_types, return_val_is_generic)
        };

        let typarams = typarams.into_iter().map(|llval| llval.as_any_value());
        let src = src_locals
            .into_iter()
            .zip(callee_arg_types)
            .map(|(local, callee_arg_type)| {
                // Pass generic values and vectors by their stack pointer
                match callee_arg_type {
                    mty::Type::TypeParameter(_) => local.llval.as_any_value(),
                    mty::Type::Vector(_) => local.llval.as_any_value(),
                    _ => self
                        .module_cx
                        .llvm_builder
                        .load_alloca(local.llval, local.llty),
                }
            });
        let byval_ret_ptr = if !return_val_is_generic {
            None
        } else {
            // By-value returns of generic types are done by
            // pointer, so pass the alloca where the return value
            // is going to be stored.
            Some(dst_locals[0].llval.as_any_value())
        };
        let src = typarams.chain(src).chain(byval_ret_ptr).collect::<Vec<_>>();

        if !return_val_is_generic {
            let dst = dst_locals
                .iter()
                .map(|l| (l.llty, l.llval))
                .collect::<Vec<_>>();

            self.module_cx.llvm_builder.call_store(ll_fn, &src, &dst);
        } else {
            self.module_cx.llvm_builder.call(ll_fn, &src);
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
        // Handle native function calls specially.
        {
            let global_env = &self.env.module_env.env;
            let fn_id = fun_id.qualified(mod_id);
            let fn_env = global_env.get_function(fn_id);
            if fn_env.is_native() {
                return self.translate_native_fun_call(mod_id, fun_id, types, dst, src);
            }
        }

        let dst_locals = dst.iter().map(|i| &self.locals[*i]).collect::<Vec<_>>();
        let src_locals = src.iter().map(|i| &self.locals[*i]).collect::<Vec<_>>();

        let ll_fn = self
            .module_cx
            .lookup_move_fn_decl(mod_id.qualified_inst(fun_id, types.to_vec()));

        let src = src_locals
            .iter()
            .map(|l| (l.llty, l.llval))
            .collect::<Vec<_>>();

        let dst = dst_locals
            .iter()
            .map(|l| (l.llty, l.llval))
            .collect::<Vec<_>>();

        self.module_cx
            .llvm_builder
            .load_call_store(ll_fn, &src, &dst);
    }

    // Optional vec_mty is only used for a vector literal (i.e., Constant<Vector(Vec<Constant>))
    // to help determine element type when vector constant data array is empty.
    fn constant(&self, mc: &sbc::Constant, vec_mty: Option<&mty::Type>) -> llvm::Constant {
        use mty::{PrimitiveType, Type};
        use sbc::Constant;
        let llcx = self.module_cx.llvm_cx;
        let builder = &self.module_cx.llvm_builder;
        let ll_int = |n, val| llvm::Constant::int(llcx.int_type(n), U256::from(val));
        match mc {
            Constant::Bool(val) => ll_int(1, *val as u128),
            Constant::U8(val) => ll_int(8, *val as u128),
            Constant::U16(val) => ll_int(16, *val as u128),
            Constant::U32(val) => ll_int(32, *val as u128),
            Constant::U64(val) => ll_int(64, *val as u128),
            Constant::U128(val) => ll_int(128, *val),
            Constant::U256(val) => {
                let as_str = format!("{}", *val);
                let newval = U256::from_str_radix(&as_str, 10).expect("cannot convert to U256");
                llvm::Constant::int(llcx.int_type(256), newval)
            }
            Constant::Address(val) => {
                let addr_len = account_address::AccountAddress::LENGTH;

                // Create a global constant value of type [LENGTH x i8] with this account address
                // as the contents (in LSB first order).
                //
                // The address is a BigUint which only stores as many bits as needed, so pad it out
                // to the full address length if needed.
                let mut bytes = val.to_bytes_le();
                bytes.extend(vec![0; addr_len - bytes.len()]);
                let aval = llcx.const_int_array::<u8>(&bytes).as_const();
                let gval = self
                    .module_cx
                    .llvm_module
                    .add_global2(aval.llvm_type(), "acct.addr");
                gval.set_constant();
                gval.set_internal_linkage();
                gval.set_initializer(aval);
                builder.build_load_global_const(gval)
            }
            Constant::AddressArray(val_vec) => {
                // This is just like Constant(Vector(_)) below, except that the stackless bytecode
                // currently treats it specially with Vec<BigUint> instead of Vec<sbc::Constant>.
                //
                // Transform `Vec<BigUint>` to `Vec<llvm::Constant>`.
                // Then create global array value containing the vector literal data.
                let addr_len = account_address::AccountAddress::LENGTH;
                let vals: Vec<llvm::Constant> = val_vec
                    .iter()
                    .map(|v| {
                        let mut bytes = v.to_bytes_le();
                        bytes.extend(vec![0; addr_len - bytes.len()]);
                        llcx.const_int_array::<u8>(&bytes).as_const()
                    })
                    .collect();
                let aval =
                    llcx.const_array(&vals, self.module_cx.rtty_cx.get_llvm_type_for_address());

                let elt_mty = Type::Primitive(PrimitiveType::Address);
                let (res_val_type, res_ptr) =
                    self.make_global_array_and_copy_to_new_vec(aval, &elt_mty);

                builder
                    .build_load(res_val_type, res_ptr, "reload")
                    .as_constant()
            }
            Constant::ByteArray(val_vec) => {
                // Similar to Constant(Vector(_)) below, except that the stackless bytecode
                // currently treats it specially with Vec<u8> instead of Vec<sbc::Constant>.
                //
                // Create global array value containing the vector literal data.
                let aval = llcx.const_int_array::<u8>(val_vec);

                let elt_mty = Type::Primitive(PrimitiveType::U8);
                let (res_val_type, res_ptr) =
                    self.make_global_array_and_copy_to_new_vec(aval, &elt_mty);

                builder
                    .build_load(res_val_type, res_ptr, "reload")
                    .as_constant()
            }
            Constant::Vector(val_vec) => {
                // What we'd like to do below is simply match Constant::* on an element of
                // val_vec. But Move allows an empty vector literal (e.g., let v = vector[]),
                // so that we may not be able to index an element of the vector. Instead, we
                // have callers pass in an mty from their context and match on that to indirectly
                // determine the Constant element type.
                //
                // Transform `Vec<sbc::Constant>` to `Vec<llvm::Constant>`.
                // Then create global array value containing the vector literal data.
                let vmty = vec_mty.unwrap();
                let elt_mty = vmty.vector_element_type();

                let aval = match elt_mty {
                    _ if elt_mty.is_number() || elt_mty.is_bool() => {
                        let vals = self.rewrap_vec_constant(val_vec);
                        llcx.const_array(&vals, self.module_cx.to_llvm_type(&elt_mty, &[]).unwrap())
                    }
                    Type::Vector(bt) if bt.is_number_u8() => {
                        // This is a Constant::ByteArray element type.
                        assert!(matches!(val_vec[0], Constant::ByteArray(_)));
                        todo!("{:?}", mc);
                    }
                    _ => {
                        todo!("unexpected vec constant: {}: {:#?}", val_vec.len(), val_vec);
                    }
                };

                let (res_val_type, res_ptr) =
                    self.make_global_array_and_copy_to_new_vec(aval, &elt_mty);

                builder
                    .build_load(res_val_type, res_ptr, "reload")
                    .as_constant()
            }
        }
    }

    // Transform `Vec<sbc::Constant>` to `Vec<llvm::Constant>`.
    fn rewrap_vec_constant(&self, vc: &[sbc::Constant]) -> Vec<llvm::Constant> {
        use sbc::Constant;
        let retvec = vc
            .iter()
            .map(|v| match v {
                Constant::Bool(_) => self.constant(v, None),
                Constant::U8(_) => self.constant(v, None),
                Constant::U16(_) => self.constant(v, None),
                Constant::U32(_) => self.constant(v, None),
                Constant::U64(_) => self.constant(v, None),
                Constant::U128(_) => self.constant(v, None),
                Constant::U256(_) => self.constant(v, None),
                _ => unreachable!("{:?}", v),
            })
            .collect();
        retvec
    }

    fn make_global_array_and_copy_to_new_vec(
        &self,
        aval: llvm::ArrayValue,
        elt_mty: &mty::Type,
    ) -> (llvm::Type, llvm::Alloca) {
        let mod_cx = &self.module_cx;
        let builder = &mod_cx.llvm_builder;
        let llcx = &mod_cx.llvm_cx;

        // Create an LLVM global for the array of literal values.
        let raw_vec_data = mod_cx
            .llvm_module
            .add_global2(aval.llvm_type(), "vec_literal");
        raw_vec_data.set_constant();
        raw_vec_data.set_internal_linkage();
        raw_vec_data.set_initializer(aval.as_const());

        // Create an LLVM global containing the vector descriptor (to be passed to the
        // runtime) and initialize it with the array created above. The format of the
        // descriptor corresponds to 'move_native::rt_types::MoveUntypedVector'
        let vec_len = aval.llvm_type().get_array_length();
        let vec_descriptor_init = llcx.const_struct(&[
            raw_vec_data.ptr(),
            self.constant(&sbc::Constant::U64(vec_len as u64), None),
            self.constant(&sbc::Constant::U64(vec_len as u64), None),
        ]);
        let vec_descriptor = mod_cx
            .llvm_module
            .add_global2(vec_descriptor_init.llvm_type(), "vdesc");
        vec_descriptor.set_constant();
        vec_descriptor.set_internal_linkage();
        vec_descriptor.set_initializer(vec_descriptor_init);

        // Generate LLVM IR to construct a new empty vector and then copy the global
        // data into the new vector.
        //   ...
        //   %newv = call { ptr, i64, i64} move_rt_vec_empty(ptr @__move_rttydesc_{T})
        //   %pv = alloca { ptr, i64, i64 }
        //   store { ptr, i64, i64 } %newv, ptr %pv
        //   call move_rt_vec_copy(ptr @__move_rttydesc_{T}, %pv, @vec_data_descriptor)
        //   ...

        let res_val = self
            .module_cx
            .emit_rtcall_with_retval(RtCall::VecEmpty(elt_mty.clone()));

        // Be sure to emit allocas only in the entry block. They may otherwise be
        // interpreted as dynamic stack allocations by some parts of the LLVM code. These
        // are not supported by the SBF/BPF back-ends.
        //
        // Temporarily reposition the builder at the entry basic block and insert there.
        let curr_bb = builder.get_insert_block();
        let parent_func = curr_bb.get_basic_block_parent();
        builder.position_at_beginning(builder.get_entry_basic_block(parent_func));

        let res_ptr = builder.build_alloca(res_val.llvm_type(), "newv");

        // Resume insertionn at the current block.
        builder.position_at_end(curr_bb);

        builder.build_store(res_val, res_ptr);

        self.module_cx.emit_rtcall_with_retval(RtCall::VecCopy(
            res_ptr.as_any_value(),
            vec_descriptor.as_any_value(),
            elt_mty.clone(),
        ));
        (res_val.llvm_type(), res_ptr)
    }

    fn emit_rtcall(&self, rtcall: RtCall) {
        match &rtcall {
            RtCall::Abort(local_idx) => {
                let llfn = ModuleContext::get_runtime_function(
                    self.module_cx.llvm_cx,
                    self.module_cx.llvm_module,
                    &self.module_cx.rtty_cx,
                    &rtcall,
                );
                let local_llval = self.locals[*local_idx].llval;
                let local_llty = self.locals[*local_idx].llty;
                self.module_cx.llvm_builder.load_call_store(
                    llfn,
                    &[(local_llty, local_llval)],
                    &[],
                );
                self.module_cx.llvm_builder.build_unreachable();
            }
            RtCall::VecDestroy(local_idx, elt_mty) => {
                let llfn = ModuleContext::get_runtime_function(
                    self.module_cx.llvm_cx,
                    self.module_cx.llvm_module,
                    &self.module_cx.rtty_cx,
                    &rtcall,
                );
                let typarams = self.module_cx.get_rttydesc_ptrs(&[elt_mty.clone()]);
                let typarams = typarams.into_iter().map(|llval| llval.as_any_value());
                // The C ABI passes the by-val-vector as a pointer.
                let local = &self.locals[*local_idx];
                let local = local.llval.as_any_value();
                let args = typarams.chain(Some(local)).collect::<Vec<_>>();
                self.module_cx.llvm_builder.call_store(llfn, &args, &[]);
            }
            _ => unreachable!(),
        }
    }
}

pub enum RtCall {
    Abort(mast::TempIndex),
    Deserialize(llvm::AnyValue, llvm::AnyValue),
    VecDestroy(mast::TempIndex, mty::Type),
    VecCopy(llvm::AnyValue, llvm::AnyValue, mty::Type),
    VecCmpEq(llvm::AnyValue, llvm::AnyValue, mty::Type),
    VecEmpty(mty::Type),
    StrCmpEq(
        llvm::AnyValue,
        llvm::AnyValue,
        llvm::AnyValue,
        llvm::AnyValue,
    ),
    StructCmpEq(llvm::AnyValue, llvm::AnyValue, mty::Type),
}

/// Compile the module to object file.
///
/// This takes the module by value because it would otherwise have
/// side effects, mutating target-specific properties.
pub fn write_object_file(
    llmod: llvm::Module,
    llmachine: &llvm::TargetMachine,
    outpath: &str,
) -> anyhow::Result<()> {
    llmod.verify();
    llmachine.emit_to_obj_file(&llmod, outpath)?;
    Ok(())
}
