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

use crate::stackless::{extensions::*, llvm, rttydesc};
use llvm_sys::prelude::LLVMValueRef;
use move_core_types::{u256, vm_status::StatusCode::ARITHMETIC_ERROR};
use move_model::{ast as mast, model as mm, ty as mty};
use move_stackless_bytecode::{
    stackless_bytecode as sbc, stackless_bytecode_generator::StacklessBytecodeGenerator,
    stackless_control_flow_graph::generate_cfg_in_dot_format,
};
use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    iter,
};

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
            Target::Solana => "",
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
        dot_info: &'this String,
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
            dot_info,
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
    dot_info: &'up String,
}

impl<'mm, 'up> ModuleContext<'mm, 'up> {
    pub fn translate(mut self) -> llvm::Module {
        let filename = self.env.get_source_path().to_str().expect("utf-8");
        self.llvm_module.set_source_file_name(filename);

        self.declare_structs();
        self.declare_functions();

        for fn_env in self.env.get_functions() {
            if fn_env.is_native() {
                continue;
            }
            let fn_cx = self.create_fn_context(fn_env);
            fn_cx.translate(self.dot_info);
        }

        self.llvm_module.verify();

        self.llvm_module
    }

    /// Generate LLVM IR struct declarations for all Move structures.
    fn declare_structs(&mut self) {
        use move_binary_format::{access::ModuleAccess, views::StructHandleView};
        let m_env = &self.env;
        let g_env = &m_env.env;

        // Collect all the externally defined structures (transitively) used within this module.
        //
        // Note that the ModuleData at ModuleEnv::data is private, while the same ModuleData is
        // public in GlobalEnv::module_data-- so we obtain it from the latter. We need access to
        // this to efficiently discover foreign structs. There is not yet a model-provided routine
        // as there is for foreign called functions.
        let mut external_sqids = BTreeSet::new();
        let mut worklist = VecDeque::new();
        let mut visited = BTreeSet::new();
        worklist.push_back(m_env.get_id());
        while let Some(mid) = worklist.pop_front() {
            let module_data = &g_env.module_data[mid.to_usize()];
            for shandle in module_data.module.struct_handles().iter() {
                let struct_view = StructHandleView::new(&module_data.module, shandle);
                let declaring_module_env = g_env
                    .find_module(&g_env.to_module_name(&struct_view.module_id()))
                    .expect("undefined module");
                let struct_env = declaring_module_env
                    .find_struct(m_env.symbol_pool().make(struct_view.name().as_str()))
                    .expect("undefined struct");
                let qid = struct_env.get_qualified_id();
                if qid.module_id != m_env.get_id() && !visited.contains(&qid.module_id) {
                    worklist.push_back(qid.module_id);
                    external_sqids.insert(qid);
                }
            }
            visited.insert(mid);
        }

        // Create a combined list of all structs (external plus local).
        let mut local_structs: Vec<_> = m_env.get_structs().collect();
        let mut all_structs: Vec<_> = external_sqids
            .iter()
            .map(|q| g_env.get_struct_qid(*q))
            .collect();
        all_structs.append(&mut local_structs);

        // Visit each struct definition, creating corresponding LLVM IR struct types.
        //
        // Note that struct defintions can depend on other struct definitions. Inconveniently, the
        // order of structs given to us above by the model are not necessarily in topological order
        // of dependence.  Since we'll need a structure type to translate structure fields during
        // the visitation later, we need to ensure any dependent structure types are already
        // available. One way would be to build a dependence graph of structs and visit the nodes
        // topologically. A second way, which we adopt here, is to traverse the struct list twice.
        // That is, on the first traversal, we create opaque structs (i.e., partially formed,
        // deferring field translation). The second traversal will then fill in the struct bodies
        // where it will have all structure types previously defined.
        for s_env in &all_structs {
            if !s_env.get_type_parameters().is_empty() {
                todo!("generic structs not yet implemented");
            }
            let ll_name = self.ll_struct_name_from_raw_name(s_env);
            self.llvm_cx.create_opaque_named_struct(&ll_name);
        }

        // Translate input IR representing Move struct MyMod::MyStruct:
        //   struct MyStruct has { copy, drop, key, store } {
        //       field1: type1, field2: type2, ..., fieldn: typeN
        //   }
        // to a LLVM IR structure type:
        //   %struct.MyMod__MyStruct = type {
        //       <llvm_type1>, <llvm_type2>, ..., <llvm_typeN>, <i8>
        //   }
        //
        // Compiler synthesized informational fields are injected following the user fields.
        //
        // The target layout is convenient in that the user field offsets [0..N) in the input IR
        // map one-to-one to values used to index into the LLVM struct with getelementptr,
        // extractvalue, and insertvalue.
        //
        // Compiler synthesized fields:
        //   <i8>   This Move struct's 'abilities'. A u8 bitvector corresponding to a
        //          move_binary_format::AbilitySet. These can be used during runtime for various
        //          safety checks.
        //
        // As the compiler evolves and the design comes into focus, additional fields may be added
        // or existing fields changed or removed.
        for s_env in &all_structs {
            if !s_env.get_type_parameters().is_empty() {
                todo!("generic structs not yet implemented");
            }
            let ll_name = self.ll_struct_name_from_raw_name(s_env);
            let ll_sty = self
                .llvm_cx
                .named_struct_type(&ll_name)
                .expect("no struct type");

            // Visit each field in this struct, collecting field types.
            let mut ll_field_tys = Vec::with_capacity(s_env.get_field_count() + 1);
            for fld_env in s_env.get_fields() {
                let ll_fld_type = self.llvm_type(&fld_env.get_type());
                ll_field_tys.push(ll_fld_type);
            }

            // Append the 'abilities' field.
            ll_field_tys.push(self.llvm_cx.int8_type());
            ll_sty.set_struct_body(&ll_field_tys);
        }
    }

    fn ll_struct_name_from_raw_name(&self, s_env: &mm::StructEnv) -> String {
        let raw_name = s_env.get_full_name_str();
        format!("struct.{}", raw_name.replace(':', "_"))
    }

    #[allow(dead_code)]
    fn dump_all_structs(&self, all_structs: &Vec<mm::StructEnv>, is_post_translation: bool) {
        for s_env in all_structs {
            let ll_name = self.ll_struct_name_from_raw_name(s_env);
            let prepost = if is_post_translation {
                "Translated"
            } else {
                "Translating"
            };
            eprintln!(
                "{} struct '{}' => '%{}'",
                prepost,
                s_env.get_full_name_str(),
                ll_name
            );
            for fld_env in s_env.get_fields() {
                eprintln!(
                    "offset {}: '{}', type {:?}",
                    fld_env.get_offset(),
                    fld_env.get_name().display(s_env.symbol_pool()),
                    fld_env.get_type()
                );
                if is_post_translation {
                    eprintln!("=>");
                    let ll_fld_type = self.llvm_type(&fld_env.get_type());
                    ll_fld_type.dump();
                }
            }
            eprintln!("with abilities: {:?}", s_env.get_abilities());
            eprintln!();
        }
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
        if fn_env.is_native() {
            self.declare_native_function(fn_env)
        } else {
            self.declare_move_function(fn_env)
        }
    }

    fn declare_move_function(&mut self, fn_env: &mm::FunctionEnv) {
        let fn_data = StacklessBytecodeGenerator::new(fn_env).generate_function();

        let ll_fn = {
            let ll_fnty = {
                let ll_rty = match fn_data.return_types.len() {
                    0 => self.llvm_cx.void_type(),
                    1 => self.llvm_type(&fn_data.return_types[0]),
                    _ => {
                        // Wrap multiple return values in a struct.
                        let tys: Vec<_> = fn_data
                            .return_types
                            .iter()
                            .map(|f| self.llvm_type(f))
                            .collect();
                        self.llvm_cx.get_anonymous_struct_type(&tys)
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

    fn declare_native_function(&mut self, fn_env: &mm::FunctionEnv) {
        assert!(fn_env.is_native());

        let fn_data = StacklessBytecodeGenerator::new(fn_env).generate_function();

        let ll_fn = {
            let ll_fnty = {
                let ll_rty = match fn_data.return_types.len() {
                    0 => self.llvm_cx.void_type(),
                    1 => self.llvm_type(&fn_data.return_types[0]),
                    _ => {
                        todo!()
                    }
                };

                // Native functions take type parameters as the
                // first arguments.
                let num_typarams = fn_env.get_type_parameter_count();
                let ll_tydesc_type = self.llvm_tydesc_type();
                let ll_tydesc_ptr_type = ll_tydesc_type.ptr_type();

                let ll_tydesc_parms = iter::repeat(ll_tydesc_ptr_type).take(num_typarams);

                let ll_parm_tys = fn_env.get_parameter_types();
                let ll_parm_tys = ll_parm_tys.iter().map(|mty| self.llvm_type(mty));

                let all_ll_parms = ll_tydesc_parms.chain(ll_parm_tys).collect::<Vec<_>>();

                llvm::FunctionType::new(ll_rty, &all_ll_parms)
            };

            self.llvm_module
                .add_function(&fn_env.llvm_native_fn_symbol_name(), ll_fnty)
        };

        let id = fn_env.get_qualified_id();
        self.fn_decls.insert(id, ll_fn);
    }

    /// The type descriptor accepted by runtime functions.
    ///
    /// Corresponds to `move_native::rt_types::MoveType`.
    fn llvm_tydesc_type(&self) -> llvm::StructType {
        rttydesc::get_llvm_tydesc_type(self.llvm_cx)
    }

    fn llvm_type(&self, mty: &mty::Type) -> llvm::Type {
        use mty::{PrimitiveType, Type};

        match mty {
            Type::Primitive(PrimitiveType::Bool) => self.llvm_cx.int1_type(),
            Type::Primitive(PrimitiveType::U8) => self.llvm_cx.int8_type(),
            Type::Primitive(PrimitiveType::U16) => self.llvm_cx.int16_type(),
            Type::Primitive(PrimitiveType::U32) => self.llvm_cx.int32_type(),
            Type::Primitive(PrimitiveType::U64) => self.llvm_cx.int64_type(),
            Type::Primitive(PrimitiveType::U128) => self.llvm_cx.int128_type(),
            Type::Primitive(PrimitiveType::U256) => self.llvm_cx.int256_type(),
            Type::Reference(_, referent_mty) => {
                let referent_llty = self.llvm_type(referent_mty);
                referent_llty.ptr_type()
            }
            Type::TypeParameter(_) => {
                // this is ok for now, while type params are only passed by reference,
                // but might end up broken in the future.
                self.llvm_cx.int8_type()
            }
            Type::Struct(declaring_module_id, struct_id, _) => {
                let global_env = &self.env.env;
                let struct_env = global_env
                    .get_module(*declaring_module_id)
                    .into_struct(*struct_id);
                let struct_name = self.ll_struct_name_from_raw_name(&struct_env);
                if let Some(stype) = self.llvm_cx.named_struct_type(&struct_name) {
                    stype.as_any_type()
                } else {
                    unreachable!("struct type for '{}' not found", &struct_name);
                }
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
            Type::Primitive(PrimitiveType::U16) => 16,
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
            llvm_cx: self.llvm_cx,
            llvm_module: &self.llvm_module,
            llvm_builder: &self.llvm_builder,
            llvm_type: Box::new(|ty| self.llvm_type(ty)),
            get_bitwidth: Box::new(|ty| self.get_bitwidth(ty)),
            ll_struct_name_from_raw_name: Box::new(|s_env| {
                self.ll_struct_name_from_raw_name(s_env)
            }),
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
    ll_struct_name_from_raw_name: Box<dyn (Fn(&mm::StructEnv) -> String) + 'up>,
    fn_decls: &'up BTreeMap<mm::QualifiedId<mm::FunId>, llvm::Function>,
    label_blocks: BTreeMap<sbc::Label, llvm::BasicBlock>,
    /// Corresponds to FunctionData:local_types
    locals: Vec<Local>,
}

/// A stackless move local variable, translated as an llvm alloca
#[derive(Clone)]
struct Local {
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
    fn(&FunctionContext<'mm, 'up>, &[Option<(mast::TempIndex, LLVMValueRef)>]) -> (),
    EmitterFnKind,
);

impl<'mm, 'up> FunctionContext<'mm, 'up> {
    fn get_global_env(&self) -> &'mm mm::GlobalEnv {
        self.env.module_env.env
    }

    fn translate(mut self, dot_info: &'up String) {
        let fn_data = StacklessBytecodeGenerator::new(&self.env).generate_function();

        // Write the control flow graph to a .dot file for viewing.
        if !dot_info.is_empty() {
            let func_target =
                move_stackless_bytecode::function_target::FunctionTarget::new(&self.env, &fn_data);
            let fname = &self.env.llvm_symbol_name();
            let dot_graph = generate_cfg_in_dot_format(&func_target);
            let graph_label = format!("digraph {{ label=\"Function: {}\"\n", fname);
            let dgraph2 = dot_graph.replacen("digraph {", &graph_label, 1);
            let (action, output_path) = dot_info.split_at(2);
            let path_sep = match output_path {
                "" => "",
                _ => "/",
            };
            let dot_file = format!("{}{}{}_cfg.dot", output_path, path_sep, fname);
            std::fs::write(&dot_file, &dgraph2).expect("generating dot file for CFG");
            // If requested by user, also invoke the xdot viewer.
            if action == "v:" {
                std::process::Command::new("xdot")
                    .arg(dot_file)
                    .status()
                    .expect("failed to execute 'xdot'");
            }
        }

        let ll_fn = &self.fn_decls[&self.env.get_qualified_id()];

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

    fn translate_instruction(&mut self, instr: &sbc::Bytecode) {
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
                        self.llvm_builder.load_store(llty, src_llval, dst_llval);
                    }
                    mty::Type::Reference(_, _) => {
                        self.llvm_builder.load_store(llty, src_llval, dst_llval);
                    }
                    mty::Type::Struct(_, _, _) => {
                        // A move renders the source location inaccessible, but the storage is
                        // to be reused for the target. We simply replace the dest local's slot
                        // with the source, so that all later references to dest use the original
                        // space (the alloca) of the source. If the input IR is correct, then
                        // src local slot should not be accessed again.
                        self.locals[*dst] = self.locals[*src].clone();
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
                        self.llvm_builder.load_store(llty, src_llval, dst_llval);
                    }
                    mty::Type::Struct(_, _, _) => {
                        self.llvm_builder.load_store(llty, src_llval, dst_llval);
                    }
                    mty::Type::Reference(_, referent) => match **referent {
                        mty::Type::Struct(_, _, _) => {
                            self.llvm_builder.load_store(llty, src_llval, dst_llval);
                        }
                        _ => todo!("{mty:?}"),
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
                        | mty::PrimitiveType::U256,
                    ) => {
                        self.llvm_builder.load_store(llty, src_llval, dst_llval);
                    }
                    mty::Type::Struct(_, _, _) => {
                        self.llvm_builder.load_store(llty, src_llval, dst_llval);
                    }
                    _ => todo!("{mty:#?}"),
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
                _ => {
                    // Multiple return values are wrapped in a struct.
                    let nvals = vals
                        .iter()
                        .map(|i| (self.locals[*i].llty, self.locals[*i].llval))
                        .collect::<Vec<_>>();

                    let ll_fn = &self.fn_decls[&self.env.get_qualified_id()];
                    let ret_ty = ll_fn.llvm_return_type();
                    self.llvm_builder.load_multi_return(ret_ty, &nvals);
                }
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

    fn emit_prepost_new_blocks_with_abort(&self, cond_reg: LLVMValueRef) {
        // All pre- and post-condition emitters generate the same conditional structure.

        // Generate and insert the two new basic blocks.
        let builder = &self.llvm_builder;
        let curr_bb = builder.get_insert_block();
        let parent_func = curr_bb.get_basic_block_parent();
        let then_bb = parent_func.insert_basic_block_after(curr_bb, "then_bb");
        let join_bb = parent_func.insert_basic_block_after(then_bb, "join_bb");

        // Generate the conditional branch and call to abort.
        builder.build_cond_br(cond_reg, then_bb, join_bb);
        builder.position_at_end(then_bb);
        self.emit_rtcall_abort_raw(ARITHMETIC_ERROR as u64);
        builder.position_at_end(join_bb);
    }

    fn emit_precond_for_shift(
        &self,
        args: &[Option<(mast::TempIndex, LLVMValueRef)>], // src0, src1, dst.
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
        let const_llval =
            llvm::Constant::generic_int(*src1_llty, u256::U256::from(src0_width)).get0();
        let cond_reg = self.llvm_builder.build_compare(
            llvm::LLVMIntPredicate::LLVMIntUGE,
            src1.1,
            const_llval,
            "rangecond",
        );

        self.emit_prepost_new_blocks_with_abort(cond_reg);
    }

    fn emit_postcond_for_add(
        &self,
        args: &[Option<(mast::TempIndex, LLVMValueRef)>], // src0, src1, dst.
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
        let cond_reg = self.llvm_builder.build_compare(
            llvm::LLVMIntPredicate::LLVMIntULT,
            dst.1,
            src0.1,
            "ovfcond",
        );

        self.emit_prepost_new_blocks_with_abort(cond_reg);
    }

    fn emit_postcond_for_sub(
        &self,
        args: &[Option<(mast::TempIndex, LLVMValueRef)>], // src0, src1, dst.
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
        let cond_reg = self.llvm_builder.build_compare(
            llvm::LLVMIntPredicate::LLVMIntUGT,
            dst.1,
            src0.1,
            "ovfcond",
        );

        self.emit_prepost_new_blocks_with_abort(cond_reg);
    }

    fn emit_postcond_for_mul(
        &self,
        args: &[Option<(mast::TempIndex, LLVMValueRef)>], // src0, src1, dst.
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
        let cond_reg = self.llvm_builder.build_extract_value(dst.1, 1, "mul_ovf");
        self.emit_prepost_new_blocks_with_abort(cond_reg);
    }

    fn emit_precond_for_div(
        &self,
        args: &[Option<(mast::TempIndex, LLVMValueRef)>], // src0, src1, dst.
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
        let const_llval = llvm::Constant::generic_int(*src1_llty, u256::U256::zero()).get0();
        let cond_reg = self.llvm_builder.build_compare(
            llvm::LLVMIntPredicate::LLVMIntEQ,
            src1.1,
            const_llval,
            "zerocond",
        );

        self.emit_prepost_new_blocks_with_abort(cond_reg);
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
        let src0_reg = self.load_reg(src[0], &format!("{name}_src_0"));
        let src1_reg = self.load_reg(src[1], &format!("{name}_src_1"));
        let dst_reg =
            self.llvm_builder
                .build_compare(pred, src0_reg, src1_reg, &format!("{name}_dst"));
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
            assert_eq!(self.get_bitwidth(src1_mty), 8);
            let src0_width = self.get_bitwidth(src0_mty);
            if src0_width > 8 {
                src1_reg =
                    self.llvm_builder
                        .build_zext(src1_reg, self.llvm_type(src0_mty).0, "zext_dst");
            }
        }

        let dst_reg = self
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
        src_reg: LLVMValueRef,
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
        let dst_maxval =
            (u256::U256::one().checked_shl(dst_width as u32)).unwrap() - u256::U256::one();
        let const_llval = llvm::Constant::generic_int(src_llty, dst_maxval).get0();
        let cond_reg = self.llvm_builder.build_compare(
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
        let src_width = self.get_bitwidth(src_mty);
        let dst_width = self.get_bitwidth(dst_mty);
        let src_reg = self.load_reg(src_idx, "cast_src");

        self.emit_precond_for_cast(src_reg, src_width, dst_width, self.llvm_type(src_mty));

        let dst_reg = if src_width < dst_width {
            // Widen
            self.llvm_builder
                .build_zext(src_reg, self.llvm_type(dst_mty).0, "zext_dst")
        } else {
            // Truncate
            self.llvm_builder
                .build_trunc(src_reg, self.llvm_type(dst_mty).0, "trunc_dst")
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
            Operation::BorrowField(mod_id, struct_id, _types, offset) => {
                // We don't yet translate generic structs, so _types is unused.
                assert_eq!(src.len(), 1);
                assert_eq!(dst.len(), 1);
                let src_llval = self.locals[src[0]].llval;
                let dst_llval = self.locals[dst[0]].llval;
                let struct_env = self
                    .get_global_env()
                    .get_module(*mod_id)
                    .into_struct(*struct_id);
                let struct_name = (self.ll_struct_name_from_raw_name)(&struct_env);
                let stype = self
                    .llvm_cx
                    .named_struct_type(&struct_name)
                    .expect("no struct type");
                self.llvm_builder
                    .field_ref_store(src_llval, dst_llval, stype, *offset);
            }
            Operation::Pack(mod_id, struct_id, _types) => {
                // We don't yet translate generic structs, so _types is unused.
                let struct_env = self
                    .get_global_env()
                    .get_module(*mod_id)
                    .into_struct(*struct_id);
                assert_eq!(dst.len(), 1);
                assert_eq!(src.len(), struct_env.get_field_count());
                let struct_name = (self.ll_struct_name_from_raw_name)(&struct_env);
                let stype = self
                    .llvm_cx
                    .named_struct_type(&struct_name)
                    .expect("no struct type");
                let fvals = src
                    .iter()
                    .map(|i| (self.locals[*i].llty, self.locals[*i].llval))
                    .collect::<Vec<_>>();
                let dst_idx = dst[0];
                let ldst = (self.locals[dst_idx].llty, self.locals[dst_idx].llval);
                self.llvm_builder
                    .insert_fields_and_store(&fvals, ldst, stype);
            }
            Operation::Unpack(mod_id, struct_id, _types) => {
                // We don't yet translate generic structs, so _types is unused.
                let struct_env = self
                    .get_global_env()
                    .get_module(*mod_id)
                    .into_struct(*struct_id);
                assert_eq!(src.len(), 1);
                assert_eq!(dst.len(), struct_env.get_field_count());
                let struct_name = (self.ll_struct_name_from_raw_name)(&struct_env);
                let stype = self
                    .llvm_cx
                    .named_struct_type(&struct_name)
                    .expect("no struct type");
                let fdstvals = dst
                    .iter()
                    .map(|i| (self.locals[*i].llty, self.locals[*i].llval))
                    .collect::<Vec<_>>();
                let src_idx = src[0];
                let lsrc = (self.locals[src_idx].llty, self.locals[src_idx].llval);
                self.llvm_builder
                    .load_and_extract_fields(lsrc, &fdstvals, stype);
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
                let dst_val = self.llvm_builder.build_intrinsic_call(
                    self.llvm_module,
                    "llvm.umul.with.overflow",
                    &[*src0_llty],
                    &[src0_reg, src1_reg],
                    "mul_val",
                );
                let prod_reg = self.llvm_builder.build_extract_value(dst_val, 0, "mul_dst");
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
                let const_llval = llvm::Constant::int(self.llvm_type(src_mty), 1).get0();
                let dst_reg = self.llvm_builder.build_binop(
                    llvm_sys::LLVMOpcode::LLVMXor,
                    src_reg,
                    const_llval,
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
            _ => todo!("{op:?}"),
        }
    }

    /// Translation of calls to native functions.
    ///
    /// Native functions are unlike Move functions in that they
    /// pass type descriptors for generics.
    fn translate_native_fun_call(
        &self,
        mod_id: mm::ModuleId,
        fun_id: mm::FunId,
        types: &[mty::Type],
        dst: &[mast::TempIndex],
        src: &[mast::TempIndex],
    ) {
        let typarams = self.get_rttydesc_ptrs(types);

        let dst_locals = dst.iter().map(|i| &self.locals[*i]).collect::<Vec<_>>();
        let src_locals = src.iter().map(|i| &self.locals[*i]).collect::<Vec<_>>();

        let ll_fn = self.fn_decls[&fun_id.qualified(mod_id)];

        if dst_locals.len() > 1 {
            todo!()
        }

        let dst = dst_locals.get(0);

        match dst {
            None => {
                let typarams = typarams.into_iter().map(|llval| llval.as_any_value());
                let src = src_locals
                    .into_iter()
                    .map(|l| self.llvm_builder.load_alloca(l.llval));
                let args = typarams.chain(src).collect::<Vec<_>>();
                self.llvm_builder.call(ll_fn, &args);
            }
            Some(_dst) => {
                todo!()
            }
        }
    }

    fn get_rttydesc_ptrs(&self, types: &[mty::Type]) -> Vec<llvm::Constant> {
        let mut ll_global_ptrs = vec![];
        for type_ in types {
            let ll_tydesc = rttydesc::define_llvm_tydesc(self.llvm_cx, self.llvm_module, type_);
            ll_global_ptrs.push(ll_tydesc.ptr());
        }
        ll_global_ptrs
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

        let ll_fn = self.fn_decls[&fun_id.qualified(mod_id)];

        let src = src_locals
            .iter()
            .map(|l| (l.llty, l.llval))
            .collect::<Vec<_>>();

        let dst = dst_locals
            .iter()
            .map(|l| (l.llty, l.llval))
            .collect::<Vec<_>>();

        self.llvm_builder.load_call_store(ll_fn, &src, &dst);
    }

    fn constant(&self, mc: &sbc::Constant) -> llvm::Constant {
        use sbc::Constant;
        match mc {
            Constant::Bool(val) => {
                let llty = self.llvm_cx.int1_type();
                llvm::Constant::int(llty, *val as u64)
            }
            Constant::U8(val) => {
                let llty = self.llvm_cx.int8_type();
                llvm::Constant::int(llty, *val as u64)
            }
            Constant::U16(val) => {
                let llty = self.llvm_cx.int16_type();
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
            Constant::U256(val) => {
                let llty = self.llvm_cx.int256_type();
                let as_str = format!("{}", *val);
                let newval =
                    u256::U256::from_str_radix(&as_str, 10).expect("cannot convert to U256");
                llvm::Constant::int256(llty, newval)
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
                    .load_call_store(llfn, &[(local_llty, local_llval)], &[]);
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

            self.llvm_module
                .add_function_with_attrs(&name, llty, &attrs)
        }
    }

    fn emit_rtcall_abort_raw(&self, val: u64) {
        // TODO: Refactor get_runtime_function to avoid the below partial duplication.
        let name = "move_rt_abort";
        let llfn = self.llvm_module.get_named_function(name);
        let thefn = if let Some(llfn) = llfn {
            llfn
        } else {
            let (llty, attrs) = {
                let ret_ty = self.llvm_cx.void_type();
                let param_tys = &[self.llvm_cx.int64_type()];
                let llty = llvm::FunctionType::new(ret_ty, param_tys);
                let attrs = vec![llvm::AttributeKind::NoReturn];
                (llty, attrs)
            };

            self.llvm_module.add_function_with_attrs(name, llty, &attrs)
        };
        //
        let param_ty = self.llvm_cx.int64_type();
        let const_llval = llvm::Constant::generic_int(param_ty, u256::U256::from(val));
        self.llvm_builder.build_call_imm(thefn, &[const_llval]);
        self.llvm_builder.build_unreachable();
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
