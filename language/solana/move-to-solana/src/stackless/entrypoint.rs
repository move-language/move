// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    options::Options,
    stackless::{
        extensions::*, llvm, module_context::ModuleContext, rttydesc::RttyContext, GlobalContext,
        RtCall,
    },
};
use log::{debug, log_enabled, Level};
use move_core_types::u256::U256;
use move_model::{model as mm, ty as mty};
use move_native::shared::MOVE_UNTYPED_VEC_DESC_SIZE;
use std::{
    cell::{Cell, RefCell},
    collections::BTreeMap,
    path::Path,
};

pub struct EntrypointGenerator<'mm, 'up> {
    pub env: &'mm mm::GlobalEnv,
    pub llvm_cx: &'up llvm::Context,
    pub llvm_module: &'up llvm::Module,
    pub llvm_builder: llvm::Builder,
    pub options: &'up Options,
    pub rtty_cx: RttyContext<'mm, 'up>,

    fn_decls: RefCell<BTreeMap<String, llvm::Function>>,

    ll_fn_solana_entrypoint: llvm::Function,
    entry_slice_ptr: llvm::AnyValue,
    entry_slice_len: llvm::AnyValue,
    insn_data_ptr: llvm::AnyValue,
    offset: llvm::Alloca,
    retval: llvm::AnyValue,
    exit_bb: llvm::BasicBlock,

    entries: Cell<u64>,
    target_machine: &'up llvm::TargetMachine,
}

/**
 * Generate solana entrypoint functon code. This function
 * recieves serialized input paramteres from the VM. It calls
 * native function `deserialize` to decode the parameters into
 * corresponding data structures. The function `deserialize`
 * returns a triple consiting of
 * - instruction_data -- a byte array,
 * - program_id -- SolanaPubkey, and
 * - accounts -- a vector of SolanaAccountInfo items.
 *
 * To select one from possibly several entry functions defined in
 * the module, the entrypoint function expects the name of the
 * requested entry function to be passed in instruction_data byte
 * array. The logic in solana entrypoint iteratively compares the
 * string slice passed in instruction_data to every entry function
 * symbol of the module. Once a matching entry function is found,
 * it is called, and its return value is used as the exit code for
 * the program.
 */
impl<'mm, 'up> EntrypointGenerator<'mm, 'up> {
    pub fn new(
        global_context: &'up GlobalContext<'mm>,
        llvm_module: &'up llvm::Module,
        target_machine: &'up llvm::TargetMachine,
        options: &'mm Options,
    ) -> EntrypointGenerator<'mm, 'up> {
        let env = global_context.env;
        let llvm_cx = &global_context.llvm_cx;
        let llvm_builder = llvm_cx.create_builder();
        let rtty_cx = RttyContext::new(env, llvm_cx, llvm_module);

        let ll_fnty = {
            let ll_rty = llvm_cx.int_type(64_usize);
            let ll_param_tys = vec![llvm_cx.ptr_type()];
            llvm::FunctionType::new(ll_rty, &ll_param_tys)
        };
        let ll_fn_solana_entrypoint = llvm_module.add_function("main", ll_fnty);
        let entry_block = ll_fn_solana_entrypoint.append_basic_block("entry");
        llvm_builder.position_at_end(entry_block);
        let retval = llvm_builder
            .build_alloca(llvm_cx.int_type(64), "retval")
            .as_any_value();
        let offset = llvm_builder.build_alloca(llvm_cx.int_type(64), "offset");
        llvm_builder.store_const(
            llvm::Constant::int(llvm_cx.int_type(64), U256::zero()),
            offset,
        );

        // Get inputs from the VM into proper data structures.
        let (insn_data, _program_id, _accounts) = {
            let input = ll_fn_solana_entrypoint.get_param(0).as_any_value();
            let ll_sret = llvm_cx.get_anonymous_struct_type(&[
                rtty_cx.get_llvm_type_for_slice(),
                llvm_cx.ptr_type(),
                rtty_cx.get_llvm_type_for_move_native_vector(),
            ]);
            let params = llvm_builder.build_alloca(ll_sret, "params").as_any_value();
            let args = vec![params, input];
            let ll_fn_deserialize = ModuleContext::get_runtime_function(
                llvm_cx,
                llvm_module,
                &rtty_cx,
                &RtCall::Deserialize(params, input),
            );
            llvm_builder.call(ll_fn_deserialize, &args);

            let insn_data = llvm_builder.getelementptr(
                params,
                &ll_sret.as_struct_type(),
                0,
                "instruction_data",
            );
            let program_id =
                llvm_builder.getelementptr(params, &ll_sret.as_struct_type(), 1, "program_id");
            let accounts =
                llvm_builder.getelementptr(params, &ll_sret.as_struct_type(), 2, "accounts");
            (insn_data, program_id, accounts)
        };
        // Make a str slice from instruction_data byte array returned
        // from a call to deserialize
        let str_slice_type = rtty_cx.get_llvm_type_for_slice();
        let insn_data_ptr = llvm_builder.getelementptr(
            insn_data,
            &str_slice_type.as_struct_type(),
            0,
            "insn_data_ptr",
        );
        let insn_data_ptr =
            llvm_builder.load(insn_data_ptr, llvm_cx.ptr_type(), "insn_data_ptr_loaded");
        let offset_value = Self::advance_offset_by_increment(
            llvm_cx,
            &llvm_builder,
            offset.as_any_value(),
            llvm::Constant::int(llvm_cx.int_type(64), U256::from(8u64)).as_any_value(),
        );
        let entry_slice_ptr = llvm_builder.build_address_with_indices(
            llvm_cx.int_type(8),
            insn_data_ptr,
            &[offset_value],
            "entry_slice_ptr",
        );
        let entry_slice_len =
            llvm_builder.load(insn_data_ptr, llvm_cx.int_type(64), "entry_slice_len");
        let _offset_value = Self::advance_offset_by_increment(
            llvm_cx,
            &llvm_builder,
            offset.as_any_value(),
            entry_slice_len,
        );

        let curr_bb = llvm_builder.get_insert_block();
        let exit_bb = ll_fn_solana_entrypoint.insert_basic_block_after(curr_bb, "exit_bb");

        EntrypointGenerator {
            env,
            llvm_cx,
            llvm_module,
            llvm_builder,
            options,
            rtty_cx,
            fn_decls: RefCell::new(BTreeMap::new()),
            ll_fn_solana_entrypoint,
            entry_slice_ptr,
            entry_slice_len,
            insn_data_ptr,
            offset,
            retval,
            exit_bb,
            entries: Cell::new(0),
            target_machine,
        }
    }

    pub fn add_entry_declaration(
        &self,
        ll_sym_name: &str,
        llfn_type: llvm::FunctionType,
        attrs: &[(llvm_sys::LLVMAttributeIndex, &str, Option<u64>)],
    ) {
        debug_assert!(
            !self.fn_decls.borrow().contains_key(ll_sym_name),
            "Duplicate entry function name found."
        );
        let tfn = self.llvm_module.add_function(ll_sym_name, llfn_type);
        self.llvm_module.add_attributes(tfn, attrs);
        tfn.as_gv()
            .set_linkage(llvm::LLVMLinkage::LLVMExternalLinkage);
        self.fn_decls
            .borrow_mut()
            .insert(ll_sym_name.to_owned(), tfn);
    }

    pub fn add_entries(&self, mod_cx: &ModuleContext) {
        debug!("unit test function {:?}", self.options.unit_test_function);
        let unit_test_function = self.options.unit_test_function.clone().unwrap_or_default();
        let entry_functions: Vec<_> = mod_cx
            .env
            .get_functions()
            .filter(|fn_env| {
                fn_env.is_entry()
                    || fn_env.get_full_name_str().replace("::", "__") == unit_test_function
            })
            .collect();

        // Do not generate solana entrypoint if module doesn't contain any entry functions.
        if entry_functions.is_empty() {
            return;
        }
        self.entries
            .set(self.entries.get() + entry_functions.len() as u64);
        let str_slice_type = self.rtty_cx.get_llvm_type_for_slice();

        // For every entry function defined in the module compare its
        // name to the name passed in the instruction_data, and call
        // the matching entry function.
        for fun in entry_functions {
            let entry = self.generate_global_str_slice(fun.llvm_symbol_name(&[]).as_str());

            let func_name_ptr = self.llvm_builder.getelementptr(
                entry.as_any_value(),
                &str_slice_type.as_struct_type(),
                0,
                "entry_func_ptr",
            );
            let func_name_ptr = self.llvm_builder.load(
                func_name_ptr,
                self.llvm_cx.ptr_type(),
                "entry_func_ptr_loaded",
            );
            let func_name_len = self.llvm_builder.getelementptr(
                entry.as_any_value(),
                &str_slice_type.as_struct_type(),
                1,
                "entry_func_len",
            );
            let func_name_len = self.llvm_builder.load(
                func_name_len,
                self.llvm_cx.int_type(64),
                "entry_func_len_loaded",
            );

            let rtcall = RtCall::StrCmpEq(
                self.entry_slice_ptr,
                self.entry_slice_len,
                func_name_ptr,
                func_name_len,
            );
            let llfn = ModuleContext::get_runtime_function(
                self.llvm_cx,
                self.llvm_module,
                &self.rtty_cx,
                &rtcall,
            );
            let params = vec![
                self.entry_slice_ptr,
                self.entry_slice_len,
                func_name_ptr,
                func_name_len,
            ];
            let condition = self.llvm_builder.call(llfn, &params);
            let curr_bb = self.llvm_builder.get_insert_block();
            let then_bb = self
                .ll_fn_solana_entrypoint
                .insert_basic_block_after(curr_bb, "then_bb");
            let else_bb = self
                .ll_fn_solana_entrypoint
                .insert_basic_block_after(then_bb, "else_bb");
            self.llvm_builder.build_cond_br(condition, then_bb, else_bb);
            self.llvm_builder.position_at_end(then_bb);
            let fn_name = fun.llvm_symbol_name(&[]);
            let fn_decls = self.fn_decls.borrow();
            let ll_fun = fn_decls.get(&fn_name).unwrap();
            let params = self.emit_entry_arguments(
                mod_cx,
                &fun,
                &self.insn_data_ptr,
                &self.offset.as_any_value(),
            );
            let ret = self.llvm_builder.call(*ll_fun, &params);
            if fun.get_return_count() > 0 {
                self.llvm_builder.store(ret, self.retval);
            } else {
                self.llvm_builder.store(
                    llvm::Constant::int(self.llvm_cx.int_type(64), U256::zero()).as_any_value(),
                    self.retval,
                );
            }
            self.llvm_builder.build_br(self.exit_bb);
            self.llvm_builder.position_at_end(else_bb);
        }
    }

    pub fn has_entries(&self) -> bool {
        self.entries.get() > 0
    }

    pub fn write_object_file(&self, out_path: &Path) -> anyhow::Result<String> {
        self.emit_exit();
        let output_file = out_path.join("solana_entrypoint.o");
        let output_file = output_file.to_str().unwrap();
        self.target_machine
            .emit_to_obj_file(self.llvm_module, output_file)?;
        Ok(output_file.to_string())
    }

    fn emit_exit(&self) {
        // Abort if no entry function matched the requested name.
        ModuleContext::emit_rtcall_abort_raw(
            self.llvm_cx,
            &self.llvm_builder,
            self.llvm_module,
            &self.rtty_cx,
            move_core_types::vm_status::StatusCode::EXECUTE_ENTRY_FUNCTION_CALLED_ON_NON_ENTRY_FUNCTION as u64,
        );
        self.llvm_builder.position_at_end(self.exit_bb);
        let ret = self
            .llvm_builder
            .load(self.retval, self.llvm_cx.int_type(64), "exit_code");
        self.llvm_builder.build_return(ret);
        self.llvm_module.verify();

        if log_enabled!(target: "entry_point", Level::Debug) {
            self.llvm_module.dump();
        }
    }

    // This function extracts an entry function actual arguments from
    // instruction_data byte array, containing values of actual
    // arguments in sequential order without gaps.
    fn emit_entry_arguments(
        &self,
        mod_cx: &ModuleContext,
        fn_env: &mm::FunctionEnv,
        instruction_data: &llvm::AnyValue,
        index: &llvm::AnyValue,
    ) -> Vec<llvm::AnyValue> {
        if fn_env.get_parameter_count() == 0 {
            return vec![];
        }
        let llcx = self.llvm_cx;
        let i64_ty = llcx.int_type(64);
        let byte_ty = llcx.int_type(8);
        let mut index_value = self.llvm_builder.load(*index, i64_ty, "index_value");
        let mut args = vec![];
        for ty in fn_env.get_parameter_types() {
            let mut arg = self.llvm_builder.build_address_with_indices(
                byte_ty,
                *instruction_data,
                &[index_value],
                "arg_ptr",
            );
            match ty {
                mty::Type::Primitive(mty::PrimitiveType::Bool) => {
                    arg = self.llvm_builder.load(arg, llcx.int_type(1), "arg");
                    index_value = Self::advance_offset_by_increment(
                        self.llvm_cx,
                        &self.llvm_builder,
                        *index,
                        llvm::Constant::int(i64_ty, U256::one()).as_any_value(),
                    );
                }
                mty::Type::Reference(_, ty) => {
                    index_value = Self::advance_offset_by_increment(
                        self.llvm_cx,
                        &self.llvm_builder,
                        *index,
                        llvm::Constant::int(i64_ty, U256::from(ty.get_bitwidth() / 8))
                            .as_any_value(),
                    );
                }
                // This code is generated when control flow of the
                // function already has branches. LLVM seems to allow
                // stack allocations only in the entry basic
                // block. Therefore the layout of serialized vector
                // must have a valid vector structure, which can be
                // referenced for loading arguments from.  This code
                // assumes that the vector is represented as
                // {data_pointer, length, capacity} triple and data
                // follows this triple immediately. We patch the
                // data_pointer location in memory to contain the
                // address of the actual vector's data. The length and
                // the capaciity are assumed to have the same value.
                mty::Type::Vector(ty) => {
                    let vec_ty = self.rtty_cx.get_llvm_type_for_move_native_vector();
                    let vec_ptr = self.llvm_builder.getelementptr(
                        arg,
                        &vec_ty.as_struct_type(),
                        0,
                        "vec_ptr",
                    );
                    let vec_len = self.llvm_builder.getelementptr(
                        arg,
                        &vec_ty.as_struct_type(),
                        1,
                        "vec_len",
                    );
                    index_value = Self::advance_offset_by_increment(
                        self.llvm_cx,
                        &self.llvm_builder,
                        *index,
                        llvm::Constant::int(i64_ty, U256::from(MOVE_UNTYPED_VEC_DESC_SIZE))
                            .as_any_value(),
                    );
                    let vec_data = self.llvm_builder.build_address_with_indices(
                        byte_ty,
                        *instruction_data,
                        &[index_value],
                        "vec_data",
                    );
                    self.llvm_builder.store(vec_data, vec_ptr);
                    arg = self.llvm_builder.load(arg, vec_ty, "vec_arg");
                    let vec_element_size =
                        llvm::Constant::int(i64_ty, U256::from(ty.get_bitwidth() / 8))
                            .as_any_value();
                    let vec_len = self.llvm_builder.load(vec_len, i64_ty, "vec_len_loaded");
                    let vec_data_len = self.llvm_builder.build_binop(
                        llvm_sys::LLVMOpcode::LLVMMul,
                        vec_len,
                        vec_element_size,
                        "vec_data_len",
                    );
                    index_value = Self::advance_offset_by_increment(
                        self.llvm_cx,
                        &self.llvm_builder,
                        *index,
                        vec_data_len,
                    );
                }
                mty::Type::Struct(mid, sid, _) => {
                    arg = self.llvm_builder.load(
                        arg,
                        mod_cx.to_llvm_type(&ty, &[]).unwrap(),
                        "str_arg",
                    );
                    let g_env = &self.env;
                    let s_env = g_env.get_module(mid).into_struct(sid);
                    // FIXME! This computes incorrect width when
                    // fields of a struct are structs themselves.
                    // get_bitwidth() on Type::Struct currently
                    // returns 0, because model creates Type::Struct
                    // values with an empty vector for field
                    // types. Only instantiations of structs include
                    // actual types of fields in their corresponding
                    // Type::Struct values.
                    let width = s_env
                        .get_fields()
                        .map(|f_env| f_env.get_type())
                        .fold(0, |acc, ty| acc + ty.get_bitwidth());
                    let size = llvm::Constant::int(i64_ty, U256::from(width / 8)).as_any_value();
                    index_value = Self::advance_offset_by_increment(
                        self.llvm_cx,
                        &self.llvm_builder,
                        *index,
                        size,
                    );
                }
                _ => {
                    arg =
                        self.llvm_builder
                            .load(arg, mod_cx.to_llvm_type(&ty, &[]).unwrap(), "arg");
                    let size = llvm::Constant::int(i64_ty, U256::from(ty.get_bitwidth() / 8))
                        .as_any_value();
                    index_value = Self::advance_offset_by_increment(
                        self.llvm_cx,
                        &self.llvm_builder,
                        *index,
                        size,
                    );
                }
            }
            args.push(arg);
        }
        args
    }

    fn generate_global_str_slice(&self, s: &str) -> llvm::Global {
        let llcx = &self.llvm_cx;

        // Create an LLVM global for the string.
        let str_literal_init = llcx.const_int_array::<u8>(s.as_bytes()).as_const();
        let str_literal = self
            .llvm_module
            .add_global2(str_literal_init.llvm_type(), "str_literal");
        str_literal.set_constant();
        str_literal.set_alignment(1);
        str_literal.set_unnamed_addr();
        str_literal.set_linkage(llvm::LLVMLinkage::LLVMPrivateLinkage);
        str_literal.set_initializer(str_literal_init);

        // Create an LLVM global for the slice, which is a struct with two fields:
        // - pointer to the string literal
        // - integer length of the string literal
        let slice_len = s.len();
        let slice_init = llcx.const_struct(&[
            str_literal.ptr(),
            llvm::Constant::int(llcx.int_type(64), U256::from(slice_len as u128)),
        ]);
        let slice = self
            .llvm_module
            .add_global2(slice_init.llvm_type(), "str_slice");
        slice.set_constant();
        slice.set_alignment(8);
        slice.set_unnamed_addr();
        slice.set_linkage(llvm::LLVMLinkage::LLVMPrivateLinkage);
        slice.set_initializer(slice_init);

        slice
    }

    fn advance_offset_by_increment(
        llvm_cx: &'up llvm::Context,
        llvm_builder: &llvm::Builder,
        offset: llvm::AnyValue,
        increment: llvm::AnyValue,
    ) -> llvm::AnyValue {
        let offset_loaded = llvm_builder.load(offset, llvm_cx.int_type(64), "offset_loaded");
        let offset_loaded = llvm_builder.build_binop(
            llvm_sys::LLVMOpcode::LLVMAdd,
            offset_loaded,
            increment,
            "offset_loaded",
        );
        llvm_builder.store(offset_loaded, offset);
        offset_loaded
    }
}
