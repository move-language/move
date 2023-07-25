// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    cli::Args,
    stackless::{
        extensions::*, llvm, llvm::TargetMachine, rttydesc::RttyContext, FunctionContext, RtCall,
        TargetPlatform,
    },
};
use log::debug;
use move_binary_format::file_format::SignatureToken;
use move_core_types::{account_address, u256::U256};
use move_model::{model as mm, ty as mty};
use move_native::shared::{MOVE_TYPE_DESC_SIZE, MOVE_UNTYPED_VEC_DESC_SIZE};
use move_stackless_bytecode::{
    function_target::FunctionData, stackless_bytecode as sbc,
    stackless_bytecode_generator::StacklessBytecodeGenerator,
};
use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    iter,
};

pub struct ModuleContext<'mm, 'up> {
    pub env: mm::ModuleEnv<'mm>,
    pub llvm_cx: &'up llvm::Context,
    pub llvm_module: &'up llvm::Module,
    pub llvm_builder: llvm::Builder,
    /// A map of move function id's to llvm function ids
    ///
    /// All functions that might be called are declared prior to function translation.
    /// This includes local functions and dependencies.
    pub fn_decls: BTreeMap<String, llvm::Function>,
    pub expanded_functions: Vec<mm::QualifiedInstId<mm::FunId>>,
    pub target: TargetPlatform,
    pub target_machine: &'up TargetMachine,
    pub args: &'up Args,
    pub rtty_cx: RttyContext<'mm, 'up>,
}

impl<'mm, 'up> ModuleContext<'mm, 'up> {
    pub fn translate(mut self) {
        let filename = self.env.get_source_path().to_str().expect("utf-8");
        self.llvm_module.set_source_file_name(filename);
        self.llvm_module.set_target(self.target.triple());
        self.llvm_module.set_data_layout(self.target_machine);

        self.declare_structs();
        self.llvm_module.declare_known_functions();

        // Declaring functions will populate list `expanded_functions` containing all
        // concrete Move functions and expanded concrete instances of generic Move functions.
        self.declare_functions();

        for fn_qiid in &self.expanded_functions {
            let fn_env = self.env.env.get_function(fn_qiid.to_qualified_id());
            assert!(!fn_env.is_native());
            self.rtty_cx.reset_func(fn_qiid);
            let fn_cx = self.create_fn_context(fn_env, &self, &fn_qiid.inst);
            fn_cx.translate();
        }

        self.llvm_module.verify();
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
            for shandle in module_data.module.struct_handles() {
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
        //
        // Initially filter out generic structure handles (i.e., representing potentially many
        // concrete structures). The expansions will occur later when the struct definition
        // instantiations are processed.
        let has_type_params = |s_env: &mm::StructEnv| !s_env.get_type_parameters().is_empty();
        let mut local_structs: Vec<_> = m_env
            .get_structs()
            .filter_map(|s_env| (!has_type_params(&s_env)).then_some((s_env, vec![])))
            .collect();

        let mut all_structs: Vec<_> = external_sqids
            .iter()
            .map(|q| g_env.get_struct_qid(*q))
            .filter_map(|s_env| (!has_type_params(&s_env)).then_some((s_env, vec![])))
            .collect();
        all_structs.append(&mut local_structs);

        debug!(target: "structs", "{}", self.dump_all_structs(&all_structs, false));

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
        for (s_env, tyvec) in &all_structs {
            assert!(!has_type_params(s_env));
            let ll_name = s_env.ll_struct_name_from_raw_name(tyvec);
            self.llvm_cx.create_opaque_named_struct(&ll_name);
        }

        let create_opaque_named_struct = |s_env: &mm::StructEnv, tys: &[mty::Type]| {
            let ll_name = s_env.ll_struct_name_from_raw_name(tys);
            if self.llvm_cx.named_struct_type(&ll_name).is_none() {
                self.llvm_cx.create_opaque_named_struct(&ll_name);
                return true;
            }
            false
        };

        // Now that all the concrete structs are available, pull in the generic ones. Each such
        // StructDefInstantiation will induce a concrete expansion once fields are visited later.
        let this_module_data = &g_env.module_data[m_env.get_id().to_usize()];
        let cm = &this_module_data.module;
        for s_def_inst in cm.struct_instantiations() {
            let tys = m_env.get_type_actuals(Some(s_def_inst.type_parameters));
            let s_env = m_env.get_struct_by_def_idx(s_def_inst.def);
            let created = create_opaque_named_struct(&s_env, &tys);
            assert!(created, "struct already exists");
            all_structs.push((s_env, tys));
        }

        // Similarly, pull in generics from field instantiations.
        for f_inst in cm.field_instantiations() {
            let fld_handle = cm.field_handle_at(f_inst.handle);
            let tys = m_env.get_type_actuals(Some(f_inst.type_parameters));
            let s_env = m_env.get_struct_by_def_idx(fld_handle.owner);
            if create_opaque_named_struct(&s_env, &tys) {
                all_structs.push((s_env, tys));
            }
        }

        // Finally, some generic instantiations still may not have been seen. That would be
        // case where no explicit definition was already available, such as passing/returning
        // a generic or constructing a generic. Visit the signature table for any remaining.
        for sig in cm.signatures() {
            for st in &sig.0 {
                let mut inst_signatures: Vec<SignatureToken> = Vec::new();
                SignatureToken::find_struct_instantiation_signatures(st, &mut inst_signatures);
                for sti in &inst_signatures {
                    let gs = m_env.globalize_signature(sti);
                    if let mty::Type::Struct(mid, sid, tys) = gs {
                        let s_env = g_env.get_module(mid).into_struct(sid);
                        if create_opaque_named_struct(&s_env, &tys) {
                            all_structs.push((s_env, tys));
                        }
                    }
                }
            }
        }

        debug!(target: "structs", "{}", self.dump_all_structs(&all_structs, false));

        // Translate input IR representing Move struct MyMod::MyStruct:
        //   struct MyStruct has { copy, drop, key, store } {
        //       field1: type1, field2: type2, ..., fieldn: typeN
        //   }
        // to a LLVM IR structure type:
        //   %struct.MyMod__MyStruct = type {
        //       <llvm_type1>, <llvm_type2>, ..., <llvm_typeN>
        //   }
        //
        // The target layout is convenient in that the user field offsets [0..N) in the input IR
        // map one-to-one to values used to index into the LLVM struct with getelementptr,
        // extractvalue, and insertvalue.
        for (s_env, tyvec) in &all_structs {
            let ll_name = s_env.ll_struct_name_from_raw_name(tyvec);
            let ll_sty = self
                .llvm_cx
                .named_struct_type(&ll_name)
                .expect("no struct type");

            // Visit each field in this struct, collecting field types.
            let mut ll_field_tys = Vec::with_capacity(s_env.get_field_count() + 1);
            for fld_env in s_env.get_fields() {
                let ll_fld_type = self.llvm_type_with_ty_params(&fld_env.get_type(), tyvec);
                ll_field_tys.push(ll_fld_type);
            }

            ll_sty.set_struct_body(&ll_field_tys);
        }

        debug!(target: "structs", "{}", self.dump_all_structs(&all_structs, true));
    }

    pub fn llvm_type_with_ty_params(&self, mty: &mty::Type, tyvec: &[mty::Type]) -> llvm::Type {
        match mty {
            mty::Type::Struct(_mid, _sid, _stys) => {
                // Substitute any generic type parameters occuring in _stys.
                let new_sty = mty.instantiate(tyvec);
                self.llvm_type(&new_sty)
            }
            mty::Type::Reference(_, referent_mty) => {
                let referent_llty = self.llvm_type_with_ty_params(referent_mty, tyvec);
                referent_llty.ptr_type()
            }
            mty::Type::TypeParameter(tp_idx) => self.llvm_type(&tyvec[*tp_idx as usize]),
            _ => self.llvm_type(mty),
        }
    }

    fn dump_all_structs(
        &self,
        all_structs: &Vec<(mm::StructEnv, Vec<mty::Type>)>,
        is_post_translation: bool,
    ) -> String {
        let mut s = "\n".to_string();
        for (s_env, tyvec) in all_structs {
            let ll_name = s_env.ll_struct_name_from_raw_name(tyvec);
            let prepost = if is_post_translation {
                "Translated"
            } else {
                "Translating"
            };
            s += &format!(
                "{} struct '{}' => '%{}'\n",
                prepost,
                s_env.struct_raw_type_name(tyvec),
                ll_name
            )
            .to_string();
            for fld_env in s_env.get_fields() {
                s += &format!(
                    "offset {}: '{}', type ",
                    fld_env.get_offset(),
                    fld_env.get_name().display(s_env.symbol_pool())
                );
                if is_post_translation {
                    let ll_fld_type = self.llvm_type_with_ty_params(&fld_env.get_type(), tyvec);
                    s += ll_fld_type.print_to_str();
                } else {
                    s += format!("{:?}", fld_env.get_type()).as_str();
                };
                s += "\n";
            }
            s += &format!("with abilities: {:?}\n\n", s_env.get_abilities());
        }
        s
    }

    /// Create LLVM function decls for all local functions and
    /// all extern functions that might be called.
    fn declare_functions(&mut self) {
        let mod_env = self.env.clone(); // fixme bad clone

        // We have previously discovered through experience that some of the model-provided
        // information we once depended on to discover all module functions, called functions,
        // and concrete instantiations are not always consistent or reliable.
        //
        // For this reason, we now take a different approach and seed our discovery with just the
        // list of functions provided by `ModuleEnv::get_functions`. For any other called functions
        // (this module or foreign) and for any generic instantiations, we will expand the seed
        // frontier incrementally by gleaning the remaining information from a visitation of every
        // function call instruction (recursively) in every seed function.
        //
        // While this results in yet another linear walk over all the code, it seems to be the
        // simplest way to work around the model inconsistencies.
        for fn_env in mod_env.get_functions() {
            self.declare_functions_walk(&mod_env, &fn_env, vec![]);
        }
    }

    fn declare_functions_walk(
        &mut self,
        mod_env: &mm::ModuleEnv,
        curr_fn_env: &mm::FunctionEnv,
        curr_type_vec: Vec<mty::Type>,
    ) {
        let g_env = &mod_env.env;

        // Do not process a previously declared function/expansion.
        let fn_name = if curr_fn_env.is_native() {
            curr_fn_env.llvm_native_fn_symbol_name()
        } else if curr_fn_env.get_type_parameter_count() == 0 {
            curr_fn_env.llvm_symbol_name(&[])
        } else {
            curr_fn_env.llvm_symbol_name(&curr_type_vec)
        };

        if self.fn_decls.get(&fn_name).is_some() {
            return;
        }

        let fn_data = StacklessBytecodeGenerator::new(curr_fn_env).generate_function();

        // If the current function is either a native function or a concrete Move function,
        // we have all the information needed to declare a corresponding single function.
        //
        // If the current function is a generic Move function, we will defer declaring its
        // concrete expansions until a call path leading to a particular call site is visited.
        // At that point, the type parameters are either resolved or the function is not used
        // in the module. The generic function itself will not be emitted.
        let curr_fn_qid = curr_fn_env.get_qualified_id();
        if curr_fn_env.is_native() {
            // Declare the native and return early--- there is no function body to visit.
            self.declare_native_function(curr_fn_env, &fn_data, curr_fn_env.llvm_linkage());
            return;
        } else if curr_fn_env.get_type_parameter_count() == 0 {
            let curr_fn_qiid = curr_fn_qid.module_id.qualified_inst(curr_fn_qid.id, vec![]);
            self.declare_move_function(curr_fn_env, &[], &fn_data, curr_fn_env.llvm_linkage());
            if curr_fn_qid.module_id != mod_env.get_id() {
                // True foreign functions are only declared in our module, don't process further.
                return;
            }
            self.expanded_functions.push(curr_fn_qiid);
        } else {
            // Determine whether any of the type parameters for this generic function are still
            // unresolved. If so, then function is not a concrete instance and we defer it until
            // a call path containing it is expanded.
            assert!(curr_fn_env.get_type_parameter_count() > 0);
            let inst_is_generic = curr_type_vec.iter().any(|t| t.is_open());
            if curr_type_vec.is_empty() || inst_is_generic {
                return;
            }

            // Note that we may be declaring a foreign function here. But since it is being
            // expanded into our current module, its linkage is effectively private.
            let curr_fn_qiid = curr_fn_qid
                .module_id
                .qualified_inst(curr_fn_qid.id, curr_type_vec.clone());
            self.declare_move_function(
                curr_fn_env,
                &curr_type_vec,
                &fn_data,
                llvm::LLVMLinkage::LLVMPrivateLinkage,
            );
            self.expanded_functions.push(curr_fn_qiid);
        }

        // Visit every call site in the current function, instantiate their type parameters,
        // and then recursively grow the frontier.
        for instr in &fn_data.code {
            if let sbc::Bytecode::Call(
                _,
                _,
                sbc::Operation::Function(mod_id, fun_id, types),
                _,
                None,
            ) = instr
            {
                // Instantiate any type parameters at the current call site with the
                // enclosing type parameter scope `curr_type_vec`.
                let types = mty::Type::instantiate_vec(types.to_vec(), &curr_type_vec);

                // Recursively discover/declare more functions on this call path.
                let called_fn_env = g_env.get_function((*mod_id).qualified(*fun_id));
                self.declare_functions_walk(mod_env, &called_fn_env, types);
            }
        }
    }

    fn declare_move_function(
        &mut self,
        fn_env: &mm::FunctionEnv,
        tyvec: &[mty::Type],
        fn_data: &FunctionData,
        linkage: llvm::LLVMLinkage,
    ) {
        let ll_sym_name = fn_env.llvm_symbol_name(tyvec);
        let ll_fn = {
            let ll_fnty = {
                let ll_rty = match fn_data.return_types.len() {
                    0 => self.llvm_cx.void_type(),
                    1 => self.llvm_type_with_ty_params(&fn_data.return_types[0], tyvec),
                    _ => {
                        // Wrap multiple return values in a struct.
                        let tys: Vec<_> = fn_data
                            .return_types
                            .iter()
                            .map(|f| self.llvm_type_with_ty_params(f, tyvec))
                            .collect();
                        self.llvm_cx.get_anonymous_struct_type(&tys)
                    }
                };

                let ll_parm_tys = fn_env
                    .get_parameter_types()
                    .iter()
                    .map(|mty| self.llvm_type_with_ty_params(mty, tyvec))
                    .collect::<Vec<_>>();

                llvm::FunctionType::new(ll_rty, &ll_parm_tys)
            };

            // For Move functions we can infer directly from parameters that:
            // - `&` and `&mut` will be `nonnull` pointers in the generated LLVM IR.
            // - '&' is `readonly` (shared, read only).
            // - '&mut' is `noalias` (exclusive, writeable).
            // There are other attributes we may infer in the future with more analysis.
            let mut attrs = Vec::new();
            for (i, pt) in fn_env.get_parameter_types().iter().enumerate() {
                let parm_num = (i + 1) as u32;
                if pt.is_reference() {
                    attrs.push((parm_num, "nonnull", None));
                }
                if pt.is_immutable_reference() {
                    attrs.push((parm_num, "readonly", None));
                } else if pt.is_mutable_reference() {
                    attrs.push((parm_num, "noalias", None));
                }
            }
            let tfn = self.llvm_module.add_function(&ll_sym_name, ll_fnty);
            self.llvm_module.add_attributes(tfn, &attrs);
            tfn
        };

        ll_fn.as_gv().set_linkage(linkage);

        self.fn_decls.insert(ll_sym_name, ll_fn);
    }

    /// Declare native functions.
    ///
    /// Native functions are unlike Move functions in that they
    /// pass type descriptors for generics, and they follow
    /// the C ABI.
    ///
    /// Tweaks to the calling conventions here must be mirrored
    /// in `translate_native_fun_call.
    ///
    /// At some point we might want to factor out the platform-specific ABI
    /// decisions, but for now there are only a few ABI concerns, and we may
    /// never support another platform for which the ABI is different.
    fn declare_native_function(
        &mut self,
        fn_env: &mm::FunctionEnv,
        fn_data: &FunctionData,
        linkage: llvm::LLVMLinkage,
    ) {
        assert!(fn_env.is_native());

        let ll_native_sym_name = fn_env.llvm_native_fn_symbol_name();
        let ll_fn = {
            let ll_fnty = {
                // Generic return values are passed through a final return pointer arg.
                let (ll_rty, ll_byref_rty) = match fn_data.return_types.len() {
                    0 => (self.llvm_cx.void_type(), None),
                    1 => match fn_data.return_types[0] {
                        mty::Type::TypeParameter(_) => (
                            self.llvm_cx.void_type(),
                            Some(self.llvm_cx.int_type(8).ptr_type()),
                        ),
                        _ => (self.llvm_type(&fn_data.return_types[0]), None),
                    },
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
                let ll_parm_tys = ll_parm_tys.iter().map(|mty| {
                    // Pass type parameters and vectors as pointers
                    match mty {
                        mty::Type::TypeParameter(_) => self.llvm_type(mty).ptr_type(),
                        mty::Type::Vector(_) => self.llvm_type(mty).ptr_type(),
                        _ => self.llvm_type(mty),
                    }
                });

                let all_ll_parms = ll_tydesc_parms
                    .chain(ll_parm_tys)
                    .chain(ll_byref_rty)
                    .collect::<Vec<_>>();

                llvm::FunctionType::new(ll_rty, &all_ll_parms)
            };

            self.llvm_module.add_function(&ll_native_sym_name, ll_fnty)
        };

        ll_fn.as_gv().set_linkage(linkage);

        self.fn_decls.insert(ll_native_sym_name, ll_fn);
    }

    /// The type descriptor accepted by runtime functions.
    ///
    /// Corresponds to `move_native::rt_types::MoveType`.
    fn llvm_tydesc_type(&self) -> llvm::StructType {
        self.rtty_cx.get_llvm_tydesc_type()
    }

    pub fn llvm_type(&self, mty: &mty::Type) -> llvm::Type {
        use mty::{PrimitiveType, Type};

        match mty {
            Type::Primitive(PrimitiveType::Bool)
            | Type::Primitive(PrimitiveType::U8)
            | Type::Primitive(PrimitiveType::U16)
            | Type::Primitive(PrimitiveType::U32)
            | Type::Primitive(PrimitiveType::U64)
            | Type::Primitive(PrimitiveType::U128)
            | Type::Primitive(PrimitiveType::U256) => {
                self.llvm_cx.int_type(mty.get_bitwidth() as usize)
            }
            Type::Primitive(PrimitiveType::Address) => self.get_llvm_type_for_address(),
            Type::Primitive(PrimitiveType::Signer) => self.get_llvm_type_for_signer(),

            Type::Primitive(PrimitiveType::Num)
            | Type::Primitive(PrimitiveType::Range)
            | Type::Primitive(PrimitiveType::EventStore) => {
                panic!("{mty:?} only appears in specifications.")
            }

            Type::Reference(_, referent_mty) => {
                let referent_llty = self.llvm_type(referent_mty);
                referent_llty.ptr_type()
            }
            Type::TypeParameter(_) => {
                // this is ok for now, while type params are only passed by reference,
                // but might end up broken in the future.
                self.llvm_cx.int_type(8)
            }
            Type::Struct(declaring_module_id, struct_id, tys) => {
                let global_env = &self.env.env;
                let struct_env = global_env
                    .get_module(*declaring_module_id)
                    .into_struct(*struct_id);
                let struct_name = struct_env.ll_struct_name_from_raw_name(tys);
                if let Some(stype) = self.llvm_cx.named_struct_type(&struct_name) {
                    stype.as_any_type()
                } else {
                    unreachable!("struct type for '{}' not found", &struct_name);
                }
            }
            Type::Vector(_) => self.get_llvm_type_for_move_native_vector(),
            Type::Tuple(_) => {
                todo!("{mty:?}")
            }
            Type::Fun(_, _)
            | Type::TypeDomain(_)
            | Type::ResourceDomain(_, _, _)
            | Type::Error
            | Type::Var(_) => {
                panic!("unexpected field type {mty:?}")
            }
        }
    }

    pub fn get_llvm_type_for_move_native_vector(&self) -> llvm::Type {
        // The type of vectors is shared with move-native,
        // where it is declared as `MoveUntypedVector`.
        // All vectors are a C struct of ( ptr, u64, u64 ).
        self.llvm_cx.get_anonymous_struct_type(&[
            self.llvm_cx.int_type(8).ptr_type(),
            self.llvm_cx.int_type(64),
            self.llvm_cx.int_type(64),
        ])
    }

    pub fn get_llvm_type_for_address(&self) -> llvm::Type {
        self.llvm_cx.array_type(
            self.llvm_cx.int_type(8),
            account_address::AccountAddress::LENGTH,
        )
    }

    fn get_llvm_type_for_signer(&self) -> llvm::Type {
        // Create a type `{ [N x i8] }` (a struct wrapping an account address) corresponding
        // to `move_native::rt_types::MoveSigner`.
        let field_ty = self.get_llvm_type_for_address();
        self.llvm_cx.get_anonymous_struct_type(&[field_ty])
    }

    fn create_fn_context<'this>(
        &'this self,
        fn_env: mm::FunctionEnv<'mm>,
        module_cx: &'mm ModuleContext,
        type_params: &'mm [mty::Type],
    ) -> FunctionContext<'mm, 'this> {
        let locals = Vec::with_capacity(fn_env.get_local_count());
        FunctionContext {
            env: fn_env,
            module_cx,
            label_blocks: BTreeMap::new(),
            locals,
            type_params,
        }
    }

    pub fn get_rttydesc_ptrs(&self, types: &[mty::Type]) -> Vec<llvm::Constant> {
        let mut ll_global_ptrs = vec![];
        for type_ in types {
            let ll_tydesc = self.rtty_cx.define_llvm_tydesc(type_);
            ll_global_ptrs.push(ll_tydesc.ptr());
        }
        ll_global_ptrs
    }

    // This version is used in contexts where TempIndexes are not used and/or where the caller
    // expects a return value that it will decide how to use or store.
    pub fn emit_rtcall_with_retval(&self, rtcall: RtCall) -> llvm::AnyValue {
        match &rtcall {
            RtCall::VecCopy(ll_dst_value, ll_src_value, elt_mty) => {
                // Note, no retval from vec_copy.
                let llfn = self.get_runtime_function(&rtcall);
                let mut typarams: Vec<_> = self
                    .get_rttydesc_ptrs(&[elt_mty.clone()])
                    .iter()
                    .map(|llval| llval.as_any_value())
                    .collect();
                typarams.push(*ll_dst_value);
                typarams.push(*ll_src_value);
                self.llvm_builder.call(llfn, &typarams)
            }
            RtCall::VecCmpEq(ll_dst_value, ll_src_value, elt_mty) => {
                let llfn = self.get_runtime_function(&rtcall);
                let mut typarams: Vec<_> = self
                    .get_rttydesc_ptrs(&[elt_mty.clone()])
                    .iter()
                    .map(|llval| llval.as_any_value())
                    .collect();
                typarams.push(*ll_dst_value);
                typarams.push(*ll_src_value);
                self.llvm_builder.call(llfn, &typarams)
            }
            RtCall::VecEmpty(elt_mty) => {
                let llfn = self.get_runtime_function(&rtcall);
                let typarams: Vec<_> = self
                    .get_rttydesc_ptrs(&[elt_mty.clone()])
                    .iter()
                    .map(|llval| llval.as_any_value())
                    .collect();
                self.llvm_builder.call(llfn, &typarams)
            }
            RtCall::StructCmpEq(ll_src1_value, ll_src2_value, s_mty) => {
                let llfn = self.get_runtime_function(&rtcall);
                let mut typarams: Vec<_> = self
                    .get_rttydesc_ptrs(&[s_mty.clone()])
                    .iter()
                    .map(|llval| llval.as_any_value())
                    .collect();
                typarams.push(*ll_src1_value);
                typarams.push(*ll_src2_value);
                self.llvm_builder.call(llfn, &typarams)
            }
            _ => unreachable!(),
        }
    }

    pub fn emit_rtcall_abort_raw(&self, val: u64) {
        let thefn = self.get_runtime_function_by_name("abort");
        let param_ty = self.llvm_cx.int_type(64);
        let const_llval = llvm::Constant::int(param_ty, U256::from(val));
        self.llvm_builder.build_call_imm(thefn, &[const_llval]);
        self.llvm_builder.build_unreachable();
    }

    pub fn get_runtime_function(&self, rtcall: &RtCall) -> llvm::Function {
        let name = match rtcall {
            RtCall::Abort(..) => "abort",
            RtCall::VecDestroy(..) => "vec_destroy",
            RtCall::VecCopy(..) => "vec_copy",
            RtCall::VecCmpEq(..) => "vec_cmp_eq",
            RtCall::VecEmpty(..) => "vec_empty",
            RtCall::StructCmpEq(..) => "struct_cmp_eq",
        };
        self.get_runtime_function_by_name(name)
    }

    fn get_runtime_function_by_name(&self, rtcall_name: &str) -> llvm::Function {
        let fn_name = format!("move_rt_{rtcall_name}");
        let llmod = &self.llvm_module;
        let llcx = &self.llvm_cx;
        let llfn = llmod.get_named_function(&fn_name);
        if let Some(llfn) = llfn {
            llfn
        } else {
            let (llty, attrs) = match rtcall_name {
                "abort" => {
                    let ret_ty = llcx.void_type();
                    let param_tys = &[llcx.int_type(64)];
                    let llty = llvm::FunctionType::new(ret_ty, param_tys);
                    let attrs = vec![
                        (llvm::LLVMAttributeFunctionIndex, "noreturn", None),
                        (llvm::LLVMAttributeFunctionIndex, "cold", None),
                    ];
                    (llty, attrs)
                }
                "vec_destroy" => {
                    // vec_destroy(type_ve: &MoveType, v: MoveUntypedVector)
                    let ret_ty = llcx.void_type();
                    let tydesc_ty = llcx.int_type(8).ptr_type();
                    // The vector is passed by value, but the C ABI here passes structs by reference,
                    // so it's another pointer.
                    let vector_ty = llcx.int_type(8).ptr_type();
                    let param_tys = &[tydesc_ty, vector_ty];
                    let llty = llvm::FunctionType::new(ret_ty, param_tys);
                    let attrs = self.mk_pattrs_for_move_type(1);
                    (llty, attrs)
                }
                "vec_copy" => {
                    // vec_copy(type_ve: &MoveType, dstv: &mut MoveUntypedVector, srcv: &MoveUntypedVector)
                    let ret_ty = llcx.void_type();
                    let tydesc_ty = llcx.int_type(8).ptr_type();
                    // The vectors are passed by value, but the C ABI here passes structs by reference,
                    // so it's another pointer.
                    let vector_ty = llcx.int_type(8).ptr_type();
                    let param_tys = &[tydesc_ty, vector_ty, vector_ty];
                    let llty = llvm::FunctionType::new(ret_ty, param_tys);
                    let mut attrs = self.mk_pattrs_for_move_type(1);
                    attrs.extend(self.mk_pattrs_for_move_untyped_vec(2, true /* mut */));
                    attrs.extend(self.mk_pattrs_for_move_untyped_vec(3, false /* !mut */));
                    (llty, attrs)
                }
                "vec_cmp_eq" => {
                    // vec_cmp_eq(type_ve: &MoveType, v1: &MoveUntypedVector, v2: &MoveUntypedVector) -> bool
                    let ret_ty = llcx.int_type(1);
                    let tydesc_ty = llcx.int_type(8).ptr_type();
                    // The vectors are passed by value, but the C ABI here passes structs by reference,
                    // so it's another pointer.
                    let vector_ty = llcx.int_type(8).ptr_type();
                    let param_tys = &[tydesc_ty, vector_ty, vector_ty];
                    let llty = llvm::FunctionType::new(ret_ty, param_tys);
                    let mut attrs = self.mk_pattrs_for_move_type(1);
                    attrs.extend(self.mk_pattrs_for_move_untyped_vec(2, false /* !mut */));
                    attrs.extend(self.mk_pattrs_for_move_untyped_vec(3, false /* !mut */));
                    (llty, attrs)
                }
                "vec_empty" => {
                    // vec_empty(type_ve: &MoveType) -> MoveUntypedVector
                    let ret_ty = self.get_llvm_type_for_move_native_vector();
                    let tydesc_ty = llcx.int_type(8).ptr_type();
                    let param_tys = &[tydesc_ty];
                    let llty = llvm::FunctionType::new(ret_ty, param_tys);
                    let attrs = self.mk_pattrs_for_move_type(1);
                    (llty, attrs)
                }
                "struct_cmp_eq" => {
                    // struct_cmp_eq(type_ve: &MoveType, s1: &AnyValue, s2: &AnyValue) -> bool;
                    let ret_ty = llcx.int_type(1);
                    let tydesc_ty = llcx.int_type(8).ptr_type();
                    let anyval_ty = llcx.int_type(8).ptr_type();
                    let param_tys = &[tydesc_ty, anyval_ty, anyval_ty];
                    let llty = llvm::FunctionType::new(ret_ty, param_tys);
                    let mut attrs = self.mk_pattrs_for_move_type(1);
                    attrs.push((2, "readonly", None));
                    attrs.push((2, "nonnull", None));
                    attrs.push((3, "readonly", None));
                    attrs.push((3, "nonnull", None));
                    (llty, attrs)
                }
                n => panic!("unknown runtime function {n}"),
            };

            let ll_fn = llmod.add_function(&fn_name, llty);
            llmod.add_attributes(ll_fn, &attrs);
            ll_fn
        }
    }

    fn mk_pattrs_for_move_type(
        &self,
        attr_idx: llvm::LLVMAttributeIndex,
    ) -> Vec<(llvm::LLVMAttributeIndex, &'static str, Option<u64>)> {
        assert!(
            attr_idx != llvm::LLVMAttributeReturnIndex
                && attr_idx != llvm::LLVMAttributeFunctionIndex
        );
        vec![
            (attr_idx, "readonly", None),
            (attr_idx, "nonnull", None),
            (attr_idx, "dereferenceable", Some(MOVE_TYPE_DESC_SIZE)),
        ]
    }

    fn mk_pattrs_for_move_untyped_vec(
        &self,
        attr_idx: llvm::LLVMAttributeIndex,
        mutable: bool,
    ) -> Vec<(llvm::LLVMAttributeIndex, &'static str, Option<u64>)> {
        assert!(
            attr_idx != llvm::LLVMAttributeReturnIndex
                && attr_idx != llvm::LLVMAttributeFunctionIndex
        );
        let mut attrs = vec![
            (attr_idx, "nonnull", None),
            (
                attr_idx,
                "dereferenceable",
                Some(MOVE_UNTYPED_VEC_DESC_SIZE),
            ),
        ];
        if !mutable {
            attrs.push((attr_idx, "readonly", None));
        }
        attrs
    }
}
