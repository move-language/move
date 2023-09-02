// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    options::Options,
    stackless::{
        extensions::*, llvm, llvm::TargetMachine, rttydesc::RttyContext, FunctionContext, RtCall,
        TargetPlatform,
    },
};
use log::{debug, log_enabled, Level};
use move_binary_format::file_format::SignatureToken;
use move_core_types::u256::U256;
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
    pub options: &'up Options,
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

        if !self.env.is_script_module() {
            self.emit_solana_entrypoint();
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

        debug!(target: "structs",
               "Combined list of all structs{}",
               self.dump_all_structs(&all_structs, false),
        );

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
            // Skip the structs that are not fully concretized,
            // i.e. any of the type parameters is not bound to
            // a concrete type.
            if Self::is_generic_struct(tys) {
                return false;
            }
            let ll_name = s_env.ll_struct_name_from_raw_name(tys);
            if self.llvm_cx.named_struct_type(&ll_name).is_none() {
                debug!(target: "structs", "Create struct {}", &ll_name);
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
            if create_opaque_named_struct(&s_env, &tys) {
                all_structs.push((s_env, tys));
            }
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

        debug!(target: "structs",
               "Structs after visiting the signature table{}",
               self.dump_all_structs(&all_structs, false),
        );

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
            self.translate_struct(s_env, tyvec);
        }

        debug!(
            target: "structs",
            "Structs after translation{}",
            self.dump_all_structs(&all_structs, true),
        );
    }

    // Translate struct declaration for structs parameterized by
    // nested struct types.
    // TODO: this probbaly doesn't work when other parameterized types
    // are mixed in the nesting of type parameters,
    // e.g. Struct_A<Vector<Struct_B<T>>>, where T is substituted by a
    // concrete type, won't be declared correctly.
    fn translate_struct(&self, s_env: &mm::StructEnv<'mm>, tyvec: &[mty::Type]) {
        let ll_name = s_env.ll_struct_name_from_raw_name(tyvec);
        debug!(target: "structs", "translating struct {}", s_env.struct_raw_type_name(tyvec));
        // Visit each field in this struct, collecting field types.
        let mut ll_field_tys = Vec::with_capacity(s_env.get_field_count() + 1);
        for fld_env in s_env.get_fields() {
            debug!(target: "structs", "translating field {:?}", &fld_env.get_type());
            if let mty::Type::Struct(_m, _s, _tys) = &fld_env.get_type() {
                let new_sty = &fld_env.get_type().instantiate(tyvec);
                if let mty::Type::Struct(m, s, tys) = new_sty {
                    let g_env = &self.env.env;
                    let s_env = g_env.get_module(*m).into_struct(*s);
                    self.translate_struct(&s_env, tys);
                }
            } else if let mty::Type::TypeParameter(x) = &fld_env.get_type() {
                if let mty::Type::Struct(m, s, tys) = &tyvec[*x as usize] {
                    let g_env = &self.env.env;
                    let s_env = g_env.get_module(*m).into_struct(*s);
                    self.translate_struct(&s_env, tys);
                }
            }
            let ll_fld_type = self.to_llvm_type(&fld_env.get_type(), tyvec).unwrap();
            debug!(
                target: "structs",
                "Field now should be concrete type for {ll_name} : {}",
                ll_fld_type.print_to_str()
            );
            ll_field_tys.push(ll_fld_type);
        }
        debug!(target: "structs", "Finished translating fields for {ll_name}");
        if self.llvm_cx.named_struct_type(&ll_name).is_none() {
            debug!(target: "structs", "Create struct {}", &ll_name);
            self.llvm_cx.create_opaque_named_struct(&ll_name);
        }
        let ll_sty = self
            .llvm_cx
            .named_struct_type(&ll_name)
            .expect("no struct type");
        ll_sty.set_struct_body(&ll_field_tys);
    }

    // This method is used to declare structs found when function
    // declrations are generated and new instantiations of generic
    // structs become known.
    // TODO: porbably other parameterized types such as Vector should
    // be handled by this function too.
    fn declare_struct_instance(&self, mty: &mty::Type, tyvec: &[mty::Type]) -> llvm::Type {
        if let mty::Type::Struct(m, s, _tys) = mty {
            let g_env = &self.env.env;
            let s_env = g_env.get_module(*m).into_struct(*s);
            self.translate_struct(&s_env, tyvec);
            self.to_llvm_type(mty, tyvec).unwrap()
        } else {
            unreachable!("Failed to declare a struct {mty:?}")
        }
    }

    fn is_generic_struct(tys: &[mty::Type]) -> bool {
        tys.iter().any(|t| match t {
            mty::Type::Reference(_, ty) => Self::is_generic_struct(&[ty.as_ref().clone()]),
            mty::Type::Struct(_m, _s, tys) => Self::is_generic_struct(tys),
            mty::Type::Tuple(tys) => Self::is_generic_struct(tys),
            mty::Type::TypeParameter(_) => true,
            mty::Type::Vector(ty) => Self::is_generic_struct(&[ty.as_ref().clone()]),
            _ => false,
        })
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
                    if let Some(ll_fld_type) = self.to_llvm_type(&fld_env.get_type(), tyvec) {
                        s += ll_fld_type.print_to_str();
                    } else {
                        s += "<unresolved>";
                    }
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
        debug!("Declare Move function {ll_sym_name}");
        let ll_fn = {
            let ll_fnty = {
                let ll_rty = match fn_data.return_types.len() {
                    0 => self.llvm_cx.void_type(),
                    1 => {
                        if let Some(ty) = self.to_llvm_type(&fn_data.return_types[0], tyvec) {
                            ty
                        } else {
                            self.declare_struct_instance(&fn_data.return_types[0], tyvec)
                        }
                    }
                    _ => {
                        // Wrap multiple return values in a struct.
                        let tys: Vec<_> = fn_data
                            .return_types
                            .iter()
                            .map(|f| self.to_llvm_type(f, tyvec).unwrap())
                            .collect();
                        self.llvm_cx.get_anonymous_struct_type(&tys)
                    }
                };

                let ll_parm_tys = fn_env
                    .get_parameter_types()
                    .iter()
                    .map(|mty| {
                        if let Some(ty) = self.to_llvm_type(mty, tyvec) {
                            ty
                        } else {
                            self.declare_struct_instance(mty, tyvec)
                        }
                    })
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

        let llcx = &self.llvm_cx;
        let ll_native_sym_name = fn_env.llvm_native_fn_symbol_name();
        let ll_fn = {
            let ll_fnty = {
                // Generic return values are passed through a final return pointer arg.
                let (ll_rty, ll_byref_rty) = match fn_data.return_types.len() {
                    0 => (llcx.void_type(), None),
                    1 => {
                        let mty0 = &fn_data.return_types[0];
                        if mty0.is_type_parameter() {
                            (llcx.void_type(), Some(llcx.ptr_type()))
                        } else {
                            (self.to_llvm_type(mty0, &[]).unwrap(), None)
                        }
                    }
                    _ => {
                        todo!()
                    }
                };

                // Native functions take type parameters as the
                // first arguments.
                let num_typarams = fn_env.get_type_parameter_count();
                let ll_tydesc_parms = iter::repeat(llcx.ptr_type()).take(num_typarams);

                let ll_parm_tys = fn_env.get_parameter_types();
                let ll_parm_tys = ll_parm_tys.iter().map(|mty| {
                    // Pass type parameters and vectors as pointers
                    if mty.is_type_parameter() || mty.is_vector() {
                        llcx.ptr_type()
                    } else {
                        self.to_llvm_type(mty, &[]).unwrap()
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

    pub fn lookup_move_fn_decl(&self, qiid: mm::QualifiedInstId<mm::FunId>) -> llvm::Function {
        let fn_env = self
            .env
            .env
            .get_module(qiid.module_id)
            .into_function(qiid.id);
        let sname = fn_env.llvm_symbol_name(&qiid.inst);
        let decl = self.fn_decls.get(&sname);
        assert!(decl.is_some(), "move fn decl not found: {}", sname);
        *decl.unwrap()
    }

    pub fn lookup_native_fn_decl(&self, qid: mm::QualifiedId<mm::FunId>) -> llvm::Function {
        let fn_env = self.env.env.get_module(qid.module_id).into_function(qid.id);
        let sname = fn_env.llvm_native_fn_symbol_name();
        let decl = self.fn_decls.get(&sname);
        assert!(decl.is_some(), "native fn decl not found: {}", sname);
        *decl.unwrap()
    }

    pub fn to_llvm_type(&self, mty: &mty::Type, tyvec: &[mty::Type]) -> Option<llvm::Type> {
        use mty::{PrimitiveType, Type};

        match mty {
            Type::Primitive(PrimitiveType::Bool)
            | Type::Primitive(PrimitiveType::U8)
            | Type::Primitive(PrimitiveType::U16)
            | Type::Primitive(PrimitiveType::U32)
            | Type::Primitive(PrimitiveType::U64)
            | Type::Primitive(PrimitiveType::U128)
            | Type::Primitive(PrimitiveType::U256) => {
                Some(self.llvm_cx.int_type(mty.get_bitwidth() as usize))
            }
            Type::Primitive(PrimitiveType::Address) => {
                Some(self.rtty_cx.get_llvm_type_for_address())
            }
            Type::Primitive(PrimitiveType::Signer) => Some(self.rtty_cx.get_llvm_type_for_signer()),
            Type::Primitive(PrimitiveType::Num)
            | Type::Primitive(PrimitiveType::Range)
            | Type::Primitive(PrimitiveType::EventStore) => {
                panic!("{mty:?} only appears in specifications.")
            }
            Type::Reference(_, _) => Some(self.llvm_cx.ptr_type()),
            Type::TypeParameter(tp_idx) => {
                if (*tp_idx as usize) < tyvec.len() {
                    self.to_llvm_type(&tyvec[*tp_idx as usize], &[])
                } else {
                    debug!("type parameter index is out of range {tp_idx}");
                    None
                }
            }
            Type::Struct(_mid, _sid, _tys) => {
                // First substitute any generic type parameters occuring in _tys.
                let new_sty = mty.instantiate(tyvec);

                debug!(
                    target: "structs",
                    "Instantiated struct {}",
                    new_sty
                        .get_struct(self.env.env)
                        .unwrap()
                        .0
                        .struct_raw_type_name(tyvec)
                );
                // Then process the (possibly type-substituted) struct.
                if let Type::Struct(declaring_module_id, struct_id, tys) = new_sty {
                    let global_env = &self.env.env;
                    let struct_env = global_env
                        .get_module(declaring_module_id)
                        .into_struct(struct_id);
                    let struct_name = struct_env.ll_struct_name_from_raw_name(&tys);
                    if let Some(stype) = self.llvm_cx.named_struct_type(&struct_name) {
                        Some(stype.as_any_type())
                    } else {
                        debug!(target: "structs", "struct type for '{}' not found", &struct_name);
                        None
                    }
                } else {
                    unreachable!("")
                }
            }
            Type::Vector(_) => Some(self.rtty_cx.get_llvm_type_for_move_native_vector()),
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
            RtCall::StrCmpEq(str1_ptr, str1_len, str2_ptr, str2_len) => {
                let llfn = self.get_runtime_function(&rtcall);
                let params = vec![*str1_ptr, *str1_len, *str2_ptr, *str2_len];
                self.llvm_builder.call(llfn, &params)
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
            RtCall::Deserialize(..) => "deserialize",
            RtCall::VecDestroy(..) => "vec_destroy",
            RtCall::VecCopy(..) => "vec_copy",
            RtCall::VecCmpEq(..) => "vec_cmp_eq",
            RtCall::VecEmpty(..) => "vec_empty",
            RtCall::StrCmpEq(..) => "str_cmp_eq",
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
                "deserialize" => {
                    let ret_ty = llcx.void_type();
                    let ptr_ty = llcx.ptr_type();
                    let int_ty = llcx.int_type(64);
                    let param_tys = &[ptr_ty, ptr_ty];
                    let ll_sret = llcx.get_anonymous_struct_type(&[
                        llcx.get_anonymous_struct_type(&[ptr_ty, int_ty]),
                        ptr_ty,
                        llcx.get_anonymous_struct_type(&[ptr_ty, int_ty, int_ty]),
                    ]);
                    let llty = llvm::FunctionType::new(ret_ty, param_tys);
                    let ll_fn = llmod.add_function(&fn_name, llty);
                    self.llvm_module
                        .add_type_attribute(ll_fn, 1, "sret", ll_sret);
                    return ll_fn;
                }
                "vec_destroy" => {
                    // vec_destroy(type_ve: &MoveType, v: MoveUntypedVector)
                    let ret_ty = llcx.void_type();
                    let tydesc_ty = llcx.ptr_type();
                    // The vector is passed by value, but the C ABI here passes structs by reference,
                    // so it's another pointer.
                    let vector_ty = llcx.ptr_type();
                    let param_tys = &[tydesc_ty, vector_ty];
                    let llty = llvm::FunctionType::new(ret_ty, param_tys);
                    let attrs = self.mk_pattrs_for_move_type(1);
                    (llty, attrs)
                }
                "vec_copy" => {
                    // vec_copy(type_ve: &MoveType, dstv: &mut MoveUntypedVector, srcv: &MoveUntypedVector)
                    let ret_ty = llcx.void_type();
                    let tydesc_ty = llcx.ptr_type();
                    // The vectors are passed by value, but the C ABI here passes structs by reference,
                    // so it's another pointer.
                    let vector_ty = llcx.ptr_type();
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
                    let tydesc_ty = llcx.ptr_type();
                    // The vectors are passed by value, but the C ABI here passes structs by reference,
                    // so it's another pointer.
                    let vector_ty = llcx.ptr_type();
                    let param_tys = &[tydesc_ty, vector_ty, vector_ty];
                    let llty = llvm::FunctionType::new(ret_ty, param_tys);
                    let mut attrs = self.mk_pattrs_for_move_type(1);
                    attrs.extend(self.mk_pattrs_for_move_untyped_vec(2, false /* !mut */));
                    attrs.extend(self.mk_pattrs_for_move_untyped_vec(3, false /* !mut */));
                    (llty, attrs)
                }
                "vec_empty" => {
                    // vec_empty(type_ve: &MoveType) -> MoveUntypedVector
                    let ret_ty = self.rtty_cx.get_llvm_type_for_move_native_vector();
                    let tydesc_ty = llcx.ptr_type();
                    let param_tys = &[tydesc_ty];
                    let llty = llvm::FunctionType::new(ret_ty, param_tys);
                    let attrs = self.mk_pattrs_for_move_type(1);
                    (llty, attrs)
                }
                "str_cmp_eq" => {
                    // str_cmp_eq(str1_ptr: &AnyValue, str1_len: &AnyValue,
                    //            str2_ptr: &AnyValue, str1_len: &AnyValue) -> bool
                    let ret_ty = llcx.int_type(1);
                    let ptr_ty = llcx.ptr_type();
                    let len_ty = llcx.int_type(64);
                    let param_tys = &[ptr_ty, len_ty, ptr_ty, len_ty];
                    let llty = llvm::FunctionType::new(ret_ty, param_tys);
                    let attrs = vec![
                        (1, "readonly", None),
                        (1, "nonnull", None),
                        (3, "readonly", None),
                        (3, "nonnull", None),
                    ];
                    (llty, attrs)
                }
                "struct_cmp_eq" => {
                    // struct_cmp_eq(type_ve: &MoveType, s1: &AnyValue, s2: &AnyValue) -> bool;
                    let ret_ty = llcx.int_type(1);
                    let tydesc_ty = llcx.ptr_type();
                    let anyval_ty = llcx.ptr_type();
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

    // This function extracts an entry function actual arguments from
    // instruction_data byte array, containing values of actual
    // arguments in sequential order without gaps.
    fn emit_entry_arguments(
        &self,
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
                    index_value = self.advance_offset_by_increment(
                        *index,
                        llvm::Constant::int(i64_ty, U256::one()).as_any_value(),
                    );
                }
                mty::Type::Reference(_, ty) => {
                    index_value = self.advance_offset_by_increment(
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
                    index_value = self.advance_offset_by_increment(
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
                    index_value = self.advance_offset_by_increment(*index, vec_data_len);
                }
                mty::Type::Struct(mid, sid, _) => {
                    arg = self.llvm_builder.load(
                        arg,
                        self.to_llvm_type(&ty, &[]).unwrap(),
                        "str_arg",
                    );
                    let m_env = &self.env;
                    let g_env = &m_env.env;
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
                    index_value = self.advance_offset_by_increment(*index, size);
                }
                _ => {
                    arg = self
                        .llvm_builder
                        .load(arg, self.to_llvm_type(&ty, &[]).unwrap(), "arg");
                    let size = llvm::Constant::int(i64_ty, U256::from(ty.get_bitwidth() / 8))
                        .as_any_value();
                    index_value = self.advance_offset_by_increment(*index, size);
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

    fn emit_rtcall_deserialize(
        &self,
        input: llvm::AnyValue,
    ) -> (llvm::AnyValue, llvm::AnyValue, llvm::AnyValue) {
        let llcx = self.llvm_cx;
        let ll_sret = llcx.get_anonymous_struct_type(&[
            self.rtty_cx.get_llvm_type_for_slice(),
            llcx.ptr_type(),
            self.rtty_cx.get_llvm_type_for_move_native_vector(),
        ]);
        let params = self
            .llvm_builder
            .build_alloca(ll_sret, "params")
            .as_any_value();
        let args = vec![params, input];
        let ll_fn_deserialize = self.get_runtime_function(&RtCall::Deserialize(params, input));
        self.llvm_builder.call(ll_fn_deserialize, &args);

        let insn_data = self.llvm_builder.getelementptr(
            params,
            &ll_sret.as_struct_type(),
            0,
            "instruction_data",
        );
        let program_id =
            self.llvm_builder
                .getelementptr(params, &ll_sret.as_struct_type(), 1, "program_id");
        let accounts =
            self.llvm_builder
                .getelementptr(params, &ll_sret.as_struct_type(), 2, "accounts");
        (insn_data, program_id, accounts)
    }

    fn advance_offset_by_increment(
        &self,
        offset: llvm::AnyValue,
        increment: llvm::AnyValue,
    ) -> llvm::AnyValue {
        let offset_loaded =
            self.llvm_builder
                .load(offset, self.llvm_cx.int_type(64), "offset_loaded");
        let offset_loaded = self.llvm_builder.build_binop(
            llvm_sys::LLVMOpcode::LLVMAdd,
            offset_loaded,
            increment,
            "offset_loaded",
        );
        self.llvm_builder.store(offset_loaded, offset);
        offset_loaded
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
    fn emit_solana_entrypoint(&mut self) {
        let entry_functions: Vec<_> = self
            .env
            .get_functions()
            .filter(|fn_env| fn_env.is_entry())
            .collect();

        // Do not generate solana entrypoint if module doesn't contain any entry functions.
        if entry_functions.is_empty() {
            return;
        }

        let ll_fn_solana_entrypoint = {
            let ll_fnty = {
                let ll_rty = self.llvm_cx.int_type(64_usize);
                let ll_param_tys = vec![self.llvm_cx.ptr_type()];
                llvm::FunctionType::new(ll_rty, &ll_param_tys)
            };
            self.llvm_module.add_function("main", ll_fnty)
        };
        let entry_block = ll_fn_solana_entrypoint.append_basic_block("entry");
        self.llvm_builder.position_at_end(entry_block);
        let retval = self
            .llvm_builder
            .build_alloca(self.llvm_cx.int_type(64), "retval")
            .as_any_value();
        let offset = self
            .llvm_builder
            .build_alloca(self.llvm_cx.int_type(64), "offset");
        self.llvm_builder.store_const(
            llvm::Constant::int(self.llvm_cx.int_type(64), U256::zero()),
            offset,
        );

        // Get inputs from the VM into proper data structures.
        let (insn_data, _program_id, _accounts) =
            self.emit_rtcall_deserialize(ll_fn_solana_entrypoint.get_param(0).as_any_value());
        // Make a str slice from instruction_data byte array returned
        // from a call to deserialize
        let str_slice_type = self.rtty_cx.get_llvm_type_for_slice();
        let insn_data_ptr = self.llvm_builder.getelementptr(
            insn_data,
            &str_slice_type.as_struct_type(),
            0,
            "insn_data_ptr",
        );
        let insn_data_ptr = self.llvm_builder.load(
            insn_data_ptr,
            self.llvm_cx.ptr_type(),
            "insn_data_ptr_loaded",
        );
        let offset_value = self.advance_offset_by_increment(
            offset.as_any_value(),
            llvm::Constant::int(self.llvm_cx.int_type(64), U256::from(8u64)).as_any_value(),
        );
        let entry_slice_ptr = self.llvm_builder.build_address_with_indices(
            self.llvm_cx.int_type(8),
            insn_data_ptr,
            &[offset_value],
            "entry_slice_ptr",
        );
        let entry_slice_len =
            self.llvm_builder
                .load(insn_data_ptr, self.llvm_cx.int_type(64), "entry_slice_len");
        let _offset_value =
            self.advance_offset_by_increment(offset.as_any_value(), entry_slice_len);

        let curr_bb = self.llvm_builder.get_insert_block();
        let exit_bb = ll_fn_solana_entrypoint.insert_basic_block_after(curr_bb, "exit_bb");
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
            let condition = self.emit_rtcall_with_retval(RtCall::StrCmpEq(
                entry_slice_ptr,
                entry_slice_len,
                func_name_ptr,
                func_name_len,
            ));

            let curr_bb = self.llvm_builder.get_insert_block();
            let then_bb = ll_fn_solana_entrypoint.insert_basic_block_after(curr_bb, "then_bb");
            let else_bb = ll_fn_solana_entrypoint.insert_basic_block_after(then_bb, "else_bb");
            self.llvm_builder.build_cond_br(condition, then_bb, else_bb);
            self.llvm_builder.position_at_end(then_bb);
            let fn_name = fun.llvm_symbol_name(&[]);
            let ll_fun = self.fn_decls.get(&fn_name).unwrap();
            let params = self.emit_entry_arguments(&fun, &insn_data_ptr, &offset.as_any_value());
            let ret = self.llvm_builder.call(*ll_fun, &params);
            if fun.get_return_count() > 0 {
                self.llvm_builder.store(ret, retval);
            } else {
                self.llvm_builder.store(
                    llvm::Constant::int(self.llvm_cx.int_type(64), U256::zero()).as_any_value(),
                    retval,
                );
            }
            self.llvm_builder.build_br(exit_bb);
            self.llvm_builder.position_at_end(else_bb);
        }
        // Abort if no entry function matched the requested name.
        self.emit_rtcall_abort_raw(move_core_types::vm_status::StatusCode::EXECUTE_ENTRY_FUNCTION_CALLED_ON_NON_ENTRY_FUNCTION as u64);
        self.llvm_builder.position_at_end(exit_bb);
        let ret = self
            .llvm_builder
            .load(retval, self.llvm_cx.int_type(64), "exit_code");
        self.llvm_builder.build_return(ret);
        ll_fn_solana_entrypoint.verify();

        if log_enabled!(target: "entry_point", Level::Debug) {
            self.llvm_module.dump();
        }
    }
}
