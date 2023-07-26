// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! The type descriptor accepted by runtime functions.
//!
//! Corresponds to `move_native::rt_types::MoveType`.

#![allow(unused)]

use crate::stackless::{
    extensions::{StructEnvExt, TypeExt},
    llvm,
    module_context::ModuleContext,
};
use log::{debug, Level};
use move_core_types::{account_address, u256::U256};
use move_model::{ast as mast, model as mm, ty as mty};
use move_native::shared::TypeDesc;

static TD_NAME: &str = "__move_rt_type";
static TD_TYPE_NAME_NAME: &str = "__move_rt_type_name";
static TD_TYPE_INFO_NAME: &str = "__move_rt_type_info";
static TD_VECTOR_TYPE_INFO_NAME: &str = "__move_rt_type_info_vec";
static TD_STRUCT_TYPE_INFO_NAME: &str = "__move_rt_type_info_struct";
static TD_REFERENCE_TYPE_INFO_NAME: &str = "__move_rt_type_info_ref";

pub struct RttyContext<'mm, 'up> {
    m_env: mm::ModuleEnv<'mm>,
    llvm_cx: &'up llvm::Context,
    llvm_module: &'up llvm::Module,
    f_env: Option<mm::FunctionEnv<'mm>>,
}

impl<'mm, 'up> RttyContext<'mm, 'up> {
    pub fn new(
        env: mm::ModuleEnv<'mm>,
        llvm_cx: &'up llvm::Context,
        llmod: &'up llvm::Module,
    ) -> RttyContext<'mm, 'up> {
        RttyContext {
            m_env: env,
            llvm_cx,
            llvm_module: llmod,
            f_env: None,
        }
    }

    pub fn reset_func(&mut self, fn_qiid: &mm::QualifiedInstId<mm::FunId>) {
        self.f_env = Some(self.m_env.env.get_function(fn_qiid.to_qualified_id()));
    }

    fn get_llvm_cx(&self) -> &llvm::Context {
        self.llvm_cx
    }

    fn get_llvm_module(&self) -> &llvm::Module {
        self.llvm_module
    }

    fn get_global_env(&self) -> &mm::GlobalEnv {
        self.m_env.env
    }

    pub fn get_llvm_type_for_move_native_vector(&self) -> llvm::Type {
        // The type of vectors is shared with move-native,
        // where it is declared as `MoveUntypedVector`.
        // All vectors are a C struct of ( ptr, u64, u64 ).
        let llcx = &self.get_llvm_cx();
        llcx.get_anonymous_struct_type(&[
            llcx.int_type(8).ptr_type(),
            llcx.int_type(64),
            llcx.int_type(64),
        ])
    }

    pub fn get_llvm_type_for_address(&self) -> llvm::Type {
        // Create a type `[N x i8]` (an account address) corresponding
        // to `move_native::rt_types::MoveAddress`.
        let llcx = &self.get_llvm_cx();
        llcx.array_type(llcx.int_type(8), account_address::AccountAddress::LENGTH)
    }

    pub fn get_llvm_type_for_signer(&self) -> llvm::Type {
        // Create a type `{ [N x i8] }` (a struct wrapping an account address) corresponding
        // to `move_native::rt_types::MoveSigner`.
        let field_ty = self.get_llvm_type_for_address();
        self.get_llvm_cx().get_anonymous_struct_type(&[field_ty])
    }

    pub fn get_llvm_tydesc_type(&self) -> llvm::StructType {
        let llcx = &self.get_llvm_cx();
        match llcx.named_struct_type(TD_NAME) {
            Some(t) => t,
            None => {
                self.declare_llvm_tydesc_type();
                llcx.named_struct_type(TD_NAME).expect(".")
            }
        }
    }

    fn declare_llvm_tydesc_type(&self) {
        let llcx = &self.get_llvm_cx();
        let td_llty = llcx.create_opaque_named_struct(TD_NAME);
        let field_tys = {
            let type_name_ty = llcx
                .anonymous_struct_type(&[llcx.int_type(8).ptr_type(), llcx.int_type(64)])
                .as_any_type();
            let type_descrim_ty = llcx.int_type(64);
            // This is a pointer to a statically-defined union of type infos
            let type_info_ptr_ty = llcx.int_type(8).ptr_type();
            &[type_name_ty, type_descrim_ty, type_info_ptr_ty]
        };

        td_llty.set_struct_body(field_tys);
    }

    pub fn define_llvm_tydesc(&self, mty: &mty::Type) -> llvm::Global {
        let llmod = &self.get_llvm_module();
        let name = self.global_tydesc_name(mty);
        match llmod.get_global(&name) {
            Some(g) => g,
            None => {
                let ll_tydesc_ty = self.get_llvm_tydesc_type();
                let ll_tydesc_ty = ll_tydesc_ty.as_any_type();
                let ll_global = llmod.add_global(ll_tydesc_ty, &name);
                ll_global.set_constant();
                ll_global.set_linkage(llvm::LLVMLinkage::LLVMPrivateLinkage);
                ll_global.set_unnamed_addr();
                let ll_constant = self.tydesc_constant(mty);
                let ll_constant_ty = ll_constant.llvm_type();
                ll_global.set_initializer(ll_constant);
                ll_global
            }
        }
    }

    fn tydesc_constant(&self, mty: &mty::Type) -> llvm::Constant {
        let llcx = &self.get_llvm_cx();
        let ll_const_type_name = self.type_name_constant(mty);
        let ll_const_type_descrim = {
            let ll_ty = llcx.int_type(64);
            llvm::Constant::int(ll_ty, U256::from(Self::type_descrim(mty)))
        };
        let ll_const_type_info_ptr = {
            let ll_global_type_info = self.define_type_info_global(mty);
            ll_global_type_info.ptr()
        };
        llcx.const_named_struct(
            &[
                ll_const_type_name,
                ll_const_type_descrim,
                ll_const_type_info_ptr,
            ],
            TD_NAME,
        )
    }

    fn type_name_constant(&self, mty: &mty::Type) -> llvm::Constant {
        let llcx = &self.get_llvm_cx();
        let llmod = &self.get_llvm_module();
        let name = self.type_name(mty);
        let len = name.len();

        // Create a static string and take a constant pointer to it.
        let ll_static_bytes_ptr = {
            let global_name = self.global_tydesc_name_name(mty);
            match llmod.get_global(&global_name) {
                Some(g) => g.ptr(),
                None => {
                    let ll_const_string = llcx.const_string(&name);
                    let ll_array_ty = ll_const_string.llvm_type();
                    let ll_global = llmod.add_global(ll_array_ty, &global_name);
                    ll_global.set_constant();
                    ll_global.set_linkage(llvm::LLVMLinkage::LLVMPrivateLinkage);
                    ll_global.set_unnamed_addr();
                    ll_global.set_initializer(ll_const_string.as_const());
                    ll_global.ptr()
                }
            }
        };

        let ll_ty_u64 = llcx.int_type(64);
        let ll_const_len = llvm::Constant::int(ll_ty_u64, U256::from(len as u128));

        llcx.const_struct(&[ll_static_bytes_ptr, ll_const_len])
    }

    fn type_name(&self, mty: &mty::Type) -> String {
        let g_env = &self.get_global_env();
        let tmty = mty.clone();
        tmty.into_type_tag(g_env)
            .expect("type tag")
            .to_canonical_string()
    }

    /// The values here correspond to `move_native::rt_types::TypeDesc`.
    fn type_descrim(mty: &mty::Type) -> u64 {
        use mty::{PrimitiveType, Type};
        match mty {
            Type::Primitive(PrimitiveType::Bool) => TypeDesc::Bool as u64,
            Type::Primitive(PrimitiveType::U8) => TypeDesc::U8 as u64,
            Type::Primitive(PrimitiveType::U16) => TypeDesc::U16 as u64,
            Type::Primitive(PrimitiveType::U32) => TypeDesc::U32 as u64,
            Type::Primitive(PrimitiveType::U64) => TypeDesc::U64 as u64,
            Type::Primitive(PrimitiveType::U128) => TypeDesc::U128 as u64,
            Type::Primitive(PrimitiveType::U256) => TypeDesc::U256 as u64,
            Type::Primitive(PrimitiveType::Address) => TypeDesc::Address as u64,
            Type::Primitive(PrimitiveType::Signer) => TypeDesc::Signer as u64,
            Type::Vector(_) => TypeDesc::Vector as u64,
            Type::Struct(_, _, _) => TypeDesc::Struct as u64,
            _ => todo!("{:?}", mty),
        }
    }

    /// The "type info" for a Move type.
    ///
    /// This is the type-specific metadata interpreted by the runtime.
    /// It is a union.
    /// It corresponds to `move_native:rt_types::TypeInfo`.
    fn define_type_info_global(&self, mty: &mty::Type) -> llvm::Global {
        let symbol_name = self.global_tydesc_info_name(mty);
        let llmod = &self.get_llvm_module();

        match llmod.get_global(&symbol_name) {
            Some(g) => g,
            None => {
                use mty::{PrimitiveType, Type};
                match mty {
                    _ if !Self::has_type_info(mty) => {
                        self.define_type_info_global_nil(&symbol_name)
                    }
                    Type::Vector(elt_ty) => match **elt_ty {
                        Type::Primitive(PrimitiveType::Bool)
                        | Type::Primitive(PrimitiveType::U8)
                        | Type::Primitive(PrimitiveType::U16)
                        | Type::Primitive(PrimitiveType::U32)
                        | Type::Primitive(PrimitiveType::U64)
                        | Type::Primitive(PrimitiveType::U128)
                        | Type::Primitive(PrimitiveType::U256)
                        | Type::Primitive(PrimitiveType::Address)
                        | Type::Primitive(PrimitiveType::Signer)
                        | Type::Vector(_)
                        | Type::Struct(_, _, _) => {
                            self.define_type_info_global_vec(&symbol_name, elt_ty)
                        }
                        _ => todo!("{:?}", mty),
                    },
                    Type::Struct(_, _, _) => self.define_type_info_global_struct(&symbol_name, mty),
                    _ => todo!("{:?}", mty),
                }
            }
        }
    }

    /// A special type info for all types that don't need type info.
    fn define_type_info_global_nil(&self, symbol_name: &str) -> llvm::Global {
        let llcx = &self.get_llvm_cx();
        let llmod = &self.get_llvm_module();
        let ll_ty = llcx.int_type(8);
        let ll_global = llmod.add_global(ll_ty, symbol_name);
        ll_global.set_constant();
        ll_global.set_linkage(llvm::LLVMLinkage::LLVMPrivateLinkage);
        ll_global.set_unnamed_addr();
        // just an eye-catching marker value
        let value = 255;
        let ll_const = llvm::Constant::int(ll_ty, U256::from(value as u128));
        ll_global.set_initializer(ll_const);
        ll_global
    }

    /// Type info for vectors.
    ///
    /// Defined in the runtime by `VectorTypeInfo`.
    fn define_type_info_global_vec(&self, symbol_name: &str, elt_mty: &mty::Type) -> llvm::Global {
        let llcx = &self.get_llvm_cx();
        let llmod = &self.get_llvm_module();
        // A struct containing a pointer to a `MoveType`
        // type descriptor of the element type.
        let ll_ty = llcx.get_anonymous_struct_type(&[llcx.int_type(8).ptr_type()]);
        let ll_global = llmod.add_global(ll_ty, symbol_name);
        ll_global.set_constant();
        ll_global.set_linkage(llvm::LLVMLinkage::LLVMPrivateLinkage);
        ll_global.set_unnamed_addr();
        let elt_tydesc_ptr = self.define_llvm_tydesc(elt_mty).ptr();
        let ll_const = llcx.const_struct(&[elt_tydesc_ptr]);
        ll_global.set_initializer(ll_const);
        ll_global
    }

    /// Generate type info for structs.
    ///
    /// Defined in the runtime by a `StructTypeInfo` containing `StructFieldInfo`s.
    fn define_type_info_global_struct(&self, symbol_name: &str, mty: &mty::Type) -> llvm::Global {
        let llcx = &self.get_llvm_cx();
        let llmod = &self.get_llvm_module();
        let global_env = &self.get_global_env();

        // Obtain the StructEnv and type parameter vector from the incoming struct mty.
        // We'll need the former to gain access to the struct fields and the latter to
        // fill in any possible generic struct type parameters.
        let (s_env, s_tys) = match mty {
            mty::Type::Struct(mod_id, s_id, tys) => {
                (global_env.get_module(*mod_id).into_struct(*s_id), tys)
            }
            _ => unreachable!(),
        };

        // Look up the corresponding LLVM struct type constructed earlier in the translation.
        // Use it to collect field offsets, struct size, and struct alignment as computed by LLVM.
        let ll_struct_name = s_env.ll_struct_name_from_raw_name(s_tys);
        let ll_struct_ty = llcx
            .named_struct_type(&ll_struct_name)
            .expect("no struct type");
        let dl = llmod.get_module_data_layout();
        let ll_struct_size = llcx.abi_size_of_type(dl, ll_struct_ty.as_any_type());
        let ll_struct_align = llcx.abi_alignment_of_type(dl, ll_struct_ty.as_any_type());

        debug!(target: "rtty", "\nll_struct_type:\n{}\nstruct size: {}, alignment: {}",
        ll_struct_ty.as_any_type().print_to_str(), ll_struct_size, ll_struct_align);

        // Create LLVM descriptor type `ll_fld_info_ty` corresponding to
        // `move_native::rt_types::StructFieldInfo`:
        //   pub struct StructFieldInfo {
        //       pub type_: MoveType,
        //       pub offset: u64,
        //       pub name: StaticName,
        //   }
        let ll_tydesc_ty = self.get_llvm_tydesc_type();
        let ll_int64_ty = llcx.int_type(64);
        let ll_fld_name_ty = llcx
            .anonymous_struct_type(&[llcx.int_type(8).ptr_type(), llcx.int_type(64)])
            .as_any_type();
        let ll_fld_info_ty = llcx.get_anonymous_struct_type(&[
            ll_tydesc_ty.as_any_type(),
            ll_int64_ty,
            ll_fld_name_ty,
        ]);

        // Visit each field of the Move struct creating a runtime descriptor `ll_fld_info_ty`
        // for each. The original Move struct fields provide the `mty::Type` needed to construct
        // a `MoveType` descriptor. The corresponding LLVM struct fields are used to query LLVM for
        // offsets. This should avoid the need to perform any manual platform/ABI/OS specific
        // computation of struct and field information.
        let fld_count = s_env.get_field_count();
        assert!(fld_count > 0);
        let ll_fld_count = ll_struct_ty.count_struct_element_types();
        assert!(fld_count == ll_fld_count);
        let mut fld_infos = Vec::with_capacity(ll_fld_count);
        for i in 0..ll_fld_count {
            let ll_elt_offset = ll_struct_ty.offset_of_element(dl, i);
            let ll_ety = ll_struct_ty.struct_get_type_at_index(i);
            debug!(target: "rtty", "\nmember offset: {}\n{}", ll_elt_offset, ll_ety.dump_properties_to_str(dl));

            let f_env = s_env.get_field_by_offset(i);
            let mut fld_type = f_env.get_type();
            let fld_name = f_env.get_name().display(s_env.symbol_pool()).to_string();

            // Subtitute type parameter that may be buried in this field.
            if fld_type.is_open() {
                fld_type = fld_type.instantiate(s_tys);
            }

            // Get the LLVM literal corresponding to `MoveType` literal for this field.
            let ll_move_type_literal = self.tydesc_constant(&fld_type);

            let ll_offset_val = llvm::Constant::int(ll_int64_ty, U256::from(ll_elt_offset as u64));

            // Create the name literal for this field.
            let ll_str = llcx.const_string(&fld_name);
            let ll_str_global = llmod.add_global2(ll_str.llvm_type(), "");
            ll_str_global.set_constant();
            ll_str_global.set_linkage(llvm::LLVMLinkage::LLVMPrivateLinkage);
            ll_str_global.set_unnamed_addr();
            ll_str_global.set_initializer(ll_str.as_const());
            let ll_str_len = llvm::Constant::int(ll_int64_ty, U256::from(fld_name.len() as u128));
            let ll_str_val = llcx.const_struct(&[ll_str_global.ptr(), ll_str_len]);

            let ll_fld_info_literal =
                llcx.const_struct(&[ll_move_type_literal, ll_offset_val, ll_str_val]);
            fld_infos.push(ll_fld_info_literal);
        }

        // Create the field array global and initialize with `StructFieldInfo`s create above:
        let aval = llcx.const_array(&fld_infos, ll_fld_info_ty);
        let ll_fld_array = llmod.add_global2(aval.llvm_type(), "s_fld_array");
        ll_fld_array.set_constant();
        ll_fld_array.set_linkage(llvm::LLVMLinkage::LLVMPrivateLinkage);
        ll_fld_array.set_unnamed_addr();
        ll_fld_array.set_initializer(aval.as_const());

        // Create the overall `ll_struct_type_info_ty` runtime descriptor global. This LLVM type
        // corresponds to `move_native::rt_types::StructTypeInfo`:
        //   pub struct StructTypeInfo {
        //     pub field_array_ptr: *const StructFieldInfo,
        //     pub field_array_len: u64,
        //     pub size: u64,
        //     pub alignment: u64,
        //   }
        let ll_struct_type_info_ty = llcx.get_anonymous_struct_type(&[
            llcx.int_type(8).ptr_type(),
            ll_int64_ty,
            ll_int64_ty,
            ll_int64_ty,
        ]);
        let ll_struct_type_info = llmod.add_global(ll_struct_type_info_ty, symbol_name);
        ll_struct_type_info.set_constant();
        ll_struct_type_info.set_linkage(llvm::LLVMLinkage::LLVMPrivateLinkage);
        ll_struct_type_info.set_unnamed_addr();

        // Create the `StructTypeInfo` initializer.
        let fld_array_len = llvm::Constant::int(ll_int64_ty, U256::from(ll_fld_count as u64));
        let struct_size = llvm::Constant::int(ll_int64_ty, U256::from(ll_struct_size as u64));
        let elt_align = llvm::Constant::int(ll_int64_ty, U256::from(ll_struct_align as u64));

        let ll_struct_type_info_literal =
            llcx.const_struct(&[ll_fld_array.ptr(), fld_array_len, struct_size, elt_align]);
        ll_struct_type_info.set_initializer(ll_struct_type_info_literal);
        ll_struct_type_info
    }

    fn global_tydesc_name(&self, mty: &mty::Type) -> String {
        let tdc = self.f_env.as_ref().expect("f_env").get_type_display_ctx();
        let name = mty.sanitized_display_name(&tdc);
        format!("__move_rttydesc_{name}")
    }

    // fixme this function name is not amazing!
    fn global_tydesc_name_name(&self, mty: &mty::Type) -> String {
        let tdc = self.f_env.as_ref().expect("f_env").get_type_display_ctx();
        let name = mty.sanitized_display_name(&tdc);
        format!("__move_rttydesc_{name}_name")
    }

    fn has_type_info(mty: &mty::Type) -> bool {
        use mty::{PrimitiveType, Type};
        match mty {
            Type::Primitive(
                PrimitiveType::Bool
                | PrimitiveType::U8
                | PrimitiveType::U16
                | PrimitiveType::U32
                | PrimitiveType::U64
                | PrimitiveType::U128
                | PrimitiveType::U256
                | PrimitiveType::Address
                | PrimitiveType::Signer,
            ) => false,
            Type::Vector(_) | Type::Struct(_, _, _) => true,
            _ => todo!(),
        }
    }

    fn global_tydesc_info_name(&self, mty: &mty::Type) -> String {
        use mty::{PrimitiveType, Type};
        let tdc = self.f_env.as_ref().expect("f_env").get_type_display_ctx();
        let name = match mty {
            _ if !Self::has_type_info(mty) => {
                // A special name for types that don't need type info.
                "NOTHING".to_string()
            }
            Type::Vector(_) | Type::Struct(_, _, _) => mty.sanitized_display_name(&tdc),
            _ => todo!(),
        };

        format!("__move_rttydesc_{name}_info")
    }
}
