// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! The type descriptor accepted by runtime functions.
//!
//! Corresponds to `move_native::rt_types::MoveType`.

#![allow(unused)]

use crate::stackless::{extensions::TypeExt, llvm};
use move_core_types::u256::U256;
use move_model::{ast as mast, model as mm, ty as mty};
use move_native::shared::TypeDesc;

static TD_NAME: &str = "__move_rt_type";
static TD_TYPE_NAME_NAME: &str = "__move_rt_type_name";
static TD_TYPE_INFO_NAME: &str = "__move_rt_type_info";
static TD_VECTOR_TYPE_INFO_NAME: &str = "__move_rt_type_info_vec";
static TD_STRUCT_TYPE_INFO_NAME: &str = "__move_rt_type_info_struct";
static TD_REFERENCE_TYPE_INFO_NAME: &str = "__move_rt_type_info_ref";

pub fn get_llvm_tydesc_type(llcx: &llvm::Context) -> llvm::StructType {
    match llcx.named_struct_type(TD_NAME) {
        Some(t) => t,
        None => {
            declare_llvm_tydesc_type(llcx);
            llcx.named_struct_type(TD_NAME).expect(".")
        }
    }
}

fn declare_llvm_tydesc_type(llcx: &llvm::Context) {
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

pub fn define_llvm_tydesc(
    llcx: &llvm::Context,
    llmod: &llvm::Module,
    mty: &mty::Type,
    type_display_ctx: &mty::TypeDisplayContext,
) -> llvm::Global {
    let name = global_tydesc_name(mty, type_display_ctx);
    match llmod.get_global(&name) {
        Some(g) => g,
        None => {
            let ll_tydesc_ty = get_llvm_tydesc_type(llcx);
            let ll_tydesc_ty = ll_tydesc_ty.as_any_type();
            let ll_global = llmod.add_global(ll_tydesc_ty, &name);
            ll_global.set_constant();
            ll_global.set_linkage(llvm::LLVMLinkage::LLVMPrivateLinkage);
            ll_global.set_unnamed_addr();
            let ll_constant = tydesc_constant(llcx, llmod, mty, type_display_ctx);
            let ll_constant_ty = ll_constant.llvm_type();
            ll_global.set_initializer(ll_constant);
            ll_global
        }
    }
}

fn tydesc_constant(
    llcx: &llvm::Context,
    llmod: &llvm::Module,
    mty: &mty::Type,
    type_display_ctx: &mty::TypeDisplayContext,
) -> llvm::Constant {
    let ll_const_type_name = type_name_constant(llcx, llmod, mty, type_display_ctx);
    let ll_const_type_descrim = {
        let ll_ty = llcx.int_type(64);
        llvm::Constant::int(ll_ty, U256::from(type_descrim(mty)))
    };
    let ll_const_type_info_ptr = {
        let ll_global_type_info = define_type_info_global(llcx, llmod, mty, type_display_ctx);
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

fn type_name_constant(
    llcx: &llvm::Context,
    llmod: &llvm::Module,
    mty: &mty::Type,
    type_display_ctx: &mty::TypeDisplayContext,
) -> llvm::Constant {
    let name = type_name(mty, type_display_ctx);
    let len = name.len();

    // Create a static string and take a constant pointer to it.
    let ll_static_bytes_ptr = {
        let global_name = global_tydesc_name_name(mty, type_display_ctx);
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

fn type_name(mty: &mty::Type, type_display_ctx: &mty::TypeDisplayContext) -> String {
    format!("{}", mty.display(type_display_ctx))
}

/// The values here correspond to `move_native::rt_types::TypeDesc`.
fn type_descrim(mty: &mty::Type) -> u64 {
    use mty::{PrimitiveType, Type};
    match mty {
        Type::Primitive(PrimitiveType::U64) => TypeDesc::U64 as u64,
        Type::Vector(_) => TypeDesc::Vector as u64,
        _ => todo!(),
    }
}

/// The "type info" for a Move type.
///
/// This is the type-specific metadata interpreted by the runtime.
/// It is a union.
/// It corresponds to `move_native:rt_types::TypeInfo`.
fn define_type_info_global(
    llcx: &llvm::Context,
    llmod: &llvm::Module,
    mty: &mty::Type,
    type_display_ctx: &mty::TypeDisplayContext,
) -> llvm::Global {
    let symbol_name = global_tydesc_info_name(mty, type_display_ctx);

    match llmod.get_global(&symbol_name) {
        Some(g) => g,
        None => {
            use mty::{PrimitiveType, Type};
            match mty {
                Type::Primitive(PrimitiveType::U64) => {
                    define_type_info_global_nil(llcx, llmod, &symbol_name)
                }
                Type::Vector(elt_ty) => match **elt_ty {
                    Type::Primitive(PrimitiveType::U64) => define_type_info_global_vec(
                        llcx,
                        llmod,
                        &symbol_name,
                        elt_ty,
                        type_display_ctx,
                    ),
                    _ => todo!(),
                },
                _ => todo!(),
            }
        }
    }
}

/// A special type info for all types that don't need type info.
fn define_type_info_global_nil(
    llcx: &llvm::Context,
    llmod: &llvm::Module,
    symbol_name: &str,
) -> llvm::Global {
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
fn define_type_info_global_vec(
    llcx: &llvm::Context,
    llmod: &llvm::Module,
    symbol_name: &str,
    elt_mty: &mty::Type,
    type_display_ctx: &mty::TypeDisplayContext,
) -> llvm::Global {
    // A struct containing a pointer to a `MoveType`
    // type descriptor of the element type.
    let ll_ty = llcx.get_anonymous_struct_type(&[llcx.int_type(8).ptr_type()]);
    let ll_global = llmod.add_global(ll_ty, symbol_name);
    ll_global.set_constant();
    ll_global.set_linkage(llvm::LLVMLinkage::LLVMPrivateLinkage);
    ll_global.set_unnamed_addr();
    let elt_tydesc_ptr = define_llvm_tydesc(llcx, llmod, elt_mty, type_display_ctx).ptr();
    let ll_const = llcx.const_struct(&[elt_tydesc_ptr]);
    ll_global.set_initializer(ll_const);
    ll_global
}

fn global_tydesc_name(mty: &mty::Type, type_display_ctx: &mty::TypeDisplayContext) -> String {
    let name = mty.sanitized_display_name(type_display_ctx);
    format!("__move_rttydesc_{name}")
}

// fixme this function name is not amazing!
fn global_tydesc_name_name(mty: &mty::Type, type_display_ctx: &mty::TypeDisplayContext) -> String {
    let name = mty.sanitized_display_name(type_display_ctx);
    format!("__move_rttydesc_{name}_name")
}

fn global_tydesc_info_name(mty: &mty::Type, type_display_ctx: &mty::TypeDisplayContext) -> String {
    use mty::{PrimitiveType, Type};
    let name = match mty {
        Type::Primitive(_) => {
            // A special name for types that don't need type info.
            "NOTHING".to_string()
        }
        Type::Vector(_) => mty.sanitized_display_name(type_display_ctx),
        _ => todo!(),
    };

    format!("__move_rttydesc_{name}_info")
}
