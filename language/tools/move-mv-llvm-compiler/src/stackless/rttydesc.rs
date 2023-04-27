// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! The type descriptor accepted by runtime functions.
//!
//! Corresponds to `move_native::rt_types::MoveType`.

#![allow(unused)]

use crate::stackless::llvm;
use move_model::{ast as mast, model as mm, ty as mty};
use move_native::shared::TypeDesc;

static TD_NAME: &'static str = "__move_rt_type";
static TD_TYPE_NAME_NAME: &'static str = "__move_rt_type_name";
static TD_TYPE_INFO_NAME: &'static str = "__move_rt_type_info";
static TD_VECTOR_TYPE_INFO_NAME: &'static str = "__move_rt_type_info_vec";
static TD_STRUCT_TYPE_INFO_NAME: &'static str = "__move_rt_type_info_struct";
static TD_REFERENCE_TYPE_INFO_NAME: &'static str = "__move_rt_type_info_ref";

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
            .anonymous_struct_type(&[llcx.int8_type().ptr_type(), llcx.int64_type()])
            .as_any_type();
        let type_descrim_ty = llcx.int32_type();
        // This is a pointer to a statically-defined union of type infos
        let type_info_ptr_ty = llcx.int8_type().ptr_type();
        &[type_name_ty, type_descrim_ty, type_info_ptr_ty]
    };

    td_llty.set_struct_body(field_tys);
}

pub fn define_llvm_tydesc(
    llcx: &llvm::Context,
    llmod: &llvm::Module,
    mty: &mty::Type,
) -> llvm::Global {
    let name = global_tydesc_name(mty);
    match llmod.get_global(&name) {
        Some(g) => g,
        None => {
            let ll_tydesc_ty = get_llvm_tydesc_type(llcx);
            let ll_tydesc_ty = ll_tydesc_ty.as_any_type();
            let ll_global = llmod.add_global(ll_tydesc_ty, &name);
            ll_global.set_constant();
            let ll_constant = tydesc_constant(llcx, llmod, mty);
            let ll_constant_ty = ll_constant.llvm_type();
            ll_global.set_initializer(ll_constant);
            ll_global
        }
    }
}

fn tydesc_constant(llcx: &llvm::Context, llmod: &llvm::Module, mty: &mty::Type) -> llvm::Constant {
    let ll_const_type_name = type_name_constant(llcx, llmod, mty);
    let ll_const_type_descrim = {
        let ll_ty = llcx.int32_type();
        llvm::Constant::int(ll_ty, type_descrim(mty))
    };
    let ll_const_type_info_ptr = {
        let ll_global_type_info = define_type_info_global(llcx, llmod, mty);
        ll_global_type_info.ptr()
    };
    let ll_const = llcx.const_named_struct(
        &[
            ll_const_type_name,
            ll_const_type_descrim,
            ll_const_type_info_ptr,
        ],
        TD_NAME,
    );
    ll_const
}

fn type_name_constant(
    llcx: &llvm::Context,
    llmod: &llvm::Module,
    mty: &mty::Type,
) -> llvm::Constant {
    let name = type_name(mty);
    let len = name.len();

    // Create a static string and take a constant pointer to it.
    let ll_static_bytes_ptr = {
        let global_name = global_tydesc_name_name(mty);
        match llmod.get_global(&global_name) {
            Some(g) => g.ptr(),
            None => {
                let ll_const_string = llcx.const_string(&name);
                let ll_array_ty = ll_const_string.llvm_type();
                let ll_global = llmod.add_global(ll_array_ty, &global_name);
                ll_global.set_constant();
                ll_global.set_initializer(ll_const_string.as_const());
                ll_global.ptr()
            }
        }
    };

    let ll_ty_u64 = llcx.int64_type();
    let ll_const_len = llvm::Constant::int(ll_ty_u64, len as u64);

    let ll_const_struct = llcx.const_struct(&[ll_static_bytes_ptr, ll_const_len]);

    ll_const_struct
}

fn type_name(mty: &mty::Type) -> String {
    use mty::{PrimitiveType, Type};
    let name = match mty {
        Type::Primitive(PrimitiveType::U64) => "u64",
        _ => todo!(),
    };

    format!("{name}")
}

/// The values here correspond to `move_native::rt_types::TypeDesc`.
fn type_descrim(mty: &mty::Type) -> u64 {
    use mty::{PrimitiveType, Type};
    match mty {
        Type::Primitive(PrimitiveType::U64) => TypeDesc::U64 as u64,
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
) -> llvm::Global {
    let symbol_name = global_tydesc_info_name(mty);

    match llmod.get_global(&symbol_name) {
        Some(g) => g,
        None => {
            use mty::{PrimitiveType, Type};
            match mty {
                Type::Primitive(PrimitiveType::U64) => {
                    define_type_info_global_nil(llcx, llmod, &symbol_name)
                }
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
    let ll_ty = llcx.int8_type();
    let ll_global = llmod.add_global(ll_ty, symbol_name);
    ll_global.set_constant();
    // just an eye-catching marker value
    let value = 255;
    let ll_const = llvm::Constant::int(ll_ty, value);
    ll_global.set_initializer(ll_const);
    ll_global
}

fn global_tydesc_name(mty: &mty::Type) -> String {
    use mty::{PrimitiveType, Type};
    let name = match mty {
        Type::Primitive(PrimitiveType::U64) => "u64",
        _ => todo!(),
    };

    format!("__move_rttydesc_{name}")
}

// fixme this function name is not amazing!
fn global_tydesc_name_name(mty: &mty::Type) -> String {
    use mty::{PrimitiveType, Type};
    let name = match mty {
        Type::Primitive(PrimitiveType::U64) => "u64",
        _ => todo!(),
    };

    format!("__move_rttydesc_{name}_name")
}

fn global_tydesc_info_name(mty: &mty::Type) -> String {
    use mty::{PrimitiveType, Type};
    let name = match mty {
        Type::Primitive(PrimitiveType::U64) => {
            // A special name for types that don't need type info.
            "NOTHING"
        }
        _ => todo!(),
    };

    format!("__move_rttydesc_{name}_info")
}
