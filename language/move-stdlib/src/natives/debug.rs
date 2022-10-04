// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::natives::helpers::make_module_natives;
use move_binary_format::errors::PartialVMResult;
use move_core_types::{
    account_address::AccountAddress, gas_algebra::InternalGas, language_storage::TypeTag,
};
use move_vm_runtime::native_functions::{NativeContext, NativeFunction};
use move_vm_types::values::VectorRef;
#[allow(unused_imports)]
use move_vm_types::values::{values_impl::debug::print_reference, Reference, StructRef};
#[allow(unused_imports)]
use move_vm_types::{
    loaded_data::runtime_types::Type, natives::function::NativeResult, pop_arg, values::Value,
};
use smallvec::smallvec;
use std::{collections::VecDeque, sync::Arc};

/***************************************************************************************************
 * native fun print
 *
 *   gas cost: base_cost
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct PrintGasParameters {
    pub base_cost: InternalGas,
}

#[cfg(feature = "testing")]
fn is_string_type(context: &NativeContext, ty: &Type, move_std_addr: AccountAddress) -> bool {
    let ty_tag = match context.type_to_type_tag(ty) {
        Ok(ty_tag) => ty_tag,
        Err(_) => return false,
    };

    match ty_tag {
        TypeTag::Struct(struct_tag) => {
            struct_tag.address == move_std_addr
                && struct_tag.module.as_str().eq("string")
                && struct_tag.name.as_str().eq("String")
        }
        _ => false,
    }
}

#[allow(unused_mut)]
#[inline]
fn native_print(
    gas_params: &PrintGasParameters,
    _context: &mut NativeContext,
    mut ty_args: Vec<Type>,
    mut args: VecDeque<Value>,
    move_std_addr: AccountAddress,
) -> PartialVMResult<NativeResult> {
    debug_assert!(ty_args.len() == 1);
    debug_assert!(args.len() == 1);

    // No-op if the feature flag is not present.
    #[cfg(feature = "testing")]
    {
        let ty = ty_args.pop().unwrap();

        match is_string_type(_context, &ty, move_std_addr) {
            true => {
                let r = pop_arg!(args, StructRef);

                let field_ref = r
                    .borrow_field(0)? // borrows the 'bytes' field of the std::string::String struct
                    .value_as::<VectorRef>()?;

                let bytes = field_ref.as_bytes_ref();

                // This is safe because we guarantee the bytes to be utf8.
                let str = unsafe { std::str::from_utf8_unchecked(bytes.as_slice()) };

                println!("[debug] {}", str);
            }
            false => {
                let r = pop_arg!(args, Reference);
                let mut buf = String::new();
                print_reference(&mut buf, &r)?;
                println!("[debug] {}", buf);
            }
        }
    }

    Ok(NativeResult::ok(gas_params.base_cost, smallvec![]))
}

pub fn make_native_print(
    gas_params: PrintGasParameters,
    move_std_addr: AccountAddress,
) -> NativeFunction {
    Arc::new(
        move |context, ty_args, args| -> PartialVMResult<NativeResult> {
            native_print(&gas_params, context, ty_args, args, move_std_addr)
        },
    )
}

/***************************************************************************************************
 * native fun print_stack_trace
 *
 *   gas cost: base_cost
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct PrintStackTraceGasParameters {
    pub base_cost: InternalGas,
}

#[allow(unused_variables)]
#[inline]
fn native_print_stack_trace(
    gas_params: &PrintStackTraceGasParameters,
    context: &mut NativeContext,
    ty_args: Vec<Type>,
    args: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    debug_assert!(ty_args.is_empty());
    debug_assert!(args.is_empty());

    #[cfg(feature = "testing")]
    {
        let mut s = String::new();
        context.print_stack_trace(&mut s)?;
        println!("{}", s);
    }

    Ok(NativeResult::ok(gas_params.base_cost, smallvec![]))
}

pub fn make_native_print_stack_trace(gas_params: PrintStackTraceGasParameters) -> NativeFunction {
    Arc::new(
        move |context, ty_args, args| -> PartialVMResult<NativeResult> {
            native_print_stack_trace(&gas_params, context, ty_args, args)
        },
    )
}

/***************************************************************************************************
 * module
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct GasParameters {
    pub print: PrintGasParameters,
    pub print_stack_trace: PrintStackTraceGasParameters,
}

pub fn make_all(
    gas_params: GasParameters,
    move_std_addr: AccountAddress,
) -> impl Iterator<Item = (String, NativeFunction)> {
    let natives = [
        ("print", make_native_print(gas_params.print, move_std_addr)),
        (
            "print_stack_trace",
            make_native_print_stack_trace(gas_params.print_stack_trace),
        ),
    ];

    make_module_natives(natives)
}
