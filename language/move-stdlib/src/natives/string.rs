// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! Implementation of native functions for utf8 strings.

use crate::natives::helpers::make_module_natives;
use move_binary_format::errors::PartialVMResult;
use move_vm_runtime::native_functions::{NativeContext, NativeFunction};
use move_vm_types::{
    loaded_data::runtime_types::Type,
    natives::function::NativeResult,
    pop_arg,
    values::{Value, VectorRef},
};
use std::{collections::VecDeque, sync::Arc};

// The implementation approach delegates all utf8 handling to Rust.
// This is possible without copying of bytes because (a) we can
// get a `std::cell::Ref<Vec<u8>>` from a `vector<u8>` and in turn a `&[u8]`
// from that (b) assuming that `vector<u8>` embedded in a string
// is already valid utf8, we can use `str::from_utf8_unchecked` to
// create a `&str` view on the bytes without a copy. Once we have this
// view, we can call ut8 functions like length, substring, etc.

/***************************************************************************************************
 * native fun internal_check_utf8
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct CheckUtf8GasParameters {
    pub base_cost: u64,
    pub unit_cost: u64,
}

fn native_check_utf8(
    gas_params: &CheckUtf8GasParameters,
    _context: &mut NativeContext,
    _ty_args: Vec<Type>,
    mut args: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    debug_assert!(args.len() == 1);
    let s_arg = pop_arg!(args, VectorRef);
    let s_ref = s_arg.as_bytes_ref();
    let ok = std::str::from_utf8(s_ref.as_slice()).is_ok();
    // TODO: extensible native cost tables

    let cost = gas_params.base_cost + gas_params.unit_cost * s_ref.as_slice().len() as u64;

    NativeResult::map_partial_vm_result_one(cost, Ok(Value::bool(ok)))
}

pub fn make_native_check_utf8(gas_params: CheckUtf8GasParameters) -> NativeFunction {
    Arc::new(
        move |context, ty_args, args| -> PartialVMResult<NativeResult> {
            native_check_utf8(&gas_params, context, ty_args, args)
        },
    )
}

/***************************************************************************************************
 * native fun internal_is_char_boundary
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct IsCharBoundaryGasParameters {
    pub base_cost: u64,
}

fn native_is_char_boundary(
    gas_params: &IsCharBoundaryGasParameters,
    _context: &mut NativeContext,
    _ty_args: Vec<Type>,
    mut args: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    debug_assert!(args.len() == 2);
    let i = pop_arg!(args, u64);
    let s_arg = pop_arg!(args, VectorRef);
    let s_ref = s_arg.as_bytes_ref();
    let ok = unsafe {
        // This is safe because we guarantee the bytes to be utf8.
        std::str::from_utf8_unchecked(s_ref.as_slice()).is_char_boundary(i as usize)
    };
    NativeResult::map_partial_vm_result_one(gas_params.base_cost, Ok(Value::bool(ok)))
}

pub fn make_native_is_char_boundary(gas_params: IsCharBoundaryGasParameters) -> NativeFunction {
    Arc::new(
        move |context, ty_args, args| -> PartialVMResult<NativeResult> {
            native_is_char_boundary(&gas_params, context, ty_args, args)
        },
    )
}

/***************************************************************************************************
 * native fun internal_is_sub_string
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct SubStringGasParameters {
    pub base_cost: u64,
    pub unit_cost: u64,
}

fn native_sub_string(
    gas_params: &SubStringGasParameters,
    _context: &mut NativeContext,
    _ty_args: Vec<Type>,
    mut args: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    debug_assert!(args.len() == 3);
    let j = pop_arg!(args, u64) as usize;
    let i = pop_arg!(args, u64) as usize;

    if j < i {
        // TODO: what abort code should we use here?
        return Ok(NativeResult::err(gas_params.base_cost, 1));
    }

    let s_arg = pop_arg!(args, VectorRef);
    let s_ref = s_arg.as_bytes_ref();
    let s_str = unsafe {
        // This is safe because we guarantee the bytes to be utf8.
        std::str::from_utf8_unchecked(s_ref.as_slice())
    };
    let v = Value::vector_u8((&s_str[i..j]).as_bytes().iter().cloned());

    let cost = gas_params.base_cost + gas_params.unit_cost * (j - i) as u64;
    NativeResult::map_partial_vm_result_one(cost, Ok(v))
}

pub fn make_native_sub_string(gas_params: SubStringGasParameters) -> NativeFunction {
    Arc::new(
        move |context, ty_args, args| -> PartialVMResult<NativeResult> {
            native_sub_string(&gas_params, context, ty_args, args)
        },
    )
}

/***************************************************************************************************
 * native fun internal_index_of
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct IndexOfGasParameters {
    pub base_cost: u64,
    pub unit_cost: u64,
}

fn native_index_of(
    gas_params: &IndexOfGasParameters,
    _context: &mut NativeContext,
    _ty_args: Vec<Type>,
    mut args: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    debug_assert!(args.len() == 2);
    let r_arg = pop_arg!(args, VectorRef);
    let r_ref = r_arg.as_bytes_ref();
    let r_str = unsafe { std::str::from_utf8_unchecked(r_ref.as_slice()) };
    let s_arg = pop_arg!(args, VectorRef);
    let s_ref = s_arg.as_bytes_ref();
    let s_str = unsafe { std::str::from_utf8_unchecked(s_ref.as_slice()) };
    let pos = match s_str.find(r_str) {
        Some(size) => size,
        None => s_str.len(),
    };
    let cost = gas_params.base_cost + gas_params.unit_cost * pos as u64;
    NativeResult::map_partial_vm_result_one(cost, Ok(Value::u64(pos as u64)))
}

pub fn make_native_index_of(gas_params: IndexOfGasParameters) -> NativeFunction {
    Arc::new(
        move |context, ty_args, args| -> PartialVMResult<NativeResult> {
            native_index_of(&gas_params, context, ty_args, args)
        },
    )
}

/***************************************************************************************************
 * module
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct GasParameters {
    pub check_utf8: CheckUtf8GasParameters,
    pub is_char_boundary: IsCharBoundaryGasParameters,
    pub sub_string: SubStringGasParameters,
    pub index_of: IndexOfGasParameters,
}

pub fn make_all(gas_params: GasParameters) -> impl Iterator<Item = (String, NativeFunction)> {
    let natives = [
        (
            "internal_check_utf8",
            make_native_check_utf8(gas_params.check_utf8),
        ),
        (
            "internal_is_char_boundary",
            make_native_is_char_boundary(gas_params.is_char_boundary),
        ),
        (
            "internal_sub_string",
            make_native_sub_string(gas_params.sub_string),
        ),
        (
            "internal_index_of",
            make_native_index_of(gas_params.index_of),
        ),
    ];

    make_module_natives(natives)
}
