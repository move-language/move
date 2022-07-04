// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! Implementation of native functions for utf8 strings.

use move_binary_format::errors::PartialVMResult;
use move_vm_runtime::native_functions::NativeContext;
use move_vm_types::{
    gas_schedule::NativeCostIndex,
    loaded_data::runtime_types::Type,
    natives::function::{native_gas, NativeResult},
    pop_arg,
    values::{Value, VectorRef},
};

use std::collections::VecDeque;

// The implementation approach delegates all utf8 handling to Rust.
// This is possible without copying of bytes because (a) we can
// get a `std::cell::Ref<Vec<u8>>` from a `vector<u8>` and in turn a `&[u8]`
// from that (b) assuming that `vector<u8>` embedded in a string
// is already valid utf8, we can use `str::from_utf8_unchecked` to
// create a `&str` view on the bytes without a copy. Once we have this
// view, we can call ut8 functions like length, substring, etc.

pub fn native_check_utf8(
    context: &mut NativeContext,
    _ty_args: Vec<Type>,
    mut args: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    debug_assert!(args.len() == 1);
    let s_arg = pop_arg!(args, VectorRef);
    let s_ref = s_arg.as_bytes_ref();
    let ok = std::str::from_utf8(s_ref.as_slice()).is_ok();
    // TODO: extensible native cost tables
    let cost = native_gas(context.cost_table(), NativeCostIndex::PUSH_BACK, 1);
    NativeResult::map_partial_vm_result_one(cost, Ok(Value::bool(ok)))
}

pub fn native_is_char_boundary(
    context: &mut NativeContext,
    _ty_args: Vec<Type>,
    mut args: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    debug_assert!(args.len() == 2);
    let i = pop_arg!(args, u64);
    let s_arg = pop_arg!(args, VectorRef);
    let s_ref = s_arg.as_bytes_ref();
    unsafe {
        let ok = std::str::from_utf8_unchecked(s_ref.as_slice()).is_char_boundary(i as usize);
        // TODO: extensible native cost tables
        let cost = native_gas(context.cost_table(), NativeCostIndex::PUSH_BACK, 1);
        NativeResult::map_partial_vm_result_one(cost, Ok(Value::bool(ok)))
    }
}

pub fn native_sub_string(
    context: &mut NativeContext,
    _ty_args: Vec<Type>,
    mut args: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    debug_assert!(args.len() == 3);
    let j = pop_arg!(args, u64) as usize;
    let i = pop_arg!(args, u64) as usize;
    let s_arg = pop_arg!(args, VectorRef);
    let s_ref = s_arg.as_bytes_ref();
    unsafe {
        let s_str = std::str::from_utf8_unchecked(s_ref.as_slice());
        let v = Value::vector_u8((&s_str[i..j]).as_bytes().iter().cloned());
        // TODO: extensible native cost tables
        let cost = native_gas(context.cost_table(), NativeCostIndex::PUSH_BACK, 1);
        NativeResult::map_partial_vm_result_one(cost, Ok(v))
    }
}

pub fn native_index_of(
    context: &mut NativeContext,
    _ty_args: Vec<Type>,
    mut args: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    debug_assert!(args.len() == 2);
    unsafe {
        let r_arg = pop_arg!(args, VectorRef);
        let r_ref = r_arg.as_bytes_ref();
        let r_str = std::str::from_utf8_unchecked(r_ref.as_slice());
        let s_arg = pop_arg!(args, VectorRef);
        let s_ref = s_arg.as_bytes_ref();
        let s_str = std::str::from_utf8_unchecked(s_ref.as_slice());
        let pos = match s_str.find(r_str) {
            Some(size) => size,
            None => s_str.len(),
        };
        // TODO: extensible native cost tables
        let cost = native_gas(context.cost_table(), NativeCostIndex::LENGTH, 1);
        NativeResult::map_partial_vm_result_one(cost, Ok(Value::u64(pos as u64)))
    }
}
