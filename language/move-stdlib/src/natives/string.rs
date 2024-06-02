// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! Implementation of native functions for utf8 strings.

use crate::natives::helpers::make_module_natives;
use move_binary_format::errors::PartialVMResult;
use move_core_types::gas_algebra::{InternalGas, InternalGasPerByte, NumBytes};
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
 *
 *   gas cost: base_cost + unit_cost * length_in_bytes
 *
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct CheckUtf8GasParameters {
    pub base: InternalGas,
    pub per_byte: InternalGasPerByte,
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

    let cost = gas_params.base + gas_params.per_byte * NumBytes::new(s_ref.as_slice().len() as u64);

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
 *
 *   gas cost: base_cost
 *
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct IsCharBoundaryGasParameters {
    pub base: InternalGas,
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
    NativeResult::map_partial_vm_result_one(gas_params.base, Ok(Value::bool(ok)))
}

pub fn make_native_is_char_boundary(gas_params: IsCharBoundaryGasParameters) -> NativeFunction {
    Arc::new(
        move |context, ty_args, args| -> PartialVMResult<NativeResult> {
            native_is_char_boundary(&gas_params, context, ty_args, args)
        },
    )
}

/***************************************************************************************************
 * native fun internal_sub_string
 *
 *   gas cost: base_cost + unit_cost * sub_string_length_in_bytes
 *
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct SubStringGasParameters {
    pub base: InternalGas,
    pub per_byte: InternalGasPerByte,
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
        return Ok(NativeResult::err(gas_params.base, 1));
    }

    let s_arg = pop_arg!(args, VectorRef);
    let s_ref = s_arg.as_bytes_ref();
    let s_str = unsafe {
        // This is safe because we guarantee the bytes to be utf8.
        std::str::from_utf8_unchecked(s_ref.as_slice())
    };
    let v = Value::vector_u8(s_str[i..j].as_bytes().iter().cloned());

    let cost = gas_params.base + gas_params.per_byte * NumBytes::new((j - i) as u64);
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
 *
 *   gas cost: base_cost + unit_cost * bytes_searched
 *
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct IndexOfGasParameters {
    pub base: InternalGas,
    pub per_byte_pattern: InternalGasPerByte,
    pub per_byte_searched: InternalGasPerByte,
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
    // TODO(Gas): What is the algorithm used for the search?
    //            Ideally it should be something like KMP with O(n) time complexity...
    let cost = gas_params.base
        + gas_params.per_byte_pattern * NumBytes::new(r_str.len() as u64)
        + gas_params.per_byte_searched * NumBytes::new(pos as u64);
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
 * native fun internal_next_char_boundary
 *
 *   gas cost: base_cost + unit_cost * bytes_iterated
 *
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct NextCharBoundaryGasParameters {
    pub base: InternalGas,
    pub per_byte: InternalGasPerByte,
}

fn native_next_char_boundary(
    gas_params: &NextCharBoundaryGasParameters,
    _context: &mut NativeContext,
    _ty_args: Vec<Type>,
    mut args: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    debug_assert!(args.len() == 2);

    let i = pop_arg!(args, u64) as usize;

    let s_arg = pop_arg!(args, VectorRef);
    let s_ref = s_arg.as_bytes_ref();
    let s_str = unsafe {
        // This is safe because we guarantee the bytes to be utf8.
        std::str::from_utf8_unchecked(s_ref.as_slice())
    };

    if i >= s_ref.len() {
        return Ok(NativeResult::err(gas_params.base, 2));
    }

    let mut j = i + 1;
    while !s_str.is_char_boundary(j) {
        j += i;
    }

    let cost = gas_params.base + gas_params.per_byte * NumBytes::new((j - i) as u64);
    NativeResult::map_partial_vm_result_one(cost, Ok(Value::u64(j as u64)))
}

pub fn make_native_next_char_boundary(gas_params: NextCharBoundaryGasParameters) -> NativeFunction {
    Arc::new(
        move |context, ty_args, args| -> PartialVMResult<NativeResult> {
            native_next_char_boundary(&gas_params, context, ty_args, args)
        },
    )
}

/***************************************************************************************************
 * native fun internal_chars_count
 *
 *   gas cost: base_cost + unit_cost * bytes_length
 *
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct CharsCountGasParameters {
    pub base: InternalGas,
    pub per_byte: InternalGasPerByte,
}

fn native_chars_count(
    gas_params: &CharsCountGasParameters,
    _context: &mut NativeContext,
    _ty_args: Vec<Type>,
    mut args: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    debug_assert!(args.len() == 1);

    let s_arg = pop_arg!(args, VectorRef);
    let s_ref = s_arg.as_bytes_ref();
    let s_str = unsafe {
        // This is safe because we guarantee the bytes to be utf8.
        std::str::from_utf8_unchecked(s_ref.as_slice())
    };

    let count = s_str.chars().count();

    let cost = gas_params.base + gas_params.per_byte * NumBytes::new(s_str.len() as u64);
    NativeResult::map_partial_vm_result_one(cost, Ok(Value::u64(count as u64)))
}

pub fn make_native_chars_count(gas_params: CharsCountGasParameters) -> NativeFunction {
    Arc::new(
        move |context, ty_args, args| -> PartialVMResult<NativeResult> {
            native_chars_count(&gas_params, context, ty_args, args)
        },
    )
}

/***************************************************************************************************
 * native fun internal_sub_string_char
 *
 *   gas cost: base_cost + unit_cost * sub_string_length_in_bytes
 *
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct SubStringCharGasParameters {
    pub base: InternalGas,
    pub per_byte: InternalGasPerByte,
}

fn native_sub_string_char(
    gas_params: &SubStringCharGasParameters,
    _context: &mut NativeContext,
    _ty_args: Vec<Type>,
    mut args: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    debug_assert!(args.len() == 3);

    let j = pop_arg!(args, u64) as usize;
    let i = pop_arg!(args, u64) as usize;

    let s_arg = pop_arg!(args, VectorRef);
    let s_ref = s_arg.as_bytes_ref();
    let s_str = unsafe {
        // This is safe because we guarantee the bytes to be utf8.
        std::str::from_utf8_unchecked(s_ref.as_slice())
    };

    let mut sub_str = "";
    let mut cost = gas_params.base;

    if j > i {
        let mut char_indices = s_str.char_indices();
        let s_len = s_str.len();
        let fun = |(e, _)| e;

        let s = char_indices.nth(i).map_or(s_len, fun);
        let e = char_indices.nth(j - i - 1).map_or(s_len, fun);

        sub_str = &s_str[s..e];
        cost += gas_params.per_byte * NumBytes::new(sub_str.len() as u64);
    }

    NativeResult::map_partial_vm_result_one(
        cost,
        Ok(Value::vector_u8(sub_str.as_bytes().iter().cloned())),
    )
}

pub fn make_native_sub_string_char(gas_params: SubStringCharGasParameters) -> NativeFunction {
    Arc::new(
        move |context, ty_args, args| -> PartialVMResult<NativeResult> {
            native_sub_string_char(&gas_params, context, ty_args, args)
        },
    )
}

/***************************************************************************************************
 * native fun internal_insert_char
 *
 *   gas cost: base_cost + unit_cost * insert_offset_in_bytes
 *
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct InsertCharGasParameters {
    pub base: InternalGas,
    pub per_byte: InternalGasPerByte,
}

fn native_insert_char(
    gas_params: &InsertCharGasParameters,
    _context: &mut NativeContext,
    _ty_args: Vec<Type>,
    mut args: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    debug_assert!(args.len() == 3);

    let c_arg = pop_arg!(args, VectorRef);
    let c_ref = c_arg.as_bytes_ref();
    let c_str = unsafe {
        // This is safe because we guarantee the bytes to be utf8.
        std::str::from_utf8_unchecked(c_ref.as_slice())
    };

    let i = pop_arg!(args, u64) as usize;

    let s_arg = pop_arg!(args, VectorRef);
    let s_ref = s_arg.as_bytes_ref();
    let s_str = unsafe {
        // This is safe because we guarantee the bytes to be utf8.
        std::str::from_utf8_unchecked(s_ref.as_slice())
    };

    let mut s_vec: Vec<char> = s_str.chars().collect();

    if i > s_vec.len() {
        return Ok(NativeResult::err(gas_params.base, 3));
    }

    s_vec.insert(i, c_str.chars().next().unwrap());
    let o_str: String = s_vec.into_iter().collect();

    let cost = gas_params.base + gas_params.per_byte * NumBytes::new(i as u64);

    NativeResult::map_partial_vm_result_one(
        cost,
        Ok(Value::vector_u8(o_str.as_bytes().iter().cloned())),
    )
}

pub fn make_native_insert_char(gas_params: InsertCharGasParameters) -> NativeFunction {
    Arc::new(
        move |context, ty_args, args| -> PartialVMResult<NativeResult> {
            native_insert_char(&gas_params, context, ty_args, args)
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
    pub next_char_boundary: NextCharBoundaryGasParameters,
    pub chars_count: CharsCountGasParameters,
    pub sub_string_char: SubStringCharGasParameters,
    pub insert_char: InsertCharGasParameters,
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
        (
            "internal_next_char_boundary",
            make_native_next_char_boundary(gas_params.next_char_boundary),
        ),
        (
            "internal_chars_count",
            make_native_chars_count(gas_params.chars_count),
        ),
        (
            "internal_sub_string_char",
            make_native_sub_string_char(gas_params.sub_string_char),
        ),
        (
            "internal_insert_char",
            make_native_insert_char(gas_params.insert_char),
        ),
    ];

    make_module_natives(natives)
}
