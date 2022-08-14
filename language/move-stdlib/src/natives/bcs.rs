// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::natives::helpers::make_module_natives;
use move_binary_format::errors::PartialVMResult;
use move_core_types::{
    gas_algebra::{InternalGas, InternalGasPerAbstractMemoryUnit, InternalGasPerByte, NumBytes},
    vm_status::sub_status::NFE_BCS_SERIALIZATION_FAILURE,
};
use move_vm_runtime::native_functions::{NativeContext, NativeFunction};
use move_vm_types::{
    loaded_data::runtime_types::Type,
    natives::function::NativeResult,
    pop_arg,
    values::{values_impl::Reference, Value},
};
use smallvec::smallvec;
use std::{collections::VecDeque, sync::Arc};
/***************************************************************************************************
 * native fun to_bytes
 *
 *   gas cost: size_of(val_type) * input_unit_cost +        | get type layout
 *             size_of(val) * input_unit_cost +             | serialize value
 *             max(size_of(output), 1) * output_unit_cost
 *
 *             If any of the first two steps fails, a partial cost + an additional failure_cost
 *             will be charged.
 *
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct ToBytesGasParameters {
    pub input_unit_cost: InternalGasPerAbstractMemoryUnit,
    pub output_unit_cost: InternalGasPerByte,
    pub legacy_min_output_size: NumBytes,
    pub failure_cost: InternalGas,
}

/// Rust implementation of Move's `native public fun to_bytes<T>(&T): vector<u8>`
#[inline]
fn native_to_bytes(
    gas_params: &ToBytesGasParameters,
    context: &mut NativeContext,
    mut ty_args: Vec<Type>,
    mut args: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    debug_assert!(ty_args.len() == 1);
    debug_assert!(args.len() == 1);

    let mut cost = 0.into();

    // pop type and value
    let ref_to_val = pop_arg!(args, Reference);
    let arg_type = ty_args.pop().unwrap();

    // get type layout
    if gas_params.input_unit_cost != 0.into() {
        cost += gas_params.input_unit_cost * arg_type.size()
    }
    let layout = match context.type_to_type_layout(&arg_type)? {
        Some(layout) => layout,
        None => {
            cost += gas_params.failure_cost;
            return Ok(NativeResult::err(cost, NFE_BCS_SERIALIZATION_FAILURE));
        }
    };
    // serialize value
    let val = ref_to_val.read_ref()?;
    if gas_params.input_unit_cost != 0.into() {
        cost += gas_params.input_unit_cost * val.size()
    }
    let serialized_value = match val.simple_serialize(&layout) {
        Some(serialized_value) => serialized_value,
        None => {
            cost += gas_params.failure_cost;
            return Ok(NativeResult::err(cost, NFE_BCS_SERIALIZATION_FAILURE));
        }
    };
    cost += gas_params.output_unit_cost
        * std::cmp::max(
            NumBytes::new(serialized_value.len() as u64),
            gas_params.legacy_min_output_size,
        );

    Ok(NativeResult::ok(
        cost,
        smallvec![Value::vector_u8(serialized_value)],
    ))
}

pub fn make_native_to_bytes(gas_params: ToBytesGasParameters) -> NativeFunction {
    Arc::new(
        move |context, ty_args, args| -> PartialVMResult<NativeResult> {
            native_to_bytes(&gas_params, context, ty_args, args)
        },
    )
}

/***************************************************************************************************
 * module
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct GasParameters {
    pub to_bytes: ToBytesGasParameters,
}

pub fn make_all(gas_params: GasParameters) -> impl Iterator<Item = (String, NativeFunction)> {
    let natives = [("to_bytes", make_native_to_bytes(gas_params.to_bytes))];

    make_module_natives(natives)
}
