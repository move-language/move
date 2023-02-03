// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::natives::helpers::make_module_natives;
use move_binary_format::errors::PartialVMResult;
use move_core_types::{
    gas_algebra::{InternalGas, },
};
use move_vm_runtime::native_functions::{NativeContext, NativeFunction};
use move_vm_types::{
    loaded_data::runtime_types::Type,
    natives::function::NativeResult,
    pop_arg,
    values::{Value},
};
use std::{collections::VecDeque, sync::Arc};
use move_core_types::account_address::AccountAddress;

#[derive(Debug, Clone)]
pub struct ExistsAtGasParameters {
    // TODO: in fact exists (the instruction) has a more complex gas cost based on reading the
    //   entire resource :-/ This here needs to be aligned with this.
    pub cost: InternalGas,
}

#[inline]
fn native_exists_at(
    gas_params: &ExistsAtGasParameters,
    context: &mut NativeContext,
    mut ty_args: Vec<Type>,
    mut args: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    debug_assert!(ty_args.len() == 1);
    debug_assert!(args.len() == 1);

    let addr = pop_arg!(args, AccountAddress);
    let arg_type = ty_args.pop().unwrap();

    let res = context.resource_exists_at(&arg_type, addr).map(|b| Value::bool(b));
    NativeResult::map_partial_vm_result_one(gas_params.cost, res)
}

pub fn make_native_exists_at(gas_params: ExistsAtGasParameters) -> NativeFunction {
    Arc::new(
        move |context, ty_args, args| -> PartialVMResult<NativeResult> {
            native_exists_at(&gas_params, context, ty_args, args)
        },
    )
}

/***************************************************************************************************
 * module
 **************************************************************************************************/
#[derive(Debug, Clone)]
pub struct GasParameters {
    pub exists_at: ExistsAtGasParameters,
}

pub fn make_all(gas_params: GasParameters) -> impl Iterator<Item = (String, NativeFunction)> {
    let natives = [("to_bytes", make_native_exists_at(gas_params.exists_at))];
    make_module_natives(natives)
}
