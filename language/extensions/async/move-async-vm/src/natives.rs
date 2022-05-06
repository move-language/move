// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::async_vm::Message;
use better_any::{Tid, TidAble};
use move_binary_format::errors::PartialVMResult;
use move_core_types::{account_address::AccountAddress, identifier::Identifier};
use move_vm_runtime::{
    native_functions,
    native_functions::{NativeContext, NativeFunction},
};
use move_vm_types::{
    gas_schedule::NativeCostIndex,
    loaded_data::runtime_types::Type,
    natives::function::{native_gas, NativeResult},
    pop_arg,
    values::Value,
};
use smallvec::smallvec;
use std::collections::VecDeque;

// TODO: make cost tables extensible; right now we forward to one of the predefined cost indices
// as an approximation.
const SELF_COST_INDEX: NativeCostIndex = NativeCostIndex::LENGTH;
const SEND_COST_INDEX: NativeCostIndex = NativeCostIndex::EMIT_EVENT;
const EPOCH_TIME_INDEX: NativeCostIndex = NativeCostIndex::LENGTH;

/// Environment extension for the Move VM which we pass down to native functions,
/// to implement message sending and retrieval of actor address.
#[derive(Tid)]
pub struct AsyncExtension {
    pub current_actor: AccountAddress,
    pub sent: Vec<Message>,
    pub virtual_time: u128,
    pub in_initializer: bool,
}

pub fn actor_natives(
    async_addr: AccountAddress,
) -> Vec<(AccountAddress, Identifier, Identifier, NativeFunction)> {
    native_functions::make_table(
        async_addr,
        &[
            ("Actor", "self", native_self),
            ("Actor", "virtual_time", native_virtual_time),
            ("Runtime", "send__0", native_send),
            ("Runtime", "send__1", native_send),
            ("Runtime", "send__2", native_send),
            ("Runtime", "send__3", native_send),
            ("Runtime", "send__4", native_send),
            ("Runtime", "send__5", native_send),
            ("Runtime", "send__6", native_send),
            ("Runtime", "send__7", native_send),
            ("Runtime", "send__8", native_send),
        ],
    )
}

fn native_self(
    context: &mut NativeContext,
    mut _ty_args: Vec<Type>,
    mut _args: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    let cost = native_gas(context.cost_table(), SELF_COST_INDEX, 1);
    let ext = context.extensions().get::<AsyncExtension>();
    Ok(NativeResult::ok(
        cost,
        smallvec![Value::address(ext.current_actor)],
    ))
}

fn native_send(
    context: &mut NativeContext,
    mut _ty_args: Vec<Type>,
    mut args: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    let ext = context.extensions_mut().get_mut::<AsyncExtension>();
    let mut bcs_args = vec![];
    while args.len() > 2 {
        bcs_args.push(pop_arg!(args, Vec<u8>));
    }
    bcs_args.reverse();
    let message_hash = pop_arg!(args, u64);
    let target = pop_arg!(args, AccountAddress);
    ext.sent.push((target, message_hash, bcs_args));
    let cost = native_gas(context.cost_table(), SEND_COST_INDEX, args.len());
    Ok(NativeResult::ok(cost, smallvec![]))
}

fn native_virtual_time(
    context: &mut NativeContext,
    mut _ty_args: Vec<Type>,
    mut _args: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    let cost = native_gas(context.cost_table(), EPOCH_TIME_INDEX, 1);
    let ext = context.extensions().get::<AsyncExtension>();
    Ok(NativeResult::ok(
        cost,
        smallvec![Value::u128(ext.virtual_time)],
    ))
}
