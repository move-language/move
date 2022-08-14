// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use diem_crypto::{ed25519, traits::*};
use move_binary_format::errors::PartialVMResult;
use move_vm_runtime::native_functions::NativeContext;
use move_vm_types::{
    loaded_data::runtime_types::Type, natives::function::NativeResult, pop_arg, values::Value,
};
use smallvec::smallvec;
use std::{collections::VecDeque, convert::TryFrom};

pub fn native_ed25519_publickey_validation(
    _context: &mut NativeContext,
    _ty_args: Vec<Type>,
    mut arguments: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    debug_assert!(_ty_args.is_empty());
    debug_assert!(arguments.len() == 1);

    let key_bytes = pop_arg!(arguments, Vec<u8>);

    let cost = 26 * usize::max(key_bytes.len(), 1) as u64;

    // This deserialization performs point-on-curve and small subgroup checks
    let valid = ed25519::Ed25519PublicKey::try_from(&key_bytes[..]).is_ok();
    Ok(NativeResult::ok(cost.into(), smallvec![Value::bool(valid)]))
}

pub fn native_ed25519_signature_verification(
    _context: &mut NativeContext,
    _ty_args: Vec<Type>,
    mut arguments: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    debug_assert!(_ty_args.is_empty());
    debug_assert!(arguments.len() == 3);

    let msg = pop_arg!(arguments, Vec<u8>);
    let pubkey = pop_arg!(arguments, Vec<u8>);
    let signature = pop_arg!(arguments, Vec<u8>);

    let cost = 62 * usize::max(msg.len(), 1) as u64;

    let sig = match ed25519::Ed25519Signature::try_from(signature.as_slice()) {
        Ok(sig) => sig,
        Err(_) => {
            return Ok(NativeResult::ok(cost.into(), smallvec![Value::bool(false)]));
        }
    };
    let pk = match ed25519::Ed25519PublicKey::try_from(pubkey.as_slice()) {
        Ok(pk) => pk,
        Err(_) => {
            return Ok(NativeResult::ok(cost.into(), smallvec![Value::bool(false)]));
        }
    };

    let verify_result = sig.verify_arbitrary_msg(msg.as_slice(), &pk).is_ok();
    Ok(NativeResult::ok(
        cost.into(),
        smallvec![Value::bool(verify_result)],
    ))
}
