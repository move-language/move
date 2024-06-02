// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use move_binary_format::errors::PartialVMResult;
use move_core_types::{
    gas_algebra::{InternalGas, InternalGasPerByte, NumBytes},
    language_storage::TypeTag,
};
use move_vm_runtime::native_functions::{NativeContext, NativeFunction};
use move_vm_types::{
    loaded_data::runtime_types::Type,
    natives::function::NativeResult,
    values::{Struct, Value},
};

use smallvec::smallvec;
use std::{collections::VecDeque, sync::Arc};

#[derive(Debug, Clone)]
pub struct GetGasParameters {
    pub base: InternalGas,
    pub per_byte: InternalGasPerByte,
}

fn native_get(
    gas_params: &GetGasParameters,
    context: &mut NativeContext,
    ty_args: Vec<Type>,
    arguments: VecDeque<Value>,
) -> PartialVMResult<NativeResult> {
    debug_assert!(ty_args.len() == 1);
    debug_assert!(arguments.is_empty());

    let type_tag = context.type_to_type_tag(&ty_args[0])?;
    let type_name = type_tag.to_canonical_string();

    let mut cost = gas_params.base;

    if let TypeTag::Struct(tag) = type_tag {
        let address = Value::address(tag.address);

        // make std::ascii::String for the module name
        let module = Value::struct_(Struct::pack(vec![Value::vector_u8(
            tag.module.into_bytes(),
        )]));

        // make std::ascii::String for the struct name
        let name = Value::struct_(Struct::pack(vec![Value::vector_u8(tag.name.into_bytes())]));

        // make a vector of std::ascii::String for the generics
        let generics_vec = tag
            .type_params
            .iter()
            .map(|ty| {
                Value::struct_(Struct::pack(vec![Value::vector_u8(
                    ty.to_canonical_string().into_bytes(),
                )]))
            })
            .collect::<Vec<Value>>();

        // convert the generics vector into move supported value.
        // using the `vector_for_testing_only` which can break as it's currently the easiest way to do this without altering the existing `Value` struct.
        // it should the replaced when the proper API is ready.
        let generics = Value::vector_for_testing_only(generics_vec);

        cost += gas_params.per_byte * NumBytes::new(type_name.len() as u64);

        Ok(NativeResult::ok(
            cost,
            smallvec![Value::struct_(Struct::pack(vec![
                address, module, name, generics
            ]))],
        ))
    } else {
        Ok(NativeResult::err(cost, 0))
    }
}

pub fn make_native_get(gas_params: GetGasParameters) -> NativeFunction {
    Arc::new(move |context, ty_args, args| native_get(&gas_params, context, ty_args, args))
}

#[derive(Debug, Clone)]
pub struct GasParameters {
    pub get: GetGasParameters,
}

pub fn make_all(gas_params: GasParameters) -> impl Iterator<Item = (String, NativeFunction)> {
    let natives = [("get", make_native_get(gas_params.get))];

    crate::natives::helpers::make_module_natives(natives)
}
