// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    account_address::AccountAddress,
    ident_str,
    identifier::Identifier,
    language_storage::{StructTag, TypeTag},
    value::{MoveStruct, MoveValue},
};
use serde_json::json;

#[test]
fn struct_deserialization() {
    let struct_type = StructTag {
        address: AccountAddress::ZERO,
        name: ident_str!("MyStruct").to_owned(),
        module: ident_str!("MyModule").to_owned(),
        type_params: vec![],
    };
    let values = vec![MoveValue::U64(7), MoveValue::Bool(true)];
    let fields = vec![ident_str!("f").to_owned(), ident_str!("g").to_owned()];
    let field_values: Vec<(Identifier, MoveValue)> =
        fields.into_iter().zip(values.clone()).collect();

    // test each deserialization scheme
    let runtime_value = MoveStruct::Runtime(values);
    assert_eq!(
        serde_json::to_value(&runtime_value).unwrap(),
        json!([7, true])
    );

    let fielded_value = MoveStruct::WithFields(field_values.clone());
    assert_eq!(
        serde_json::to_value(&fielded_value).unwrap(),
        json!({ "f": 7, "g": true })
    );

    let typed_value = MoveStruct::with_types(struct_type, field_values);
    assert_eq!(
        serde_json::to_value(&typed_value).unwrap(),
        json!({
                "fields": { "f": 7, "g": true },
                "type": "0x0::MyModule::MyStruct"
            }
        )
    );
}

#[test]
fn nested_typed_struct_deserialization() {
    let struct_type = StructTag {
        address: AccountAddress::ZERO,
        name: ident_str!("MyStruct").to_owned(),
        module: ident_str!("MyModule").to_owned(),
        type_params: vec![],
    };
    let nested_struct_type = StructTag {
        address: AccountAddress::ZERO,
        name: ident_str!("NestedStruct").to_owned(),
        module: ident_str!("NestedModule").to_owned(),
        type_params: vec![TypeTag::U8],
    };

    // test each deserialization scheme
    let nested_runtime_struct = MoveValue::Struct(MoveStruct::Runtime(vec![MoveValue::U64(7)]));
    let runtime_value = MoveStruct::Runtime(vec![nested_runtime_struct]);
    assert_eq!(serde_json::to_value(&runtime_value).unwrap(), json!([[7]]));

    let nested_fielded_struct = MoveValue::Struct(MoveStruct::with_fields(vec![(
        ident_str!("f").to_owned(),
        MoveValue::U64(7),
    )]));
    let fielded_value = MoveStruct::with_fields(vec![(
        ident_str!("inner").to_owned(),
        nested_fielded_struct,
    )]);
    assert_eq!(
        serde_json::to_value(&fielded_value).unwrap(),
        json!({ "inner": { "f": 7 } })
    );

    let nested_typed_struct = MoveValue::Struct(MoveStruct::with_types(
        nested_struct_type,
        vec![(ident_str!("f").to_owned(), MoveValue::U64(7))],
    ));
    let typed_value = MoveStruct::with_types(
        struct_type,
        vec![(ident_str!("inner").to_owned(), nested_typed_struct)],
    );
    assert_eq!(
        serde_json::to_value(&typed_value).unwrap(),
        json!({
            "fields": {
                "inner": {
                    "fields": { "f": 7},
                    "type": "0x0::NestedModule::NestedStruct<u8>",
                }
            },
            "type": "0x0::MyModule::MyStruct"
        })
    );
}
