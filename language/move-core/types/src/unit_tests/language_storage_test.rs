// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    account_address::AccountAddress,
    identifier::{IdentStr, Identifier},
    language_storage::{ModuleId, StructTag, TypeTag},
};
use bcs::test_helpers::assert_canonical_encode_decode;
use proptest::prelude::*;

#[cfg(not(feature = "nostd"))]
proptest! {
    #[test]
    fn test_module_id_canonical_roundtrip(module_id in any::<ModuleId>()) {
        assert_canonical_encode_decode(module_id);
    }
}

#[test]
fn test_type_tag_deserialize_case_insensitive() {
    let org_struct_tag = StructTag {
        address: AccountAddress::ONE,
        module: Identifier::from(IdentStr::new("TestModule").unwrap()),
        name: Identifier::from(IdentStr::new("TestStruct").unwrap()),
        type_params: vec![
            TypeTag::U8,
            TypeTag::U64,
            TypeTag::U128,
            TypeTag::Bool,
            TypeTag::Address,
            TypeTag::Signer,
        ],
    };

    let current_json = serde_json::to_string(&org_struct_tag).unwrap();

    let upper_case_json = format!(
        r##"{{"address":"{}","module":"TestModule","name":"TestStruct","type_params":["U8","U64","U128","Bool","Address","Signer"]}}"##,
        AccountAddress::ONE
    );
    let upper_case_decoded = serde_json::from_str(upper_case_json.as_str()).unwrap();
    assert_eq!(org_struct_tag, upper_case_decoded);

    let lower_case_json = format!(
        r##"{{"address":"{}","module":"TestModule","name":"TestStruct","type_args":["u8","u64","u128","bool","address","signer"]}}"##,
        AccountAddress::ONE
    );
    let lower_case_decoded = serde_json::from_str(lower_case_json.as_str()).unwrap();
    assert_eq!(org_struct_tag, lower_case_decoded);

    assert_eq!(current_json, lower_case_json);
}
