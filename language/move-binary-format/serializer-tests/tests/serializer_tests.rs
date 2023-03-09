// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use move_binary_format::file_format::*;
use proptest::prelude::*;

proptest! {
    #[test]
    fn serializer_roundtrip(module in CompiledModule::valid_strategy(20)) {
        let mut serialized = Vec::with_capacity(2048);
        module.serialize(&mut serialized).expect("serialization should work");

        let deserialized_module = CompiledModule::deserialize(&serialized)
            .expect("deserialization should work");

        prop_assert_eq!(module, deserialized_module);
    }
}

proptest! {
    // Generating arbitrary compiled modules is really slow, possibly because of
    // https://github.com/AltSysrq/proptest/issues/143.
    #![proptest_config(ProptestConfig::with_cases(16))]

    /// Make sure that garbage inputs don't crash the serializer and deserializer.
    #[test]
    fn garbage_inputs(module in any_with::<CompiledModule>(16)) {
        let mut serialized = Vec::with_capacity(65536);
        module.serialize(&mut serialized).expect("serialization should work");

        let deserialized_module = CompiledModule::deserialize_no_check_bounds(&serialized)
            .expect("deserialization should work");
        prop_assert_eq!(module, deserialized_module);
    }
}

#[test]
fn single_fp_test() {
    let mut module = empty_module();

    module.signatures.push(Signature(vec![
        SignatureToken::Function(Box::new(FunctionType {
            parameters: vec![SignatureToken::U8],
            return_: vec![SignatureToken::U128],
        }))
    ]));

    let mut serialized = Vec::with_capacity(65536);
    module.serialize(&mut serialized).expect("serialization should work");

    let deserialized_module = CompiledModule::deserialize_no_check_bounds(&serialized)
        .expect("deserialization should work");

    assert_eq!(module, deserialized_module);
}
