// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use move_binary_format::file_format::*;
use move_bytecode_verifier::{limits::LimitsVerifier, VerifierConfig};
use move_core_types::vm_status::StatusCode;

#[test]
fn test_function_handle_type_instantiation() {
    let mut m = basic_test_module();
    m.function_handles.push(FunctionHandle {
        module: ModuleHandleIndex::new(0),
        name: IdentifierIndex::new(0),
        parameters: SignatureIndex(0),
        return_: SignatureIndex(0),
        type_parameters: std::iter::repeat(AbilitySet::ALL).take(10).collect(),
    });

    assert_eq!(
        LimitsVerifier::verify_module(
            &VerifierConfig {
                max_generic_instantiation_length: Some(9),
                ..Default::default()
            },
            &m
        )
        .unwrap_err()
        .major_status(),
        StatusCode::TOO_MANY_TYPE_PARAMETERS
    );

    let mut s = basic_test_script();
    s.function_handles.push(FunctionHandle {
        module: ModuleHandleIndex::new(0),
        name: IdentifierIndex::new(0),
        parameters: SignatureIndex(0),
        return_: SignatureIndex(0),
        type_parameters: std::iter::repeat(AbilitySet::ALL).take(10).collect(),
    });

    assert_eq!(
        LimitsVerifier::verify_script(
            &VerifierConfig {
                max_generic_instantiation_length: Some(9),
                ..Default::default()
            },
            &s
        )
        .unwrap_err()
        .major_status(),
        StatusCode::TOO_MANY_TYPE_PARAMETERS
    );
}

#[test]
fn test_struct_handle_type_instantiation() {
    let mut m = basic_test_module();
    m.struct_handles.push(StructHandle {
        module: ModuleHandleIndex::new(0),
        name: IdentifierIndex::new(0),
        abilities: AbilitySet::ALL,
        type_parameters: std::iter::repeat(StructTypeParameter {
            constraints: AbilitySet::ALL,
            is_phantom: false,
        })
        .take(10)
        .collect(),
    });

    assert_eq!(
        LimitsVerifier::verify_module(
            &VerifierConfig {
                max_generic_instantiation_length: Some(9),
                ..Default::default()
            },
            &m
        )
        .unwrap_err()
        .major_status(),
        StatusCode::TOO_MANY_TYPE_PARAMETERS
    );

    let mut s = basic_test_script();
    s.struct_handles.push(StructHandle {
        module: ModuleHandleIndex::new(0),
        name: IdentifierIndex::new(0),
        abilities: AbilitySet::ALL,
        type_parameters: std::iter::repeat(StructTypeParameter {
            constraints: AbilitySet::ALL,
            is_phantom: false,
        })
        .take(10)
        .collect(),
    });

    assert_eq!(
        LimitsVerifier::verify_script(
            &VerifierConfig {
                max_generic_instantiation_length: Some(9),
                ..Default::default()
            },
            &s
        )
        .unwrap_err()
        .major_status(),
        StatusCode::TOO_MANY_TYPE_PARAMETERS
    );
}

#[test]
fn test_function_handle_parameters() {
    let mut m = basic_test_module();
    m.signatures.push(Signature(
        std::iter::repeat(SignatureToken::Bool).take(10).collect(),
    ));
    m.function_handles.push(FunctionHandle {
        module: ModuleHandleIndex::new(0),
        name: IdentifierIndex::new(0),
        parameters: SignatureIndex(1),
        return_: SignatureIndex(0),
        type_parameters: vec![],
    });

    assert_eq!(
        LimitsVerifier::verify_module(
            &VerifierConfig {
                max_function_parameters: Some(9),
                ..Default::default()
            },
            &m
        )
        .unwrap_err()
        .major_status(),
        StatusCode::TOO_MANY_PARAMETERS
    );

    let mut s = basic_test_script();
    s.signatures.push(Signature(
        std::iter::repeat(SignatureToken::Bool).take(10).collect(),
    ));
    s.function_handles.push(FunctionHandle {
        module: ModuleHandleIndex::new(0),
        name: IdentifierIndex::new(0),
        parameters: SignatureIndex(1),
        return_: SignatureIndex(0),
        type_parameters: vec![],
    });

    assert_eq!(
        LimitsVerifier::verify_script(
            &VerifierConfig {
                max_function_parameters: Some(9),
                ..Default::default()
            },
            &s
        )
        .unwrap_err()
        .major_status(),
        StatusCode::TOO_MANY_PARAMETERS
    );
}
