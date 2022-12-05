// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use move_binary_format::file_format::*;
use move_bytecode_verifier::{limits::LimitsVerifier, verify_module_with_config, VerifierConfig};
use move_core_types::{
    account_address::AccountAddress, identifier::Identifier, vm_status::StatusCode,
};

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

#[test]
fn big_vec_unpacks() {
    const N_TYPE_PARAMS: usize = 16;
    let mut st = SignatureToken::Vector(Box::new(SignatureToken::U8));
    let type_params = vec![st; N_TYPE_PARAMS];
    st = SignatureToken::StructInstantiation(StructHandleIndex(0), type_params);
    const N_VEC_PUSH: u16 = 1000;
    let mut code = vec![];
    // 1. CopyLoc:     ...         -> ... st
    // 2. VecPack:     ... st      -> ... Vec<st>
    // 3. VecUnpack:   ... Vec<st> -> ... st, st, st, ... st
    for _ in 0..N_VEC_PUSH {
        code.push(Bytecode::CopyLoc(0));
        code.push(Bytecode::VecPack(SignatureIndex(1), 1));
        code.push(Bytecode::VecUnpack(SignatureIndex(1), 1 << 15));
    }
    // 1. VecPack:   ... st, st, st, ... st -> ... Vec<st>
    // 2. Pop:       ... Vec<st>            -> ...
    for _ in 0..N_VEC_PUSH {
        code.push(Bytecode::VecPack(SignatureIndex(1), 1 << 15));
        code.push(Bytecode::Pop);
    }
    code.push(Bytecode::Ret);
    let type_param_constraints = StructTypeParameter {
        constraints: AbilitySet::EMPTY,
        is_phantom: false,
    };
    let module = CompiledModule {
        version: 5,
        self_module_handle_idx: ModuleHandleIndex(0),
        module_handles: vec![ModuleHandle {
            address: AddressIdentifierIndex(0),
            name: IdentifierIndex(0),
        }],
        struct_handles: vec![StructHandle {
            module: ModuleHandleIndex(0),
            name: IdentifierIndex(1),
            abilities: AbilitySet::ALL,
            type_parameters: vec![type_param_constraints; N_TYPE_PARAMS],
        }],
        function_handles: vec![FunctionHandle {
            module: ModuleHandleIndex(0),
            name: IdentifierIndex(0),
            parameters: SignatureIndex(1),
            return_: SignatureIndex(0),
            type_parameters: vec![],
        }],
        field_handles: vec![],
        friend_decls: vec![],
        struct_def_instantiations: vec![],
        function_instantiations: vec![],
        field_instantiations: vec![],
        signatures: vec![Signature(vec![]), Signature(vec![st])],
        identifiers: vec![
            Identifier::new("f").unwrap(),
            Identifier::new("generic_struct").unwrap(),
        ],
        address_identifiers: vec![AccountAddress::ONE],
        constant_pool: vec![],
        metadata: vec![],
        struct_defs: vec![StructDefinition {
            struct_handle: StructHandleIndex(0),
            field_information: StructFieldInformation::Native,
        }],
        function_defs: vec![FunctionDefinition {
            function: FunctionHandleIndex(0),
            visibility: Visibility::Public,
            is_entry: true,
            acquires_global_resources: vec![],
            code: Some(CodeUnit {
                locals: SignatureIndex(0),
                code,
            }),
        }],
    };

    // save module and verify that it can ser/de
    let mut mvbytes = vec![];
    module.serialize(&mut mvbytes).unwrap();
    let module = CompiledModule::deserialize(&mvbytes).unwrap();

    // run with mainnet aptos config
    let res = verify_module_with_config(
        &VerifierConfig {
            max_loop_depth: Some(5),
            treat_friend_as_private: false,
            max_generic_instantiation_length: Some(32),
            max_function_parameters: Some(128),
            max_basic_blocks: Some(1024),
            ..Default::default()
        },
        &module,
    );
    assert_eq!(
        res.unwrap_err().major_status(),
        StatusCode::VALUE_STACK_PUSH_OVERFLOW
    );
}
