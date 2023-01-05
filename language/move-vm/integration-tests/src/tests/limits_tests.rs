// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#![allow(dead_code)]
#![allow(unused_must_use)]
#![allow(unused_imports)]

use move_binary_format::{
    errors::VMResult,
    file_format::{
        AbilitySet, AddressIdentifierIndex, Bytecode, Bytecode::*, CodeUnit, CompiledModule,
        Constant, ConstantPoolIndex, FieldDefinition, FunctionDefinition, FunctionHandle,
        FunctionHandleIndex, FunctionInstantiation, FunctionInstantiationIndex, IdentifierIndex,
        ModuleHandle, ModuleHandleIndex, Signature, SignatureIndex, SignatureToken,
        SignatureToken::*, StructDefInstantiation, StructDefInstantiationIndex, StructDefinition,
        StructDefinitionIndex, StructFieldInformation, StructHandle, StructHandleIndex,
        StructTypeParameter, TypeSignature, Visibility::*,
    },
};
use move_bytecode_verifier::{verify_module, VerifierConfig};
use move_core_types::{
    account_address::AccountAddress,
    identifier::{IdentStr, Identifier},
    language_storage::{ModuleId, StructTag, TypeTag},
    vm_status::StatusCode,
};
use move_vm_runtime::{
    config::VMConfig,
    move_vm::MoveVM,
    session::{SerializedReturnValues, Session},
};
use move_vm_test_utils::{
    gas_schedule::{Gas, GasStatus, INITIAL_COST_SCHEDULE},
    InMemoryStorage,
};
use move_vm_types::loaded_data::runtime_types::Type;
use once_cell::sync::Lazy;
use std::time::Instant;

const MODULE_NAME: &str = "Mod";
const OUTER_NAME: &str = "Outer";
const INNER_NAME: &str = "Inner";
const FIELD_NAME: &str = "inner";
const ENTRY_POINT_NAME_1: &str = "entry_point";
const ENTRY_POINT_NAME_2: &str = "entry_point_one_ty_arg";
const ENTRY_POINT_NAME_3: &str = "entry_point_mul_ty_args";

const RECURSIVE_NAME: &str = "recursive";
const EMPTY_NAME: &str = "empty";

static DEFAULT_SIGNATURES: Lazy<Vec<Signature>> =
    Lazy::new(|| vec![Signature(vec![]), Signature(vec![TypeParameter(0)])]);

#[test]
fn test_limit_vector() {
    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(128),
        ..Default::default()
    };
    // instantiated tests
    let res = run_with_module(&verifier, vector_instantiated_2_nodes);
    res.expect("max_type_instantiation_size(128) vector_instantiated_2_nodes failed");
    let res = run_with_module(&verifier, vector_instantiated_4_nodes);
    res.expect("max_type_instantiation_size(128) vector_instantiated_4_nodes failed");
    let res = run_with_module(&verifier, vector_instantiated_6_nodes);
    res.expect("max_type_instantiation_size(128) vector_instantiated_6_nodes failed");
    let res = run_with_module(&verifier, vector_instantiated_51_nodes);
    res.expect("max_type_instantiation_size(128) vector_instantiated_51_nodes failed");
    // instantiated over simple function type parameter
    let res = run_with_module(&verifier, vector_single_type_arg_2_nodes);
    res.expect("max_type_instantiation_size(128) vector_single_type_arg_2_nodes failed");
    let res = run_with_module(&verifier, vector_single_type_arg_4_nodes);
    res.expect("max_type_instantiation_size(128) vector_single_type_arg_4_nodes failed");
    let res = run_with_module(&verifier, vector_single_type_arg_6_nodes);
    res.expect("max_type_instantiation_size(128) vector_single_type_arg_6_nodes failed");
    let res = run_with_module(&verifier, vector_single_type_arg_51_nodes);
    res.expect("max_type_instantiation_size(128) vector_single_type_arg_51_nodes failed");
    // instantiated via complex/rich function type parameter
    let res = run_with_module(&verifier, vector_mul_type_args_6_nodes);
    res.expect("max_type_instantiation_size(128) vector_mul_type_args_6_nodes failed");
    let res = run_with_module(&verifier, vector_mul_type_args_51_nodes);
    res.expect("max_type_instantiation_size(128) vector_mul_type_args_51_nodes failed");
    let res = run_with_module(&verifier, vector_mul_type_args_51_nodes_mix);
    res.expect("max_type_instantiation_size(128) vector_mul_type_args_51_nodes_mix failed");

    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(60),
        ..Default::default()
    };
    // instantiated tests
    let res = run_with_module(&verifier, vector_instantiated_2_nodes);
    res.expect("max_type_instantiation_size(60) vector_instantiated_2_nodes failed");
    let res = run_with_module(&verifier, vector_instantiated_4_nodes);
    res.expect("max_type_instantiation_size(60) vector_instantiated_4_nodes failed");
    let res = run_with_module(&verifier, vector_instantiated_6_nodes);
    res.expect("max_type_instantiation_size(60) vector_instantiated_6_nodes failed");
    let res = run_with_module(&verifier, vector_instantiated_51_nodes);
    res.expect("max_type_instantiation_size(60) vector_instantiated_51_nodes failed");
    // instantiated over simple function type parameter
    let res = run_with_module(&verifier, vector_single_type_arg_2_nodes);
    res.expect("max_type_instantiation_size(60) vector_single_type_arg_2_nodes failed");
    let res = run_with_module(&verifier, vector_single_type_arg_4_nodes);
    res.expect("max_type_instantiation_size(60) vector_single_type_arg_4_nodes failed");
    let res = run_with_module(&verifier, vector_single_type_arg_6_nodes);
    res.expect("max_type_instantiation_size(60) vector_single_type_arg_6_nodes failed");
    let res = run_with_module(&verifier, vector_single_type_arg_51_nodes);
    res.expect("max_type_instantiation_size(60) vector_single_type_arg_51_nodes failed");
    // instantiated via complex/rich function type parameter
    let res = run_with_module(&verifier, vector_mul_type_args_6_nodes);
    res.expect("max_type_instantiation_size(60) vector_mul_type_args_6_nodes failed");
    let res = run_with_module(&verifier, vector_mul_type_args_51_nodes);
    res.expect("max_type_instantiation_size(60) vector_mul_type_args_51_nodes failed");
    let res = run_with_module(&verifier, vector_mul_type_args_51_nodes_mix);
    res.expect("max_type_instantiation_size(60) vector_mul_type_args_51_nodes_mix failed");

    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(52),
        ..Default::default()
    };
    // instantiated tests
    let res = run_with_module(&verifier, vector_instantiated_51_nodes);
    res.expect("max_type_instantiation_size(52) vector_instantiated_51_nodes failed");
    // instantiated over simple function type parameter
    let res = run_with_module(&verifier, vector_single_type_arg_51_nodes);
    res.expect("max_type_instantiation_size(52) vector_single_type_arg_51_nodes failed");
    // instantiated via complex/rich function type parameter
    let res = run_with_module(&verifier, vector_mul_type_args_51_nodes);
    res.expect("max_type_instantiation_size(52) vector_mul_type_args_51_nodes failed");
    let res = run_with_module(&verifier, vector_mul_type_args_51_nodes_mix);
    res.expect("max_type_instantiation_size(52) vector_mul_type_args_51_nodes_mix failed");

    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(51),
        ..Default::default()
    };
    // instantiated tests
    let res = run_with_module(&verifier, vector_instantiated_51_nodes);
    res.expect("max_type_instantiation_size(51) vector_instantiated_51_nodes failed");
    // instantiated over simple function type parameter
    let res = run_with_module(&verifier, vector_single_type_arg_51_nodes);
    res.expect("max_type_instantiation_size(51) vector_single_type_arg_51_nodes failed");
    // instantiated via complex/rich function type parameter
    let res = run_with_module(&verifier, vector_mul_type_args_51_nodes);
    res.expect("max_type_instantiation_size(51) vector_mul_type_args_51_nodes failed");
    let res = run_with_module(&verifier, vector_mul_type_args_51_nodes_mix);
    res.expect("max_type_instantiation_size(51) vector_mul_type_args_51_nodes_mix failed");

    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(50),
        ..Default::default()
    };
    // instantiated tests
    let res = run_with_module(&verifier, vector_instantiated_51_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(50) vector_instantiated_51_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    // instantiated over simple function type parameter
    let res = run_with_module(&verifier, vector_single_type_arg_51_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(50) vector_single_type_arg_51_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    // instantiated via complex/rich function type parameter
    let res = run_with_module(&verifier, vector_mul_type_args_51_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(50) vector_mul_type_args_51_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, vector_mul_type_args_51_nodes_mix);
    let err = res
        .expect_err("max_type_instantiation_size(50) vector_mul_type_args_51_nodes_mix must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);

    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(4),
        ..Default::default()
    };
    // instantiated tests
    let res = run_with_module(&verifier, vector_instantiated_6_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(4) vector_instantiated_6_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, vector_instantiated_51_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(4) vector_instantiated_51_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, vector_instantiated_4_nodes);
    res.expect("max_type_instantiation_size(4) vector_instantiated_4_nodes failed");
    // instantiated over simple function type parameter
    let res = run_with_module(&verifier, vector_single_type_arg_6_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(4) vector_single_type_arg_6_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, vector_single_type_arg_51_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(4) vector_single_type_arg_51_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, vector_single_type_arg_4_nodes);
    res.expect("max_type_instantiation_size(4) vector_single_type_arg_4_nodes failed");
    // instantiated via complex/rich function type parameter
    let res = run_with_module(&verifier, vector_mul_type_args_6_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(4) vector_mul_type_args_6_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, vector_mul_type_args_51_nodes_mix);
    let err = res
        .expect_err("max_type_instantiation_size(50) vector_mul_type_args_51_nodes_mix must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);

    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(2),
        ..Default::default()
    };
    let res = run_with_module(&verifier, vector_instantiated_6_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(2) vector_instantiated_6_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, vector_instantiated_4_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(2) vector_instantiated_4_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);

    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(6),
        ..Default::default()
    };
    let res = run_with_module(&verifier, vector_mul_type_args_6_nodes);
    res.expect("max_type_instantiation_size(5) vector_mul_type_args_6_nodes failed");
}

#[test]
fn test_limit_global_ops() {
    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(1000),
        ..Default::default()
    };
    // instantiated tests
    let res = run_with_module(&verifier, mut_borrow_instantiated_2_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(1000) mut_borrow_instantiated_2_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, imm_borrow_instantiated_2_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(1000) imm_borrow_instantiated_2_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, exists_instantiated_2_nodes);
    res.expect("max_type_instantiation_size(1000) exists_instantiated_2_nodes failed");
    let res = run_with_module(&verifier, move_from_instantiated_2_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(1000) move_from_instantiated_2_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, mut_borrow_instantiated_6_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(1000) mut_borrow_instantiated_6_node must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, imm_borrow_instantiated_6_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(1000) imm_borrow_instantiated_6_node must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, exists_instantiated_6_nodes);
    res.expect("max_type_instantiation_size(1000) exists_instantiated_6_node failed");
    let res = run_with_module(&verifier, move_from_instantiated_6_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(1000) move_from_instantiated_6_node must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, mut_borrow_instantiated_51_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(1000) mut_borrow_instantiated_51_node must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, imm_borrow_instantiated_51_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(1000) imm_borrow_instantiated_51_node must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, exists_instantiated_51_nodes);
    res.expect("max_type_instantiation_size(1000) exists_instantiated_51_node failed");
    let res = run_with_module(&verifier, move_from_instantiated_51_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(1000) move_from_instantiated_51_node must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    // instantiated over simple function type parameter
    let res = run_with_module(&verifier, mut_borrow_single_type_arg_2_nodes);
    let err = res.expect_err(
        "max_type_instantiation_size(1000) mut_borrow_single_type_arg_2_nodes must fail",
    );
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, imm_borrow_single_type_arg_2_nodes);
    let err = res.expect_err(
        "max_type_instantiation_size(1000) imm_borrow_single_type_arg_2_nodes must fail",
    );
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, exists_single_type_arg_2_nodes);
    res.expect("max_type_instantiation_size(1000) exists_single_type_arg_2_nodes failed");
    let res = run_with_module(&verifier, move_from_single_type_arg_2_nodes);
    let err = res.expect_err(
        "max_type_instantiation_size(1000) move_from_single_type_arg_2_nodes must fail",
    );
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, mut_borrow_single_type_arg_6_nodes);
    let err = res.expect_err(
        "max_type_instantiation_size(1000) mut_borrow_single_type_arg_6_node must fail",
    );
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, imm_borrow_single_type_arg_6_nodes);
    let err = res.expect_err(
        "max_type_instantiation_size(1000) imm_borrow_single_type_arg_6_node must fail",
    );
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, exists_single_type_arg_6_nodes);
    res.expect("max_type_instantiation_size(1000) exists_single_type_arg_6_node failed");
    let res = run_with_module(&verifier, move_from_single_type_arg_6_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(1000) move_from_single_type_arg_6_node must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, mut_borrow_single_type_arg_51_nodes);
    let err = res.expect_err(
        "max_type_instantiation_size(1000) mut_borrow_single_type_arg_51_node must fail",
    );
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, imm_borrow_single_type_arg_51_nodes);
    let err = res.expect_err(
        "max_type_instantiation_size(1000) imm_borrow_single_type_arg_51_node must fail",
    );
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, exists_single_type_arg_51_nodes);
    res.expect("max_type_instantiation_size(1000) exists_single_type_arg_51_node failed");
    let res = run_with_module(&verifier, move_from_single_type_arg_51_nodes);
    let err = res.expect_err(
        "max_type_instantiation_size(1000) move_from_single_type_arg_51_node must fail",
    );
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    // instantiated via complex/rich function type parameter
    let res = run_with_module(&verifier, mut_borrow_mul_type_args_6_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(1000) mut_borrow_mul_type_args_6_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, imm_borrow_mul_type_args_6_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(1000) imm_borrow_mul_type_args_6_node must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, exists_mul_type_args_6_nodes);
    res.expect("max_type_instantiation_size(1000) exists_mul_type_args_6_node failed");
    let res = run_with_module(&verifier, move_from_mul_type_args_6_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(1000) move_from_mul_type_args_6_node must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, mut_borrow_mul_type_args_51_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(1000) mut_borrow_mul_type_args_51_node must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, imm_borrow_mul_type_args_51_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(1000) imm_borrow_mul_type_args_51_node must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, exists_mul_type_args_51_nodes);
    res.expect("max_type_instantiation_size(1000) exists_mul_type_args_51_node failed");
    let res = run_with_module(&verifier, move_from_mul_type_args_51_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(1000) move_from_mul_type_args_51_node must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, mut_borrow_mul_type_args_51_nodes_mix);
    let err = res.expect_err(
        "max_type_instantiation_size(1000) mut_borrow_mul_type_args_51_node_mix must fail",
    );
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, imm_borrow_mul_type_args_51_nodes_mix);
    let err = res.expect_err(
        "max_type_instantiation_size(1000) imm_borrow_mul_type_args_51_node_mix must fail",
    );
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, exists_mul_type_args_51_nodes_mix);
    res.expect("max_type_instantiation_size(1000) exists_mul_type_args_51_node_mix failed");
    let res = run_with_module(&verifier, move_from_mul_type_args_51_nodes_mix);
    let err = res.expect_err(
        "max_type_instantiation_size(1000) move_from_mul_type_args_51_node_mix must fail",
    );
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);

    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(60),
        ..Default::default()
    };
    // instantiated tests
    let res = run_with_module(&verifier, exists_instantiated_2_nodes);
    res.expect("max_type_instantiation_size(60) exists_instantiated_2_nodes failed");
    let res = run_with_module(&verifier, imm_borrow_instantiated_6_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(60) imm_borrow_instantiated_6_node must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, move_from_instantiated_51_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(60) move_from_instantiated_51_node must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    // instantiated over simple function type parameter
    let res = run_with_module(&verifier, move_from_single_type_arg_2_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(60) move_from_single_type_arg_2_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, exists_single_type_arg_6_nodes);
    res.expect("max_type_instantiation_size(60) exists_single_type_arg_6_node failed");
    let res = run_with_module(&verifier, imm_borrow_single_type_arg_51_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(60) imm_borrow_single_type_arg_51_node must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, mut_borrow_single_type_arg_51_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(60) imm_borrow_single_type_arg_51_node must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, exists_mul_type_args_6_nodes);
    res.expect("max_type_instantiation_size(60) exists_mul_type_args_6_nodes failed");
    let res = run_with_module(&verifier, imm_borrow_mul_type_args_51_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(60) imm_borrow_mul_type_args_51_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, move_from_mul_type_args_51_nodes_mix);
    let err = res.expect_err(
        "max_type_instantiation_size(60) move_from_mul_type_args_51_nodes_mix must fail",
    );
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);

    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(52),
        ..Default::default()
    };
    // instantiated tests
    let res = run_with_module(&verifier, mut_borrow_instantiated_2_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(52) mut_borrow_instantiated_2_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, move_from_instantiated_6_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(52) move_from_instantiated_6_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, exists_instantiated_51_nodes);
    res.expect("max_type_instantiation_size(52) exists_instantiated_51_nodes failed");
    // instantiated over simple function type parameter
    let res = run_with_module(&verifier, move_from_single_type_arg_2_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(52) move_from_single_type_arg_2_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, imm_borrow_single_type_arg_6_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(52) imm_borrow_single_type_arg_6_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, exists_single_type_arg_51_nodes);
    res.expect("max_type_instantiation_size(52) exists_single_type_arg_51_nodes failed");
    let res = run_with_module(&verifier, imm_borrow_mul_type_args_51_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(52) imm_borrow_mul_type_args_51_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, move_from_mul_type_args_51_nodes_mix);
    let err = res.expect_err(
        "max_type_instantiation_size(52) move_from_mul_type_args_51_nodes_mix must fail",
    );
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);

    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(51),
        ..Default::default()
    };
    // instantiated tests
    let res = run_with_module(&verifier, mut_borrow_instantiated_2_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(51) mut_borrow_instantiated_2_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, exists_instantiated_51_nodes);
    res.expect("max_type_instantiation_size(51) exists_instantiated_51_nodes failed");
    let res = run_with_module(&verifier, move_from_instantiated_51_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(51) move_from_instantiated_51_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    // instantiated over simple function type parameter
    let res = run_with_module(&verifier, imm_borrow_single_type_arg_6_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(51) imm_borrow_single_type_arg_6_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, move_from_single_type_arg_51_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(51) move_from_single_type_arg_51_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, imm_borrow_single_type_arg_51_nodes);
    let err = res.expect_err(
        "max_type_instantiation_size(51) imm_borrow_single_type_arg_51_nodes must fail",
    );
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, exists_mul_type_args_51_nodes);
    res.expect("max_type_instantiation_size(51) exists_mul_type_args_51_nodes failed");
    let res = run_with_module(&verifier, imm_borrow_mul_type_args_51_nodes_mix);
    let err = res.expect_err(
        "max_type_instantiation_size(51) imm_borrow_mul_type_args_51_nodes_mix must fail",
    );
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);

    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(50),
        ..Default::default()
    };
    // instantiated tests
    let res = run_with_module(&verifier, move_from_instantiated_2_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(50) move_from_instantiated_2_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, exists_instantiated_51_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(50) exists_instantiated_51_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, move_from_instantiated_51_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(50) move_from_instantiated_51_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, mut_borrow_instantiated_51_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(50) mut_borrow_instantiated_51_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    // instantiated over simple function type parameter
    let res = run_with_module(&verifier, mut_borrow_single_type_arg_6_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(50) mut_borrow_single_type_arg_6_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, imm_borrow_single_type_arg_51_nodes);
    let err = res.expect_err(
        "max_type_instantiation_size(50) imm_borrow_single_type_arg_51_nodes must fail",
    );
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, mut_borrow_single_type_arg_51_nodes);
    let err = res.expect_err(
        "max_type_instantiation_size(50) mut_borrow_single_type_arg_51_nodes must fail",
    );
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, exists_single_type_arg_51_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(50) exists_single_type_arg_51_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, exists_mul_type_args_51_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(50) exists_single_type_arg_51_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, mut_borrow_mul_type_args_51_nodes_mix);
    let err = res.expect_err(
        "max_type_instantiation_size(51) mut_borrow_mul_type_args_51_nodes_mix must fail",
    );
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, move_from_mul_type_args_51_nodes_mix);
    let err = res.expect_err(
        "max_type_instantiation_size(51) move_from_mul_type_args_51_nodes_mix must fail",
    );
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);

    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(5),
        ..Default::default()
    };
    let res = run_with_module(&verifier, exists_instantiated_2_nodes);
    res.expect("max_type_instantiation_size(51) exists_instantiated_2_nodes failed");
    let res = run_with_module(&verifier, imm_borrow_instantiated_2_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(5) imm_borrow_instantiated_2_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, mut_borrow_instantiated_6_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(5) mut_borrow_instantiated_6_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, move_from_instantiated_6_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(5) move_from_instantiated_6_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, exists_instantiated_6_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(5) exists_instantiated_6_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, exists_instantiated_51_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(5) exists_instantiated_51_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    // instantiated over simple function type parameter
    let res = run_with_module(&verifier, move_from_single_type_arg_2_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(5) move_from_single_type_arg_2_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::MISSING_DATA);
    let res = run_with_module(&verifier, imm_borrow_single_type_arg_6_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(5) imm_borrow_single_type_arg_6_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, exists_single_type_arg_6_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(5) exists_single_type_arg_6_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, exists_single_type_arg_51_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(5) exists_single_type_arg_51_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, mut_borrow_mul_type_args_6_nodes);
    let err =
        res.expect_err("max_type_instantiation_size(5) mut_borrow_mul_type_args_6_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, imm_borrow_mul_type_args_51_nodes_mix);
    let err = res.expect_err(
        "max_type_instantiation_size(5) imm_borrow_mul_type_args_51_nodes_mix must fail",
    );
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
}

#[test]
fn test_pack_generic() {
    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(80),
        ..Default::default()
    };
    // instantiated tests
    let res = run_with_module(&verifier, pack_generic_instantiated_6_nodes);
    res.expect("max_type_instantiation_size(80) pack_generic_instantiated_6_nodes failed");
    let res = run_with_module(&verifier, pack_generic_instantiated_51_nodes);
    res.expect("max_type_instantiation_size(80) pack_generic_instantiated_51_nodes failed");
    let res = run_with_module(&verifier, pack_generic_single_type_arg_6_nodes);
    res.expect("max_type_instantiation_size(80) pack_generic_single_type_arg_6_nodes failed");
    let res = run_with_module(&verifier, pack_generic_single_type_arg_51_nodes);
    res.expect("max_type_instantiation_size(80) pack_generic_single_type_arg_51_nodes failed");
    let res = run_with_module(&verifier, pack_generic_multi_type_args_6_nodes);
    res.expect("max_type_instantiation_size(80) pack_generic_multi_type_args_6_nodes failed");
    let res = run_with_module(&verifier, pack_generic_multi_type_args_51_nodes);
    res.expect("max_type_instantiation_size(80) pack_generic_multi_type_args_51_nodes failed");

    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(52),
        ..Default::default()
    };
    // instantiated tests
    let res = run_with_module(&verifier, pack_generic_instantiated_6_nodes);
    res.expect("max_type_instantiation_size(52) pack_generic_instantiated_6_nodes failed");
    let res = run_with_module(&verifier, pack_generic_instantiated_51_nodes);
    res.expect("max_type_instantiation_size(52) pack_generic_instantiated_51_nodes failed");
    let res = run_with_module(&verifier, pack_generic_single_type_arg_6_nodes);
    res.expect("max_type_instantiation_size(52) pack_generic_single_type_arg_6_nodes failed");
    let res = run_with_module(&verifier, pack_generic_single_type_arg_51_nodes);
    res.expect("max_type_instantiation_size(52) pack_generic_single_type_arg_51_nodes failed");
    let res = run_with_module(&verifier, pack_generic_multi_type_args_6_nodes);
    res.expect("max_type_instantiation_size(52) pack_generic_multi_type_args_6_nodes failed");
    let res = run_with_module(&verifier, pack_generic_multi_type_args_51_nodes);
    res.expect("max_type_instantiation_size(52) pack_generic_multi_type_args_51_nodes failed");

    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(51),
        ..Default::default()
    };
    // instantiated tests
    let res = run_with_module(&verifier, pack_generic_instantiated_6_nodes);
    res.expect("max_type_instantiation_size(51) pack_generic_instantiated_6_nodes failed");
    let res = run_with_module(&verifier, pack_generic_instantiated_51_nodes);
    res.expect("max_type_instantiation_size(51) pack_generic_instantiated_51_nodes failed");
    let res = run_with_module(&verifier, pack_generic_single_type_arg_6_nodes);
    res.expect("max_type_instantiation_size(51) pack_generic_single_type_arg_6_nodes failed");
    let res = run_with_module(&verifier, pack_generic_single_type_arg_51_nodes);
    res.expect("max_type_instantiation_size(51) pack_generic_single_type_arg_51_nodes failed");
    let res = run_with_module(&verifier, pack_generic_multi_type_args_6_nodes);
    res.expect("max_type_instantiation_size(51) pack_generic_multi_type_args_6_nodes failed");
    let res = run_with_module(&verifier, pack_generic_multi_type_args_51_nodes);
    res.expect("max_type_instantiation_size(51) pack_generic_multi_type_args_51_nodes failed");

    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(50),
        ..Default::default()
    };
    // instantiated tests
    let res = run_with_module(&verifier, pack_generic_instantiated_6_nodes);
    res.expect("max_type_instantiation_size(50) pack_generic_instantiated_6_nodes failed");
    let res = run_with_module(&verifier, pack_generic_instantiated_51_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(50) pack_generic_instantiated_51_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, pack_generic_single_type_arg_6_nodes);
    res.expect("max_type_instantiation_size(50) pack_generic_single_type_arg_6_nodes failed");
    let res = run_with_module(&verifier, pack_generic_single_type_arg_51_nodes);
    let err = res.expect_err(
        "max_type_instantiation_size(50) pack_generic_single_type_arg_51_nodes must fail",
    );
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, pack_generic_multi_type_args_6_nodes);
    res.expect("max_type_instantiation_size(50) pack_generic_multi_type_args_6_nodes failed");
    let res = run_with_module(&verifier, pack_generic_multi_type_args_51_nodes);
    let err = res.expect_err(
        "max_type_instantiation_size(50) pack_generic_multi_type_args_51_nodes must fail",
    );
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);

    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(5),
        ..Default::default()
    };
    // instantiated tests
    let res = run_with_module(&verifier, pack_generic_instantiated_6_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(5) pack_generic_instantiated_6_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, pack_generic_instantiated_51_nodes);
    let err = res
        .expect_err("max_type_instantiation_size(5) pack_generic_instantiated_51_nodes must fail");
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, pack_generic_single_type_arg_6_nodes);
    let err = res.expect_err(
        "max_type_instantiation_size(5) pack_generic_single_type_arg_6_nodes must fail",
    );
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, pack_generic_single_type_arg_51_nodes);
    let err = res.expect_err(
        "max_type_instantiation_size(5) pack_generic_single_type_arg_51_nodes must fail",
    );
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, pack_generic_multi_type_args_6_nodes);
    let err = res.expect_err(
        "max_type_instantiation_size(50) pack_generic_multi_type_args_6_nodes must fail",
    );
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
    let res = run_with_module(&verifier, pack_generic_multi_type_args_51_nodes);
    let err = res.expect_err(
        "max_type_instantiation_size(50) pack_generic_multi_type_args_51_nodes must fail",
    );
    assert_eq!(err.major_status(), StatusCode::VERIFICATION_ERROR);
}

#[test]
fn test_call_generic() {
    let verifier = VerifierConfig {
        max_function_instantiation_size: Some(50),
        ..Default::default()
    };
    let res = run_with_module(&verifier, call_instantiated_1_2);
    res.expect("max_function_instantiation_size(50) call_instantiated_1_2 failed");
    let res = run_with_module(&verifier, call_instantiated_6_6);
    res.expect("max_function_instantiation_size(50) call_instantiated_6_6 failed");
    let res = run_with_module(&verifier, call_instantiated_11_31);
    res.expect("max_function_instantiation_size(50) call_instantiated_11_31 failed");
    let res = run_with_module(&verifier, call_single_type_arg_11_31);
    res.expect("max_function_instantiation_size(50) call_single_tye_arg_11_31 failed");
    let res = run_with_module(&verifier, call_multi_type_args_11_31);
    res.expect("max_function_instantiation_size(50) call_multi_type_args_11_31 failed");

    let verifier = VerifierConfig {
        max_function_instantiation_size: Some(42),
        ..Default::default()
    };
    let res = run_with_module(&verifier, call_instantiated_1_2);
    res.expect("max_function_instantiation_size(42) call_instantiated_1_2 failed");
    let res = run_with_module(&verifier, call_instantiated_11_31);
    res.expect("max_function_instantiation_size(42) call_instantiated_11_31 failed");
    let res = run_with_module(&verifier, call_single_type_arg_11_31);
    res.expect("max_function_instantiation_size(42) call_single_tye_arg_11_31 failed");
    let res = run_with_module(&verifier, call_multi_type_args_11_31);
    res.expect("max_function_instantiation_size(42) call_multi_type_args_11_31 failed");

    let verifier = VerifierConfig {
        max_function_instantiation_size: Some(41),
        ..Default::default()
    };
    let res = run_with_module(&verifier, call_instantiated_6_6);
    res.expect("max_function_instantiation_size(41) call_instantiated_6_6 failed");
    let res = run_with_module(&verifier, call_instantiated_11_31);
    let err = res.expect_err("max_type_instantiation_size(41) call_instantiated_11_31 must fail");
    assert_eq!(err.major_status(), StatusCode::TOO_MANY_TYPE_NODES);

    let verifier = VerifierConfig {
        max_function_instantiation_size: Some(40),
        ..Default::default()
    };
    let res = run_with_module(&verifier, call_instantiated_11_31);
    let err = res.expect_err("max_type_instantiation_size(40) call_instantiated_11_31 must fail");
    assert_eq!(err.major_status(), StatusCode::TOO_MANY_TYPE_NODES);
    let res = run_with_module(&verifier, call_single_type_arg_11_31);
    let err = res.expect_err("max_type_instantiation_size(40) call_single_tye_arg_11_31 must fail");
    assert_eq!(err.major_status(), StatusCode::TOO_MANY_TYPE_NODES);
    let res = run_with_module(&verifier, call_multi_type_args_11_31);
    let err =
        res.expect_err("max_type_instantiation_size(40) call_multi_type_args_11_31 must fail");
    assert_eq!(err.major_status(), StatusCode::TOO_MANY_TYPE_NODES);

    let verifier = VerifierConfig {
        max_function_instantiation_size: Some(30),
        ..Default::default()
    };
    let res = run_with_module(&verifier, call_instantiated_11_31);
    let err = res.expect_err("max_type_instantiation_size(40) call_instantiated_11_31 must fail");
    assert_eq!(err.major_status(), StatusCode::TOO_MANY_TYPE_NODES);
    let res = run_with_module(&verifier, call_single_type_arg_11_31);
    let err = res.expect_err("max_type_instantiation_size(40) call_single_tye_arg_11_31 must fail");
    assert_eq!(err.major_status(), StatusCode::TOO_MANY_TYPE_NODES);
    let res = run_with_module(&verifier, call_multi_type_args_11_31);
    let err =
        res.expect_err("max_type_instantiation_size(40) call_multi_type_args_11_31 must fail");
    assert_eq!(err.major_status(), StatusCode::TOO_MANY_TYPE_NODES);

    let verifier = VerifierConfig {
        max_function_instantiation_size: Some(10),
        ..Default::default()
    };
    let res = run_with_module(&verifier, call_instantiated_11_31);
    let err = res.expect_err("max_type_instantiation_size(10) call_instantiated_11_31 must fail");
    assert_eq!(err.major_status(), StatusCode::TOO_MANY_TYPE_NODES);
    let res = run_with_module(&verifier, call_single_type_arg_11_31);
    let err = res.expect_err("max_type_instantiation_size(10) call_single_tye_arg_11_31 must fail");
    assert_eq!(err.major_status(), StatusCode::TOO_MANY_TYPE_NODES);
    let res = run_with_module(&verifier, call_multi_type_args_11_31);
    let err =
        res.expect_err("max_type_instantiation_size(10) call_multi_type_args_11_31 must fail");
    assert_eq!(err.major_status(), StatusCode::TOO_MANY_TYPE_NODES);
}

#[test]
fn test_call_generic_type_error() {
    let verifier = VerifierConfig {
        max_type_instantiation_size: Some(10),
        ..Default::default()
    };
    let res = call_vector_arg_9(&verifier);
    res.expect("max_function_instantiation_size(10) call_vector_arg_9 failed");
    let res = call_vector_arg_10(&verifier);
    res.expect("max_function_instantiation_size(10) call_vector_arg_10 failed");
    let res = call_vector_arg_11(&verifier);
    let err = res.expect_err("max_type_instantiation_size(10) call_vector_arg_11 must fail");
    assert_eq!(err.major_status(), StatusCode::TOO_MANY_TYPE_NODES);
    let res = call_generic_arg(&verifier, 8);
    res.expect("max_function_instantiation_size(10) call_generic_arg 9 args failed");
    let res = call_generic_arg(&verifier, 9);
    res.expect("max_function_instantiation_size(10) call_generic_arg 10 args failed");
    let res = call_generic_arg(&verifier, 10);
    let err = res.expect_err("max_type_instantiation_size(10) call_generic_arg 11 args must fail");
    assert_eq!(err.major_status(), StatusCode::TOO_MANY_TYPE_NODES);
}

// Generate a verifiable module with code that can be used to test instantiations.
// The code is generated by the different tests.
// Provide 2 structs and 3 functions and allow customization of structs and functions
// to test instantiations.
// The 2 structs have the following shape
// struct Outer { inner: vector<Inner> }
// struct Inner<X, Y, .., Z> { field1: X, field2: Y, ..., field(n): Z }
// so that an instance of the Outer struct can be created with an empty vector
// and tests for complex instantiation can be built.
// The number of type parameters for Inner is defined by `struct_type_args_count`.
// The 3 functions have the following signature
// fun entry_point() { ... }
// fun entry_point_one_ty_arg<T>() { ... }
// fun entry_point_mul_ty_args<X, Y, ..., Z>() { ... }
// The number of type parameters for entry_point_mul_ty_args is defined by `fun_type_args_count`.
// Definitions for the 3 functions is provided via `acquires` and `code`, where the content
// of each function is provided in inverse order, so
// [entry_point_mul_ty_args, entry_point_one_ty_arg, entry_point].
// Other aspects of the module are defined in the arguments following `code`.
//
// Please see usage in test to familiarize with how this function is used.
fn make_module(
    session: &mut Session<InMemoryStorage>,
    addr: AccountAddress,
    struct_type_args_count: usize,
    fun_type_args_count: usize,
    mut acquires: Vec<Vec<StructDefinitionIndex>>,
    mut code: Vec<Vec<Bytecode>>,
    mut parameters: Vec<SignatureIndex>,
    mut locals: Vec<SignatureIndex>,
    signatures: Vec<Signature>,
    struct_def_instantiations: Vec<StructDefInstantiation>,
    function_instantiations: Vec<FunctionInstantiation>,
) -> ModuleId {
    // default identifiers
    let mut identifiers = vec![
        Identifier::new(MODULE_NAME).unwrap(),
        Identifier::new(OUTER_NAME).unwrap(),
        Identifier::new(INNER_NAME).unwrap(),
        Identifier::new(FIELD_NAME).unwrap(),
        Identifier::new(ENTRY_POINT_NAME_1).unwrap(),
        Identifier::new(ENTRY_POINT_NAME_2).unwrap(),
        Identifier::new(ENTRY_POINT_NAME_3).unwrap(),
    ];

    // define one field for each generic parameter, e.g.
    // struct S<T, W> { field_0: T, field_1: W }
    let mut field_defs = vec![];
    for idx in 0..struct_type_args_count {
        identifiers.push(Identifier::new(format!("field_{}", idx).as_str()).unwrap());
        let id_idx = identifiers.len() - 1;
        field_defs.push(FieldDefinition {
            name: IdentifierIndex(id_idx as u16),
            signature: TypeSignature(TypeParameter(idx as u16)),
        });
    }
    let fields = StructFieldInformation::Declared(field_defs);

    let module = CompiledModule {
        version: 6,
        // Module definition
        self_module_handle_idx: ModuleHandleIndex(0),
        module_handles: vec![ModuleHandle {
            address: AddressIdentifierIndex(0),
            name: IdentifierIndex(0),
        }],
        // struct definition
        struct_handles: vec![
            StructHandle {
                module: ModuleHandleIndex(0),
                name: IdentifierIndex(1),
                abilities: AbilitySet::ALL,
                type_parameters: vec![StructTypeParameter {
                    constraints: AbilitySet::EMPTY,
                    is_phantom: false,
                }],
            },
            StructHandle {
                module: ModuleHandleIndex(0),
                name: IdentifierIndex(2),
                abilities: AbilitySet::ALL,
                type_parameters: vec![
                    StructTypeParameter {
                        constraints: AbilitySet::EMPTY,
                        is_phantom: false,
                    };
                    struct_type_args_count
                ],
            },
        ],
        struct_defs: vec![
            // struct Outer<T> { inner: vector<T>; }
            // defines a struct that mimics an `Option` field.
            // It allows for the easy creation of complex generic instance without
            // having to build the instance. E.g.
            // struct Outer<Inner<Inner<vector<Inner<vector<.....>>> and so
            // let outer = Outer { inner: vector[], };
            // move_to<...>(addr, outer); // or other bytecodes
            // which results in the ability to test different instantiations at runtime
            StructDefinition {
                struct_handle: StructHandleIndex(0),
                field_information: StructFieldInformation::Declared(vec![FieldDefinition {
                    name: IdentifierIndex(3),
                    signature: TypeSignature(Vector(Box::new(TypeParameter(0)))),
                }]),
            },
            // struct Inner<T, W, ..., Z> { field1: T field2: W, ..., field3: Z; }
            // allows checks of field instantiations instructions
            StructDefinition {
                struct_handle: StructHandleIndex(1),
                field_information: fields,
            },
        ],
        // function definition
        function_handles: vec![
            // fun entry_point()
            FunctionHandle {
                module: ModuleHandleIndex(0),
                name: IdentifierIndex(4),
                parameters: parameters.pop().unwrap(),
                return_: SignatureIndex(0),
                type_parameters: vec![],
            },
            // fun entry_point_one_ty_arg<T>()
            FunctionHandle {
                module: ModuleHandleIndex(0),
                name: IdentifierIndex(5),
                parameters: parameters.pop().unwrap(),
                return_: SignatureIndex(0),
                type_parameters: vec![AbilitySet::VECTOR],
            },
            // fun entry_point_mul_ty_args<X, Y, ..., Z>()
            // for `fun_type_args_count` args
            FunctionHandle {
                module: ModuleHandleIndex(0),
                name: IdentifierIndex(6),
                parameters: parameters.pop().unwrap(),
                return_: SignatureIndex(0),
                type_parameters: vec![AbilitySet::VECTOR; fun_type_args_count],
            },
        ],
        function_defs: vec![
            FunctionDefinition {
                function: FunctionHandleIndex(0),
                visibility: Public,
                is_entry: true,
                acquires_global_resources: acquires.pop().unwrap(),
                code: Some(CodeUnit {
                    locals: locals.pop().unwrap(),
                    code: code.pop().unwrap(),
                }),
            },
            FunctionDefinition {
                function: FunctionHandleIndex(1),
                visibility: Public,
                is_entry: true,
                acquires_global_resources: acquires.pop().unwrap(),
                code: Some(CodeUnit {
                    locals: locals.pop().unwrap(),
                    code: code.pop().unwrap(),
                }),
            },
            FunctionDefinition {
                function: FunctionHandleIndex(2),
                visibility: Public,
                is_entry: true,
                acquires_global_resources: acquires.pop().unwrap(),
                code: Some(CodeUnit {
                    locals: locals.pop().unwrap(),
                    code: code.pop().unwrap(),
                }),
            },
        ],
        // addresses
        address_identifiers: vec![addr],
        // identifiers
        identifiers,
        // constants
        constant_pool: vec![Constant {
            type_: Address,
            data: addr.to_vec(),
        }],
        // signatures
        signatures,
        // struct instantiations
        struct_def_instantiations,
        // function instantiations
        function_instantiations,
        // unused...
        field_handles: vec![],
        friend_decls: vec![],
        field_instantiations: vec![],
        metadata: vec![],
    };
    // uncomment to see the module generated
    // println!("Module: {:#?}", module);
    let res = verify_module(&module);
    if let Err(err) = res {
        println!("Error {:?}", err);
        println!("{:#?}", module);
        panic!("Verification Error");
    }

    let mut mod_bytes = vec![];
    module
        .serialize(&mut mod_bytes)
        .expect("Module must serialize");
    session
        .publish_module(mod_bytes, addr, &mut GasStatus::new_unmetered())
        .expect("Module must publish");
    module.self_id()
}

// Generic function to run some code. Take few arguments and a closure
// that can return an entry point to call.
// This function creates a VM, invokes the closure, and on return it builds the call
// for the entry point.
fn run_with_module(
    verifier: &VerifierConfig,
    entry_spec: fn(
        AccountAddress,
        &mut Session<InMemoryStorage>,
    ) -> (ModuleId, Identifier, Vec<TypeTag>),
) -> VMResult<SerializedReturnValues> {
    let addr = AccountAddress::from_hex_literal("0xcafe").unwrap();

    let vm = MoveVM::new_with_config(
        vec![],
        VMConfig {
            verifier: verifier.clone(),
            ..Default::default()
        },
    )
    .unwrap();
    let storage: InMemoryStorage = InMemoryStorage::new();
    let mut session = vm.new_session(&storage);

    let (module_id, entry_name, type_args) = entry_spec(addr, &mut session);

    let mut gas = GasStatus::new_unmetered();
    session.execute_entry_function(
        &module_id,
        entry_name.as_ref(),
        type_args,
        Vec::<Vec<u8>>::new(),
        &mut gas,
    )
}

//
// Vector tests
//

fn get_vector_ops(sig_idx: usize) -> Vec<Bytecode> {
    let vec = SignatureIndex(sig_idx as u16);
    let vec_of_vec = SignatureIndex((sig_idx + 1) as u16);
    let code = vec![
        VecPack(vec_of_vec, 0),
        StLoc(0),
        ImmBorrowLoc(0),
        VecLen(vec_of_vec),
        Pop,
        MutBorrowLoc(0),
        VecPack(vec, 0),
        VecPushBack(vec_of_vec),
        MutBorrowLoc(0),
        VecPack(vec, 0),
        VecPushBack(vec_of_vec),
        MutBorrowLoc(0),
        LdU64(0),
        LdU64(1),
        VecSwap(vec_of_vec),
        MutBorrowLoc(0),
        VecPopBack(vec_of_vec),
        Pop,
        MutBorrowLoc(0),
        VecPopBack(vec_of_vec),
        Pop,
        MoveLoc(0),
        VecUnpack(vec_of_vec, 0),
        Ret,
    ];
    code
}

//
// Tests with instantiated types as in Outer<Inner<u64>>, Inner<u8>, vector<bool>
//

fn vector_instantiated_2_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let sig = U128;
    let code_inst_signatures = vec![
        Signature(vec![sig.clone()]),
        Signature(vec![Vector(Box::new(sig.clone()))]),
        Signature(vec![Vector(Box::new(Vector(Box::new(sig))))]),
    ];
    vector_instantiated(addr, session, code_inst_signatures)
}

fn vector_instantiated_4_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let sig = StructInstantiation(StructHandleIndex(1), vec![U128]);
    let code_inst_signatures = vec![
        Signature(vec![sig.clone()]),
        Signature(vec![Vector(Box::new(sig.clone()))]),
        Signature(vec![Vector(Box::new(Vector(Box::new(sig))))]),
    ];
    vector_instantiated(addr, session, code_inst_signatures)
}

fn vector_instantiated_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let sig = StructInstantiation(
        StructHandleIndex(1),
        vec![Vector(Box::new(StructInstantiation(
            StructHandleIndex(1),
            vec![Bool],
        )))],
    );
    let code_inst_signatures = vec![
        Signature(vec![sig.clone()]),
        Signature(vec![Vector(Box::new(sig.clone()))]),
        Signature(vec![Vector(Box::new(Vector(Box::new(sig))))]),
    ];
    vector_instantiated(addr, session, code_inst_signatures)
}

fn vector_instantiated_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let mut sig = Bool;
    for _ in 0..48 {
        sig = StructInstantiation(StructHandleIndex(1), vec![sig]);
    }
    let code_inst_signatures = vec![
        Signature(vec![sig.clone()]),
        Signature(vec![Vector(Box::new(sig.clone()))]),
        Signature(vec![Vector(Box::new(Vector(Box::new(sig))))]),
    ];
    vector_instantiated(addr, session, code_inst_signatures)
}

fn vector_instantiated(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    mut code_inst_signatures: Vec<Signature>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    //
    // Module definition and publishing
    let struct_type_args_count = 1;
    let fun_type_args_count = 0;
    let sig_start = DEFAULT_SIGNATURES.len();
    let struct_def_instantiations = vec![];
    let function_instantiations = vec![];
    let code = get_vector_ops(sig_start);
    let acquires = vec![vec![], vec![], vec![]];
    let code = vec![vec![Ret], vec![Ret], code];
    let parameters = vec![SignatureIndex(0); 3];
    let locals = vec![
        SignatureIndex(0),
        SignatureIndex(0),
        SignatureIndex((sig_start + code_inst_signatures.len() - 1) as u16),
    ];

    let mut signatures = DEFAULT_SIGNATURES.clone();
    signatures.append(&mut code_inst_signatures);
    let self_id = make_module(
        session,
        addr,
        struct_type_args_count,
        fun_type_args_count,
        acquires,
        code,
        parameters,
        locals,
        signatures,
        struct_def_instantiations,
        function_instantiations,
    );

    // Entry specification
    (
        self_id,
        Identifier::new(ENTRY_POINT_NAME_1).unwrap(),
        vec![],
    )
}

//
// Tests with types instantiated via single function type parameter
// as in Outer<T>, Inner<T>, vector<T>
//

fn vector_single_type_arg_2_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let code_inst_signatures = vec![
        Signature(vec![Vector(Box::new(TypeParameter(0)))]),
        Signature(vec![Vector(Box::new(Vector(Box::new(TypeParameter(0)))))]),
    ];
    let (module_id, entry_fn) = vector_single_type_arg(addr, session, code_inst_signatures);
    (module_id, entry_fn, vec![TypeTag::U128])
}

fn vector_single_type_arg_4_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let code_inst_signatures = vec![
        Signature(vec![Vector(Box::new(TypeParameter(0)))]),
        Signature(vec![Vector(Box::new(Vector(Box::new(TypeParameter(0)))))]),
    ];
    let (module_id, entry_fn) = vector_single_type_arg(addr, session, code_inst_signatures);
    let ty_arg = TypeTag::Struct(Box::new(StructTag {
        address: addr,
        module: Identifier::new(MODULE_NAME).unwrap(),
        name: Identifier::new(INNER_NAME).unwrap(),
        type_params: vec![TypeTag::Address],
    }));
    (module_id, entry_fn, vec![ty_arg])
}

fn vector_single_type_arg_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let code_inst_signatures = vec![
        Signature(vec![Vector(Box::new(TypeParameter(0)))]),
        Signature(vec![Vector(Box::new(Vector(Box::new(TypeParameter(0)))))]),
    ];
    let (module_id, entry_fn) = vector_single_type_arg(addr, session, code_inst_signatures);
    let ty_arg = TypeTag::Struct(Box::new(StructTag {
        address: addr,
        module: Identifier::new(MODULE_NAME).unwrap(),
        name: Identifier::new(INNER_NAME).unwrap(),
        type_params: vec![TypeTag::Vector(Box::new(TypeTag::Struct(Box::new(
            StructTag {
                address: addr,
                module: Identifier::new(MODULE_NAME).unwrap(),
                name: Identifier::new(INNER_NAME).unwrap(),
                type_params: vec![TypeTag::U64],
            },
        ))))],
    }));
    (module_id, entry_fn, vec![ty_arg])
}

fn vector_single_type_arg_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let code_inst_signatures = vec![
        Signature(vec![Vector(Box::new(TypeParameter(0)))]),
        Signature(vec![Vector(Box::new(Vector(Box::new(TypeParameter(0)))))]),
    ];
    let (module_id, entry_fn) = vector_single_type_arg(addr, session, code_inst_signatures);
    let mut ty_arg = TypeTag::U128;
    for _ in 0..48 {
        ty_arg = TypeTag::Struct(Box::new(StructTag {
            address: addr,
            module: Identifier::new(MODULE_NAME).unwrap(),
            name: Identifier::new(INNER_NAME).unwrap(),
            type_params: vec![ty_arg],
        }));
    }
    (module_id, entry_fn, vec![ty_arg])
}

fn vector_single_type_arg(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    mut code_inst_signatures: Vec<Signature>,
) -> (ModuleId, Identifier) {
    //
    // Module definition and publishing
    let struct_type_args_count = 1;
    let fun_type_args_count = 1;
    let struct_def_instantiations = vec![];
    let function_instantiations = vec![];
    let code = get_vector_ops(1);
    let acquires = vec![vec![], vec![], vec![]];
    let code = vec![vec![Ret], code, vec![Ret]];
    let parameters = vec![SignatureIndex(0); 3];
    let locals = vec![
        SignatureIndex(0),
        SignatureIndex((DEFAULT_SIGNATURES.len() + code_inst_signatures.len() - 1) as u16),
        SignatureIndex(0),
    ];

    let mut signatures = DEFAULT_SIGNATURES.clone();
    signatures.append(&mut code_inst_signatures);
    let self_id = make_module(
        session,
        addr,
        struct_type_args_count,
        fun_type_args_count,
        acquires,
        code,
        parameters,
        locals,
        signatures,
        struct_def_instantiations,
        function_instantiations,
    );

    // Entry specification
    (self_id, Identifier::new(ENTRY_POINT_NAME_2).unwrap())
}

//
// Tests with types instantiated via multiple function type parameters
// as in Outer<Inner<X, Z, W>>
//

fn vector_mul_type_args_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let sig = StructInstantiation(
        StructHandleIndex(1),
        vec![TypeParameter(0), TypeParameter(1), TypeParameter(2)],
    );
    let code_inst_signatures = vec![
        Signature(vec![sig.clone()]),
        Signature(vec![Vector(Box::new(sig.clone()))]),
        Signature(vec![Vector(Box::new(Vector(Box::new(sig))))]),
    ];
    let (module_id, entry_fn) = vector_mul_type_args(addr, session, code_inst_signatures);
    (
        module_id,
        entry_fn,
        vec![TypeTag::U64, TypeTag::Bool, TypeTag::Address],
    )
}

fn vector_mul_type_args_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let sig = StructInstantiation(
        StructHandleIndex(1),
        vec![TypeParameter(0), TypeParameter(1), TypeParameter(2)],
    );
    let code_inst_signatures = vec![
        Signature(vec![sig.clone()]),
        Signature(vec![Vector(Box::new(sig.clone()))]),
        Signature(vec![Vector(Box::new(Vector(Box::new(sig))))]),
    ];
    let (module_id, entry_fn) = vector_mul_type_args(addr, session, code_inst_signatures);
    let mut ty_arg_long = TypeTag::U128;
    for _ in 0..2 {
        ty_arg_long = TypeTag::Struct(Box::new(StructTag {
            address: addr,
            module: Identifier::new(MODULE_NAME).unwrap(),
            name: Identifier::new(INNER_NAME).unwrap(),
            type_params: vec![ty_arg_long.clone(), ty_arg_long.clone(), ty_arg_long],
        }));
    }
    let mut ty_arg_short = TypeTag::Address;
    for _ in 0..21 {
        ty_arg_short = TypeTag::Vector(Box::new(ty_arg_short));
    }
    (
        module_id,
        entry_fn,
        vec![ty_arg_long.clone(), ty_arg_long, ty_arg_short],
    )
}

fn vector_mul_type_args_51_nodes_mix(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let struct_sig = StructInstantiation(
        StructHandleIndex(1),
        vec![TypeParameter(0), TypeParameter(0), TypeParameter(0)],
    );
    let sig = StructInstantiation(
        StructHandleIndex(1),
        vec![struct_sig.clone(), struct_sig, TypeParameter(2)],
    );
    let code_inst_signatures = vec![
        Signature(vec![sig.clone()]),
        Signature(vec![Vector(Box::new(sig.clone()))]),
        Signature(vec![Vector(Box::new(Vector(Box::new(sig))))]),
    ];
    let (module_id, entry_fn) = vector_mul_type_args(addr, session, code_inst_signatures);
    let ty_arg_long = TypeTag::Struct(Box::new(StructTag {
        address: addr,
        module: Identifier::new(MODULE_NAME).unwrap(),
        name: Identifier::new(INNER_NAME).unwrap(),
        type_params: vec![TypeTag::Bool, TypeTag::U8, TypeTag::Address],
    }));
    let mut ty_arg_vec = TypeTag::Address;
    for _ in 0..21 {
        ty_arg_vec = TypeTag::Vector(Box::new(ty_arg_vec));
    }
    (
        module_id,
        entry_fn,
        vec![ty_arg_long.clone(), ty_arg_long, ty_arg_vec],
    )
}

fn vector_mul_type_args(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    mut code_inst_signatures: Vec<Signature>,
) -> (ModuleId, Identifier) {
    //
    // Module definition and publishing
    let struct_type_args_count = 3;
    let fun_type_args_count = 3;
    let sig_start = DEFAULT_SIGNATURES.len();
    let struct_def_instantiations = vec![];
    let function_instantiations = vec![];

    let code = get_vector_ops(sig_start);
    let acquires = vec![vec![], vec![], vec![]];
    let code = vec![code, vec![Ret], vec![Ret]];
    let parameters = vec![SignatureIndex(0); 3];
    let locals = vec![
        SignatureIndex((DEFAULT_SIGNATURES.len() + code_inst_signatures.len() - 1) as u16),
        SignatureIndex(0),
        SignatureIndex(0),
    ];

    let mut signatures = DEFAULT_SIGNATURES.clone();
    signatures.append(&mut code_inst_signatures);
    let self_id = make_module(
        session,
        addr,
        struct_type_args_count,
        fun_type_args_count,
        acquires,
        code,
        parameters,
        locals,
        signatures,
        struct_def_instantiations,
        function_instantiations,
    );

    // Entry specification
    (self_id, Identifier::new(ENTRY_POINT_NAME_3).unwrap())
}

//
// Global data tests
//

fn get_mut_borrow_global_ops() -> Vec<Bytecode> {
    let code = vec![
        LdConst(ConstantPoolIndex(0)),
        MutBorrowGlobalGeneric(StructDefInstantiationIndex(0)),
        Pop,
        Ret,
    ];
    code
}

fn get_imm_borrow_global_ops() -> Vec<Bytecode> {
    let code = vec![
        LdConst(ConstantPoolIndex(0)),
        ImmBorrowGlobalGeneric(StructDefInstantiationIndex(0)),
        Pop,
        Ret,
    ];
    code
}

fn get_exist_global_ops() -> Vec<Bytecode> {
    let code = vec![
        LdConst(ConstantPoolIndex(0)),
        ExistsGeneric(StructDefInstantiationIndex(0)),
        Pop,
        Ret,
    ];
    code
}

fn get_move_from_global_ops() -> Vec<Bytecode> {
    let code = vec![
        LdConst(ConstantPoolIndex(0)),
        MoveFromGeneric(StructDefInstantiationIndex(0)),
        Pop,
        Ret,
    ];
    code
}

// TODO: setting up the args to call MoveToGeneric in a "customizable fashion"
// is not done yet
fn get_move_to_global_ops() -> Vec<Bytecode> {
    let code = vec![
        CopyLoc(0),
        CopyLoc(1),
        MoveToGeneric(StructDefInstantiationIndex(0)),
        Ret,
    ];
    code
}

enum LoadCodeForGlobal {
    MutBorrow,
    ImmBorrow,
    Exists,
    MoveFrom,
    MoveTo,
}

impl LoadCodeForGlobal {
    fn get_code(&self) -> Vec<Bytecode> {
        use LoadCodeForGlobal::*;

        match self {
            MutBorrow => get_mut_borrow_global_ops(),
            ImmBorrow => get_imm_borrow_global_ops(),
            Exists => get_exist_global_ops(),
            MoveFrom => get_move_from_global_ops(),
            MoveTo => get_move_to_global_ops(),
        }
    }

    fn get_acquire(&self, idx: u16) -> Vec<StructDefinitionIndex> {
        use LoadCodeForGlobal::*;

        match self {
            MutBorrow => vec![StructDefinitionIndex(idx)],
            ImmBorrow => vec![StructDefinitionIndex(idx)],
            Exists => vec![],
            MoveFrom => vec![StructDefinitionIndex(idx)],
            MoveTo => vec![],
        }
    }
}

//
// Tests with instantiated types as in Outer<Inner<u64>>, Inner<u8>, vector<bool>
//

fn mut_borrow_instantiated_2_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_instantiated_2_nodes(addr, session, LoadCodeForGlobal::MutBorrow)
}

fn imm_borrow_instantiated_2_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_instantiated_2_nodes(addr, session, LoadCodeForGlobal::ImmBorrow)
}

fn exists_instantiated_2_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_instantiated_2_nodes(addr, session, LoadCodeForGlobal::Exists)
}

fn move_from_instantiated_2_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_instantiated_2_nodes(addr, session, LoadCodeForGlobal::MoveFrom)
}

fn global_instantiated_2_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    code: LoadCodeForGlobal,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let code_inst_signatures = vec![Signature(vec![U128])];
    global_instantiated(addr, session, code_inst_signatures, code)
}

fn mut_borrow_instantiated_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_instantiated_6_nodes(addr, session, LoadCodeForGlobal::MutBorrow)
}

fn imm_borrow_instantiated_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_instantiated_6_nodes(addr, session, LoadCodeForGlobal::ImmBorrow)
}

fn exists_instantiated_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_instantiated_6_nodes(addr, session, LoadCodeForGlobal::Exists)
}

fn move_from_instantiated_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_instantiated_6_nodes(addr, session, LoadCodeForGlobal::MoveFrom)
}

fn global_instantiated_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    code: LoadCodeForGlobal,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let sig = StructInstantiation(
        StructHandleIndex(0),
        vec![Vector(Box::new(StructInstantiation(
            StructHandleIndex(1),
            vec![Vector(Box::new(U16))],
        )))],
    );
    let code_inst_signatures = vec![Signature(vec![sig])];
    global_instantiated(addr, session, code_inst_signatures, code)
}

fn mut_borrow_instantiated_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_instantiated_51_nodes(addr, session, LoadCodeForGlobal::MutBorrow)
}

fn imm_borrow_instantiated_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_instantiated_51_nodes(addr, session, LoadCodeForGlobal::ImmBorrow)
}

fn exists_instantiated_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_instantiated_51_nodes(addr, session, LoadCodeForGlobal::Exists)
}

fn move_from_instantiated_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_instantiated_51_nodes(addr, session, LoadCodeForGlobal::MoveFrom)
}

fn global_instantiated_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    code: LoadCodeForGlobal,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let mut sig = Vector(Box::new(U32));
    for i in 0u16..48 {
        sig = StructInstantiation(StructHandleIndex(i % 2), vec![sig]);
    }
    let code_inst_signatures = vec![Signature(vec![sig])];
    global_instantiated(addr, session, code_inst_signatures, code)
}

fn global_instantiated(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    mut code_inst_signatures: Vec<Signature>,
    op: LoadCodeForGlobal,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    //
    // Module definition and publishing
    let struct_type_args_count = 1;
    let fun_type_args_count = 0;
    let sig_start = DEFAULT_SIGNATURES.len();
    let struct_def_instantiations = vec![StructDefInstantiation {
        def: StructDefinitionIndex(1),
        type_parameters: SignatureIndex(sig_start as u16),
    }];
    let function_instantiations = vec![];
    let code = op.get_code();
    let acquire = op.get_acquire(1);
    let acquires = vec![vec![], vec![], acquire];
    let code = vec![vec![Ret], vec![Ret], code];
    let parameters = vec![SignatureIndex(0); 3];
    let locals = vec![SignatureIndex(0), SignatureIndex(0), SignatureIndex(0)];

    let mut signatures = DEFAULT_SIGNATURES.clone();
    signatures.append(&mut code_inst_signatures);
    let self_id = make_module(
        session,
        addr,
        struct_type_args_count,
        fun_type_args_count,
        acquires,
        code,
        parameters,
        locals,
        signatures,
        struct_def_instantiations,
        function_instantiations,
    );

    // Entry specification
    (
        self_id,
        Identifier::new(ENTRY_POINT_NAME_1).unwrap(),
        vec![],
    )
}

//
// Tests with types instantiated via single function type parameter
// as in Outer<T>, Inner<T>, vector<T>
//

fn mut_borrow_single_type_arg_2_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_single_type_arg_2_nodes(addr, session, LoadCodeForGlobal::MutBorrow)
}

fn imm_borrow_single_type_arg_2_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_single_type_arg_2_nodes(addr, session, LoadCodeForGlobal::ImmBorrow)
}

fn exists_single_type_arg_2_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_single_type_arg_2_nodes(addr, session, LoadCodeForGlobal::Exists)
}

fn move_from_single_type_arg_2_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_single_type_arg_2_nodes(addr, session, LoadCodeForGlobal::MoveFrom)
}

fn global_single_type_arg_2_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    code: LoadCodeForGlobal,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let code_inst_signatures = vec![];
    let (module_id, entry_fn) = global_single_type_arg(addr, session, code_inst_signatures, code);
    (module_id, entry_fn, vec![TypeTag::U128])
}

fn mut_borrow_single_type_arg_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_single_type_arg_6_nodes(addr, session, LoadCodeForGlobal::MutBorrow)
}

fn imm_borrow_single_type_arg_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_single_type_arg_6_nodes(addr, session, LoadCodeForGlobal::ImmBorrow)
}

fn exists_single_type_arg_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_single_type_arg_6_nodes(addr, session, LoadCodeForGlobal::Exists)
}

fn move_from_single_type_arg_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_single_type_arg_6_nodes(addr, session, LoadCodeForGlobal::MoveFrom)
}

fn global_single_type_arg_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    code: LoadCodeForGlobal,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let sig = StructInstantiation(
        StructHandleIndex(1),
        vec![StructInstantiation(
            StructHandleIndex(1),
            vec![TypeParameter(0)],
        )],
    );
    let code_inst_signatures = vec![Signature(vec![sig])];
    let (module_id, entry_fn) = global_single_type_arg(addr, session, code_inst_signatures, code);
    let ty_arg = TypeTag::Struct(Box::new(StructTag {
        address: addr,
        module: Identifier::new(MODULE_NAME).unwrap(),
        name: Identifier::new(INNER_NAME).unwrap(),
        type_params: vec![TypeTag::Struct(Box::new(StructTag {
            address: addr,
            module: Identifier::new(MODULE_NAME).unwrap(),
            name: Identifier::new(INNER_NAME).unwrap(),
            type_params: vec![TypeTag::U64],
        }))],
    }));
    (module_id, entry_fn, vec![ty_arg])
}

fn mut_borrow_single_type_arg_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_single_type_arg_51_nodes(addr, session, LoadCodeForGlobal::MutBorrow)
}

fn imm_borrow_single_type_arg_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_single_type_arg_51_nodes(addr, session, LoadCodeForGlobal::ImmBorrow)
}

fn exists_single_type_arg_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_single_type_arg_51_nodes(addr, session, LoadCodeForGlobal::Exists)
}

fn move_from_single_type_arg_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_single_type_arg_51_nodes(addr, session, LoadCodeForGlobal::MoveFrom)
}

fn global_single_type_arg_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    code: LoadCodeForGlobal,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let sig = StructInstantiation(
        StructHandleIndex(0),
        vec![Vector(Box::new(TypeParameter(0)))],
    );
    let code_inst_signatures = vec![Signature(vec![sig])];
    let (module_id, entry_fn) = global_single_type_arg(addr, session, code_inst_signatures, code);
    let mut ty_arg = TypeTag::U128;
    for _ in 0..47 {
        ty_arg = TypeTag::Struct(Box::new(StructTag {
            address: addr,
            module: Identifier::new(MODULE_NAME).unwrap(),
            name: Identifier::new(INNER_NAME).unwrap(),
            type_params: vec![ty_arg],
        }));
    }
    (module_id, entry_fn, vec![ty_arg])
}

fn global_single_type_arg(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    mut code_inst_signatures: Vec<Signature>,
    op: LoadCodeForGlobal,
) -> (ModuleId, Identifier) {
    //
    // Module definition and publishing
    let struct_type_args_count = 1;
    let fun_type_args_count = 1;
    let sig_start = if code_inst_signatures.is_empty() {
        DEFAULT_SIGNATURES.len() - 1
    } else {
        DEFAULT_SIGNATURES.len()
    };
    let struct_def_instantiations = vec![StructDefInstantiation {
        def: StructDefinitionIndex(1),
        type_parameters: SignatureIndex(sig_start as u16),
    }];
    let function_instantiations = vec![];
    let code = op.get_code();
    let acquire = op.get_acquire(1);
    let acquires = vec![vec![], acquire, vec![]];
    let code = vec![vec![Ret], code, vec![Ret]];
    let parameters = vec![SignatureIndex(0); 3];
    let locals = vec![SignatureIndex(0), SignatureIndex(0), SignatureIndex(0)];

    let mut signatures = DEFAULT_SIGNATURES.clone();
    signatures.append(&mut code_inst_signatures);
    let self_id = make_module(
        session,
        addr,
        struct_type_args_count,
        fun_type_args_count,
        acquires,
        code,
        locals,
        parameters,
        signatures,
        struct_def_instantiations,
        function_instantiations,
    );

    // Entry specification
    (self_id, Identifier::new(ENTRY_POINT_NAME_2).unwrap())
}

//
// Tests with types instantiated via multiple function type parameters
// as in Outer<Inner<X, Z, W>>
//

fn mut_borrow_mul_type_args_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_mul_type_args_6_nodes(addr, session, LoadCodeForGlobal::MutBorrow)
}

fn imm_borrow_mul_type_args_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_mul_type_args_6_nodes(addr, session, LoadCodeForGlobal::ImmBorrow)
}

fn exists_mul_type_args_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_mul_type_args_6_nodes(addr, session, LoadCodeForGlobal::Exists)
}

fn move_from_mul_type_args_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_mul_type_args_6_nodes(addr, session, LoadCodeForGlobal::MoveFrom)
}

fn global_mul_type_args_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    code: LoadCodeForGlobal,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let sig = Signature(vec![
        TypeParameter(0),
        Vector(Box::new(TypeParameter(1))),
        StructInstantiation(StructHandleIndex(0), vec![TypeParameter(2)]),
    ]);
    let code_inst_signatures = vec![sig];
    let (module_id, entry_fn) = global_mul_type_args(addr, session, code_inst_signatures, code);
    (
        module_id,
        entry_fn,
        vec![TypeTag::U64, TypeTag::Bool, TypeTag::Address],
    )
}

fn mut_borrow_mul_type_args_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_mul_type_args_51_nodes(addr, session, LoadCodeForGlobal::MutBorrow)
}

fn imm_borrow_mul_type_args_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_mul_type_args_51_nodes(addr, session, LoadCodeForGlobal::ImmBorrow)
}

fn exists_mul_type_args_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_mul_type_args_51_nodes(addr, session, LoadCodeForGlobal::Exists)
}

fn move_from_mul_type_args_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_mul_type_args_51_nodes(addr, session, LoadCodeForGlobal::MoveFrom)
}

fn global_mul_type_args_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    code: LoadCodeForGlobal,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let sig = Signature(vec![
        Vector(Box::new(TypeParameter(1))),
        TypeParameter(0),
        StructInstantiation(StructHandleIndex(0), vec![TypeParameter(2)]),
    ]);
    let code_inst_signatures = vec![sig];
    let (module_id, entry_fn) = global_mul_type_args(addr, session, code_inst_signatures, code);
    let mut ty_arg_long = TypeTag::U128;
    for _ in 0..2 {
        ty_arg_long = TypeTag::Struct(Box::new(StructTag {
            address: addr,
            module: Identifier::new(MODULE_NAME).unwrap(),
            name: Identifier::new(INNER_NAME).unwrap(),
            type_params: vec![ty_arg_long.clone(), ty_arg_long.clone(), ty_arg_long],
        }));
    }
    let mut ty_arg_short = TypeTag::Address;
    for _ in 0..21 {
        ty_arg_short = TypeTag::Vector(Box::new(ty_arg_short));
    }
    (
        module_id,
        entry_fn,
        vec![ty_arg_long.clone(), ty_arg_long, ty_arg_short],
    )
}

fn mut_borrow_mul_type_args_51_nodes_mix(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_mul_type_args_51_nodes_mix(addr, session, LoadCodeForGlobal::MutBorrow)
}

fn imm_borrow_mul_type_args_51_nodes_mix(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_mul_type_args_51_nodes_mix(addr, session, LoadCodeForGlobal::ImmBorrow)
}

fn exists_mul_type_args_51_nodes_mix(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_mul_type_args_51_nodes_mix(addr, session, LoadCodeForGlobal::Exists)
}

fn move_from_mul_type_args_51_nodes_mix(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    global_mul_type_args_51_nodes_mix(addr, session, LoadCodeForGlobal::MoveFrom)
}

fn global_mul_type_args_51_nodes_mix(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    code: LoadCodeForGlobal,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let sig = Signature(vec![
        Vector(Box::new(TypeParameter(1))),
        StructInstantiation(
            StructHandleIndex(1),
            vec![
                TypeParameter(0),
                Vector(Box::new(TypeParameter(2))),
                TypeParameter(2),
            ],
        ),
        Vector(Box::new(TypeParameter(0))),
    ]);
    let code_inst_signatures = vec![sig];
    let (module_id, entry_fn) = global_mul_type_args(addr, session, code_inst_signatures, code);
    let ty_arg_long = TypeTag::Struct(Box::new(StructTag {
        address: addr,
        module: Identifier::new(MODULE_NAME).unwrap(),
        name: Identifier::new(INNER_NAME).unwrap(),
        type_params: vec![TypeTag::Bool, TypeTag::U8, TypeTag::Address],
    }));
    let mut ty_arg_vec = TypeTag::Address;
    for _ in 0..16 {
        ty_arg_vec = TypeTag::Vector(Box::new(ty_arg_vec));
    }
    (
        module_id,
        entry_fn,
        vec![ty_arg_long.clone(), ty_arg_long, ty_arg_vec],
    )
}

fn global_mul_type_args(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    mut code_inst_signatures: Vec<Signature>,
    op: LoadCodeForGlobal,
) -> (ModuleId, Identifier) {
    //
    // Module definition and publishing
    let struct_type_args_count = 3;
    let fun_type_args_count = 3;
    let sig_start = DEFAULT_SIGNATURES.len();
    let struct_def_instantiations = vec![StructDefInstantiation {
        def: StructDefinitionIndex(1),
        type_parameters: SignatureIndex(sig_start as u16),
    }];
    let function_instantiations = vec![];
    let code = op.get_code();
    let acquire = op.get_acquire(1);
    let acquires = vec![acquire, vec![], vec![]];
    let code = vec![code, vec![Ret], vec![Ret]];
    let parameters = vec![SignatureIndex(0); 3];
    let locals = vec![SignatureIndex(0), SignatureIndex(0), SignatureIndex(0)];

    let mut signatures = DEFAULT_SIGNATURES.clone();
    signatures.append(&mut code_inst_signatures);
    let self_id = make_module(
        session,
        addr,
        struct_type_args_count,
        fun_type_args_count,
        acquires,
        code,
        parameters,
        locals,
        signatures,
        struct_def_instantiations,
        function_instantiations,
    );

    // Entry specification
    (self_id, Identifier::new(ENTRY_POINT_NAME_3).unwrap())
}

//
// Pack generic tests
//

fn get_pack_generic(idx: u16, vector_count: u16) -> Vec<Bytecode> {
    let mut code = vec![];
    for idx in idx..idx + vector_count {
        code.push(VecPack(SignatureIndex(idx), 0));
    }
    code.push(PackGeneric(StructDefInstantiationIndex(0)));
    code.push(UnpackGeneric(StructDefInstantiationIndex(0)));
    for _ in 0..vector_count {
        code.push(Pop);
    }
    code.push(Ret);
    code
}

//
// Tests with instantiated types as in Outer<Inner<u64>>, Inner<u8>, vector<bool>
//

fn pack_generic_instantiated_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let sig1 = Bool;
    let sig2 = StructInstantiation(StructHandleIndex(0), vec![Signer]);
    let sig0 = vec![
        Vector(Box::new(sig1.clone())),
        Vector(Box::new(sig2.clone())),
    ];
    let code_inst_signatures = vec![
        Signature(sig0),
        Signature(vec![sig1]),
        Signature(vec![sig2]),
    ];
    pack_generic_instantiated(addr, session, code_inst_signatures)
}

fn pack_generic_instantiated_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let mut sig1 = Bool;
    for _ in 0..23 {
        sig1 = StructInstantiation(StructHandleIndex(0), vec![sig1]);
    }
    let mut sig2 = Address;
    for _ in 0..23 {
        sig2 = Vector(Box::new(sig2));
    }
    let sig0 = vec![
        Vector(Box::new(sig1.clone())),
        Vector(Box::new(sig2.clone())),
    ];
    let code_inst_signatures = vec![
        Signature(sig0),
        Signature(vec![sig1]),
        Signature(vec![sig2]),
    ];
    pack_generic_instantiated(addr, session, code_inst_signatures)
}

fn pack_generic_instantiated(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    mut code_inst_signatures: Vec<Signature>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    //
    // Module definition and publishing
    let struct_type_args_count = 2usize;
    let fun_type_args_count = 0;
    let sig_start = DEFAULT_SIGNATURES.len();
    let struct_def_instantiations = vec![StructDefInstantiation {
        def: StructDefinitionIndex(1),
        type_parameters: SignatureIndex(sig_start as u16),
    }];
    let function_instantiations = vec![];
    let code = get_pack_generic(sig_start as u16 + 1, struct_type_args_count as u16);
    let acquires = vec![vec![], vec![], vec![]];
    let code = vec![vec![Ret], vec![Ret], code];
    let parameters = vec![SignatureIndex(0); 3];
    let locals = vec![SignatureIndex(0), SignatureIndex(0), SignatureIndex(0)];

    let mut signatures = DEFAULT_SIGNATURES.clone();
    signatures.append(&mut code_inst_signatures);
    let self_id = make_module(
        session,
        addr,
        struct_type_args_count,
        fun_type_args_count,
        acquires,
        code,
        parameters,
        locals,
        signatures,
        struct_def_instantiations,
        function_instantiations,
    );

    // Entry specification
    (
        self_id,
        Identifier::new(ENTRY_POINT_NAME_1).unwrap(),
        vec![],
    )
}

//
// Tests with types instantiated via single function type parameter
// as in Outer<T>, Inner<T>, vector<T>
//

fn pack_generic_single_type_arg_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let sig1 = Vector(Box::new(TypeParameter(0)));
    let sig2 = Address;
    let sig0 = vec![
        Vector(Box::new(sig1.clone())),
        Vector(Box::new(sig2.clone())),
    ];
    let code_inst_signatures = vec![
        Signature(sig0),
        Signature(vec![sig1]),
        Signature(vec![sig2]),
    ];
    let (module_id, entry_point) =
        pack_generic_single_type_arg(addr, session, code_inst_signatures);
    (module_id, entry_point, vec![TypeTag::U32])
}

fn pack_generic_single_type_arg_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let mut sig1 = TypeParameter(0);
    for _ in 0..22 {
        sig1 = StructInstantiation(StructHandleIndex(0), vec![sig1]);
    }
    let mut sig2 = Address;
    for _ in 0..23 {
        sig2 = Vector(Box::new(sig2));
    }
    let sig0 = vec![
        Vector(Box::new(sig1.clone())),
        Vector(Box::new(sig2.clone())),
    ];
    let code_inst_signatures = vec![
        Signature(sig0),
        Signature(vec![sig1]),
        Signature(vec![sig2]),
    ];
    let (module_id, entry_point) =
        pack_generic_single_type_arg(addr, session, code_inst_signatures);
    (
        module_id,
        entry_point,
        vec![TypeTag::Vector(Box::new(TypeTag::U32))],
    )
}

fn pack_generic_single_type_arg(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    mut code_inst_signatures: Vec<Signature>,
) -> (ModuleId, Identifier) {
    //
    // Module definition and publishing
    let struct_type_args_count = 2usize;
    let fun_type_args_count = 0;
    let sig_start = DEFAULT_SIGNATURES.len();
    let struct_def_instantiations = vec![StructDefInstantiation {
        def: StructDefinitionIndex(1),
        type_parameters: SignatureIndex(sig_start as u16),
    }];
    let function_instantiations = vec![];
    let code = get_pack_generic(sig_start as u16 + 1, struct_type_args_count as u16);
    let acquires = vec![vec![], vec![], vec![]];
    let code = vec![vec![Ret], code, vec![Ret]];
    let parameters = vec![SignatureIndex(0); 3];
    let locals = vec![SignatureIndex(0), SignatureIndex(0), SignatureIndex(0)];

    let mut signatures = DEFAULT_SIGNATURES.clone();
    signatures.append(&mut code_inst_signatures);
    let self_id = make_module(
        session,
        addr,
        struct_type_args_count,
        fun_type_args_count,
        acquires,
        code,
        parameters,
        locals,
        signatures,
        struct_def_instantiations,
        function_instantiations,
    );

    // Entry specification
    (self_id, Identifier::new(ENTRY_POINT_NAME_2).unwrap())
}

//
// Tests with types instantiated via multiple function type parameters
// as in Outer<Inner<X, Z, W>>
//

fn pack_generic_multi_type_args_6_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let sig1 = TypeParameter(1);
    let sig2 = TypeParameter(2);
    let sig0 = vec![
        Vector(Box::new(sig1.clone())),
        Vector(Box::new(sig2.clone())),
    ];
    let code_inst_signatures = vec![
        Signature(sig0),
        Signature(vec![sig1]),
        Signature(vec![sig2]),
    ];
    let (module_id, entry_point) =
        pack_generic_multi_type_args(addr, session, code_inst_signatures);
    (
        module_id,
        entry_point,
        vec![
            TypeTag::U32,
            TypeTag::Vector(Box::new(TypeTag::Bool)),
            TypeTag::Address,
        ],
    )
}

fn pack_generic_multi_type_args_51_nodes(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let mut sig1 = TypeParameter(0);
    for _ in 0..20 {
        sig1 = StructInstantiation(StructHandleIndex(0), vec![sig1]);
    }
    let mut sig2 = TypeParameter(1);
    for _ in 0..20 {
        sig2 = Vector(Box::new(sig2));
    }
    let sig0 = vec![
        Vector(Box::new(sig1.clone())),
        Vector(Box::new(sig2.clone())),
    ];
    let code_inst_signatures = vec![
        Signature(sig0),
        Signature(vec![sig1]),
        Signature(vec![sig2]),
    ];
    let (module_id, entry_point) =
        pack_generic_multi_type_args(addr, session, code_inst_signatures);
    let mut ty_arg_long = TypeTag::U128;
    for _ in 0..2 {
        ty_arg_long = TypeTag::Struct(Box::new(StructTag {
            address: addr,
            module: Identifier::new(MODULE_NAME).unwrap(),
            name: Identifier::new(INNER_NAME).unwrap(),
            type_params: vec![ty_arg_long.clone(), ty_arg_long],
        }));
    }
    let mut ty_arg_short = TypeTag::Address;
    for _ in 0..21 {
        ty_arg_short = TypeTag::Vector(Box::new(ty_arg_short));
    }
    (
        module_id,
        entry_point,
        vec![ty_arg_long, TypeTag::U8, ty_arg_short],
    )
}

fn pack_generic_multi_type_args(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    mut code_inst_signatures: Vec<Signature>,
) -> (ModuleId, Identifier) {
    //
    // Module definition and publishing
    let struct_type_args_count = 2usize;
    let fun_type_args_count = 3;
    let sig_start = DEFAULT_SIGNATURES.len();
    let struct_def_instantiations = vec![StructDefInstantiation {
        def: StructDefinitionIndex(1),
        type_parameters: SignatureIndex(sig_start as u16),
    }];
    let function_instantiations = vec![];
    let code = get_pack_generic(sig_start as u16 + 1, struct_type_args_count as u16);
    let acquires = vec![vec![], vec![], vec![]];
    let code = vec![code, vec![Ret], vec![Ret]];
    let parameters = vec![SignatureIndex(0); 3];
    let locals = vec![SignatureIndex(0), SignatureIndex(0), SignatureIndex(0)];

    let mut signatures = DEFAULT_SIGNATURES.clone();
    signatures.append(&mut code_inst_signatures);
    let self_id = make_module(
        session,
        addr,
        struct_type_args_count,
        fun_type_args_count,
        acquires,
        code,
        parameters,
        locals,
        signatures,
        struct_def_instantiations,
        function_instantiations,
    );

    // Entry specification
    (self_id, Identifier::new(ENTRY_POINT_NAME_3).unwrap())
}

//
// Pack generic tests
//

fn get_call_generic() -> Vec<Vec<Bytecode>> {
    let code1 = vec![CallGeneric(FunctionInstantiationIndex(0)), Ret];
    let code2 = vec![CallGeneric(FunctionInstantiationIndex(1)), Ret];
    let code3 = vec![Ret];
    vec![code3, code2, code1]
}

fn call_instantiated_1_2(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let code_inst_signatures = vec![Signature(vec![U128]), Signature(vec![Address, Bool])];
    let entry_point = ENTRY_POINT_NAME_1;
    let ty_args = vec![];
    call_generic(addr, session, code_inst_signatures, entry_point, ty_args)
}

fn call_instantiated_6_6(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let mut sig = Vector(Box::new(Vector(Box::new(U128))));
    for _ in 0..3 {
        sig = StructInstantiation(StructHandleIndex(1), vec![sig]);
    }
    let mut sig1 = Address;
    for _ in 0..3 {
        sig1 = StructInstantiation(StructHandleIndex(0), vec![sig1]);
    }
    for _ in 0..2 {
        sig1 = Vector(Box::new(sig1));
    }
    let code_inst_signatures = vec![Signature(vec![sig.clone()]), Signature(vec![sig1, sig])];
    let entry_point = ENTRY_POINT_NAME_1;
    let ty_args = vec![];
    call_generic(addr, session, code_inst_signatures, entry_point, ty_args)
}

fn call_instantiated_11_31(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let mut sig = Bool;
    for _ in 0..10 {
        sig = StructInstantiation(StructHandleIndex(1), vec![sig]);
    }
    let mut sig1 = Address;
    for _ in 0..30 {
        sig1 = StructInstantiation(StructHandleIndex(0), vec![sig1]);
    }
    let code_inst_signatures = vec![Signature(vec![sig.clone()]), Signature(vec![sig1, sig])];
    let ty_args = vec![];
    call_generic(
        addr,
        session,
        code_inst_signatures,
        ENTRY_POINT_NAME_1,
        ty_args,
    )
}

fn call_single_type_arg_11_31(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    let sig = StructInstantiation(StructHandleIndex(1), vec![TypeParameter(0)]);
    let mut sig1 = Address;
    for _ in 0..29 {
        sig1 = StructInstantiation(StructHandleIndex(0), vec![sig1]);
    }
    let code_inst_signatures = vec![Signature(vec![U128]), Signature(vec![sig1, sig])];
    let mut ty_arg = TypeTag::Bool;
    for _ in 0..10 {
        ty_arg = TypeTag::Struct(Box::new(StructTag {
            address: addr,
            module: Identifier::new(MODULE_NAME).unwrap(),
            name: Identifier::new(INNER_NAME).unwrap(),
            type_params: vec![ty_arg],
        }))
    }
    let ty_args = vec![ty_arg];
    call_generic(
        addr,
        session,
        code_inst_signatures,
        ENTRY_POINT_NAME_2,
        ty_args,
    )
}

fn call_multi_type_args_11_31(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    // those are only just so the code compiles
    let code_inst_signatures = vec![Signature(vec![U128]), Signature(vec![U128, U128])];
    let mut ty_arg1 = TypeTag::Bool;
    for _ in 0..10 {
        ty_arg1 = TypeTag::Struct(Box::new(StructTag {
            address: addr,
            module: Identifier::new(MODULE_NAME).unwrap(),
            name: Identifier::new(INNER_NAME).unwrap(),
            type_params: vec![ty_arg1],
        }));
    }
    let mut ty_arg2 = TypeTag::Address;
    for _ in 0..30 {
        ty_arg2 = TypeTag::Struct(Box::new(StructTag {
            address: addr,
            module: Identifier::new(MODULE_NAME).unwrap(),
            name: Identifier::new(INNER_NAME).unwrap(),
            type_params: vec![ty_arg2],
        }));
    }

    let ty_args = vec![ty_arg1, ty_arg2];
    call_generic(
        addr,
        session,
        code_inst_signatures,
        ENTRY_POINT_NAME_3,
        ty_args,
    )
}

fn call_generic(
    addr: AccountAddress,
    session: &mut Session<InMemoryStorage>,
    mut code_inst_signatures: Vec<Signature>,
    entry_point: &str,
    ty_args: Vec<TypeTag>,
) -> (ModuleId, Identifier, Vec<TypeTag>) {
    //
    // Module definition and publishing
    let struct_type_args_count = 1;
    let fun_type_args_count = 2;
    let struct_def_instantiations = vec![];
    let sig_start = DEFAULT_SIGNATURES.len();
    let function_instantiations = vec![
        FunctionInstantiation {
            handle: FunctionHandleIndex(1),
            type_parameters: SignatureIndex(sig_start as u16),
        },
        FunctionInstantiation {
            handle: FunctionHandleIndex(2),
            type_parameters: SignatureIndex(sig_start as u16 + 1),
        },
    ];
    let acquires = vec![vec![], vec![], vec![]];
    let code = get_call_generic();
    let parameters = vec![SignatureIndex(0); 3];
    let locals = vec![SignatureIndex(0), SignatureIndex(0), SignatureIndex(0)];

    let mut signatures = DEFAULT_SIGNATURES.clone();
    signatures.append(&mut code_inst_signatures);
    let self_id = make_module(
        session,
        addr,
        struct_type_args_count,
        fun_type_args_count,
        acquires,
        code,
        parameters,
        locals,
        signatures,
        struct_def_instantiations,
        function_instantiations,
    );

    // Entry specification
    (self_id, Identifier::new(entry_point).unwrap(), ty_args)
}

//
// Test call to a function that instantiate a type
//

fn call_vector_arg_9(verifier: &VerifierConfig) -> VMResult<SerializedReturnValues> {
    let addr = AccountAddress::from_hex_literal("0xcafe").unwrap();
    let mut vector_tag = TypeTag::U8;
    for _ in 0..8 {
        vector_tag = TypeTag::Vector(Box::new(vector_tag));
    }
    let type_args = vec![vector_tag];
    let arg = vec![vec![vec![vec![vec![0u8]]]]];
    call_type_arg(
        addr,
        verifier,
        type_args,
        vec![bcs::to_bytes(&arg).unwrap()],
    )
}

fn call_vector_arg_10(verifier: &VerifierConfig) -> VMResult<SerializedReturnValues> {
    let addr = AccountAddress::from_hex_literal("0xcafe").unwrap();
    let mut vector_tag = TypeTag::U8;
    for _ in 0..9 {
        vector_tag = TypeTag::Vector(Box::new(vector_tag));
    }
    let type_args = vec![vector_tag];
    let arg = vec![vec![vec![vec![vec![vec![vec![vec![vec![0u8]]]]]]]]];
    call_type_arg(
        addr,
        verifier,
        type_args,
        vec![bcs::to_bytes(&arg).unwrap()],
    )
}

fn call_vector_arg_11(verifier: &VerifierConfig) -> VMResult<SerializedReturnValues> {
    let addr = AccountAddress::from_hex_literal("0xcafe").unwrap();
    let mut vector_tag = TypeTag::U8;
    for _ in 0..10 {
        vector_tag = TypeTag::Vector(Box::new(vector_tag));
    }
    let type_args = vec![vector_tag];
    let arg = vec![vec![vec![vec![vec![vec![vec![vec![vec![vec![vec![
        vec![vec![vec![0u8]]],
    ]]]]]]]]]]];
    call_type_arg(
        addr,
        verifier,
        type_args,
        vec![bcs::to_bytes(&arg).unwrap()],
    )
}

fn call_generic_arg(
    verifier: &VerifierConfig,
    nest_count: usize,
) -> VMResult<SerializedReturnValues> {
    let addr = AccountAddress::from_hex_literal("0xcafe").unwrap();
    let mut struct_tag_outer = TypeTag::U8;
    for _ in 0..nest_count {
        struct_tag_outer = TypeTag::Struct(Box::new(StructTag {
            address: addr,
            module: Identifier::new(MODULE_NAME).unwrap(),
            name: Identifier::new(OUTER_NAME).unwrap(),
            type_params: vec![struct_tag_outer],
        }));
    }
    let type_args = vec![struct_tag_outer];
    let arg = 0u8;
    call_type_arg(
        addr,
        verifier,
        type_args,
        vec![bcs::to_bytes(&arg).unwrap()],
    )
}

fn call_type_arg(
    addr: AccountAddress,
    verifier: &VerifierConfig,
    type_args: Vec<TypeTag>,
    args: Vec<Vec<u8>>,
) -> VMResult<SerializedReturnValues> {
    let vm = MoveVM::new_with_config(
        vec![],
        VMConfig {
            verifier: verifier.clone(),
            ..Default::default()
        },
    )
    .unwrap();
    let storage: InMemoryStorage = InMemoryStorage::new();
    let mut session = vm.new_session(&storage);

    //
    // Module definition and publishing
    let struct_type_args_count = 1;
    let fun_type_args_count = 1;
    let acquires = vec![vec![], vec![], vec![]];
    let code = vec![vec![Ret], vec![Ret], vec![Ret]];
    let parameters = vec![SignatureIndex(0), SignatureIndex(1), SignatureIndex(0)];
    let locals = vec![SignatureIndex(0); 3];
    let signatures = DEFAULT_SIGNATURES.clone();
    let struct_def_instantiations = vec![];
    let function_instantiations = vec![];

    let self_id = make_module(
        &mut session,
        addr,
        struct_type_args_count,
        fun_type_args_count,
        acquires,
        code,
        parameters,
        locals,
        signatures,
        struct_def_instantiations,
        function_instantiations,
    );

    let mut gas = GasStatus::new_unmetered();
    session.execute_entry_function(
        &self_id,
        IdentStr::new(ENTRY_POINT_NAME_2).unwrap(),
        type_args,
        args,
        &mut gas,
    )
}
