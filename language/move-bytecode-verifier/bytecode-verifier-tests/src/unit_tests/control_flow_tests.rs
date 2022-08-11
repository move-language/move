// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::support::dummy_procedure_module;
use move_binary_format::{
    access::ModuleAccess,
    errors::PartialVMResult,
    file_format::{Bytecode, CompiledModule, FunctionDefinitionIndex, TableIndex},
};
use move_bytecode_verifier::{control_flow, VerifierConfig};
use move_core_types::vm_status::StatusCode;

fn verify_module(verifier_config: &VerifierConfig, module: &CompiledModule) -> PartialVMResult<()> {
    for (idx, function_definition) in module
        .function_defs()
        .iter()
        .enumerate()
        .filter(|(_, def)| !def.is_native())
    {
        control_flow::verify(
            verifier_config,
            Some(FunctionDefinitionIndex(idx as TableIndex)),
            function_definition
                .code
                .as_ref()
                .expect("unexpected native function"),
        )?
    }
    Ok(())
}

//**************************************************************************************************
// Simple cases -  Copied from code unit verifier
//**************************************************************************************************

#[test]
fn invalid_fallthrough_br_true() {
    let module = dummy_procedure_module(vec![Bytecode::LdFalse, Bytecode::BrTrue(1)]);
    let result = verify_module(&Default::default(), &module);
    assert_eq!(
        result.unwrap_err().major_status(),
        StatusCode::INVALID_FALL_THROUGH
    );
}

#[test]
fn invalid_fallthrough_br_false() {
    let module = dummy_procedure_module(vec![Bytecode::LdTrue, Bytecode::BrFalse(1)]);
    let result = verify_module(&Default::default(), &module);
    assert_eq!(
        result.unwrap_err().major_status(),
        StatusCode::INVALID_FALL_THROUGH
    );
}

// all non-branch instructions should trigger invalid fallthrough; just check one of them
#[test]
fn invalid_fallthrough_non_branch() {
    let module = dummy_procedure_module(vec![Bytecode::LdTrue, Bytecode::Pop]);
    let result = verify_module(&Default::default(), &module);
    assert_eq!(
        result.unwrap_err().major_status(),
        StatusCode::INVALID_FALL_THROUGH
    );
}

#[test]
fn valid_fallthrough_branch() {
    let module = dummy_procedure_module(vec![Bytecode::Branch(0)]);
    let result = verify_module(&Default::default(), &module);
    assert!(result.is_ok());
}

#[test]
fn valid_fallthrough_ret() {
    let module = dummy_procedure_module(vec![Bytecode::Ret]);
    let result = verify_module(&Default::default(), &module);
    assert!(result.is_ok());
}

#[test]
fn valid_fallthrough_abort() {
    let module = dummy_procedure_module(vec![Bytecode::LdU64(7), Bytecode::Abort]);
    let result = verify_module(&Default::default(), &module);
    assert!(result.is_ok());
}

#[test]
fn nested_loops_max_depth() {
    let module = dummy_procedure_module(vec![
        Bytecode::LdFalse,
        Bytecode::LdFalse,
        Bytecode::BrFalse(1),
        Bytecode::BrFalse(0),
        Bytecode::Ret,
    ]);
    let result = verify_module(
        &VerifierConfig {
            max_loop_depth: Some(2),
        },
        &module,
    );
    assert!(result.is_ok());
}

#[test]
fn nested_loops_exceed_max_depth() {
    let module = dummy_procedure_module(vec![
        Bytecode::LdFalse,
        Bytecode::LdFalse,
        Bytecode::LdFalse,
        Bytecode::BrFalse(2),
        Bytecode::BrFalse(1),
        Bytecode::BrFalse(0),
        Bytecode::Ret,
    ]);
    let result = verify_module(
        &VerifierConfig {
            max_loop_depth: Some(2),
        },
        &module,
    );
    assert_eq!(
        result.unwrap_err().major_status(),
        StatusCode::LOOP_MAX_DEPTH_REACHED
    );
}
