// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::Result;
use evm::{backend::MemoryVicinity, ExitReason};
use evm_exec_utils::{compile, exec::Executor};
use move_model::{options::ModelBuilderOptions, run_model_builder_with_options};
use move_to_yul::{generator::Generator, options::Options};
use primitive_types::{H160, U256};
use std::path::{Path, PathBuf};

pub const DISPATCHER_TESTS_LOCATION: &str = "tests/test-dispatcher";

fn contract_path(file_name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(DISPATCHER_TESTS_LOCATION)
        .join(file_name)
}

pub fn generate_testing_vincinity() -> MemoryVicinity {
    MemoryVicinity {
        gas_price: 0.into(),
        origin: H160::zero(),
        chain_id: 0.into(),
        block_hashes: vec![],
        block_number: 0.into(),
        block_coinbase: H160::zero(),
        block_timestamp: 0.into(),
        block_difficulty: 0.into(),
        block_gas_limit: U256::MAX,
        block_base_fee_per_gas: 0.into(),
    }
}

fn compile_yul_to_bytecode_bytes(filename: &str) -> Result<Vec<u8>> {
    let env = run_model_builder_with_options(
        &[contract_path(filename).to_string_lossy().to_string()],
        &[],
        ModelBuilderOptions::default(),
        move_stdlib::move_stdlib_named_addresses(),
    )?;
    let options = Options::default();
    let (_, out) = Generator::run(&options, &env);
    let (bc, _) = compile::solc_yul(&out, false)?;
    Ok(bc)
}

/// Test DispatcherBasic
#[test]
fn test_dispatch_basic() -> Result<()> {
    let contract_code = compile_yul_to_bytecode_bytes("DispatcherBasic.move")?;
    let vicinity = generate_testing_vincinity();
    let mut exec = Executor::new(&vicinity);
    let contract_address = exec
        .create_contract(H160::zero(), contract_code)
        .expect("failed to create contract");
    for i in 0..3 {
        let sig = format!("return_{}()", i);
        let (exit_reason, buffer) =
            exec.call_function(H160::zero(), contract_address, 0.into(), &sig, &[]);
        assert!(matches!(exit_reason, ExitReason::Succeed(_)));
        assert!(buffer.len() >= 32);
        let mut expected = [0u8; 32];
        expected[31] = i;
        assert_eq!(&buffer[..32], &expected);
    }
    Ok(())
}

/// Test DispatcherRevert
#[test]
fn test_dispatch_revert() -> Result<()> {
    let contract_code = compile_yul_to_bytecode_bytes("DispatcherRevert.move")?;
    let vicinity = generate_testing_vincinity();
    let mut exec = Executor::new(&vicinity);
    let contract_address = exec
        .create_contract(H160::zero(), contract_code)
        .expect("failed to create contract");
    let sig = "return_1";
    let (exit_reason, _) = exec.call_function(H160::zero(), contract_address, 0.into(), sig, &[]);
    assert!(matches!(exit_reason, ExitReason::Revert(_)));
    Ok(())
}

/// Test DispatcherFallback
#[test]
fn test_dispatch_fallback() -> Result<()> {
    let contract_code = compile_yul_to_bytecode_bytes("DispatcherFallback.move")?;
    let vicinity = generate_testing_vincinity();
    let mut exec = Executor::new(&vicinity);
    let contract_address = exec
        .create_contract(H160::zero(), contract_code)
        .expect("failed to create contract");
    let sig = "return_1";
    let (exit_reason, _) = exec.call_function(H160::zero(), contract_address, 0.into(), sig, &[]);
    assert!(matches!(exit_reason, ExitReason::Succeed(_)));
    Ok(())
}
