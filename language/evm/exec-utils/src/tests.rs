// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{compile::solc, exec::Executor};
use anyhow::Result;
use evm::{backend::MemoryVicinity, ExitReason};
use primitive_types::{H160, U256};
use std::path::{Path, PathBuf};

fn contract_path(file_name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("contracts")
        .join(file_name)
}

fn test_vincinity() -> MemoryVicinity {
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

#[test]
fn test_hello_world() -> Result<()> {
    let (_contract_name, contract_code) = solc([contract_path("hello_world.sol")])?
        .into_iter()
        .next()
        .unwrap();

    let vicinity = test_vincinity();
    let mut exec = Executor::new(&vicinity);

    assert!(exec.create_contract(H160::zero(), contract_code).is_ok());

    Ok(())
}

#[test]
fn test_two_functions() -> Result<()> {
    let (_contract_name, contract_code) = solc([contract_path("two_functions.sol")])?
        .into_iter()
        .next()
        .unwrap();

    let vicinity = test_vincinity();
    let mut exec = Executor::new(&vicinity);

    let contract_address = exec
        .create_contract(H160::zero(), contract_code)
        .expect("failed to create contract");

    let (exit_reason, _buffer) =
        exec.call_function(H160::zero(), contract_address, "do_nothing()", &[]);
    assert!(matches!(exit_reason, ExitReason::Succeed(_)));

    let (exit_reason, _buffer) = exec.call_function(H160::zero(), contract_address, "panic()", &[]);
    assert!(matches!(exit_reason, ExitReason::Revert(_)));

    Ok(())
}

#[test]
fn test_a_plus_b() -> Result<()> {
    let (_contract_name, contract_code) = solc([contract_path("a_plus_b.sol")])?
        .into_iter()
        .next()
        .unwrap();

    let vicinity = test_vincinity();
    let mut exec = Executor::new(&vicinity);

    let contract_address = exec
        .create_contract(H160::zero(), contract_code)
        .expect("failed to create contract");

    let mut args = vec![0u8; 64];
    args[0] = 1;
    args[32] = 2;

    let (exit_reason, buffer) = exec.call_function(
        H160::zero(),
        contract_address,
        "plus(uint256,uint256)",
        &args,
    );
    assert!(matches!(exit_reason, ExitReason::Succeed(_)));

    assert!(buffer.len() >= 32);
    let mut expected = [0u8; 32];
    expected[0] = 0x3;
    assert_eq!(&buffer[..32], &expected);

    Ok(())
}
