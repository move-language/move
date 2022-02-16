// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use evm::{
    backend::{ApplyBackend, MemoryBackend, MemoryVicinity},
    executor::stack::{MemoryStackState, StackExecutor, StackSubstateMetadata},
    Config, CreateScheme, ExitReason,
};
use primitive_types::{H160, U256};
use sha3::{Digest, Keccak256};
use std::collections::BTreeMap;

/// Stateful EVM executor backed by an in-memory storage.
pub struct Executor<'v> {
    storage_backend: MemoryBackend<'v>,
}

/// Return the 4-byte method selector derived from the signature, which is encoded as a string (e.g. `"foo(uint256,uint256)"`).
//
// TODO: Rust type to represent the signature.
pub fn derive_method_selector(sig: &str) -> [u8; 4] {
    let mut keccak = Keccak256::new();
    keccak.update(sig.as_bytes());
    let digest = keccak.finalize();
    [digest[0], digest[1], digest[2], digest[3]]
}

impl<'v> Executor<'v> {
    /// Create a new `Executor` with an empty in-memory storage backend.
    //
    // TODO: review the lifetime of vicinity.
    pub fn new(vicinity: &'v MemoryVicinity) -> Self {
        Self {
            storage_backend: MemoryBackend::new(vicinity, BTreeMap::new()),
        }
    }

    /// Return a reference to the in-memory storage backend.
    pub fn storage(&self) -> &MemoryBackend<'v> {
        &self.storage_backend
    }

    /// Create a contract and return the contract address if successful.
    pub fn create_contract(
        &mut self,
        caller_address: H160,
        contract_code: Vec<u8>,
    ) -> Result<H160, ExitReason> {
        // TODO: due to some lifetime issues, the following code is duplicated a few times.
        // Figure out how we can reuse this code.
        let config = Config::london();
        let metadata = StackSubstateMetadata::new(u64::MAX, &config);
        let state = MemoryStackState::new(metadata, &self.storage_backend);
        let mut exec = StackExecutor::new_with_precompiles(state, &config, &());

        let contract_address = exec.create_address(CreateScheme::Legacy {
            caller: caller_address,
        });

        let exit_reason =
            exec.transact_create(caller_address, 0.into(), contract_code, u64::MAX, vec![]);

        let state = exec.into_state();
        let (changes, logs) = state.deconstruct();
        self.storage_backend.apply(changes, logs, false);

        match &exit_reason {
            ExitReason::Succeed(_) => Ok(contract_address),
            _ => Err(exit_reason),
        }
    }

    /// Call a contract method with the given signature.
    /// The signature is represented by a string consisting of the name of the method and
    /// a list of parameter types (e.g. `foo(uint256,uint256)`).
    pub fn call_function(
        &mut self,
        caller_address: H160,
        contract_address: H160,
        method_sig: &str,
        method_args: &[u8],
    ) -> (ExitReason, Vec<u8>) {
        let config = Config::london();
        let metadata = StackSubstateMetadata::new(u64::MAX, &config);
        let state = MemoryStackState::new(metadata, &self.storage_backend);
        let mut exec = StackExecutor::new_with_precompiles(state, &config, &());

        let mut data = vec![];
        data.extend(derive_method_selector(method_sig));
        data.extend(method_args);

        let (exit_reason, buffer) = exec.transact_call(
            caller_address,
            contract_address,
            U256::zero(),
            data,
            u64::MAX,
            vec![],
        );

        let state = exec.into_state();
        let (changes, logs) = state.deconstruct();
        self.storage_backend.apply(changes, logs, false);

        (exit_reason, buffer)
    }
}
