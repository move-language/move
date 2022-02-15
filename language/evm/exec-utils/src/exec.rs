// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use evm::{
    backend::{Apply, ApplyBackend, MemoryBackend, MemoryVicinity},
    executor::stack::{MemoryStackState, StackExecutor, StackSubstateMetadata},
    Config, ExitReason,
};
use primitive_types::{H160, H256, U256};
use sha3::{Digest, Keccak256};
use std::collections::BTreeMap;

/// Stateful EVM executor backed by an in-memory storage.
pub struct Executor<'v> {
    storage_backend: MemoryBackend<'v>,
}

fn map_apply<F, T, U>(apply: Apply<T>, mut f: F) -> Apply<U>
where
    F: FnMut(T) -> U,
{
    match apply {
        Apply::Modify {
            address,
            basic,
            code,
            storage,
            reset_storage,
        } => Apply::Modify {
            address,
            basic,
            code,
            storage: f(storage),
            reset_storage,
        },
        Apply::Delete { address } => Apply::Delete { address },
    }
}

// Try to find the address at which the contract has been deployed to.
//
// TODO: right now this feels more like a hack.
// We should study it more and determine if it's acceptable and see if we need to use CREATE2 instead.
fn find_contract_address<'a, I, T>(caller_address: H160, applies: I) -> H160
where
    I: IntoIterator<Item = &'a Apply<T>>,
    T: 'a,
{
    for apply in applies {
        if let Apply::Modify {
            address,
            code: Some(_),
            ..
        } = apply
        {
            if *address != caller_address {
                return *address;
            }
        }
    }

    panic!("failed to find contract address -- something is wrong")
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
        let config = Config::london();
        let metadata = StackSubstateMetadata::new(u64::MAX, &config);
        let state = MemoryStackState::new(metadata, &self.storage_backend);
        let mut exec = StackExecutor::new_with_precompiles(state, &config, &());

        let exit_reason =
            exec.transact_create(caller_address, 0.into(), contract_code, u64::MAX, vec![]);
        let state = exec.into_state();

        let (changes, logs) = state.deconstruct();

        match &exit_reason {
            ExitReason::Succeed(_) => {
                let changes: Vec<Apply<Vec<(H256, H256)>>> = changes
                    .into_iter()
                    .map(|app| map_apply(app, |entries| entries.into_iter().collect()))
                    .collect();

                let contract_addr = find_contract_address(caller_address, &changes);

                self.storage_backend.apply(changes, logs, false);
                Ok(contract_addr)
            }
            _ => {
                self.storage_backend.apply(changes, logs, false);
                Err(exit_reason)
            }
        }
    }

    /// Call a contract method with the given signature.
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

        exec.transact_call(
            caller_address,
            contract_address,
            U256::zero(),
            data,
            u64::MAX,
            vec![],
        )
    }

    // TODO: implement this.
    // pub fn run_custom_code(&mut self, _code: Vec<u8>, _data: Vec<u8>) {
    //     unimplemented!()
    // }
}
