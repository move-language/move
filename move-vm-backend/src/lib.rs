#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

mod abi;
pub mod storage;
mod warehouse;

use abi::ModuleAbi;
use alloc::vec::Vec;
use anyhow::{anyhow, Error};

use move_binary_format::CompiledModule;
use move_core_types::account_address::AccountAddress;

use move_core_types::{
    language_storage::CORE_CODE_ADDRESS,
    resolver::{ModuleResolver, ResourceResolver},
};
use move_vm_runtime::move_vm::MoveVM;

use move_stdlib::natives::{all_natives, GasParameters};
use move_vm_types::gas::GasMeter;

use crate::storage::Storage;
use crate::warehouse::Warehouse;

/// Main MoveVM structure, which is used to represent the virutal machine itself.
pub struct Mvm<S>
where
    S: Storage,
{
    // MoveVM instance - from move_vm_runtime crate
    vm: MoveVM,
    // Storage instance
    warehouse: Warehouse<S>,
}

impl<S> Mvm<S>
where
    S: Storage,
{
    /// Create a new Move VM with the given storage.
    pub fn new(storage: S) -> Result<Mvm<S>, Error> {
        Self::new_with_config(storage)
    }

    /// Create a new Move VM with the given storage and configuration.
    pub(crate) fn new_with_config(
        storage: S,
        // config: VMConfig,
    ) -> Result<Mvm<S>, Error> {
        Ok(Mvm {
            vm: MoveVM::new(all_natives(CORE_CODE_ADDRESS, GasParameters::zeros())).map_err(
                |err| {
                    let (code, _, msg, _, _, _, _) = err.all_data();
                    anyhow!("Error code:{:?}: msg: '{}'", code, msg.unwrap_or_default())
                },
            )?,
            warehouse: Warehouse::new(storage),
        })
    }

    /// Get module binary using the module ID.
    // TODO: should we use Identifier and AccountAddress here instead to create the ModuleID?
    pub fn get_module(&self, module_id: &[u8]) -> Result<Option<Vec<u8>>, Error> {
        let module_id = bcs::from_bytes(module_id).map_err(Error::msg)?;
        self.warehouse.get_module(&module_id)
    }

    /// Get module binary ABI using the module ID.
    // TODO: should we use Identifier and AccountAddress here instead to create the ModuleID?
    pub fn get_module_abi(&self, module_id: &[u8]) -> Result<Option<Vec<u8>>, Error> {
        if let Some(bytecode) = self.get_module(module_id)? {
            return Ok(Some(
                bcs::to_bytes(&ModuleAbi::from(
                    CompiledModule::deserialize(&bytecode).map_err(Error::msg)?,
                ))
                .map_err(Error::msg)?,
            ));
        }

        Ok(None)
    }

    /// Get resource using an address and a tag.
    // TODO: could we use Identifier and AccountAddress here instead as arguments?
    pub fn get_resource(
        &self,
        address: &AccountAddress,
        tag: &[u8],
    ) -> Result<Option<Vec<u8>>, Error> {
        let tag = bcs::from_bytes(tag).map_err(Error::msg)?;
        self.warehouse.get_resource(address, &tag)
    }

    /// Publish module into the storage. Module is published under the given address.
    pub fn publish_module(
        &self,
        module: &[u8],
        address: AccountAddress,
        gas: &mut impl GasMeter,
    ) -> Result<(), Error> {
        let mut sess = self.vm.new_session(&self.warehouse);

        sess.publish_module(module.to_vec(), address, gas)
            .map_err(|err| {
                let (code, _, msg, _, _, _, _) = err.all_data();
                anyhow!("Error code:{:?}: msg: '{}'", code, msg.unwrap_or_default())
            })?;

        let (changeset, _) = sess.finish().map_err(|err| {
            let (code, _, msg, _, _, _, _) = err.all_data();
            anyhow!("Error code:{:?}: msg: '{}'", code, msg.unwrap_or_default())
        })?;

        self.warehouse.apply_changes(changeset)
    }
}
