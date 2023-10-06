use anyhow::Error;

use hashbrown::HashMap;
use move_core_types::account_address::AccountAddress;

use move_core_types::identifier::Identifier;
use move_core_types::language_storage::{ModuleId, StructTag};
use move_core_types::resolver::{ModuleResolver, ResourceResolver};

use crate::storage::Storage;

/// Structure holding account data which is held under one Move address (or one key
/// in Substrate storage).
#[derive(Clone, Debug, Default)]
pub struct AccountData {
    /// Hashmap of the modules kept under this account.
    pub modules: HashMap<Identifier, Vec<u8>>,
    /// Hashmap of the resources kept under this account.
    pub resources: HashMap<StructTag, Vec<u8>>,
}

/// Move VM storage implementation for Substrate storage.
pub struct Warehouse<S: Storage> {
    /// Substrate storage implementing Storage trait
    storage: S,
}

impl<S: Storage> Storage for Warehouse<S> {
    fn get(&self, key: &[u8]) -> Option<AccountData> {
        self.storage.get(key)
    }

    fn set(&self, key: &[u8], value: &AccountData) {
        self.storage.set(key, value);
    }

    fn remove(&self, key: &[u8]) {
        self.storage.remove(key);
    }
}

impl<S: Storage> Warehouse<S> {
    pub fn new(storage: S) -> Warehouse<S> {
        Warehouse { storage }
    }
}

impl<S: Storage> ModuleResolver for Warehouse<S> {
    type Error = Error;

    fn get_module(&self, module_id: &ModuleId) -> Result<Option<Vec<u8>>, Self::Error> {
        let acc = self.storage.get(module_id.address().as_slice());

        if let Some(acc) = acc {
            Ok(acc.modules.get(module_id.name()).map(|v| v.clone()))
        } else {
            // Even if account is not found, we still return Ok(None) - it's not an error
            // for MoveVM.
            Ok(None)
        }
    }
}

impl<S: Storage> ResourceResolver for Warehouse<S> {
    type Error = Error;

    fn get_resource(
        &self,
        address: &AccountAddress,
        tag: &StructTag,
    ) -> Result<Option<Vec<u8>>, Self::Error> {
        let acc = self.storage.get(address.as_slice());

        if let Some(acc) = acc {
            Ok(acc.resources.get(tag).map(|v| v.clone()))
        } else {
            // Even if account is not found, we still return Ok(None) - it's not an error
            // for MoveVM.
            Ok(None)
        }
    }
}
