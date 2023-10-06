use alloc::vec::Vec;
use anyhow::Error;

use move_core_types::account_address::AccountAddress;
use move_core_types::language_storage::{ModuleId, StructTag};
use move_core_types::resolver::{ModuleResolver, ResourceResolver};

/// Trait for a storage engine. This is used by the Move VM to store data. Used for
/// mapping Substrate storage which is typical key-value container.
pub trait Storage {
    /// Returns the data for specified `key`.
    /// `None` if the key cannot be obtained.
    fn get(&self, key: &[u8]) -> Option<Vec<u8>>;

    /// Set a `value` for the specified `key` in the storage.
    /// If the key is non-existent, a new key-value pair is inserted.
    fn set(&self, key: &[u8], value: &[u8]);

    /// Remove `key` and its value from the storage.
    fn remove(&self, key: &[u8]);
}

/// Move VM storage implementation for Substrate storage.
pub struct MoveStorage<S: Storage> {
    /// Substrate storage implementing Storage trait
    storage: S,
}

impl<S: Storage> Storage for MoveStorage<S> {
    fn get(&self, key: &[u8]) -> Option<Vec<u8>> {
        self.storage.get(key)
    }

    fn set(&self, key: &[u8], value: &[u8]) {
        self.storage.set(key, value);
    }

    fn remove(&self, key: &[u8]) {
        self.storage.remove(key);
    }
}

impl<S: Storage> MoveStorage<S> {
    pub fn new(storage: S) -> MoveStorage<S> {
        MoveStorage { storage }
    }
}

impl<S: Storage> ModuleResolver for MoveStorage<S> {
    type Error = Error;

    fn get_module(&self, module_id: &ModuleId) -> Result<Option<Vec<u8>>, Self::Error> {
        Ok(self.storage.get(module_id.address().as_slice()))
    }
}

impl<S: Storage> ResourceResolver for MoveStorage<S> {
    type Error = Error;

    fn get_resource(
        &self,
        address: &AccountAddress,
        tag: &StructTag,
    ) -> Result<Option<Vec<u8>>, Self::Error> {
        let tag = tag.access_vector();
        let mut key = Vec::with_capacity(AccountAddress::LENGTH + tag.len());
        key.extend_from_slice(address.as_ref());
        key.extend_from_slice(&tag);

        Ok(self.storage.get(key.as_slice()))
    }
}
