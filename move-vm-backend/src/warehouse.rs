use core::ops::Deref;

use anyhow::Error;

use hashbrown::HashMap;
use move_core_types::account_address::AccountAddress;

use move_core_types::effects::ChangeSet;
use move_core_types::effects::Op::{Delete, Modify, New};
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

impl<S: Storage> Warehouse<S> {
    pub(crate) fn new(storage: S) -> Warehouse<S> {
        Self { storage }
    }

    pub(crate) fn apply_changes(&self, changeset: ChangeSet) {
        for (account, identifier, operation) in changeset.modules() {
            match operation {
                New(data) | Modify(data) => {
                    if let Some(acc_data) = self.storage.get(account.as_slice()) {
                        let mut acc_data = acc_data.clone();

                        // This will insert or update concrete module, saving other things in the account.
                        acc_data
                            .modules
                            .insert(identifier.to_owned(), data.to_vec());
                        self.storage.set(account.as_slice(), &acc_data);
                    } else {
                        // This is a new account, so we need to create it.
                        let mut acc_data = AccountData {
                            modules: HashMap::new(),
                            resources: HashMap::new(),
                        };

                        acc_data
                            .modules
                            .insert(identifier.to_owned(), data.to_vec());

                        self.storage.set(account.as_slice(), &acc_data);
                    }
                }
                Delete => {
                    self.storage.remove(account.as_slice());
                }
            }
        }
    }
}

impl<S: Storage> Deref for Warehouse<S> {
    type Target = S;

    fn deref(&self) -> &Self::Target {
        &self.storage
    }
}

impl<S: Storage> ModuleResolver for Warehouse<S> {
    type Error = Error;

    fn get_module(&self, module_id: &ModuleId) -> Result<Option<Vec<u8>>, Self::Error> {
        let acc = self.storage.get(module_id.address().as_slice());

        if let Some(acc) = acc {
            Ok(acc.modules.get(module_id.name()).cloned())
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
            Ok(acc.resources.get(tag).cloned())
        } else {
            // Even if account is not found, we still return Ok(None) - it's not an error
            // for MoveVM.
            Ok(None)
        }
    }
}
