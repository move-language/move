use core::ops::Deref;
use alloc::collections::{btree_map, BTreeMap};

use anyhow::Error;

use move_core_types::account_address::AccountAddress;

use move_core_types::effects::ChangeSet;
use move_core_types::effects::Op;
use move_core_types::identifier::Identifier;
use move_core_types::language_storage::{ModuleId, StructTag};
use move_core_types::resolver::{ModuleResolver, ResourceResolver};
use anyhow::{Result, bail};

use crate::storage::Storage;

/// Structure holding account data which is held under one Move address (or one key
/// in Substrate storage).
#[derive(Clone, Debug, Default)]
pub struct AccountData {
    /// Hashmap of the modules kept under this account.
    pub modules: BTreeMap<Identifier, Vec<u8>>,
    /// Hashmap of the resources kept under this account.
    pub resources: BTreeMap<StructTag, Vec<u8>>,
}

impl AccountData {
    fn apply_changes<K, V>(
        map: &mut BTreeMap<K, V>,
        changes: impl IntoIterator<Item = (K, Op<V>)>,
        ) -> Result<()>
        where
            K: Ord + core::fmt::Debug,
        {
            use btree_map::Entry::*;
            use Op::*;

            for (k, op) in changes.into_iter() {
                match (map.entry(k), op) {
                    (Occupied(entry), New(_)) => {
                        bail!(
                            "Failed to apply changes -- key {:?} already exists",
                            entry.key()
                            )
                    }
                    (Occupied(entry), Delete) => {
                        entry.remove();
                    }
                    (Occupied(entry), Modify(val)) => {
                        *entry.into_mut() = val;
                    }
                    (Vacant(entry), New(val)) => {
                        entry.insert(val);
                    }
                    (Vacant(entry), Delete | Modify(_)) => bail!(
                        "Failed to apply changes -- key {:?} does not exist",
                        entry.key()
                        ),
                }
            }
            Ok(())
        }
}


/// Move VM storage implementation for Substrate storage.
pub struct Warehouse<S: Storage> {
    /// Substrate storage implementing the Storage trait
    storage: S,
}

impl<S: Storage> Warehouse<S> {
    pub(crate) fn new(storage: S) -> Warehouse<S> {
        Self { storage }
    }

    pub(crate) fn apply_changes(&self, changeset: ChangeSet) -> Result<()> {
        for (account, changeset) in changeset.into_inner() {
            let key = account.as_slice();
            let mut account = self.storage.get(key).unwrap_or_default();

            let (modules, resources) = changeset.into_inner();
            AccountData::apply_changes(&mut account.modules, modules)?;
            AccountData::apply_changes(&mut account.resources, resources)?;

            self.storage.set(key, &account);
        }

        Ok(())
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
