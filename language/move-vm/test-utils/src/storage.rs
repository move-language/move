// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#[allow(unused)]
use anyhow::{anyhow, format_err, Error, Result};
#[allow(unused)]
use move_core_types::gas_schedule::{GasAlgebra, GasCarrier, InternalGasUnits};
use move_core_types::{
    account_address::AccountAddress,
    effects::{AccountChangeSet, ChangeSet},
    identifier::Identifier,
    language_storage::{ModuleId, StructTag},
    resolver::{ModuleResolver, MoveResolver, ResourceResolver},
};
use std::collections::{btree_map, BTreeMap};

#[cfg(feature = "table-extension")]
use move_table_extension::{TableChangeSet, TableHandle, TableOperation, TableResolver};

/// A dummy storage containing no modules or resources.
#[derive(Debug, Clone)]
pub struct BlankStorage;

impl BlankStorage {
    pub fn new() -> Self {
        Self
    }
}

impl ModuleResolver for BlankStorage {
    type Error = ();

    fn get_module(&self, _module_id: &ModuleId) -> Result<Option<Vec<u8>>, Self::Error> {
        Ok(None)
    }
}

impl ResourceResolver for BlankStorage {
    type Error = ();

    fn get_resource(
        &self,
        _address: &AccountAddress,
        _tag: &StructTag,
    ) -> Result<Option<Vec<u8>>, Self::Error> {
        Ok(None)
    }
}

#[cfg(feature = "table-extension")]
impl TableResolver for BlankStorage {
    fn resolve_table_entry(
        &self,
        _handle: &TableHandle,
        _key: &[u8],
    ) -> Result<Option<Vec<u8>>, Error> {
        Ok(None)
    }

    fn operation_cost(
        &self,
        _op: TableOperation,
        _key_size: usize,
        _val_size: usize,
    ) -> InternalGasUnits<GasCarrier> {
        InternalGasUnits::new(1)
    }
}

// A storage adapter created by stacking a change set on top of an existing storage backend.
/// The new storage can be used for additional computations without modifying the base.
#[derive(Debug, Clone)]
pub struct DeltaStorage<'a, 'b, S> {
    base: &'a S,
    delta: &'b ChangeSet,
}

impl<'a, 'b, S: ModuleResolver> ModuleResolver for DeltaStorage<'a, 'b, S> {
    type Error = S::Error;

    fn get_module(&self, module_id: &ModuleId) -> Result<Option<Vec<u8>>, Self::Error> {
        if let Some(account_storage) = self.delta.accounts().get(module_id.address()) {
            if let Some(blob_opt) = account_storage.modules().get(module_id.name()) {
                return Ok(blob_opt.clone());
            }
        }

        self.base.get_module(module_id)
    }
}

impl<'a, 'b, S: ResourceResolver> ResourceResolver for DeltaStorage<'a, 'b, S> {
    type Error = S::Error;

    fn get_resource(
        &self,
        address: &AccountAddress,
        tag: &StructTag,
    ) -> Result<Option<Vec<u8>>, S::Error> {
        if let Some(account_storage) = self.delta.accounts().get(address) {
            if let Some(blob_opt) = account_storage.resources().get(tag) {
                return Ok(blob_opt.clone());
            }
        }

        self.base.get_resource(address, tag)
    }
}

#[cfg(feature = "table-extension")]
impl<'a, 'b, S: TableResolver> TableResolver for DeltaStorage<'a, 'b, S> {
    fn resolve_table_entry(
        &self,
        handle: &TableHandle,
        key: &[u8],
    ) -> std::result::Result<Option<Vec<u8>>, Error> {
        // TODO: No support for table deltas
        self.base.resolve_table_entry(handle, key)
    }

    fn operation_cost(
        &self,
        op: TableOperation,
        key_size: usize,
        val_size: usize,
    ) -> InternalGasUnits<GasCarrier> {
        // TODO: No support for table deltas
        self.base.operation_cost(op, key_size, val_size)
    }
}

impl<'a, 'b, S: MoveResolver> DeltaStorage<'a, 'b, S> {
    pub fn new(base: &'a S, delta: &'b ChangeSet) -> Self {
        Self { base, delta }
    }
}

/// Simple in-memory storage for modules and resources under an account.
#[derive(Debug, Clone)]
struct InMemoryAccountStorage {
    resources: BTreeMap<StructTag, Vec<u8>>,
    modules: BTreeMap<Identifier, Vec<u8>>,
}

/// Simple in-memory storage that can be used as a Move VM storage backend for testing purposes.
#[derive(Debug, Clone)]
pub struct InMemoryStorage {
    accounts: BTreeMap<AccountAddress, InMemoryAccountStorage>,
    #[cfg(feature = "table-extension")]
    tables: BTreeMap<TableHandle, BTreeMap<Vec<u8>, Vec<u8>>>,
}

fn apply_changes<K, V, F, E>(
    tree: &mut BTreeMap<K, V>,
    changes: impl IntoIterator<Item = (K, Option<V>)>,
    make_err: F,
) -> std::result::Result<(), E>
where
    K: Ord,
    F: FnOnce(K) -> E,
{
    for (k, v_opt) in changes.into_iter() {
        match (tree.entry(k), v_opt) {
            (btree_map::Entry::Vacant(entry), None) => return Err(make_err(entry.into_key())),
            (btree_map::Entry::Vacant(entry), Some(v)) => {
                entry.insert(v);
            }
            (btree_map::Entry::Occupied(entry), None) => {
                entry.remove();
            }
            (btree_map::Entry::Occupied(entry), Some(v)) => {
                *entry.into_mut() = v;
            }
        }
    }
    Ok(())
}

impl InMemoryAccountStorage {
    fn apply(&mut self, account_changeset: AccountChangeSet) -> Result<()> {
        let (modules, resources) = account_changeset.into_inner();
        apply_changes(&mut self.modules, modules, |module_name| {
            format_err!(
                "Failed to delete module {}: module does not exist.",
                module_name
            )
        })?;

        apply_changes(&mut self.resources, resources, |struct_tag| {
            format_err!(
                "Failed to delete resource {}: resource does not exist.",
                struct_tag
            )
        })?;

        Ok(())
    }

    fn new() -> Self {
        Self {
            modules: BTreeMap::new(),
            resources: BTreeMap::new(),
        }
    }
}

impl InMemoryStorage {
    pub fn apply_extended(
        &mut self,
        changeset: ChangeSet,
        #[cfg(feature = "table-extension")] table_changes: TableChangeSet,
    ) -> Result<()> {
        for (addr, account_changeset) in changeset.into_inner() {
            match self.accounts.entry(addr) {
                btree_map::Entry::Occupied(entry) => {
                    entry.into_mut().apply(account_changeset)?;
                }
                btree_map::Entry::Vacant(entry) => {
                    let mut account_storage = InMemoryAccountStorage::new();
                    account_storage.apply(account_changeset)?;
                    entry.insert(account_storage);
                }
            }
        }

        #[cfg(feature = "table-extension")]
        self.apply_table(table_changes);

        Ok(())
    }

    pub fn apply(&mut self, changeset: ChangeSet) -> Result<()> {
        self.apply_extended(
            changeset,
            #[cfg(feature = "table-extension")]
            TableChangeSet::default(),
        )
    }

    #[cfg(feature = "table-extension")]
    fn apply_table(&mut self, changes: TableChangeSet) {
        let TableChangeSet {
            new_tables,
            removed_tables,
            changes,
        } = changes;
        self.tables.retain(|h, _| !removed_tables.contains(h));
        self.tables.extend(
            new_tables
                .keys()
                .into_iter()
                .map(|h| (*h, BTreeMap::default())),
        );
        for (h, c) in changes {
            assert!(
                self.tables.contains_key(&h),
                "inconsistent table change set: stale table handle"
            );
            let table = self.tables.get_mut(&h).unwrap();
            for (key, val) in c.entries {
                if let Some(v) = val {
                    table.insert(key, v);
                } else {
                    table.remove(&key);
                }
            }
        }
    }

    pub fn new() -> Self {
        Self {
            accounts: BTreeMap::new(),
            #[cfg(feature = "table-extension")]
            tables: BTreeMap::new(),
        }
    }

    pub fn publish_or_overwrite_module(&mut self, module_id: ModuleId, blob: Vec<u8>) {
        let mut delta = ChangeSet::new();
        delta.publish_module(module_id, blob).unwrap();
        self.apply_extended(
            delta,
            #[cfg(feature = "table-extension")]
            TableChangeSet::default(),
        )
        .unwrap();
    }

    pub fn publish_or_overwrite_resource(
        &mut self,
        addr: AccountAddress,
        struct_tag: StructTag,
        blob: Vec<u8>,
    ) {
        let mut delta = ChangeSet::new();
        delta.publish_resource(addr, struct_tag, blob).unwrap();
        self.apply_extended(
            delta,
            #[cfg(feature = "table-extension")]
            TableChangeSet::default(),
        )
        .unwrap();
    }
}

impl ModuleResolver for InMemoryStorage {
    type Error = ();

    fn get_module(&self, module_id: &ModuleId) -> Result<Option<Vec<u8>>, Self::Error> {
        if let Some(account_storage) = self.accounts.get(module_id.address()) {
            return Ok(account_storage.modules.get(module_id.name()).cloned());
        }
        Ok(None)
    }
}

impl ResourceResolver for InMemoryStorage {
    type Error = ();

    fn get_resource(
        &self,
        address: &AccountAddress,
        tag: &StructTag,
    ) -> Result<Option<Vec<u8>>, Self::Error> {
        if let Some(account_storage) = self.accounts.get(address) {
            return Ok(account_storage.resources.get(tag).cloned());
        }
        Ok(None)
    }
}

#[cfg(feature = "table-extension")]
impl TableResolver for InMemoryStorage {
    fn resolve_table_entry(
        &self,
        handle: &TableHandle,
        key: &[u8],
    ) -> std::result::Result<Option<Vec<u8>>, Error> {
        Ok(self.tables.get(handle).and_then(|t| t.get(key).cloned()))
    }

    fn operation_cost(
        &self,
        _op: TableOperation,
        _key_size: usize,
        _val_size: usize,
    ) -> InternalGasUnits<GasCarrier> {
        InternalGasUnits::new(1)
    }
}
