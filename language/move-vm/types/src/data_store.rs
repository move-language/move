// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    loaded_data::runtime_types::Type,
    values::{GlobalValue, Value},
};
use move_binary_format::errors::{PartialVMResult, VMResult};
use move_core_types::{
    account_address::AccountAddress, gas_algebra::NumBytes, identifier::IdentStr,
    language_storage::ModuleId, value::MoveTypeLayout,
};

/// Provide an implementation for bytecodes related to data with a given data store.
///
/// The `DataStore` is a generic concept that includes both data and events.
/// A default implementation of the `DataStore` is `TransactionDataCache` which provides
/// an in memory cache for a given transaction and the atomic transactional changes
/// proper of a script execution (transaction).
pub trait DataStore {
    // ---
    // StateStore operations
    // ---

    /// Try to load a resource from remote storage and create a corresponding GlobalValue
    /// that is owned by the data store.
    fn load_resource(
        &mut self,
        addr: AccountAddress,
        ty: &Type,
    ) -> PartialVMResult<(&mut GlobalValue, Option<Option<NumBytes>>)>;

    /// The link context identifies the mapping from runtime `ModuleId`s to the `ModuleId`s in
    /// storage that they are loaded from as returned by `relocate`.  Implementors of `DataStore`
    /// are required to keep the link context stable for the duration of
    /// `Interpreter::execute_main`.
    fn link_context(&self) -> AccountAddress;

    /// Translate the runtime `module_id` to the on-chain `ModuleId` that it should be loaded from.
    fn relocate(&self, module_id: &ModuleId) -> PartialVMResult<ModuleId>;

    /// Translate the runtime fully-qualified struct name to the on-chain `ModuleId` that originally
    /// defined that type.
    fn defining_module(
        &self,
        module_id: &ModuleId,
        struct_: &IdentStr,
    ) -> PartialVMResult<ModuleId>;

    /// Get the serialized format of a `CompiledModule` given a `ModuleId`.
    fn load_module(&self, module_id: &ModuleId) -> VMResult<Vec<u8>>;

    /// Publish a module.
    fn publish_module(&mut self, module_id: &ModuleId, blob: Vec<u8>) -> VMResult<()>;

    // ---
    // EventStore operations
    // ---

    /// Emit an event to the EventStore
    fn emit_event(
        &mut self,
        guid: Vec<u8>,
        seq_num: u64,
        ty: Type,
        val: Value,
    ) -> PartialVMResult<()>;

    fn events(&self) -> &Vec<(Vec<u8>, u64, Type, MoveTypeLayout, Value)>;
}
