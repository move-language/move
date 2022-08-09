// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::source_package::parsed_manifest::NodeInfo;
use anyhow::bail;
use move_symbol_pool::Symbol;
use once_cell::sync::Lazy;
use std::sync::Mutex;

// TODO: remove static hooks and refactor this crate for better customizability

/// A trait providing hooks to customize the package system for a particular Move application.
/// An instance of the trait can be registered globally.
pub trait PackageHooks {
    /// Returns custom fields allowed in `PackageInfo`.
    fn custom_package_info_fields(&self) -> Vec<String>;

    /// A resolver for `node` dependencies in the manifest. This is called to download the
    /// dependency from the node into the `info.local_path` location, similar as with git
    /// dependencies.
    fn resolve_node_dependency(&self, dep_name: Symbol, info: &NodeInfo) -> anyhow::Result<()>;
}
static HOOKS: Lazy<Mutex<Option<Box<dyn PackageHooks + Send + Sync>>>> =
    Lazy::new(|| Mutex::new(None));

/// Registers package hooks for the process in which the package system is used.
pub fn register_package_hooks(hooks: Box<dyn PackageHooks + Send + Sync>) {
    *HOOKS.lock().unwrap() = Some(hooks)
}

/// Calls any registered hook to resolve a node dependency. Bails if none is registered.
pub(crate) fn resolve_node_dependency(dep_name: Symbol, info: &NodeInfo) -> anyhow::Result<()> {
    if let Some(hooks) = &*HOOKS.lock().unwrap() {
        hooks.resolve_node_dependency(dep_name, info)
    } else {
        bail!("use of unsupported 'node' dependency in package manifest")
    }
}

/// Calls any registered hook to return custom package fields.
pub(crate) fn custom_package_info_fields() -> Vec<String> {
    if let Some(hooks) = &*HOOKS.lock().unwrap() {
        hooks.custom_package_info_fields()
    } else {
        vec![]
    }
}
