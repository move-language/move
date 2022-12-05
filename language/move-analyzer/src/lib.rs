// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

extern crate move_ir_types;
pub mod completion;
pub mod context;
pub mod diagnostics;
pub mod goto_definition;
#[cfg(test)]
mod ide_test;
pub mod item;
pub mod module_visitor;
pub mod modules;
pub mod scope;
pub mod scopes;

pub mod types;
pub mod utils;
pub mod vfs;
