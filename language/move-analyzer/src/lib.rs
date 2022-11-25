// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#[macro_use(sp)]
extern crate move_ir_types;
pub mod completion;
pub mod context;
pub mod diagnostics;
pub mod goto_definition;
#[cfg(test)]
mod ide_test;
pub mod item;
pub mod modules;
pub mod scope;
pub mod scopes;
pub mod symbols;
pub mod types;
pub mod utils;
pub mod vfs;
