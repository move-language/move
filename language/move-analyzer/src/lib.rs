// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0
#[macro_use]
extern crate lazy_static;
extern crate move_ir_types;
pub mod completion;
pub mod context;
pub mod diagnostics;
pub mod document_symbol;
pub mod goto_definition;
pub mod hover;
#[cfg(test)]
mod ide_test;
pub mod item;
pub mod misc;
pub mod modules;
pub mod modules_visitor;
pub mod references;
pub mod scope;
pub mod scopes;
pub mod syntax;
pub mod types;
pub mod utils;
pub mod vfs;
