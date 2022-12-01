// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::modules::Modules;

use lsp_server::Connection;

/// The context within which the language server is running.
pub struct Context {
    pub modules: Modules,
    /// The connection with the language server's client.
    pub connection: Connection,
}
