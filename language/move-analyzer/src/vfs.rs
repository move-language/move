// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! The language server must operate upon Move source buffers as they are being edited.
//! As a result, it is frequently queried about buffers that have not yet (or may never be) saved
//! to the actual file system.
//!
//! To manage these buffers, this module provides a "virtual file system" -- in reality, it is
//! basically just a mapping from file identifier (this could be the file's path were it to be
//! saved) to its textual contents.

use lsp_server::Notification;
use lsp_types::{
    notification::Notification as _, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams,
};
use std::path::PathBuf;

/// A mapping from identifiers (file names, potentially, but not necessarily) to their contents.
#[derive(Debug, Default)]
pub struct VirtualFileSystem {
    files: std::collections::HashMap<PathBuf, String>,
}

impl VirtualFileSystem {
    /// Returns a reference to the buffer corresponding to the given identifier, or `None` if it
    /// is not present in the system.
    pub fn get(&self, identifier: &PathBuf) -> Option<&str> {
        self.files.get(identifier).map(|s| s.as_str())
    }

    /// Inserts or overwrites the buffer corresponding to the given identifier.
    ///
    /// TODO: A far more efficient "virtual file system" would update its buffers with changes sent
    /// from the client, instead of completely replacing them each time. The rust-analyzer has a
    /// 'vfs' module that is capable of doing just that, but it is not published on crates.io. If
    /// we could help get it published, we could use it here.
    pub fn update(&mut self, identifier: PathBuf, content: &str) {
        self.files.insert(identifier, content.to_string());
    }

    /// Removes the buffer and its identifier from the system.
    pub fn remove(&mut self, identifier: &PathBuf) {
        self.files.remove(identifier);
    }
}
