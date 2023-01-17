// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use std::collections::HashMap;
use std::path::PathBuf;

use crate::modules::Modules;
use crate::references::ReferencesCache;
use lsp_server::Connection;

/// The context within which the language server is running.
pub struct Context {
    pub modules: MultiProject,
    /// The connection with the language server's client.
    pub connection: Connection,
    pub ref_caches: ReferencesCache,
}

#[derive(Default)]
pub struct MultiProject {
    projects: HashMap<PathBuf, Modules>,
}
impl MultiProject {
    pub fn new() -> MultiProject {
        let dir = std::env::current_dir().unwrap();
        let mut m = MultiProject::default();
        for x in walkdir::WalkDir::new(dir) {
            let x = match x {
                Ok(x) => x,
                Err(_) => {
                    continue;
                }
            };
            if x.file_type().is_file() && x.file_name().to_string_lossy().ends_with("Move.toml") {
                let mut mani = x.clone().into_path();
                mani.pop();
                eprintln!("load manifest:{:?}", mani.as_path());
                m.projects.insert(
                    mani.clone(),
                    match Modules::new(&mani) {
                        Ok(x) => x,
                        Err(err) => {
                            log::error!(
                                "load manifest {:?} failed,err:{:?}",
                                mani.clone().as_path(),
                                err
                            );
                            continue;
                        }
                    },
                );
            };
        }
        eprintln!("finish loading all projects");
        m
    }
    pub fn get_modules(&self, x: &PathBuf) -> Option<&Modules> {
        let (manifest, _) = super::utils::discover_manifest_and_kind(x.as_path())?;
        self.projects.get(&manifest)
    }
    pub fn get_modules_mut(&mut self, x: &PathBuf) -> Option<&mut Modules> {
        let (manifest, _) = super::utils::discover_manifest_and_kind(x.as_path())?;
        self.projects.get_mut(&manifest)
    }
}
