// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

use crate::modules::Modules;
use crate::modules::*;
use crate::references::ReferencesCache;
use im::HashSet;
use lsp_server::Connection;
use move_compiler::parser::ast::Definition;

use move_package::source_package::layout::SourcePackageLayout;

/// The context within which the language server is running.
pub struct Context {
    pub projects: MultiProject,
    /// The connection with the language server's client.
    pub connection: Connection,
    pub ref_caches: ReferencesCache,
}

#[derive(Default)]
pub struct MultiProject {
    projects: HashMap<HashSet<PathBuf>, Modules>,
    pub(crate) asts: HashMap<PathBuf, Rc<RefCell<IDEModule>>>,
}
impl MultiProject {
    pub fn new() -> MultiProject {
        let dir = std::env::current_dir().unwrap();
        let mut m = MultiProject::default();
        static MAX: usize = 10;

        for x in walkdir::WalkDir::new(dir) {
            let x = match x {
                Ok(x) => x,
                Err(_) => {
                    continue;
                }
            };
            if x.file_type().is_file() && x.file_name().to_string_lossy().ends_with("Move.toml") {
                if let Some(usage) = memory_stats::memory_stats() {
                    if usage.physical_mem >= 500 * 1024 * 1024 || m.projects.len() >= MAX {
                        log::error!(
                            "move-analyzer used to much memory or exceed max number,project {:?} not loaded. ",
                            x.path()
                        );
                        continue;
                    }
                } else {
                    eprintln!("Couldn't get the current memory usage :(");
                };
                let mut mani = x.clone().into_path();
                mani.pop();
                eprintln!("load manifest:{:?}", mani.as_path());
                let modules = match Modules::new(&mani, &mut m) {
                    Ok(x) => x,
                    Err(err) => {
                        log::error!(
                            "load manifest {:?} failed,err:{:?}",
                            mani.clone().as_path(),
                            err
                        );
                        continue;
                    }
                };
                m.projects.insert(
                    {
                        let mut v = HashSet::default();
                        for x in modules.manifest_paths.iter() {
                            v.insert(x.clone());
                        }
                        v
                    },
                    modules,
                );
            };
        }
        eprintln!("finish loading all projects");
        m
    }

    pub fn get_modules(&self, x: &PathBuf) -> Option<&Modules> {
        let (manifest, _) = super::utils::discover_manifest_and_kind(x.as_path())?;
        for (k, v) in self.projects.iter() {
            if k.contains(&manifest) {
                return Some(v);
            }
        }
        None
    }

    pub fn get_modules_mut(&mut self, x: &PathBuf) -> Vec<&mut Modules> {
        let (manifest, _) = match super::utils::discover_manifest_and_kind(x.as_path()) {
            Some(x) => x,
            None => return vec![],
        };
        let mut ret = Vec::new();
        for (k, v) in self.projects.iter_mut() {
            if k.contains(&manifest) {
                ret.push(v);
            }
        }
        ret
    }
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn update_defs(
        &mut self,
        file_path: PathBuf,
        defs: Vec<Definition>,
    ) -> Option<Vec<Definition>> {
        let (manifest, layout) = match super::utils::discover_manifest_and_kind(file_path.as_path())
        {
            Some(x) => x,
            None => unreachable!(),
        };
        let mut b = self.asts.get_mut(&manifest).unwrap().borrow_mut();
        if layout == SourcePackageLayout::Sources {
            b.sources.insert(file_path, defs).clone()
        } else if layout == SourcePackageLayout::Tests {
            b.tests.insert(file_path, defs).clone()
        } else if layout == SourcePackageLayout::Scripts {
            b.scripts.insert(file_path, defs).clone()
        } else {
            unreachable!()
        }
    }
}
