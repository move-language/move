// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use super::utils::*;
use crate::{project::*, references::ReferencesCache, symbols::Symbols, vfs::VirtualFileSystem};
use im::HashSet;
use lsp_server::Connection;
use lsp_types::{notification::Notification, MessageType};
use move_command_line_common::files::FileHash;
use move_compiler::parser::ast::Definition;
use move_ir_types::location::Loc;
use move_package::source_package::layout::SourcePackageLayout;
use std::{
    cell::RefCell,
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
    sync::{Arc, Mutex},
};

/// The context within which the language server is running.
pub struct Context {
    /// The connection with the language server's client.
    pub connection: Connection,
    /// The files that the language server is providing information about.
    pub files: VirtualFileSystem,
    /// Symbolication information
    pub symbols: Arc<Mutex<Symbols>>,
    pub projects: MultiProject,
    pub ref_caches: ReferencesCache,
    pub diag_version: FileDiags,
}

impl_convert_loc!(MultiProject);

#[derive(Default)]
pub struct MultiProject {
    pub projects: HashMap<HashSet<PathBuf>, Project>,
    pub hash_file: Rc<RefCell<PathBufHashMap>>,
    pub file_line_mapping: Rc<RefCell<FileLineMapping>>,
    pub asts: HashMap<PathBuf, Rc<RefCell<SourceDefs>>>,
}

impl MultiProject {
    pub fn insert_project(&mut self, p: Project) {
        self.projects.insert(p.mk_multi_project_key(), p);
    }

    pub fn load_project(
        &mut self,
        sender: &lsp_server::Connection,
        mani: &PathBuf,
    ) -> anyhow::Result<Project> {
        if LOAD_DEPS {
            use std::{
                process::{Command, Stdio},
                time::Duration,
            };
            use wait_timeout::ChildExt;
            let mut c = Command::new("sui");
            c.current_dir(mani.as_path());
            c.args([
                "move",
                "build",
                "--fetch-deps-only",
                "--skip-fetch-latest-git-deps",
            ]);
            c.stdin(Stdio::null())
                .stdout(Stdio::null())
                .stderr(Stdio::null());

            let mut child = c.spawn().unwrap();
            let mut fetch_ok = false;
            match child.wait_timeout(Duration::new(30, 0)) {
                Ok(_) => {
                    fetch_ok = true;
                }
                Err(err) => {
                    log::error!("exec cmd fetch deps failed,err:{:?}", err);
                }
            }
            let _ = child.kill();
            if !fetch_ok {
                log::error!("fetch deps failed");
                send_show_message(
                    sender,
                    lsp_types::MessageType::ERROR,
                    format!("project at {:?} can't fetch deps.\nMaybe you need execute 'sui move build --fetch-deps-only --skip-fetch-latest-git-deps' yourself.", mani.as_path()),
                );
                return anyhow::Result::Err(anyhow::anyhow!("fetch deps failed"));
            }
        }
        Project::new(mani, self, |msg: String| {
            send_show_message(sender, MessageType::ERROR, msg)
        })
    }

    pub fn new() -> MultiProject {
        MultiProject::default()
    }

    pub fn get_project(&self, x: &Path) -> Option<&Project> {
        let (manifest, _) = super::utils::discover_manifest_and_kind(x)?;
        for (k, v) in self.projects.iter() {
            if k.contains(&manifest) {
                return Some(v);
            }
        }
        None
    }

    fn get_projects_mut(&mut self, x: &Path) -> Vec<&mut Project> {
        let (manifest, _) = match super::utils::discover_manifest_and_kind(x) {
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

    pub fn update_defs(&mut self, file_path: PathBuf, defs: Vec<Definition>) {
        let (manifest, layout) = match super::utils::discover_manifest_and_kind(file_path.as_path())
        {
            Some(x) => x,
            None => {
                log::error!("file_path {:?} not found", file_path.as_path());
                return;
            }
        };
        let mut b = self.asts.get_mut(&manifest).unwrap().borrow_mut();
        let old_defs = if layout == SourcePackageLayout::Sources {
            b.sources.insert(file_path.clone(), defs)
        } else if layout == SourcePackageLayout::Tests {
            b.tests.insert(file_path.clone(), defs)
        } else if layout == SourcePackageLayout::Scripts {
            b.scripts.insert(file_path.clone(), defs)
        } else {
            unreachable!()
        };
        drop(b);
        self.get_projects_mut(&file_path)
            .into_iter()
            .for_each(|x| x.update_defs(&file_path, old_defs.as_ref()));
    }
}

pub(crate) fn send_show_message(
    sender: &lsp_server::Connection,
    typ: lsp_types::MessageType,
    msg: String,
) {
    use std::time::Duration;
    sender
        .sender
        .send_timeout(
            lsp_server::Message::Notification(lsp_server::Notification {
                method: lsp_types::notification::ShowMessage::METHOD.into(),
                params: serde_json::to_value(lsp_types::LogMessageParams { typ, message: msg })
                    .unwrap(),
            }),
            Duration::new(5, 0),
        )
        .unwrap();
}

#[derive(Default)]
pub struct FileDiags {
    diags: HashMap<PathBuf, HashMap<url::Url, usize>>,
}

impl FileDiags {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn update(&mut self, mani: &PathBuf, fpath: &url::Url, diags: usize) {
        if let Some(x) = self.diags.get_mut(mani) {
            x.insert(fpath.clone(), diags);
        } else {
            let mut x: HashMap<url::Url, usize> = HashMap::new();
            x.insert(fpath.clone(), diags);
            self.diags.insert(mani.clone(), x);
        }
    }

    pub fn with_manifest(&self, mani: &PathBuf, mut call: impl FnMut(&HashMap<url::Url, usize>)) {
        let empty = Default::default();
        call(self.diags.get(mani).unwrap_or(&empty));
    }
}

///
static LOAD_DEPS: bool = false;

impl MultiProject {
    pub fn try_reload_projects(&mut self, connection: &Connection) {
        let mut all = Vec::new();
        let not_founds = {
            let mut x = Vec::new();
            for (k, v) in self.projects.iter() {
                if !v.manifest_not_exists.is_empty() {
                    x.push((
                        k.clone(),
                        v.manifest_not_exists.clone(),
                        v.manifest_paths.first().cloned().unwrap(),
                    ));
                }
            }
            x
        };
        let mut modifies = Vec::new();
        for (k, p) in self.projects.iter() {
            if p.manifest_beed_modified() {
                let root = p.manifest_paths.first().cloned().unwrap();
                if !not_founds.iter().any(|x| x.2 == root) {
                    modifies.push((k.clone(), root));
                }
            }
        }
        for (k, not_founds, root_manifest) in not_founds.into_iter() {
            let mut exists_now = false;
            for v in not_founds.iter() {
                let mut v = v.clone();
                v.push(PROJECT_FILE_NAME);
                if v.exists() {
                    exists_now = true;
                    break;
                }
            }
            if !exists_now {
                continue;
            }
            eprintln!("reload  {:?}", root_manifest.as_path());
            let x = match Project::new(root_manifest, self, |msg| {
                send_show_message(connection, MessageType::ERROR, msg)
            }) {
                Ok(x) => x,
                Err(_) => {
                    log::error!("reload project failed");
                    return;
                }
            };
            all.push((k, x));
        }
        for (k, root_manifest) in modifies.into_iter() {
            send_show_message(
                connection,
                MessageType::INFO,
                format!("trying reload {:?}.", root_manifest.as_path()),
            );
            let x = match Project::new(root_manifest, self, |msg| {
                send_show_message(connection, MessageType::ERROR, msg);
            }) {
                Ok(x) => x,
                Err(err) => {
                    send_show_message(
                        connection,
                        MessageType::ERROR,
                        format!("reload project failed,err:{:?}", err),
                    );
                    continue;
                }
            };
            all.push((k, x));
        }
        for (k, v) in all.into_iter() {
            debug_assert!(self.projects.remove(&k).is_some());
            self.insert_project(v);
        }
    }
}
