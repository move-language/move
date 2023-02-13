// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use super::utils::*;
use crate::modules::Project;
use crate::modules::*;
use crate::references::ReferencesCache;
use im::HashSet;
use lsp_server::Connection;
use lsp_types::notification::Notification;
use move_command_line_common::files::FileHash;
use move_compiler::parser::ast::Definition;
use move_ir_types::location::Loc;
use move_package::source_package::layout::SourcePackageLayout;
use std::cell::RefCell;
use std::collections::HashMap;

use std::path::PathBuf;
use std::rc::Rc;

/// The context within which the language server is running.
pub struct Context {
    pub projects: MultiProject,
    /// The connection with the language server's client.
    pub connection: Connection,
    pub ref_caches: ReferencesCache,
    pub diag_version: DiagVersions,
}

impl ConvertLoc for MultiProject {
    fn convert_file_hash_filepath(&self, hash: &FileHash) -> Option<PathBuf> {
        self.hash_file
            .as_ref()
            .borrow()
            .get_path(hash)
            .map(|x| x.clone())
    }
    fn convert_loc_range(&self, loc: &Loc) -> Option<FileRange> {
        self.convert_file_hash_filepath(&loc.file_hash())
            .map(|file| {
                self.file_line_mapping
                    .as_ref()
                    .borrow()
                    .translate(&file, loc.start(), loc.end())
            })
            .flatten()
    }
}
#[derive(Default)]
pub struct MultiProject {
    projects: HashMap<HashSet<PathBuf>, Project>,
    pub hash_file: Rc<RefCell<PathBufHashMap>>,
    pub file_line_mapping: Rc<RefCell<FileLineMapping>>,
    pub(crate) asts: HashMap<PathBuf, Rc<RefCell<SourceDefs>>>,
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
        use std::process::Command;
        use std::process::Stdio;
        use std::time::Duration;
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
        if fetch_ok == false {
            log::error!("fetch deps failed");
            send_show_message(
                sender,
                lsp_types::MessageType::Error,
                format!("project at {:?} can't fetch deps.\nMaybe you need execute 'sui move build --fetch-deps-only --skip-fetch-latest-git-deps' yourself.", mani.as_path()),
            );
            return anyhow::Result::Err(anyhow::anyhow!("fetch deps failed"));
        }
        let modules = Project::new(&mani, self);
        modules
    }
    pub fn new() -> MultiProject {
        let m = MultiProject::default();
        m
    }
    pub fn get_project(&self, x: &PathBuf) -> Option<&Project> {
        let (manifest, _) = super::utils::discover_manifest_and_kind(x.as_path())?;
        for (k, v) in self.projects.iter() {
            if k.contains(&manifest) {
                return Some(v);
            }
        }
        None
    }

    pub fn get_projects_mut(&mut self, x: &PathBuf) -> Vec<&mut Project> {
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

    pub fn update_defs(
        &mut self,
        file_path: PathBuf,
        defs: Vec<Definition>,
    ) -> Option<Vec<Definition>> {
        let (manifest, layout) = match super::utils::discover_manifest_and_kind(file_path.as_path())
        {
            Some(x) => x,
            None => {
                log::error!("file_path {:?} not found", file_path.as_path());
                return None;
            }
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
pub struct DiagVersions {
    versions: HashMap<PathBuf, HashMap<PathBuf, i32>>,
}
impl DiagVersions {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn update(&mut self, mani: &PathBuf, fpath: &PathBuf) {
        if let Some(x) = self.versions.get_mut(mani) {
            x.insert(fpath.clone(), Default::default());
        } else {
            let mut x: HashMap<PathBuf, i32> = HashMap::new();
            x.insert(fpath.clone(), Default::default());
            self.versions.insert(mani.clone(), x);
        }
    }
    pub fn get(&mut self, mani: &PathBuf, fpath: &PathBuf) -> Option<i32> {
        if let Some(x) = self.versions.get(mani) {
            x.get(fpath).map(|x| *x)
        } else {
            None
        }
    }
    pub fn with_mani(&self, mani: &PathBuf, mut call: impl FnMut(&HashMap<PathBuf, i32>)) {
        let empty = Default::default();
        call(self.versions.get(mani).unwrap_or(&empty));
    }
}
