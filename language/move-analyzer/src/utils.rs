// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use codespan_reporting::files::{Files, SimpleFiles};
use lsp_types::Position;
use move_command_line_common::files::FileHash;
use move_compiler::parser::ast::NameAccessChain;
use move_ir_types::location::*;
use move_symbol_pool::Symbol;
use std::collections::HashMap;
use std::{path::*, vec};

/// Converts a location from the byte index format to the line/character (Position) format, where
/// line/character are 0-based.
pub fn get_loc(
    fhash: &FileHash,
    pos: ByteIndex,
    files: &SimpleFiles<Symbol, String>,
    file_id_mapping: &HashMap<FileHash, usize>,
) -> Option<Position> {
    let id = match file_id_mapping.get(fhash) {
        Some(v) => v,
        None => return None,
    };
    match files.location(*id, pos as usize) {
        Ok(v) => Some(Position {
            // we need 0-based column location
            line: v.line_number as u32 - 1,
            character: v.column_number as u32 - 1,
        }),
        Err(_) => None,
    }
}

/// Double way mapping from FileHash and FilePath.
#[derive(Debug, Default)]
pub(crate) struct PathBufHashMap {
    path_2_hash: HashMap<PathBuf, FileHash>,
    hash_2_path: HashMap<FileHash, PathBuf>,
}

impl PathBufHashMap {
    pub(crate) fn update(&mut self, path: PathBuf, hash: FileHash) {
        if let Some(hash) = self.path_2_hash.get(&path) {
            self.hash_2_path.remove(&hash);
        }
        self.path_2_hash.insert(path.clone(), hash.clone());
        self.hash_2_path.insert(hash, path);
    }

    pub(crate) fn get_hash(&self, path: &PathBuf) -> Option<&'_ FileHash> {
        self.path_2_hash.get(path)
    }
    pub(crate) fn get_path(&self, hash: &FileHash) -> Option<&'_ PathBuf> {
        self.hash_2_path.get(hash)
    }
}

#[derive(Debug, Default)]
pub(crate) struct FileLineMapping {
    m: HashMap<PathBuf /* filepath */, Vec<ByteIndex> /*  all position that have \n */>,
}

impl FileLineMapping {
    pub(crate) fn update(&mut self, filepath: PathBuf, content: &str) {
        let mut v = vec![0];
        for (index, s) in content.as_bytes().iter().enumerate() {
            // TODO how to support windows \r\n
            if *s == 10 {
                // \n
                v.push(index as u32);
            }
        }
        self.m.insert(filepath, v);
    }

    pub(crate) fn translate(
        &self,
        filepath: &PathBuf,
        start_index: ByteIndex,
        end_index: ByteIndex,
    ) -> Option<FileRange> {
        if let Some(v) = self.m.get(filepath) {
            // Using a binary search to search line.
            let mut start = 0;
            let mut end = v.len() - 1;
            while start < end {
                let index = (start + end) / 2;
                if v[index] > (start_index as u32) {
                    end = index;
                } else {
                    start = index;
                }
            }
            let col = v[start];
            Some(FileRange {
                path: filepath.clone(),
                line: start as u32,
                col_start: start_index - col,
                col_end: start_index - col,
            })
        } else {
            None
        }
    }
}

pub struct FileRange {
    path: PathBuf,
    line: u32,
    col_start: u32,
    col_end: u32,
}

impl FileRange {
    pub(crate) fn in_range(&self, path: PathBuf, line: u32, col: u32) -> bool {
        if self.path != path {
            return false;
        }
        self.line == line && (col >= self.col_start && col <= self.col_end)
    }
}

/// Path concat from
pub(crate) fn path_concat(p1: &Path, p2: &Path) -> PathBuf {
    let p2: Vec<_> = p2.components().collect();
    let is_abs = match p2.get(0).unwrap() {
        Component::RootDir | Component::Prefix(_) => true,
        _ => false,
    };
    let mut p1: Vec<_> = p1.components().collect();
    normal_path_components(if is_abs {
        &p2
    } else {
        {
            p1.extend(p2);
            &p1
        }
    })
}

/// concat Move.toml file.
pub(crate) fn path_concat_move_toml(p1: &Path, p2: &Path) -> PathBuf {
    let p1_is_move_toml = match p1.to_str() {
        Some(x) => x.ends_with("Move.toml"),
        None => false,
    };

    if p1_is_move_toml {
        let mut p1 = p1.to_path_buf();
        p1.pop();
        path_concat(p1.as_path(), p2)
    } else {
        path_concat(p1, p2)
    }
}

pub(crate) fn normal_path_components<'a>(x: &Vec<Component<'a>>) -> PathBuf {
    let mut ret = PathBuf::new();
    for v in x {
        match v {
            Component::Prefix(x) => ret.push(x.as_os_str()),
            Component::RootDir => ret.push("/"),
            Component::CurDir => {}
            Component::ParentDir => {
                let _ = ret.pop();
            }
            Component::Normal(x) => ret.push(*x),
        }
    }
    if ret.to_str().unwrap() == "" {
        ret.push(".")
    }
    ret
}

pub(crate) fn normal_path(p: &Path) -> PathBuf {
    let x: Vec<_> = p.components().collect();
    normal_path_components(&x)
}

// #[allow(dead_code)]
// fn read_all_move_toml_files(path: &PathBuf) -> Vec<PathBuf> {
//     let mut ret = vec![];
//     for item in WalkDir::new(path) {
//         let item = item.unwrap();
//         if item.file_type().is_file()
//             && item
//                 .path()
//                 .to_str()
//                 .unwrap()
//                 .ends_with(SourcePackageLayout::Manifest.location_str())
//         {
//             ret.push(PathBuf::from(item.path()));
//         }
//     }
//     ret
// }
