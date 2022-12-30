// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0
#![allow(dead_code)]
use codespan_reporting::files::{Files, SimpleFiles};
use lsp_types::Position;
use move_command_line_common::files::FileHash;

use move_core_types::effects::Op;
use move_ir_types::location::*;
use move_package::source_package::layout::SourcePackageLayout;
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
    m: HashMap<
        PathBuf,                     /* filepath */
        Vec<(ByteIndex, ByteIndex)>, /*  all position that have \n */
    >,
}

impl FileLineMapping {
    pub(crate) fn update(&mut self, filepath: PathBuf, content: &str) {
        let mut v = vec![];
        let mut start = 0;
        for (index, s) in content.as_bytes().iter().enumerate() {
            // TODO how to support windows \r\n
            if *s == 10 {
                // \n
                v.push((start, index as u32));
                start = (index + 1) as u32;
            }
        }
        if let Some(last) = content.as_bytes().last() {
            if *last != 10 {
                v.push((start, (content.as_bytes().len() - 1) as u32))
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
            let mut p = None;
            let mut line = 0;
            for (index, (start, end)) in v.iter().enumerate() {
                if start_index >= *start && start_index <= *end {
                    p = Some((*start, *end));
                    line = index;
                    break;
                }
            }
            let p = p?;
            Some(FileRange {
                path: filepath.clone(),
                line: line as u32,
                col_start: start_index - p.0,
                col_end: end_index - p.0,
            })
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn file_mapping() {
        let filepath = PathBuf::from("test");

        let mut f = FileLineMapping::default();
        f.update(
            filepath.clone(),
            r#"123456
123456
abc        "#,
        );

        let r = f.translate(&filepath, 0, 2).unwrap();
        assert_eq!(
            r,
            FileRange {
                path: filepath.clone(),
                line: 0,
                col_start: 0,
                col_end: 2
            }
        );

        let r = f.translate(&filepath, 9, 10).unwrap();
        assert_eq!(
            r,
            FileRange {
                path: filepath.clone(),
                line: 1,
                col_start: 2,
                col_end: 3
            }
        );
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FileRange {
    pub(crate) path: PathBuf,
    pub(crate) line: u32,
    pub(crate) col_start: u32,
    pub(crate) col_end: u32,
}

impl std::fmt::Display for FileRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?}:{}:({},{})",
            self.path.as_path(),
            self.line,
            self.col_start,
            self.col_end
        )
    }
}
impl FileRange {
    pub(crate) fn in_range(&self, path: PathBuf, line: u32, col: u32) -> bool {
        if self.path != path {
            return false;
        }
        self.line == line && (col >= self.col_start && col <= self.col_end)
    }
    pub(crate) fn unknown() -> Self {
        Self {
            path: PathBuf::from("<unknown>"),
            line: 0,
            col_start: 0,
            col_end: 0,
        }
    }
}

/// Path concat from
pub fn path_concat(p1: &Path, p2: &Path) -> PathBuf {
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
pub fn path_concat_move_toml(p1: &Path, p2: &Path) -> PathBuf {
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

pub fn normal_path_components<'a>(x: &Vec<Component<'a>>) -> PathBuf {
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

pub trait GetPosition {
    fn get_position(&self) -> (PathBuf, u32 /* line */, u32 /* col */);
}

pub(crate) fn in_range(x: &dyn GetPosition, start: &FileRange, end: &FileRange) -> bool {
    let (filepath, line, col) = x.get_position();
    if filepath != start.path.clone() {
        return false;
    }
    if line < start.line {
        return false;
    }
    if line == start.line && col < start.col_start {
        return false;
    }
    if line > end.line {
        return false;
    }
    if line == end.line && col > end.col_end {
        return false;
    }
    true
}

pub(crate) fn discover_manifest_and_kind(x: &Path) -> Option<(PathBuf, SourcePackageLayout)> {
    let mut x: Vec<_> = x.components().collect();
    // We should be able at least pop one.
    x.pop()?;
    let mut layout = None;

    while x.len() > 0 {
        layout = x
            .last()
            .map(|x| match x.as_os_str().to_str().unwrap() {
                "tests" => Some(SourcePackageLayout::Tests),
                "sources" => Some(SourcePackageLayout::Sources),
                "scripts" => Some(SourcePackageLayout::Scripts),
                _ => return None,
            })
            .flatten();
        if layout.is_some() {
            break;
        }
        x.pop();
    }
    let layout = layout?;
    // Pop tests or sources ...
    x.pop()?;
    let mut manifest_dir = PathBuf::new();
    for x in x.iter() {
        manifest_dir.push(x);
    }
    let mut manifest_file = manifest_dir.clone();
    manifest_file.push("Move.toml");
    if manifest_file.exists() {
        Some((manifest_dir, layout))
    } else {
        None
    }
}

#[test]
fn discover_manifest_and_kind_test() {
    let (manifest_dir, kind) = discover_manifest_and_kind(
        PathBuf::from("/Users/temp/projects/test-move2/scripts/aaa.move").as_path(),
    )
    .unwrap();
    eprintln!("path:{:?} kind:{:?}", manifest_dir, kind);
    let (manifest_dir, kind) = discover_manifest_and_kind(
        PathBuf::from("/Users/temp/projects/test-move2/sources/some.move").as_path(),
    )
    .unwrap();
    eprintln!("path:{:?} kind:{:?}", manifest_dir, kind);
    let (manifest_dir, kind) = discover_manifest_and_kind(
        PathBuf::from("/Users/temp/projects/test-move2/sources/configs/some.move").as_path(),
    )
    .unwrap();
    eprintln!("path:{:?} kind:{:?}", manifest_dir, kind);
}
