// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0
#![allow(dead_code)]
use codespan_reporting::files::{Files, SimpleFiles};

use lsp_types::Position;
use move_command_line_common::files::FileHash;
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
    m: HashMap<PathBuf /* filepath */, Vec<ByteIndex>>,
}

impl FileLineMapping {
    pub(crate) fn update(&mut self, filepath: PathBuf, content: &str) {
        let mut v = vec![0];
        for (index, s) in content.as_bytes().iter().enumerate() {
            // TODO how to support windows \r\n
            if *s == 10 {
                // \n
                v.push((index + 1) as ByteIndex);
            }
        }
        if let Some(last) = content.as_bytes().last() {
            if *last != 10 {
                v.push((content.as_bytes().len()) as ByteIndex);
            }
        }
        self.m.insert(filepath, v);
    }

    pub(crate) fn translate(
        &self,
        filepath: &PathBuf,
        start_index: ByteIndex,
        mut end_index: ByteIndex,
    ) -> Option<FileRange> {
        if end_index < start_index {
            // maybe something goes wrong with syntax.rs
            // sometimes end_index < start_index.
            end_index = start_index;
        }
        let vec = self.m.get(filepath)?;
        let too_big = vec.last().map(|x| *x <= end_index).unwrap_or(false);
        if too_big {
            return None;
        }
        fn search(vec: &[ByteIndex], byte_index: ByteIndex, line_increment: u32) -> (u32, u32) {
            let mut index = bisection::bisect_left(vec, &byte_index);
            if vec[index] != byte_index {
                index = index - 1;
            }
            (
                (index as u32) + line_increment,
                byte_index - vec[index as usize],
            )
        }
        let (line_start, col_start) = search(&vec[..], start_index, 0);
        let (line_end, col_end) = search(
            &vec[(line_start as usize)..vec.len()],
            end_index,
            line_start,
        );
        Some(FileRange {
            path: filepath.clone(),
            line_start,
            col_start,
            line_end,
            col_end,
        })
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
                line_start: 0,
                line_end: 0,
                col_start: 0,
                col_end: 2
            }
        );

        let r = f.translate(&filepath, 9, 10).unwrap();
        assert_eq!(
            r,
            FileRange {
                path: filepath.clone(),
                line_start: 1,
                line_end: 1,
                col_start: 2,
                col_end: 3
            }
        );
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FileRange {
    pub(crate) path: PathBuf,
    /// Start.
    pub(crate) line_start: u32,
    pub(crate) col_start: u32,

    /// End.
    pub(crate) line_end: u32,
    pub(crate) col_end: u32,
}

impl FileRange {
    pub(crate) fn mk_location(&self) -> lsp_types::Location {
        let range = lsp_types::Range {
            start: lsp_types::Position {
                line: self.line_start,
                character: self.col_start,
            },
            end: Position {
                line: self.line_end,
                character: self.col_end,
            },
        };
        let uri = url::Url::from_file_path(self.path.as_path()).unwrap();
        lsp_types::Location::new(uri, range)
    }
}

impl std::fmt::Display for FileRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?}:({},{}):({},{})",
            self.path.as_path(),
            self.line_start,
            self.col_start,
            self.line_end,
            self.col_end
        )
    }
}
impl FileRange {
    pub(crate) fn in_range(&self, path: PathBuf, line: u32, col: u32) -> bool {
        if self.path != path {
            return false;
        }
        self.line_start == line && (col >= self.col_start && col <= self.col_end)
    }
    pub(crate) fn unknown() -> Self {
        Self {
            path: PathBuf::from("<unknown>"),
            line_start: 0,
            col_start: 0,
            col_end: 0,
            line_end: 0,
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
    fn in_range(x: &impl GetPosition, range: &FileRange) -> bool {
        let (filepath, line, col) = x.get_position();
        if filepath != range.path.clone() {
            return false;
        }
        if line < range.line_start {
            return false;
        }
        if line == range.line_start && col < range.col_start {
            return false;
        }
        if line > range.line_end {
            return false;
        }
        if line == range.line_end && col > range.col_end {
            return false;
        }
        true
    }
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
