// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use move_ir_types::location::Loc;

use super::lexer::{is_comment_spaces, Token};

/// TokenRange is used to mark the start and endindex of an cst struct.
/// We could reverse the cst back the text without losing information.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct TokenRange {
    pub start: usize,
    pub end: usize,
}

impl TokenRange {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn full_loc(&self, tokens: &[Token]) -> Loc {
        make_loc(tokens[self.start].loc, tokens[self.end].loc)
    }

    pub fn loc(&self, tokens: &[Token]) -> Loc {
        let start = tokens[self.start..self.end]
            .iter()
            .position(|t| !is_comment_spaces(t.kind))
            .unwrap();
        let end = tokens[self.start..self.end]
            .iter()
            .rev()
            .position(|t| !is_comment_spaces(t.kind))
            .unwrap();
        make_loc(
            tokens[self.start + start].loc,
            tokens[self.end - end - 1].loc,
        )
    }
}

fn make_loc(start: Loc, end: Loc) -> Loc {
    Loc::new(start.file_hash(), start.start(), end.end())
}
