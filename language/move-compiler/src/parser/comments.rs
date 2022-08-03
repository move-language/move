// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    diag,
    diagnostics::{codes::Severity, Diagnostics},
    shared::CompilationEnv,
};
use move_command_line_common::{character_sets::is_permitted_chars, files::FileHash};
use move_ir_types::location::*;
use move_symbol_pool::Symbol;
use std::{collections::BTreeMap, mem};

use super::{
    cst::{self, TokensSpanned},
    lexer::Tok,
};

/// Types to represent comments.
pub type CommentMap = BTreeMap<FileHash, MatchedFileCommentMap>;
pub type MatchedFileCommentMap = BTreeMap<u32, String>;
pub type FileCommentMap = BTreeMap<(u32, u32), String>;

// We restrict strings to only ascii visual characters (0x20 <= c <= 0x7E) or a permitted newline
// character--\r--,--\n--or a tab--\t.
pub fn verify_string(file_hash: FileHash, string: &str) -> Result<(), Diagnostics> {
    match string
        .chars()
        .enumerate()
        .find(|(idx, _)| !is_permitted_chars(string.as_bytes(), *idx))
    {
        None => Ok(()),
        Some((idx, chr)) => {
            let loc = Loc::new(file_hash, idx as u32, idx as u32);
            let msg = format!(
                "Invalid character '{}' found when reading file. Only ASCII printable characters, \
                 tabs (\\t), lf (\\n) and crlf (\\r\\n) are permitted.",
                chr
            );
            Err(Diagnostics::from(vec![diag!(
                Syntax::InvalidCharacter,
                (loc, msg)
            )]))
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum CommentKind {
    DocComment,      //  /// comment
    SignleLine,      //  // comment
    BlockComment,    // /* comment */
    DocBlockComment, // /** comment */
}

#[derive(Debug, Clone, Copy)]
pub struct Comment {
    pub kind: CommentKind,
    pub content: Symbol,
    pub loc: Loc,
}

impl Comment {
    pub fn new(kind: CommentKind, content: Symbol, loc: Loc) -> Self {
        Comment { kind, content, loc }
    }
}

struct Context {
    matched_doc_comments: MatchedFileCommentMap,
    comments: FileCommentMap,
}

impl Context {
    pub fn insert_comment(&mut self, start: u32, end: u32, comment: String) {
        self.comments.insert((start, end), comment);
    }

    pub fn new() -> Self {
        Self {
            matched_doc_comments: Default::default(),
            comments: Default::default(),
        }
    }

    pub fn take_matched_map(&mut self) -> MatchedFileCommentMap {
        mem::take(&mut self.matched_doc_comments)
    }
}

// Find all the doc comments from the cst.
pub fn find_comments<'env>(
    program: &cst::Program,
    env: &'env mut CompilationEnv,
    source_comments: &mut CommentMap,
) -> Result<(), Diagnostics> {
    for def in program.source_definitions.iter() {
        let mut context = Context::new();
        find_all_doc_comments(&mut context, def);
        find_matched_comments(&mut context, def, &def.source_trees);

        let matched_comments = context.take_matched_map();
        warn_no_matched_doc_comments(&context, env, def.file_hash);
        source_comments.insert(def.file_hash, matched_comments);
    }
    env.check_diags_at_or_above_severity(Severity::Bug)?;
    Ok(())
}

fn warn_no_matched_doc_comments(
    context: &Context,
    env: &'_ mut CompilationEnv,
    file_hash: FileHash,
) {
    context.comments.iter().for_each(|((start, end), _)| {
        env.add_diag(diag!(
            Syntax::InvalidDocComment,
            (
                Loc::new(file_hash, *start, *end),
                "Documentation comment cannot be matched to a language item",
            )
        ))
    });
}

fn find_all_doc_comments(context: &mut Context, ast: &cst::PackageDefinition) {
    for token in ast.source_tokens.iter() {
        if let Tok::Comment(CommentKind::DocBlockComment | CommentKind::DocComment) = token.kind {
            context.insert_comment(
                token.loc.start(),
                token.loc.end(),
                token.content.to_string(),
            )
        }
    }
}

fn find_matched_comments(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    trees: &[cst::ParseTree],
) {
    for tree in trees.iter() {
        match tree {
            cst::ParseTree::Module(module) => {
                match_doc_comments(context, ast, module);
                find_matched_comments(context, ast, module.value().body.value())
            }
            cst::ParseTree::Script(script) => {
                find_matched_comments(context, ast, script.value().members.value())
            }
            cst::ParseTree::Address(address) => {
                find_matched_comments(context, ast, address.value().modules.value())
            }
            cst::ParseTree::Function(function) => {
                match_doc_comments(context, ast, function);
                if let Some(b) = function.value().body.as_ref() {
                    find_matched_comments(context, ast, b.value())
                }
            }
            cst::ParseTree::Struct(struct_) => {
                match_doc_comments(context, ast, struct_);
                if let Some(f) = struct_.value().fields.as_ref() {
                    f.members
                        .iter()
                        .for_each(|(name, _)| match_doc_comments(context, ast, name))
                }
            }
            cst::ParseTree::Attribute(attr) => match_doc_comments(context, ast, attr),
            cst::ParseTree::UseDecl(_) => (),
            cst::ParseTree::FriendDecl(_) => (),
            cst::ParseTree::Declare(_) => (),
            cst::ParseTree::LetAssign(_) => (),
            cst::ParseTree::Constant(constant) => match_doc_comments(context, ast, constant),
            cst::ParseTree::Exp(_, _) => (),
            cst::ParseTree::Spec(spec, _) => {
                match_doc_comments(context, ast, spec);
                find_matched_comments(context, ast, spec.value().members.value())
            }
            cst::ParseTree::SpecMember(member) => match_doc_comments(context, ast, member),
        }
    }
}

fn match_doc_comments<T>(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    item: &TokensSpanned<T>,
) {
    let leading_tokens_opt = item.leading_tokens(&ast.source_tokens);
    let loc = item.full_loc(&ast.source_tokens);

    let mut newline_count = 0;
    leading_tokens_opt.map_or((), |leading_tokens| {
        let mut matched_doc_start_index = loc.start();
        let matched_doc_end_index = leading_tokens.last().map_or(loc.start(), |t| t.loc.end());
        if matched_doc_end_index == matched_doc_start_index {
            return;
        }
        leading_tokens.iter().for_each(|t| match t.kind {
            Tok::Comment(CommentKind::DocComment | CommentKind::DocBlockComment) => {
                if newline_count >= 2 {
                    matched_doc_start_index = t.loc.start()
                }
                newline_count = 0
            }
            Tok::LF => newline_count += 1,
            _ => (),
        });
        let mut matched = vec![];

        let merged = context
            .comments
            .range(
                (matched_doc_start_index, matched_doc_start_index)
                    ..(matched_doc_end_index, matched_doc_end_index),
            )
            .map(|(span, s)| {
                matched.push(*span);
                s.clone()
            })
            .collect::<Vec<String>>()
            .join("\n");

        for span in matched {
            context.comments.remove(&span);
        }
        context
            .matched_doc_comments
            .insert(matched_doc_end_index, merged);
    })
}
