// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{context::Context, symbols::Symbols};
use im::HashMap;
use lsp_server::Request;
use lsp_types::lsif::Item;
use lsp_types::{CompletionItem, CompletionItemKind, CompletionParams, Position};
use move_command_line_common::files::FileHash;
use move_compiler::cfgir::ast::{ModuleDefinition, Script};
use move_compiler::parser::ast::ModuleName;
use move_compiler::parser::ast::{Definition, ModuleIdent};
use move_compiler::parser::*;
use move_compiler::typing::ast::Function;
use move_compiler::{
    cfgir::ast::Program,
    parser::{
        keywords::{BUILTINS, CONTEXTUAL_KEYWORDS, KEYWORDS, PRIMITIVE_TYPES},
        lexer::{Lexer, Tok},
    },
    CommentMap,
};
use move_ir_types::location::Loc;
use move_symbol_pool::Symbol;
use serde::__private::de;
use std::path::Path;
use std::vec;
use std::{collections::HashSet, path::PathBuf, rc::Rc};

/// Constructs an `lsp_types::CompletionItem` with the given `label` and `kind`.
fn completion_item(label: &str, kind: CompletionItemKind) -> CompletionItem {
    CompletionItem {
        label: label.to_owned(),
        kind: Some(kind),
        ..Default::default()
    }
}

/// Return a list of completion items corresponding to each one of Move's keywords.
///
/// Currently, this does not filter keywords out based on whether they are valid at the completion
/// request's cursor position, but in the future it ought to. For example, this function returns
/// all specification language keywords, but in the future it should be modified to only do so
/// within a spec block.
fn keywords() -> Vec<CompletionItem> {
    KEYWORDS
        .iter()
        .chain(CONTEXTUAL_KEYWORDS.iter())
        .chain(PRIMITIVE_TYPES.iter())
        .map(|label| {
            let kind = if label == &"copy" || label == &"move" {
                CompletionItemKind::Operator
            } else {
                CompletionItemKind::Keyword
            };
            completion_item(label, kind)
        })
        .collect()
}

/// Return a list of completion items of Move's primitive types
fn primitive_types() -> Vec<CompletionItem> {
    PRIMITIVE_TYPES
        .iter()
        .map(|label| completion_item(label, CompletionItemKind::Keyword))
        .collect()
}

/// Return a list of completion items corresponding to each one of Move's builtin functions.
fn builtins() -> Vec<CompletionItem> {
    BUILTINS
        .iter()
        .map(|label| completion_item(label, CompletionItemKind::Function))
        .collect()
}

/// Sends the given connection a response to a completion request.
///
/// The completions returned depend upon where the user's cursor is positioned.
pub fn on_completion_request2(context: &Context, request: &Request, symbols: &Symbols) {
    // eprintln!("handling completion request");
    // let parameters = serde_json::from_value::<CompletionParams>(request.params.clone())
    //     .expect("could not deserialize completion request");

    // let path = parameters
    //     .text_document_position
    //     .text_document
    //     .uri
    //     .to_file_path()
    //     .unwrap();
    // let buffer = context.files.get(&path);

    // if buffer.is_none() {
    //     eprintln!(
    //         "Could not read '{:?}' when handling completion request",
    //         path
    //     );
    // }

    // let buffer = buffer.unwrap();

    // let file_hash = FileHash::new(buffer);

    // match element {}
    // let mut items = vec![];
    // items.extend(builtins());
    // items.extend(primitive_types());
    // items.extend(keywords());

    // let result = serde_json::to_value(items).expect("could not serialize completion response");
    // eprintln!("about to send completion response");
    // let response = lsp_server::Response::new_ok(request.id.clone(), result);
    // if let Err(err) = context
    //     .connection
    //     .sender
    //     .send(lsp_server::Message::Response(response))
    // {
    //     eprintln!("could not send completion response: {:?}", err);
    // }
}

/// All Modules.
pub struct Modules {
    modules: HashMap<ModuleIdent, IDEModule>,
}

struct Position {
    path: PathBuf,
    line: u32,
    character: u32,
}

impl Modules {
    pub fn new() -> Self {
        Self {
            modules: Default::default(),
        }
    }
    ///
    pub fn scan_all(path: impl Into<PathBuf>) {
        unimplemented!()
    }

    fn in_range(&self, p: Position, l: Loc) -> bool {}
    /// Enter Scope for you.
    pub fn enter_scope(&self, path: PathBuf, line: u32, character: u32) {
        let mut scopes = vec![];
        let global_scope = Scope_::default();

        // Enter all global to global_scope.
        for (_, modules) in self.modules.iter() {
            for (_, d) in modules.asts.iter() {
                match d {
                    Definition::Module(ref m) => {}
                    Definition::Address(ref a) => todo!(),
                    Definition::Script(ref s) => todo!(),
                }
            }
        }

        // Scan all for.
        for (_, modules) in self.modules.iter() {
            for (f, d) in modules.asts.iter() {
                // Only scan match.
                if *f != path {
                    continue;
                }
                // We have match the files.
                match d {
                    Definition::Module(x) => {
                        if self.in_range(path, line, character, x.loc) {
                            continue;
                        }
                        self.visit_module(x, &mut scopes);
                    }
                    Definition::Address(_) => todo!(),
                    Definition::Script(_) => todo!(),
                }
            }
        }
    }

    fn visit_module(&self, module: &ModuleDefinition, outer: Scope) {}

    fn visit_function(&self, function: &Function, outer: Scope, p: Position) {
        let s = Scope::new_nest(outer);
    }
}

struct ScopeGuard {}

impl Drop for ScopeGuard {}

#[derive(Default)]
pub struct Scope(Rc<Scope_>);

impl Scope {
    fn new_global() -> Self {
        Default::default()
    }
    fn new_nest(outer: Scope) -> Self {
        Self(Rc::new(Scope_ {
            outer: Some(outer),
            values: Default::default(),
        }))
    }
    fn visit<R>(&self, visitor: impl ScopeVisitor<R>) {
        unimplemented!();
    }
}

pub trait ScopeVisitor<R> {
    fn visit(x: Scope, stop: &mut bool /*  need stop. */);
    fn finish(self) -> R;
}

#[derive(Debug, Default)]
struct Scope_ {
    outer: Option<Scope>,
    values: HashMap<String, IDEDefinition>,
}

impl Scope_ {
    fn enter(&mut self, s: String, d: IDEDefinition) {
        s.values[s] = d;
    }
}

enum IDEDefinition {
    Module(Loc),
}

// pub trait Visitor<R> {
//     pub fn visit_definition(&mut self, _: &Definition) {
//         // do nothing.
//     }
//     pub fn finish(self) -> R;
// }

struct IDEModule {
    asts: HashMap<PathBuf, move_compiler::parser::ast::Definition>,
}
