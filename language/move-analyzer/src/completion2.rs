// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{context::Context, symbols::Symbols};
use im::HashMap;
use lsp_server::Request;
use lsp_types::{CompletionItem, CompletionItemKind, CompletionParams};
use move_command_line_common::files::FileHash;
use move_compiler::parser::ast::ModuleName;
use move_compiler::parser::ast::{Definition, ModuleIdent};
use move_compiler::parser::*;
use move_compiler::shared::Identifier;
use move_compiler::{
    parser::{
        ast::*,
        keywords::{BUILTINS, CONTEXTUAL_KEYWORDS, KEYWORDS, PRIMITIVE_TYPES},
        lexer::{Lexer, Tok},
    },
    shared::*,
    CommentMap,
};
use move_ir_types::ast::Statement_;
use move_ir_types::location::Loc;
use move_symbol_pool::Symbol;
use petgraph::visit::VisitMap;
use serde::__private::de;
use std::ops::Deref;
use std::path::Path;
use std::vec;
use std::{collections::HashSet, path::PathBuf, rc::Rc};
use tempfile::TempPath;

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
    p: Position,
}

#[derive(Clone)]
pub struct Position {
    path: PathBuf,
    line: u32,
    character: u32,
}

impl Modules {
    pub fn new() -> Self {
        unimplemented!()
    }
    ///
    pub fn scan_all(path: impl Into<PathBuf>) {
        unimplemented!()
    }

    fn in_range(&self, l: Loc) -> bool {
        unimplemented!()
    }
    /// Enter Scope for you.
    pub fn enter_scope(&self) {
        let mut global_scope = Scopes::default();
        // Enter all global to global_scope.
        for (_, modules) in self.modules.iter() {
            for (_, d) in modules.defs.iter() {
                match d {
                    Definition::Module(ref m) => {
                        self.enter_module_top(&mut global_scope, m);
                    }
                    Definition::Address(ref a) => {
                        self.enter_address_top(&mut global_scope, a);
                    }
                    Definition::Script(ref s) => {
                        self.enter_script_top(&mut global_scope, s);
                    }
                }
            }
        }
        // Scan all for.
        for (_, modules) in self.modules.iter() {
            for (f, d) in modules.defs.iter() {
                // Only scan match.
                if *f != self.p.path {
                    continue;
                }
                // We have match the files.
                match d {
                    Definition::Module(x) => {
                        if self.in_range(x.loc) {
                            continue;
                        }
                    }
                    Definition::Address(_) => todo!(),
                    Definition::Script(_) => todo!(),
                }
            }
        }
    }

    /// Enter Top level
    fn enter_module_top(&self, s: &mut Scopes, module: &ModuleDefinition) {}
    fn enter_script_top(&self, s: &mut Scopes, module: &Script) {}
    fn enter_address_top(&self, s: &mut Scopes, module: &AddressDefinition) {}
    ///
    fn visit_function(
        &self,
        function: &Function,
        outer: &mut Scopes,
        p: Position,
        visitor: &mut dyn ScopeVisitor,
    ) -> bool {
        return outer.enter_scope(|s| -> bool {
            if self.visit_signature(&function.signature, s, visitor) {
                return true;
            }
            match function.body.value {
                FunctionBody_::Native => {}
                FunctionBody_::Defined(ref seq) => {}
            }
            false
        });
    }

    fn visit_signature(
        &self,
        signature: &FunctionSignature,
        s: &mut Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) -> bool {
        //
        for (name, v) in signature.type_parameters.iter() {
            let loc = name.loc;
            let item = Item::TParam((name.clone(), v.clone()));
            if self.in_range(loc) {
                // found
                visitor.visit(s, item);
                return true;
            }
            // Enter this.
            s.enter_item(name.value, item);
        }
        for (v, t) in signature.parameters.iter() {
            {
                let loc = v.loc();
                let item = Item::Parameter((v.clone(), t.clone()));
                if self.in_range(loc) {
                    // found
                    visitor.visit(s, item);
                    return true;
                }
                s.enter_item(v.value(), item)
            }
            {
                let loc = t.loc;
                let item = Item::Type(t.clone());
                if self.in_range(loc) {
                    // found
                    visitor.visit(s, item);
                    return true;
                }
            }
        }
        false
    }
}

#[derive(Default, Debug)]
pub struct Scope {
    defs: HashMap<Symbol, Item>,
}

impl Scope {
    fn enter_item(&mut self, s: Symbol, item: Item) {
        self.defs.insert(s, item);
    }
}

/// Visit scopes for inner to outer.
pub trait ScopeVisitor {
    /// All Scopes that you can visit.
    fn visit(&mut self, scopes: &Scopes, item: Item);
}

#[derive(Debug, Clone)]
pub enum Item {
    // Here are all definition.
    TParam((Name, Vec<Ability>)),
    Parameter((Var, Type)),

    // A type apply.
    Type(Type),
}

pub struct IDEModule {
    defs: HashMap<PathBuf, move_compiler::parser::ast::Definition>,
    filepath_to_filehash: HashMap<String /* file path */, FileHash>,
}

#[derive(Default)]
pub struct Scopes {
    scopes: Vec<Scope>,
}

impl Scopes {
    fn enter_scope<R>(&mut self, x: impl FnOnce(&mut Scopes) -> R) -> R {
        let s = Scope::default();
        self.scopes.push(s);
        let r = x(self);
        self.scopes.pop();
        r
    }
    // Enter
    fn enter_item(&mut self, name: Symbol, item: Item) {
        self.current().enter_item(name, item);
    }
    /// Current Scope.
    fn current(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    /// Visit all scope from inner to outer.
    fn inner_fist_visit(&self, mut visitor: impl FnMut(&Scope)) {
        for s in self.scopes.iter().rev() {
            visitor(s);
        }
    }
}
