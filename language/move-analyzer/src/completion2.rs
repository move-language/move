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
use move_ir_types::location::{Loc, Spanned};
use move_symbol_pool::Symbol;
use serde::__private::de;
use std::cell::RefCell;
use std::ops::Deref;
use std::path::Path;
use std::sync::Mutex;
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
    fn enter_module_top(&self, s: &Scopes, module: &ModuleDefinition) {}
    fn enter_script_top(&self, s: &Scopes, module: &Script) {}
    fn enter_address_top(&self, s: &Scopes, module: &AddressDefinition) {}

    ///
    fn visit_function(
        &self,
        function: &Function,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) -> bool {
        return scopes.enter_scope(|s| -> bool {
            if self.visit_signature(&function.signature, s, visitor) {
                return true;
            }
            match function.body.value {
                FunctionBody_::Native => {}
                FunctionBody_::Defined(ref seq) => {
                    for u in seq.0.iter() {
                        if self.visit_use_decl(u, scopes, visitor) {
                            return true;
                        }
                    }

                    for s in seq.1.iter() {
                        if self.visit_sequence_item(s, scopes, visitor) {
                            return true;
                        }
                    }
                }
            }
            false
        });
    }

    fn visit_sequence_item(
        &self,
        seq: &SequenceItem,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) -> bool {
        match seq.value {
            SequenceItem_::Seq(ref e) => {
                if self.visit_expr(e, scopes, visitor) {
                    return true;
                }
            }
            SequenceItem_::Declare(ref list, ref ty) => {
                if self.visit_declare(list, ty, None, scopes, visitor) {
                    return true;
                }
            }
            SequenceItem_::Bind(ref list, ref ty, ref expr) => {
                if self.visit_declare(list, ty, Some(expr), scopes, visitor) {
                    return true;
                }
            }
        }
        false
    }

    fn visit_declare(
        &self,
        item: &BindList,
        ty: &Option<Type>,
        expr: Option<&Box<Exp>>,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) -> bool {
        false
    }

    fn get_name_from_value(v: &Value) -> Option<&Name> {
        match &v.value {
            Value_::Address(ref x) => match &x.value {
                LeadingNameAccess_::AnonymousAddress(_) => None,
                LeadingNameAccess_::Name(ref name) => Some(name),
            },
            _ => None,
        }
    }

    fn visit_expr_leading(
        &self,
        leading: &LeadingNameAccess,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) -> bool {
        unimplemented!();
    }

    fn visit_expr_name_access_chain(
        &self,
        leading: &NameAccessChain,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) -> bool {
        unimplemented!();
    }
    fn visit_access_type(
        &self,
        ty: &Type,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) -> bool {
        if self.in_range(ty.loc) {
            return false;
        }
        let item = Item::ApplyType(ty.clone());
        visitor.visit(scopes, item);
        return true;
    }

    fn visit_expr(&self, exp: &Exp, scopes: &Scopes, visitor: &mut dyn ScopeVisitor) -> bool {
        match &exp.value {
            Exp_::Value(ref v) => {
                if let Some(name) = Self::get_name_from_value(v) {
                    if self.in_range(name.loc) {
                        let item = Item::ExprName(name.clone());
                        visitor.visit(scopes, item);
                        return true;
                    }
                }
                return false;
            }
            Exp_::Move(var) | Exp_::Copy(var) => {
                let item = Item::ExprVar(var.clone());
                if self.in_range(var.loc()) {
                    return true;
                }
            }
            Exp_::Name(chain, ty) => match chain.value {
                NameAccessChain_::One(one) => {
                    if self.in_range(one.loc) {
                        let item = Item::ExprName(one.clone());
                        visitor.visit(scopes, item);
                        return true;
                    }
                    return false;
                }
                NameAccessChain_::Two(ref leading, two) => {
                    if self.visit_expr_leading(leading, scopes, visitor) {
                        return true;
                    }
                    let item = Item::NameAccessChainTwo(leading.clone(), two.clone());
                    if self.in_range(two.loc) {
                        visitor.visit(scopes, item);
                        return true;
                    }
                    return false;
                }
                NameAccessChain_::Three(one, _) => todo!(),
            },

            Exp_::Call(ref leading, _, ref types, ref exprs) => {
                if self.visit_expr_name_access_chain(leading, scopes, visitor) {
                    return true;
                }
                if let Some(ref types) = types {
                    for t in types.iter() {
                        if self.in_range(t.loc) {
                            let item = Item::ApplyType(t.clone());
                            visitor.visit(scopes, item);
                            return true;
                        }
                    }
                }

                for expr in exprs.value.iter() {
                    if self.visit_expr(exp, scopes, visitor) {
                        return true;
                    }
                }
            }
            Exp_::Pack(_, _, _) => todo!(),
            Exp_::Vector(_loc, ref ty, ref exprs) => {}
            Exp_::IfElse(_, _, _) => todo!(),
            Exp_::While(_, _) => todo!(),
            Exp_::Loop(_) => todo!(),
            Exp_::Block(_) => todo!(),
            Exp_::Lambda(_, _) => todo!(),
            Exp_::Quant(_, _, _, _, _) => todo!(),
            Exp_::ExpList(_) => todo!(),
            Exp_::Unit => todo!(),
            Exp_::Assign(_, _) => todo!(),
            Exp_::Return(_) => todo!(),
            Exp_::Abort(_) => todo!(),
            Exp_::Break | Exp_::Continue => {
                if self.in_range(exp.loc) {
                    return true;
                } else {
                    return false;
                }
            }
            Exp_::Dereference(_) => todo!(),
            Exp_::UnaryExp(_, _) => todo!(),
            Exp_::BinopExp(_, _, _) => todo!(),
            Exp_::Borrow(_, _) => todo!(),
            Exp_::Dot(_, _) => todo!(),
            Exp_::Index(_, _) => todo!(),
            Exp_::Cast(_, _) => todo!(),
            Exp_::Annotate(_, _) => todo!(),
            Exp_::Spec(_) => todo!(),
            Exp_::UnresolvedError => todo!(),
        }
        false
    }

    fn visit_use_decl(
        &self,
        use_decl: &UseDecl,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) -> bool {
        match use_decl.use_ {
            Use::Module(ref m, ref name) => {
                if self.in_range(m.loc) {
                    // found;
                    let item = Item::AccessModule(m.clone());
                    visitor.visit(scopes, item);
                    return true;
                }
                if let Some(ref name) = name {
                    if self.in_range(name.loc()) {
                        let item = Item::AccessModuleAlias(m.clone(), name.clone());
                        visitor.visit(scopes, item);
                        return true;
                    }
                }
            }
            Use::Members(ref m, ref members) => {
                if self.in_range(m.loc) {
                    // found;
                    let item = Item::AccessModule(m.clone());
                    visitor.visit(scopes, item);
                    return true;
                }
                for (name, alias) in members {
                    let item = Item::AccessModuleMember(m.clone(), name.clone(), alias.clone());
                    if self.in_range(name.loc)
                        || alias.clone().map(|s| self.in_range(s.loc)).unwrap_or(false)
                    {
                        visitor.visit(scopes, item);
                        return true;
                    }
                }
            }
        }

        false
    }

    fn visit_signature(
        &self,
        signature: &FunctionSignature,
        s: &Scopes,
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
                let item = Item::ApplyType(t.clone());
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
    is_function: bool,
    is_spec: bool,
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
    ApplyType(Type),
    AccessModule(ModuleIdent),
    // TODO what this means.
    AccessModuleAlias(ModuleIdent, ModuleName /*  alias  */),
    AccessModuleMember(ModuleIdent, Name, Option<Name> /*  alias  */),
    /// Access the last Name.
    NameAccessChainTwo(LeadingNameAccess, Name),
    /// Access the last name.
    NameAccessChianThree(LeadingNameAccess, Name, Name),
    ExprVar(Var),
    ExprName(Name),
}

pub struct IDEModule {
    defs: HashMap<PathBuf, move_compiler::parser::ast::Definition>,
    filepath_to_filehash: HashMap<String /* file path */, FileHash>,
}

#[derive(Default, Clone)]
pub struct Scopes {
    scopes: Rc<RefCell<Vec<Scope>>>,
}

/// RAII type pop on Scope.
struct ScopesGuarder(Scopes);

impl ScopesGuarder {
    fn new(s: Scopes) -> Self {
        Self(s)
    }
}

impl Drop for ScopesGuarder {
    fn drop(&mut self) {
        self.0.scopes.as_ref().borrow_mut().pop().unwrap();
    }
}

impl Scopes {
    fn enter_scope<R>(&self, call_back: impl FnOnce(&Scopes) -> R) -> R {
        let s = Scope::default();
        self.scopes.borrow_mut().push(s);
        let _pop = ScopesGuarder::new(self.clone());
        let r = call_back(self);
        r
    }

    // Enter
    fn enter_item(&self, name: Symbol, item: Item) {
        self.scopes
            .as_ref()
            .borrow_mut()
            .last_mut()
            .unwrap()
            .enter_item(name, item);
    }

    /// Visit all scope from inner to outer.
    fn inner_first_visit(&self, mut visitor: impl FnMut(&Scope) -> bool /*  stop??? */) {
        for s in self.scopes.as_ref().borrow().iter().rev() {
            if visitor(s) {
                return;
            }
        }
    }
    ///
    fn setup_scope(&self, f: impl FnOnce(&mut Scope)) {
        let mut x = self.scopes.borrow_mut();
        let x = x.last_mut().unwrap();
        f(x);
    }

    fn under_function(&self) -> Option<()> {
        let mut r = None;
        self.inner_first_visit(|s| {
            if s.is_function {
                r = Some(());
                return true;
            }
            false
        });
        r
    }

    fn under_spec(&self) -> Option<()> {
        let mut r = None;
        self.inner_first_visit(|s| {
            if s.is_spec {
                r = Some(());
                return true;
            }
            false
        });
        r
    }
}

/*
    代码里面有很多
    // in_range表示找到了
    但是查找引用应该返回所有的这里肯定需要啥特殊的处理

    if self.in_range(.. )  {
        return true
    }

*/

#[test]
fn xxxx() {
    let s = Scopes::default();
    s.enter_scope(|s| s.enter_scope(|s| s.enter_scope(|_| {})));
}
