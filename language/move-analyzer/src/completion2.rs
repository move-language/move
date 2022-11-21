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
    // p: Position,
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

    /// Enter Scope for you.
    pub fn enter_scope(&self, visitor: &mut dyn ScopeVisitor) {
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
                // We have match the files.
                match d {
                    Definition::Module(x) => {
                        if !visitor.file_should_visit(x.loc.file_hash()) {
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
    fn visit_function(&self, function: &Function, scopes: &Scopes, visitor: &mut dyn ScopeVisitor) {
        return scopes.enter_scope(|s| {
            self.visit_signature(&function.signature, s, visitor);
            if visitor.finished() {
                return;
            }
            match function.body.value {
                FunctionBody_::Native => {}
                FunctionBody_::Defined(ref seq) => {
                    for u in seq.0.iter() {
                        self.visit_use_decl(u, scopes, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                    for s in seq.1.iter() {
                        self.visit_sequence_item(s, scopes, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                }
            }
        });
    }

    fn visit_sequence_item(
        &self,
        seq: &SequenceItem,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        match seq.value {
            SequenceItem_::Seq(ref e) => {
                self.visit_expr(e, scopes, visitor);
                if visitor.finished() {
                    return;
                }
            }
            SequenceItem_::Declare(ref list, ref ty) => {
                self.visit_bind_list(list, ty, None, scopes, visitor);
                if visitor.finished() {
                    return;
                }
            }
            SequenceItem_::Bind(ref list, ref ty, ref expr) => {
                self.visit_bind_list(list, ty, Some(expr), scopes, visitor);
                if visitor.finished() {
                    return;
                }
            }
        }
    }

    fn visit_bind_list(
        &self,
        bind_list: &BindList,
        ty: &Option<Type>,
        expr: Option<&Box<Exp>>,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        let ty = if let Some(ty) = ty {
            scopes.resolve_type(ty)
        } else if let Some(expr) = expr {
            self.get_expr_type(expr, scopes)
        } else {
            ResolvedType::mk_default(unimplemented!())
        };

        for (index, bind) in bind_list.value.iter().enumerate() {
            let ty = ty.typ(index);
            let unknown = ResolvedType::mk_default(unimplemented!());
            let ty = ty.unwrap_or(&unknown);
            self.visit_bind(bind, ty, scopes, None, visitor);
            if visitor.finished() {
                return;
            }
        }
    }

    fn visit_bind(
        &self,
        bind: &Bind,
        ty: &ResolvedType,
        scopes: &Scopes,
        field: Option<&'_ Field>,
        visitor: &mut dyn ScopeVisitor,
    ) {
        match &bind.value {
            Bind_::Var(var) => {
                let item = Item::ExprVar(var.clone());
                visitor.handle_item(scopes, item.clone());
                if visitor.finished() {
                    return;
                }
                scopes.enter_value_item(var.0.value, item);
                return;
            }
            Bind_::Unpack(_, _, _) => todo!(),
        }
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
    ) {
        unimplemented!();
    }

    fn visit_expr_name_access_chain(
        &self,
        leading: &NameAccessChain,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        unimplemented!();
    }

    fn visit_access_type(&self, ty: &Type, scopes: &Scopes, visitor: &mut dyn ScopeVisitor) {
        let ty = scopes.resolve_type(ty);
        let item = Item::ApplyType(ty);
        visitor.handle_item(scopes, item);
    }

    /// Get A Type for expr if possible.
    fn get_expr_type(&self, expr: &Exp, scopes: &Scopes) -> ResolvedType {
        match &expr.value {
            Exp_::Value(ref x) => match &x.value {
                Value_::Address(_) => ResolvedType::new_build_in(BuildInType::Address),
                Value_::Num(_) => ResolvedType::new_build_in(BuildInType::NumType),
                Value_::Bool(_) => ResolvedType::new_build_in(BuildInType::Bool),
                // TODO 余洋 这2个都是string类型 但是好像没有这个内置类型
                Value_::HexString(_) => todo!(),
                Value_::ByteString(_) => todo!(),
            },
            Exp_::Move(x) | Exp_::Copy(x) => scopes.find_var_type(x.0.value),
            Exp_::Name(name, _ /*  TODO this is a error. */) => {
                // scopes.find_name_access_chain_type(name)
                return UNKNOWN_TYPE.clone();
            }
            Exp_::Call(name, _, _, _) => {
                let fun_type = scopes.find_name_access_chain_type(name);
                match &fun_type.0.value {
                    ResolvedType_::Fun(_, _, _) => todo!(),
                    _ => return UNKNOWN_TYPE.clone(),
                }
                unimplemented!()
            }
            Exp_::Pack(_, _, _) => todo!(),
            Exp_::Vector(_, _, _) => todo!(),
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
            Exp_::Break => todo!(),
            Exp_::Continue => todo!(),
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
    }

    fn visit_expr(&self, exp: &Exp, scopes: &Scopes, visitor: &mut dyn ScopeVisitor) {
        match &exp.value {
            Exp_::Value(ref v) => {
                if let Some(name) = Self::get_name_from_value(v) {
                    let item = Item::ExprName(name.clone());
                    visitor.handle_item(scopes, item);
                    if visitor.finished() {
                        return;
                    }
                }
                return;
            }
            Exp_::Move(var) | Exp_::Copy(var) => {
                unimplemented!();
            }
            Exp_::Name(chain, _ty) => {
                // TODO how to use _ty.
                // let _ty: Option<Vec<ResolvedType>> =
                //     ty.map(|s| s.iter().map(|t| scopes.resolve_type(t)).collect());
                let item = Item::ExprNameAccessChain(chain.clone());
                visitor.handle_item(scopes, item);
            }

            Exp_::Call(ref leading, _, ref types, ref exprs) => {
                // self.visit_expr_name_access_chain(leading, scopes, visitor);
                // if visitor.finished() {
                //     return;
                // }
                // if let Some(ref types) = types {
                //     for t in types.iter() {
                //         if self.in_range(t.loc) {
                //             let ty = scopes.resolve_type(t);
                //             let item = Item::ApplyType(ty);
                //             visitor.handle_item(scopes, item);
                //             return true;
                //         }
                //     }
                // }
                // for expr in exprs.value.iter() {
                //     self.visit_expr(exp, scopes, visitor);
                //     if visitor.finished() {
                //         return;
                //     }
                // }
                unimplemented!();
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
            Exp_::Break | Exp_::Continue => {}
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
    }

    fn visit_use_decl(&self, use_decl: &UseDecl, scopes: &Scopes, visitor: &mut dyn ScopeVisitor) {
        unimplemented!();
    }

    fn visit_signature(
        &self,
        signature: &FunctionSignature,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        // //
        // for (name, v) in signature.type_parameters.iter() {
        //     let loc = name.loc;
        //     let item = Item::TParam(name.clone(), v.clone());
        //     if self.in_range(loc) {
        //         // found
        //         visitor.handle_item(scopes, item);
        //         if visitor.finished() {
        //             return;
        //         }
        //     }
        //     // Enter this.
        //     scopes.enter_type_item(name.value, item);
        // }
        // for (v, t) in signature.parameters.iter() {
        //     let loc = t.loc;
        //     let ty = scopes.resolve_type(t);
        //     let item = Item::ApplyType(ty.clone());
        //     if self.in_range(loc) {
        //         // found
        //         visitor.handle_item(scopes, item);
        //         if visitor.finished() {
        //             return;
        //         }
        //     }
        //     {
        //         let loc = v.loc();
        //         let item = Item::Parameter(v.clone(), ty);
        //         // found
        //         visitor.handle_item(scopes, item);
        //         if visitor.finished() {
        //             return;
        //         }
        //         scopes.enter_value_item(v.value(), item)
        //     }
        // }

        unimplemented!();
    }
}

#[derive(Default, Debug)]
pub struct Scope {
    values: HashMap<Symbol, Item>,
    types: HashMap<Symbol, Item>,
    is_function: bool,
    is_spec: bool,
    is_global: bool,
}

impl Scope {
    fn enter_value_item(&mut self, s: Symbol, item: Item) {
        self.values.insert(s, item);
    }
    fn enter_type_item(&mut self, s: Symbol, item: Item) {
        self.types.insert(s, item);
    }
}

/// Visit scopes for inner to outer.
pub trait ScopeVisitor {
    /// Handle this item.
    /// If `should_finish` return true. All `enter_scope` and enter_scope called function will return.
    fn handle_item(&mut self, scopes: &Scopes, item: Item);
    /// Need not visit this structure???
    fn file_should_visit(&self, p: FileHash) -> bool;
    /// Visitor is finished.
    fn finished(&self) -> bool;
}

#[derive(Clone, Debug)]
enum ResolvedType_ {
    UnKnown,
    Struct(Name, Vec<StructTypeParameter>, Vec<(Field, ResolvedType)>),
    /// struct { ... }
    BuildInType(BuildInType),
    /// T : drop
    TParam(Name, Vec<Ability>),
    ApplyTParam(
        Box<ResolvedType>,
        /* two field copied from TParam  */ Name,
        Vec<Ability>,
    ),
    /// & mut ...
    Ref(bool, Box<ResolvedType>),
    /// ()
    Unit,
    /// (t1, t2, ... , tn)
    /// Used for return values and expression blocks
    Multiple(Vec<ResolvedType>),
    Fun(
        Vec<(Name, Vec<Ability>)>, // type parameters.
        Vec<ResolvedType>,         // parameters.
        Box<ResolvedType>,         // return type.
    ),
    Vec(Box<ResolvedType>),
    /// Can't resolve the Type,Keep the ast type.
    ResolvedFailed(Type_),
}

#[derive(Clone, Debug)]
pub struct ResolvedType(Spanned<ResolvedType_>);
impl ResolvedType {
    fn typ(&self, index: usize) -> Option<&'_ ResolvedType> {
        unimplemented!()
    }

    fn field(&self, index: usize) -> Option<&'_ Field> {
        unimplemented!()
    }

    const fn mk_default(loc: Loc) -> ResolvedType {
        Self(Spanned {
            loc,
            value: ResolvedType_::UnKnown,
        })
    }

    fn new_build_in(b: BuildInType) -> Self {
        unimplemented!();
    }
    fn is_unknown(&self) -> bool {
        match &self.0.value {
            ResolvedType_::UnKnown => true,
            _ => false,
        }
    }
    fn is_fun(&self) -> bool {
        match &self.0.value {
            ResolvedType_::Fun(_, _, _) => true,
            _ => false,
        }
    }

    /// bind type parameter to concrete tpe
    fn bind_type_parameter(&mut self, types: &HashMap<Symbol, ResolvedType>) {
        match &mut self.0.value {
            ResolvedType_::UnKnown => {}
            ResolvedType_::Struct(_, _, ref mut fields) => {
                for i in 0..fields.len() {
                    let mut t = fields.get_mut(i).unwrap();
                    t.1.bind_type_parameter(types);
                }
            }
            ResolvedType_::BuildInType(_) => {}
            ResolvedType_::TParam(name, _) => {
                if let Some(x) = types.get(&name.value) {
                    std::mem::replace(&mut self.0.value, x.clone().0.value);
                }
            }
            ResolvedType_::Ref(_, ref mut b) => {
                b.as_mut().bind_type_parameter(types);
            }
            ResolvedType_::Unit => {}
            ResolvedType_::Multiple(ref mut xs) => {
                for i in 0..xs.len() {
                    let mut t = xs.get_mut(i).unwrap();
                    t.bind_type_parameter(types);
                }
            }
            ResolvedType_::Fun(_, ref mut xs, ref mut ret) => {
                for i in 0..xs.len() {
                    let mut t = xs.get_mut(i).unwrap();
                    t.bind_type_parameter(types);
                }
                ret.as_mut().bind_type_parameter(types);
            }
            ResolvedType_::Vec(ref mut b) => {
                b.as_mut().bind_type_parameter(types);
            }
            ResolvedType_::ResolvedFailed(_) => {}
            ResolvedType_::ApplyTParam(_, _, _) => {
                unreachable!("called multiple times.")
            }
        }
    }
}

#[derive(Clone, Debug, Copy)]
pub enum BuildInType {
    Bool,
    U8,
    U64,
    U128,
    Address,
    /// A number type from literal.
    /// Could be u8 and ... depend on How it is used.
    NumType,
}

impl BuildInType {
    fn from_symbol(s: Symbol) -> Self {
        match s.as_str() {
            "u8" => Self::U8,
            "u64" => Self::U64,
            "u128" => Self::U128,
            "bool" => Self::Bool,
            "address" => Self::Address,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Item {
    /////////////////////////////
    /// VALUE types
    Parameter(Var, ResolvedType),

    /////////////////////////
    /// TYPE types
    Struct(Name, Vec<StructTypeParameter>, Vec<(Field, ResolvedType)>),
    /// build in types.
    BuildInType(BuildInType),
    /// Here are all definition.
    TParam(Name, Vec<Ability>),

    ////////////////////////////////
    /// various access types.
    // A type apply.
    ApplyType(ResolvedType),
    Use(UseDecl),
    ExprVar(Var),
    ExprName(Name),
    ExprNameAccessChain(NameAccessChain),

    ///////////////
    /// key words
    KeyWords(&'static str),
}

impl Item {
    fn to_type(&self, loc: Loc) -> Option<ResolvedType> {
        let x = match self {
            Item::TParam(name, ab) => ResolvedType_::TParam(name.clone(), ab.clone()),
            Item::Struct(name, types, fields) => {
                ResolvedType_::Struct(name.clone(), types.clone(), fields.clone())
            }
            Item::BuildInType(b) => ResolvedType_::BuildInType(*b),
            _ => return None,
        };
        Some(ResolvedType(Spanned { loc, value: x }))
    }
    fn loc() -> Loc {
        unimplemented!();
    }
}
pub struct IDEModule {
    defs: HashMap<PathBuf, move_compiler::parser::ast::Definition>,
    filepath_to_filehash: HashMap<String /* file path */, FileHash>,
}

#[derive(Default, Clone)]
pub struct Scopes {
    scopes: Rc<RefCell<Vec<Scope>>>,
}

impl Scopes {
    fn enter_scope<R>(&self, call_back: impl FnOnce(&Scopes) -> R) -> R {
        let s = Scope::default();
        self.scopes.borrow_mut().push(s);
        let _guard = ScopesGuarder::new(self.clone());
        let r = call_back(self);
        r
    }

    // Enter
    fn enter_value_item(&self, name: Symbol, item: Item) {
        self.scopes
            .as_ref()
            .borrow_mut()
            .last_mut()
            .unwrap()
            .enter_value_item(name, item);
    }

    fn enter_type_item(&self, name: Symbol, item: Item) {
        self.scopes
            .as_ref()
            .borrow_mut()
            .last_mut()
            .unwrap()
            .enter_type_item(name, item);
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

    fn resolve_name_access_chain_type(&self, chain: &NameAccessChain) -> ResolvedType {
        let scopes = self.scopes.as_ref().borrow();

        for s in scopes.iter() {
            // We must be in global scope.
            let item = match &chain.value {
                NameAccessChain_::One(x) => {
                    if let Some(item) = s.types.get(&x.value) {
                        if let Some(ty) = item.to_type(x.loc) {
                            return ty;
                        }
                    }
                }
                NameAccessChain_::Two(_, _) => todo!(),
                NameAccessChain_::Three(_, _) => todo!(),
            };
        }
        // make sure return a false type.
        return ResolvedType(Spanned {
            loc: chain.loc,
            value: ResolvedType_::ResolvedFailed(Type_::Apply(Box::new(chain.clone()), vec![])),
        });
    }

    fn find_var_type(&self, name: Symbol) -> ResolvedType {
        let mut ret = None;
        self.inner_first_visit(|s| {
            if let Some(v) = s.values.get(&name) {
                match v {
                    Item::Parameter(_, t) => {
                        ret = Some(t.clone());
                        return true;
                    }
                    _ => {}
                }
            };
            false
        });
        return ResolvedType(Spanned {
            loc: unimplemented!(),
            value: ResolvedType_::UnKnown,
        });
    }
    fn find_name_access_chain_type(&self, s: &NameAccessChain) -> ResolvedType {
        unimplemented!();
    }

    fn resolve_type(&self, ty: &Type) -> ResolvedType {
        let r = match &ty.value {
            Type_::Apply(ref chain, types) => {
                let chain_ty = self.resolve_name_access_chain_type(chain);
                let _types: Vec<_> = types.iter().map(|ty| self.resolve_type(ty)).collect();
                return chain_ty;
            }
            Type_::Ref(m, ref b) => ResolvedType_::Ref(*m, Box::new(self.resolve_type(b.as_ref()))),
            Type_::Fun(ref parameters, ref ret) => {
                let parameters: Vec<_> = parameters.iter().map(|v| self.resolve_type(v)).collect();
                let ret = self.resolve_type(ret.as_ref());
                ResolvedType_::Fun(vec![], parameters, Box::new(ret))
            }
            Type_::Unit => ResolvedType_::Unit,
            Type_::Multiple(ref types) => {
                let types: Vec<_> = types.iter().map(|v| self.resolve_type(v)).collect();
                ResolvedType_::Multiple(types)
            }
        };

        ResolvedType(Spanned {
            loc: ty.loc,
            value: r,
        })
    }
}

fn get_name_access_chain_loc(chain: &NameAccessChain) -> Loc {
    unimplemented!()
}

/// RAII type pop on `enter_scope`.
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

#[test]
fn xxxx() {
    let s = Scopes::default();
    s.enter_scope(|s| s.enter_scope(|s| s.enter_scope(|_| {})));
}

const UNKNOWN_TYPE: ResolvedType = ResolvedType::mk_default(Loc::new(FileHash::empty(), 0, 0));
