use super::item::*;
use super::scope::*;
use super::scopes::*;
use super::types::*;
use super::utils::*;
use move_core_types::account_address::*;

use anyhow::{Ok, Result};

use move_command_line_common::files::FileHash;

use move_compiler::parser::ast::Definition;

use move_compiler::shared::Identifier;
use move_compiler::{parser::ast::*, shared::*};

use move_ir_types::location::Spanned;
use move_package::source_package::layout::SourcePackageLayout;
use move_package::source_package::manifest_parser::*;

use move_symbol_pool::Symbol;

use std::cell::RefCell;
use std::collections::btree_map::BTreeMap;
use std::collections::HashMap;

use std::{path::PathBuf, rc::Rc};

use super::modules::*;

impl Modules {
    pub fn run_visitor_for_manifest(
        &self,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
        manifest: &PathBuf,
    ) {
        self.with_module(manifest, |addr, module_name| {
            scopes.set_up_module(addr, module_name.name);
        });

        self.with_const(manifest, |addr, name, c| {
            self.visit_const(addr, name, c, scopes, visitor);
        });

        self.with_struct(manifest, |addr, module_name, c| {
            let item = Item::StructNameRef(addr, module_name, c.name.clone());
            scopes.enter_top_item(self, addr, module_name, c.name.0.value, item);
        });

        self.with_use_decl(manifest, |addr, module_name, u| {
            self.visit_use_decl(Some((addr, module_name)), u, scopes, None)
        });

        self.with_struct(manifest, |addr, module_name, s| {
            let _guard = scopes.clone_scope_and_enter(addr, module_name);
            scopes.enter_scope(|scopes| {
                for t in s.type_parameters.iter() {
                    self.visit_struct_tparam(t, scopes, visitor);
                }
                let fields = match &s.fields {
                    StructFields::Defined(x) => {
                        let mut fields = Vec::with_capacity(x.len());
                        for (f, ty) in x.iter() {
                            self.visit_type_apply(ty, scopes, visitor);
                            if visitor.finished() {
                                return;
                            }
                            let ty = scopes.resolve_type(ty, self);
                            {
                                let item = ItemOrAccess::Item(Item::Field(f.clone(), ty.clone()));
                                visitor.handle_item(self, scopes, &item);
                                if visitor.finished() {
                                    return;
                                }
                            }
                            fields.push((f.clone(), ty));
                        }
                        fields
                    }
                    StructFields::Native(_) => vec![],
                };
                let item = Item::Struct(ItemStruct {
                    name: s.name,
                    type_parameters: s.type_parameters.clone(),
                    type_parameters_ins: vec![],
                    fields,
                });
                scopes.enter_top_item(self, addr, module_name, s.name.value(), item)
            });
        });

        self.with_function(manifest, |addr, module_name, f| {
            // This clone scope make sure we can visit module level item.
            let _guard = scopes.clone_scope_and_enter(addr, module_name);
            let enter_function = |modules: &Modules,
                                  f: &Function,
                                  scopes: &Scopes,
                                  visitor: &mut dyn ScopeVisitor,
                                  address: AccountAddress,
                                  module_name: Symbol| {
                // This enter scope make sure the visit_tparam cannot override some module level item.
                scopes.enter_scope(|scopes| {
                    let s = &f.signature;
                    let ts = s.type_parameters.clone();
                    for t in s.type_parameters.iter() {
                        self.visit_tparam(t, scopes, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                    let params: Vec<_> = s
                        .parameters
                        .iter()
                        .map(|(var, ty)| (var.clone(), scopes.resolve_type(ty, self)))
                        .collect();
                    let ret = scopes.resolve_type(&s.return_type, self);
                    let item = Item::Fun(ItemFunction {
                        name: f.name.clone(),
                        type_parameters: ts,
                        parameters: params,
                        ret_type: Box::new(ret),
                    });
                    let item = ItemOrAccess::Item(item);
                    visitor.handle_item(modules, scopes, &item);
                    scopes.enter_top_item(self, address, module_name, f.name.value(), item);
                });
            };
            enter_function(self, f, scopes, visitor, addr, module_name);
        });

        // visit use decl again.
        self.with_use_decl(manifest, |addr, module_name, u| {
            self.visit_use_decl(Some((addr, module_name)), u, scopes, Some(visitor));
        });
        self.with_friend(manifest, |addr, module_name, f| {
            self.visit_friend(f, scopes, visitor);
        });
        // visit function body.
        self.with_function(manifest, |addr, module_name, f| {
            if !visitor
                .file_should_visit(self.convert_file_hash_filepath(&f.loc.file_hash()).unwrap())
            {
                return;
            }
            let _guard = scopes.clone_scope_and_enter(addr, module_name);
            self.visit_function(f, scopes, visitor);
        });
    }

    /// Entrance for `ScopeVisitor` base on analyze.
    pub fn run_visitor(&self, visitor: &mut dyn ScopeVisitor) {
        let scopes = Scopes::new();
        log::info!("run visitor for {} ", visitor);
        // visit should `rev`.
        let x: Vec<_> = self.manifests.iter().rev().map(|x| x.clone()).collect();
        for m in x.iter() {
            self.run_visitor_for_manifest(&scopes, visitor, m);
            if visitor.finished() {
                return;
            }
        }
    }

    pub(crate) fn visit_const(
        &self,
        address: AccountAddress,
        module: Symbol,
        c: &Constant,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        self.visit_type_apply(&c.signature, scopes, visitor);
        if visitor.finished() {
            return;
        }
        // const can only be declared at top scope
        let ty = scopes.resolve_type(&c.signature, self);
        let item = ItemOrAccess::Item(Item::Const(c.name.clone(), ty));
        visitor.handle_item(self, scopes, &item);
        let item: Item = item.into();
        scopes.enter_top_item(self, address, module, c.name.value(), item.clone());
    }

    pub(crate) fn get_module_addr(
        &self,
        addr: Option<LeadingNameAccess>,
        m: &ModuleDefinition,
    ) -> AccountAddress {
        match addr {
            Some(x) => match x.value {
                LeadingNameAccess_::AnonymousAddress(x) => x.bytes,
                LeadingNameAccess_::Name(name) => self.name_to_addr(name.value),
            },
            None => match m.address {
                Some(x) => match x.value {
                    LeadingNameAccess_::AnonymousAddress(x) => x.bytes,
                    LeadingNameAccess_::Name(name) => self.name_to_addr(name.value),
                },
                None => ERR_ADDRESS.clone(),
            },
        }
    }

    ///
    pub(crate) fn visit_function(
        &self,
        function: &Function,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        return scopes.enter_scope(|s| {
            self.visit_signature(&function.signature, s, visitor);
            if visitor.finished() {
                return;
            }

            for v in function.acquires.iter() {
                let ty = &Spanned {
                    loc: v.loc,
                    value: Type_::Apply(Box::new(v.clone()), vec![]),
                };
                self.visit_type_apply(&ty, scopes, visitor);
                if visitor.finished() {
                    return;
                }
            }

            match function.body.value {
                FunctionBody_::Native => {}
                FunctionBody_::Defined(ref seq) => self.visit_block(seq, scopes, visitor),
            }
        });
    }

    pub(crate) fn visit_block(
        &self,
        seq: &Sequence,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        scopes.enter_scope(|scopes| {
            for u in seq.0.iter() {
                self.visit_use_decl(None, u, scopes, Some(visitor));
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
            if let Some(ref exp) = seq.3.as_ref() {
                self.visit_expr(exp, scopes, visitor);
            }
        });
    }

    pub(crate) fn visit_sequence_item(
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

    pub(crate) fn visit_bind_list(
        &self,
        bind_list: &BindList,
        ty: &Option<Type>,
        expr: Option<&Box<Exp>>,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        let ty = if let Some(ty) = ty {
            self.visit_type_apply(ty, scopes, visitor);
            if visitor.finished() {
                return;
            }
            scopes.resolve_type(ty, self)
        } else if let Some(expr) = expr {
            let ty = self.get_expr_type(expr, scopes);
            self.visit_expr(expr, scopes, visitor);
            if visitor.finished() {
                return;
            }
            ty
        } else {
            ResolvedType::new_unknown()
        };
        for (index, bind) in bind_list.value.iter().enumerate() {
            let ty = ty.nth_ty(index);
            let unknown = ResolvedType::new_unknown();
            let ty = ty.unwrap_or(&unknown);
            self.visit_bind(bind, ty, scopes, visitor);
            if visitor.finished() {
                return;
            }
        }
    }

    pub(crate) fn visit_bind(
        &self,
        bind: &Bind,
        infer_ty: &ResolvedType,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        log::info!("visit_bind:{:?}", bind);
        match &bind.value {
            Bind_::Var(var) => {
                let item = ItemOrAccess::Item(Item::Var(var.clone(), infer_ty.clone()));
                visitor.handle_item(self, scopes, &item);
                if visitor.finished() {
                    return;
                }
                scopes.enter_item(self, var.0.value, item);
                return;
            }
            Bind_::Unpack(chain, tys, field_binds) => {
                let struct_ty = scopes.find_name_chain_type(chain, &mut None, self, false);
                let item = ItemOrAccess::Access(Access::ApplyType(
                    chain.as_ref().clone(),
                    Box::new(struct_ty.clone()),
                ));
                visitor.handle_item(self, scopes, &item);
                if visitor.finished() {
                    return;
                }

                if let Some(tys) = tys {
                    for t in tys.iter() {
                        self.visit_type_apply(t, scopes, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                }
                let mut struct_ty = struct_ty.struct_ref_to_struct(scopes);
                match &struct_ty.clone() {
                    ResolvedType::Struct(ItemStruct {
                        name,
                        type_parameters,
                        type_parameters_ins,
                        fields,
                    }) => {
                        let struct_ty = if let Some(tys) = tys {
                            let tys: Vec<_> =
                                tys.iter().map(|x| scopes.resolve_type(x, self)).collect();
                            let mut m = HashMap::new();
                            type_parameters.iter().zip(tys.iter()).for_each(|(t, ty)| {
                                m.insert(t.name.value, ty.clone());
                            });
                            struct_ty.bind_type_parameter(&mut m, scopes);
                            struct_ty
                        } else {
                            // use
                            infer_ty.clone()
                        };
                        for (field, bind) in field_binds.iter() {
                            let field_ty = struct_ty.find_filed_by_name(field.0.value);
                            if let Some(field_ty) = field_ty {
                                self.visit_bind(bind, &field_ty.1, scopes, visitor);
                            } else {
                                self.visit_bind(bind, &UNKNOWN_TYPE, scopes, visitor);
                            }
                        }
                        //
                    }
                    _ => {}
                };
            }
        }
    }

    pub(crate) fn visit_type_apply(
        &self,
        ty: &Type,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        match &ty.value {
            Type_::Apply(chain, types) => {
                let ty = scopes.find_name_chain_type(chain.as_ref(), &mut None, self, true);
                let item =
                    ItemOrAccess::Access(Access::ApplyType(chain.as_ref().clone(), Box::new(ty)));
                visitor.handle_item(self, scopes, &item);
                if visitor.finished() {
                    return;
                }
                for t in types.iter() {
                    self.visit_type_apply(t, scopes, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
            Type_::Ref(_, ty) => self.visit_type_apply(ty, scopes, visitor),
            Type_::Fun(_, _) => {}
            Type_::Unit => {}
            Type_::Multiple(types) => {
                for t in types.iter() {
                    self.visit_type_apply(t, scopes, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
        }
    }

    pub(crate) fn visit_expr(&self, exp: &Exp, scopes: &Scopes, visitor: &mut dyn ScopeVisitor) {
        log::trace!("visit_expr:{:?}", exp);
        match &exp.value {
            Exp_::Value(ref v) => {
                if let Some(name) = get_name_from_value(v) {
                    let item = ItemOrAccess::Access(Access::ExprAddressName(name.clone()));
                    visitor.handle_item(self, scopes, &item);
                }
            }
            Exp_::Move(var) | Exp_::Copy(var) => {
                let mut item = Some(Item::new_dummy());
                scopes.find_name_chain_type(
                    &Spanned {
                        loc: var.loc(),
                        value: NameAccessChain_::One(var.0.clone()),
                    },
                    &mut item,
                    self,
                    false,
                );
                let item =
                    ItemOrAccess::Access(Access::ExprVar(var.clone(), Box::new(item.unwrap())));
                visitor.handle_item(self, scopes, &item);
            }
            Exp_::Name(
                chain,
                _ty, /*
                     yuyang:
                      TODO How to use _ty,
                      looks like _ty is not used. */
            ) => {
                let mut item = Some(Item::new_dummy());
                scopes.find_name_chain_type(chain, &mut item, self, false);
                let item = ItemOrAccess::Access(Access::ExprAccessChain(
                    chain.clone(),
                    Box::new(item.unwrap()),
                ));
                visitor.handle_item(self, scopes, &item);
            }
            Exp_::Call(ref chain, is_macro, ref types, ref exprs) => {
                if *is_macro {
                    let c = MacroCall::from_chain(chain);
                    let item = ItemOrAccess::Access(Access::MacroCall(c));
                    visitor.handle_item(self, scopes, &item);
                } else {
                    let mut item = Some(Item::new_dummy());
                    scopes.find_name_chain_type(chain, &mut item, self, false);
                    let item = ItemOrAccess::Access(Access::ExprAccessChain(
                        chain.clone(),
                        Box::new(item.unwrap()),
                    ));
                    visitor.handle_item(self, scopes, &item);
                }
                if visitor.finished() {
                    return;
                }
                if let Some(ref types) = types {
                    for t in types.iter() {
                        self.visit_type_apply(t, scopes, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                }
                for expr in exprs.value.iter() {
                    self.visit_expr(expr, scopes, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
            Exp_::Pack(ref chain, ref types, fields) => {
                let ty = scopes.find_name_chain_type(chain, &mut None, self, false);
                let item =
                    ItemOrAccess::Access(Access::ApplyType(chain.clone(), Box::new(ty.clone())));
                visitor.handle_item(self, scopes, &item);
                if visitor.finished() {
                    return;
                }
                if let Some(types) = types {
                    for t in types.iter() {
                        self.visit_type_apply(t, scopes, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                    //TODO bind type.
                }

                for f in fields.iter() {
                    self.visit_expr(&f.1, scopes, visitor);
                    if visitor.finished() {
                        return;
                    }

                    let field_type = ty.find_filed_by_name(f.0.value());
                    if let Some(field_type) = field_type {
                        let item = ItemOrAccess::Access(Access::AccessFiled(
                            f.0.clone(),
                            field_type.0.clone(),
                            field_type.1.clone(),
                        ));
                        visitor.handle_item(self, scopes, &item);
                    }
                }
            }

            Exp_::Vector(_loc, ref ty, ref exprs) => {
                if let Some(ty) = ty {
                    for t in ty.iter() {
                        self.visit_type_apply(t, scopes, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                }
                for e in exprs.value.iter() {
                    self.visit_expr(e, scopes, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }

            Exp_::IfElse(condition, then_, else_) => {
                self.visit_expr(condition, scopes, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_expr(then_, scopes, visitor);
                if visitor.finished() {
                    return;
                }
                if let Some(else_) = else_ {
                    self.visit_expr(else_.as_ref(), scopes, visitor);
                }
            }
            Exp_::While(condition, body) => {
                self.visit_expr(condition, scopes, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_expr(body.as_ref(), scopes, visitor);
            }
            Exp_::Loop(e) => {
                self.visit_expr(e.as_ref(), scopes, visitor);
            }
            Exp_::Block(b) => self.visit_block(b, scopes, visitor),
            Exp_::Lambda(_, _) => {
                log::error!("handle Lambda");
            }
            Exp_::Quant(_, _, _, _, _) => {
                log::error!("handle Quant");
            }
            Exp_::ExpList(list) => {
                for e in list.iter() {
                    self.visit_expr(e, scopes, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
            Exp_::Unit => {
                // Nothing.
            }
            Exp_::Assign(left, right) => {
                self.visit_expr(left, scopes, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_expr(right, scopes, visitor);
            }
            Exp_::Return(e) => {
                if let Some(e) = e {
                    self.visit_expr(e, scopes, visitor);
                }
            }
            Exp_::Abort(e) => self.visit_expr(e.as_ref(), scopes, visitor),
            Exp_::Break => {
                let item = ItemOrAccess::Access(Access::KeyWords("break"));
                visitor.handle_item(self, scopes, &item);
            }
            Exp_::Continue => {
                let item = ItemOrAccess::Access(Access::KeyWords("continue"));
                visitor.handle_item(self, scopes, &item);
            }
            Exp_::Dereference(x) => {
                self.visit_expr(x.as_ref(), scopes, visitor);
            }
            Exp_::UnaryExp(_, e) => {
                self.visit_expr(e.as_ref(), scopes, visitor);
            }
            Exp_::BinopExp(left, _, right) => {
                self.visit_expr(left, scopes, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_expr(right, scopes, visitor);
            }
            Exp_::Borrow(_, e) => {
                self.visit_expr(e.as_ref(), scopes, visitor);
            }
            Exp_::Dot(e, field) => {
                self.visit_expr(e.as_ref(), scopes, visitor);
                if visitor.finished() {
                    return;
                }
                let struct_ty = self.get_expr_type(e, scopes);
                let struct_ty = struct_ty.struct_ref_to_struct(scopes);
                if let Some(def_field) = struct_ty.find_filed_by_name(field.value) {
                    let item = ItemOrAccess::Access(Access::AccessFiled(
                        Field(field.clone()),
                        def_field.0.clone(),
                        def_field.1.clone(),
                    ));
                    visitor.handle_item(self, scopes, &item);
                }
            }
            Exp_::Index(_, _) => {
                log::error!("handle index.");
            }
            Exp_::Cast(e, ty) => {
                self.visit_expr(e.as_ref(), scopes, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_type_apply(ty, scopes, visitor);
            }
            Exp_::Annotate(e, ty) => {
                self.visit_expr(e.as_ref(), scopes, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_type_apply(ty, scopes, visitor);
            }
            Exp_::Spec(_) => {
                log::error!("handle Spec.");
            }
            Exp_::UnresolvedError => {
                //
            }
        }
    }

    pub(crate) fn visit_friend(
        &self,
        friend_decl: &FriendDecl,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        match &friend_decl.friend.value {
            NameAccessChain_::One(_) => {
                log::error!("access friend one")
            }
            NameAccessChain_::Two(addr, name) => {
                let addr = match &addr.value {
                    LeadingNameAccess_::AnonymousAddress(x) => x.bytes,
                    LeadingNameAccess_::Name(name) => self.name_to_addr(name.value),
                };
                let m = scopes.resolve_friend(addr, name.value);
                let m = match m {
                    Some(x) => x,
                    None => return,
                };
                let item = ItemOrAccess::Access(Access::Friend(friend_decl.friend.clone(), m));
                visitor.handle_item(self, scopes, &item);
            }
            NameAccessChain_::Three(_, _) => {
                log::error!("access friend three")
            }
        }
    }

    pub(crate) fn visit_use_decl(
        &self,
        is_global: Option<(AccountAddress, Symbol)>,
        use_decl: &UseDecl,
        scopes: &Scopes,
        visitor: Option<&mut dyn ScopeVisitor>,
    ) {
        let mut _dummy = DummyVisitor;
        let visitor = visitor.unwrap_or(&mut _dummy);
        let get_module = |module: &ModuleIdent| {
            let module_scope = scopes.visit_address(|top| -> Option<Rc<RefCell<Scope>>> {
                let x = top
                    .address
                    .get(&match &module.value.address.value {
                        LeadingNameAccess_::AnonymousAddress(num) => num.bytes,
                        LeadingNameAccess_::Name(name) => self.name_to_addr(name.value),
                    })?
                    .modules
                    .get(&module.value.module.0.value)?
                    .clone();
                Some(x)
            });
            let module_scope =
                module_scope.expect(&format!("use decl {:?} not found", use_decl.use_));
            module_scope
        };
        match &use_decl.use_ {
            Use::Module(module, alias) => {
                let module_scope = get_module(module);
                let item = ItemOrAccess::Item(Item::UseModule(
                    module.clone(),
                    alias.clone(),
                    module_scope,
                ));
                visitor.handle_item(self, scopes, &item);
                if visitor.finished() {
                    return;
                }
                let name = if let Some(alias) = alias {
                    alias.value()
                } else {
                    module.value.module.value()
                };
                if let Some((addr, module_name)) = is_global {
                    scopes.enter_top_item(self, addr, module_name, name, item);
                } else {
                    scopes.enter_item(self, name, item);
                }
            }
            Use::Members(module, members) => {
                let module_scope = get_module(module);
                for (member, alias) in members.iter() {
                    if member.value.as_str() == "Self" {
                        // Special handle for Self.
                        let item = ItemOrAccess::Item(Item::UseModule(
                            module.clone(),
                            Some(ModuleName(member.clone())),
                            module_scope.clone(),
                        ));
                        visitor.handle_item(self, scopes, &item);
                        if visitor.finished() {
                            return;
                        }
                        if let Some((addr, module_name)) = is_global {
                            scopes.enter_top_item(
                                self,
                                addr,
                                module_name,
                                module.value.module.value(),
                                item,
                            );
                        } else {
                            scopes.enter_item(self, module.value.module.value(), item);
                        }
                        continue;
                    }
                    let name = if let Some(alias) = alias {
                        alias.clone()
                    } else {
                        member.clone()
                    };
                    let item = ItemOrAccess::Item(Item::UseMember(
                        module.clone(),
                        member.clone(),
                        alias.clone(),
                        module_scope.clone(),
                    ));
                    visitor.handle_item(self, scopes, &item);
                    if visitor.finished() {
                        return;
                    }
                    if let Some((addr, module_name)) = is_global {
                        scopes.enter_top_item(self, addr, module_name, name.value, item);
                    } else {
                        scopes.enter_item(self, name.value, item);
                    }
                }
            }
        }
    }
}
