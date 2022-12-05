use super::item::*;
use super::scope::*;
use super::scopes::*;
use super::types::*;
use move_compiler::shared::Name;

use move_core_types::account_address::*;

use move_compiler::parser::ast::*;
use move_compiler::shared::Identifier;

use move_ir_types::location::Spanned;

use move_symbol_pool::Symbol;

use std::cell::RefCell;

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
            let item =
                Item::StructNameRef(addr, module_name, c.name.clone(), c.type_parameters.clone());
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
                    let item = Item::Fun(ItemFun {
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
        // enter schema.
        self.with_spec_schema(manifest, |addr, module_name, name, spec| {
            //
            let item = ItemOrAccess::Item(Item::SpecSchema(name.clone(), spec.clone()));
            visitor.handle_item(self, scopes, &item);
            scopes.enter_top_item(self, addr, module_name, name.value, item);
        });

        self.with_spec(manifest, |addr, module_name, spec| {
            let _guard = scopes.clone_scope_and_enter(addr, module_name);
            self.visit_spec(spec, scopes, visitor);
        });
    }

    pub fn visit_spec(&self, spec: &SpecBlock, scopes: &Scopes, visitor: &mut dyn ScopeVisitor) {
        let _guard = scopes.enter_scope_guard(Scope::new_spec());
        match &spec.value.target.value {
            SpecBlockTarget_::Code => {
                // Nothing to do here.
            }
            SpecBlockTarget_::Module => {
                // TODO
                // here should to jump to module definition.
                // But We don't have the loc to the `module` key words.
            }

            SpecBlockTarget_::Member(m, _) => {
                let mut item_ret = None;
                let chain = Spanned {
                    loc: m.loc,
                    value: NameAccessChain_::One(Spanned {
                        loc: m.loc,
                        value: m.value,
                    }),
                };
                scopes.find_name_chain_type(&chain, &mut item_ret, &mut None, self, false);
                if let Some(item_ret) = item_ret {
                    {
                        let item = ItemOrAccess::Access(Access::ExprAccessChain(
                            chain.clone(),
                            None,
                            Box::new(item_ret.clone()),
                        ));
                        visitor.handle_item(self, scopes, &item);
                        if visitor.finished() {
                            return;
                        }
                    }
                    match &item_ret {
                        Item::Fun(x) => {
                            for (var, ty) in x.parameters.iter() {
                                scopes.enter_item(
                                    self,
                                    var.0.value,
                                    Item::Parameter(var.clone(), ty.clone()),
                                );
                            }
                        }
                        _ => {}
                    }
                }
                // TODO why spec have function signature.
                // 看起来是重复定义，必须和函数的定义一模一样。
            }
            SpecBlockTarget_::Schema(_, _) => {
                // Handled before.
                // Nothing to do.
            } // enter parameter and ret.
        }

        for u in spec.value.uses.iter() {
            self.visit_use_decl(None, u, scopes, Some(visitor));
            if visitor.finished() {
                return;
            }
        }
        for m in spec.value.members.iter() {
            self.visit_spec_member(m, scopes, visitor);
            if visitor.finished() {
                return;
            }
        }
    }

    pub fn visit_spec_member(
        &self,
        member: &SpecBlockMember,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        match &member.value {
            SpecBlockMember_::Condition {
                kind,
                properties,
                exp,
                additional_exps,
            } => {
                let empty = vec![];
                let type_parameters = get_spec_condition_type_parameters(kind).unwrap_or(&empty);
                scopes.enter_scope(|scopes: &Scopes| {
                    // enter type parameters.
                    for (name, ab) in type_parameters.iter() {
                        let item = ItemOrAccess::Item(Item::TParam(name.clone(), ab.clone()));
                        visitor.handle_item(self, scopes, &item);

                        scopes.enter_item(self, name.value, item);
                    }
                    self.visit_expr(exp, scopes, visitor);
                    if visitor.finished() {
                        return;
                    }
                    for exp in additional_exps.iter() {
                        self.visit_expr(exp, scopes, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                });
            }
            SpecBlockMember_::Function {
                uninterpreted,
                name,
                signature,
                body,
            } => {
                self.visit_signature(signature, scopes, visitor);
                if visitor.finished() {
                    return;
                }
                let _guard = scopes.enter_scope_guard(Scope::new_fun());
                for t in signature.type_parameters.iter() {
                    self.visit_tparam(t, scopes, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
                let parameter: Vec<_> = signature
                    .parameters
                    .iter()
                    .map(|(var, ty)| (var.clone(), scopes.resolve_type(ty, self)))
                    .collect();

                let ret_ty = scopes.resolve_type(&signature.return_type, self);
                let item = Item::Fun(ItemFun {
                    name: name.clone(),
                    type_parameters: signature.type_parameters.clone(),
                    parameters: parameter.clone(),
                    ret_type: Box::new(ret_ty),
                });
                scopes.enter_item(self, name.value(), item);
                for (var, ty) in parameter {
                    scopes.enter_item(self, var.value(), Item::Parameter(var, ty));
                }
                // visit function body.
                match &body.value {
                    FunctionBody_::Defined(s) => self.visit_block(s, scopes, visitor),
                    FunctionBody_::Native => {}
                }
            }

            SpecBlockMember_::Variable {
                is_global,
                name,
                type_parameters,
                type_: ty,
                init,
            } => {
                self.visit_type_apply(ty, scopes, visitor);
                if visitor.finished() {
                    return;
                }
                let ty = scopes.resolve_type(&ty, self);
                let item = ItemOrAccess::Item(Item::Var(Var(name.clone()), ty));
                visitor.handle_item(self, scopes, &item);
                scopes.enter_item(self, name.value, item);
                if let Some(init) = init {
                    self.visit_expr(init, scopes, visitor);
                }
            }

            SpecBlockMember_::Let {
                name,
                post_state,
                def,
            } => {
                let ty = self.get_expr_type(&def, scopes);
                let item = ItemOrAccess::Item(Item::Var(Var(name.clone()), ty));
                visitor.handle_item(self, scopes, &item);
                scopes.enter_item(self, name.value, item);
                self.visit_expr(def, scopes, visitor);
            }
            SpecBlockMember_::Update { lhs, rhs } => {
                self.visit_expr(&lhs, scopes, visitor);
                self.visit_expr(&rhs, scopes, visitor);
            }
            SpecBlockMember_::Include { properties, exp } => {
                // TODO.
                let chain = match &exp.value {
                    Exp_::Name(chain, _) => Some(chain),
                    _ => None,
                };
                match chain {
                    Some(chain) => {
                        let mut item_ret = None;
                        let mut module_ret = None;
                        scopes.find_name_chain_type(
                            chain,
                            &mut item_ret,
                            &mut module_ret,
                            self,
                            false,
                        );
                        if let Some(item_ret) = item_ret.clone() {
                            let item = ItemOrAccess::Access(Access::ExprAccessChain(
                                chain.clone(),
                                module_ret,
                                Box::new(item_ret),
                            ));
                            visitor.handle_item(self, scopes, &item);
                            if visitor.finished() {
                                return;
                            }
                        }
                        match &item_ret {
                            Some(x) => match x {
                                // TODO should I visit spec block.
                                Item::SpecSchema(_, block) => {
                                    self.visit_spec(block, scopes, visitor);
                                }
                                _ => {}
                            },
                            None => {}
                        }
                    }
                    None => {}
                }
            }

            SpecBlockMember_::Apply {
                exp,
                patterns,
                exclusion_patterns,
            } => {
                // TODO.
                let rule = match &exp.value {
                    Exp_::Name(chain, _) => Some(chain),
                    _ => None,
                };
                if let Some(rule) = rule {
                    let mut item_ret = None;
                    scopes.find_name_chain_type(&rule, &mut item_ret, &mut None, self, false);
                    match &item_ret {
                        Some(x) => match x {
                            Item::SpecSchema(name, _) => {
                                let item = ItemOrAccess::Access(Access::IncludeSchema(
                                    rule.clone(),
                                    name.clone(),
                                ));
                                visitor.handle_item(self, scopes, &item);
                            }
                            _ => {}
                        },
                        None => {}
                    }
                }

                for x in patterns.iter().chain(exclusion_patterns.iter()) {
                    for x in x.value.name_pattern.iter() {
                        match &x.value {
                            SpecApplyFragment_::Wildcard => {}
                            SpecApplyFragment_::NamePart(name) => {
                                let mut item_ret = None;
                                let mut module_ret = None;
                                let chain = Spanned {
                                    loc: name.loc,
                                    value: NameAccessChain_::One(name.clone()),
                                };
                                scopes.find_name_chain_type(
                                    &chain,
                                    &mut item_ret,
                                    &mut module_ret,
                                    self,
                                    false,
                                );
                                if let Some(x) = item_ret {
                                    let item = ItemOrAccess::Access(Access::ExprAccessChain(
                                        chain.clone(),
                                        module_ret,
                                        Box::new(x),
                                    ));
                                    visitor.handle_item(self, scopes, &item);
                                    if visitor.finished() {
                                        return;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            SpecBlockMember_::Pragma { properties } => {
                // https://github.com/move-language/move/blob/main/language/move-prover/doc/user/spec-lang.md#pragmas-and-properties
                // TODO Does't create a local variable all something,handle this later.
            }
        }
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
                let struct_ty =
                    scopes.find_name_chain_type(chain, &mut None, &mut None, self, false);
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
                let ty =
                    scopes.find_name_chain_type(chain.as_ref(), &mut None, &mut None, self, true);
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
                    &mut None,
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
                scopes.find_name_chain_type(chain, &mut item, &mut None, self, false);
                let item = ItemOrAccess::Access(Access::ExprAccessChain(
                    chain.clone(),
                    None,
                    Box::new(item.unwrap()),
                ));
                visitor.handle_item(self, scopes, &item);
            }
            Exp_::Call(ref chain, is_macro, ref types, ref exprs) => {
                if *is_macro {
                    let c = MacroCall::from_chain(chain);
                    let item = ItemOrAccess::Access(Access::MacroCall(c, chain.clone()));
                    visitor.handle_item(self, scopes, &item);
                } else if let Some(b) = {
                    let x = MoveBuildInFun::from_chain(chain);
                    //TODO should under_function have this build in function.
                    x
                } {
                    let mut item = ItemOrAccess::Access(Access::MoveBuildInFun(b, chain.clone()));
                    visitor.handle_item(self, scopes, &item);
                    if visitor.finished() {
                        return;
                    }
                } else if let Some(b) = {
                    let x = SpecBuildInFun::from_chain(chain);
                    x.map(|x| scopes.under_function().map(|_| x)).flatten()
                } {
                    let mut item = ItemOrAccess::Access(Access::SpecBuildInFun(b, chain.clone()));
                    visitor.handle_item(self, scopes, &item);
                    if visitor.finished() {
                        return;
                    }
                } else {
                    let mut item = Some(Item::new_dummy());
                    let mut module = None;
                    scopes.find_name_chain_type(chain, &mut item, &mut module, self, false);
                    let item = ItemOrAccess::Access(Access::ExprAccessChain(
                        chain.clone(),
                        module,
                        Box::new(item.unwrap()),
                    ));
                    visitor.handle_item(self, scopes, &item);
                    if visitor.finished() {
                        return;
                    }
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
                let ty = scopes.find_name_chain_type(chain, &mut None, &mut None, self, false);
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
                // TODO have lambda expression  in ast structure.
                // But I don't find in msl spec.
                // for bind in binds.value.iter() {
                //     self.visit_bind(bind, &ResolvedType::UnKnown, scopes, visitor);
                //     if visitor.finished() {
                //         return;
                //     }
                // }
                // self.visit_expr(expr.as_ref(), scopes, visitor);
                log::error!("lambda expression in ast.");
            }

            Exp_::Quant(_, binds, bodies, where_, result) => {
                // TODO look list t can be use a type alias.
                // forall t: type, addr: address where exists<R<t>>(addr): exists<T<t>>(addr)
                scopes.enter_scope(|scopes| {
                    for bind_expr in binds.value.iter() {
                        let bind = &bind_expr.value.0;
                        let expr = &bind_expr.value.1;
                        let ty = self.get_expr_type(&expr, scopes);
                        let is_spec_domain: bool = match &expr.value {
                            Exp_::Call(chain, _, _, _) => match &chain.value {
                                NameAccessChain_::One(name) => {
                                    if name.value.as_str() == SPEC_DOMAIN {
                                        true
                                    } else {
                                        false
                                    }
                                }
                                NameAccessChain_::Two(_, _) => false,
                                NameAccessChain_::Three(_, _) => false,
                            },
                            _ => false,
                        };
                        match &bind.value {
                            Bind_::Var(var) => {
                                let ty = if is_spec_domain {
                                    ty
                                } else if let Some(vec_ty) = ty.is_vector() {
                                    vec_ty.clone()
                                } else if let Some(_) = ty.is_range() {
                                    ResolvedType::new_build_in(BuildInType::NumType)
                                } else {
                                    log::error!("bind the wrong type");
                                    ty
                                };
                                let item = ItemOrAccess::Item(Item::Parameter(var.clone(), ty));
                                visitor.handle_item(self, scopes, &item);
                                scopes.enter_item(self, var.value(), item);
                            }
                            _ => {
                                // Not supported,according to the msl spec.
                            }
                        }
                        // bodies
                        for body in bodies.iter() {
                            scopes.enter_scope(|scopes| {
                                for exp in body.iter() {
                                    self.visit_expr(exp, scopes, visitor);
                                }
                            });
                        }
                        //
                        if let Some(exp) = where_ {
                            self.visit_expr(exp.as_ref(), scopes, visitor);
                        };
                        self.visit_expr(result.as_ref(), scopes, visitor);
                    }
                });
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
            Exp_::Index(e, index) => {
                self.visit_expr(e.as_ref(), scopes, visitor);
                self.visit_expr(index.as_ref(), scopes, visitor)
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
            Exp_::Spec(spec) => {
                self.visit_spec(spec, scopes, visitor);
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

pub(crate) const SPEC_DOMAIN: &str = "$spec_domain";

fn get_spec_condition_type_parameters(x: &SpecConditionKind) -> Option<&Vec<(Name, Vec<Ability>)>> {
    match &x.value {
        SpecConditionKind_::Invariant(x)
        | SpecConditionKind_::InvariantUpdate(x)
        | SpecConditionKind_::Axiom(x) => Some(x),
        _ => None,
    }
}
