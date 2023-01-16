use super::item::*;
use super::modules::*;
use super::scope::*;
use super::scopes::*;
use super::types::*;
use move_compiler::parser::ast::*;
use move_compiler::shared::Identifier;
use move_compiler::shared::Name;
use move_core_types::account_address::*;
use move_ir_types::location::Spanned;
use move_package::source_package::layout::SourcePackageLayout;
use move_symbol_pool::Symbol;
use std::cell::RefCell;
use std::collections::HashMap;
use std::vec;
use std::{path::PathBuf, rc::Rc};

impl Modules {
    pub fn visit_modules_or_tests(
        &self,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
        provider: impl AstProvider,
    ) {
        provider.with_module(|addr, module_def| {
            let item = ItemOrAccess::Item(Item::ModuleName(module_def.name));
            visitor.handle_item_or_access(self, scopes, &item);
            if !module_def.is_spec_module {
                scopes.set_up_module(
                    addr,
                    module_def.name,
                    provider.found_in_test()
                        || attributes_has_test(&module_def.attributes).is_test(),
                );
            }
        });

        provider.with_const(|addr, name, c| {
            self.visit_const(Some((addr, name)), c, scopes, visitor);
        });
        provider.with_struct(|addr, module_name, c| {
            let item = Item::StructNameRef(ItemStructNameRef {
                addr,
                module_name,
                name: c.name.clone(),
                type_parameters: c.type_parameters.clone(),
                is_test: attributes_has_test(&c.attributes).is_test(),
            });
            scopes.enter_top_item(self, addr, module_name, c.name.0.value, item, false);
        });

        provider.with_use_decl(|addr, module_name, u, is_spec_module| {
            self.visit_use_decl(Some((addr, module_name)), u, scopes, None, is_spec_module)
        });

        provider.with_struct(|addr, module_name, s| {
            let _guard = scopes.clone_scope_and_enter(addr, module_name, false);
            scopes.enter_scope(|scopes| {
                scopes.set_is_test(
                    provider.found_in_test()
                        || scopes.module_is_test(addr, module_name).unwrap_or_default(),
                );

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
                                visitor.handle_item_or_access(self, scopes, &item);
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
                let item = ItemOrAccess::Item(Item::Struct(ItemStruct {
                    name: s.name,
                    type_parameters: s.type_parameters.clone(),
                    type_parameters_ins: vec![],
                    fields,
                    is_test: attributes_has_test(&s.attributes).is_test(),
                }));
                visitor.handle_item_or_access(self, scopes, &item);
                scopes.enter_top_item(self, addr, module_name, s.name.value(), item, false)
            });
        });

        let enter_function = |modules: &Modules,
                              f: &Function,
                              scopes: &Scopes,
                              visitor: &mut dyn ScopeVisitor,
                              addr: AccountAddress,
                              module_name: Symbol,
                              is_spec_module: bool,
                              is_spec: bool| {
            // This enter scope make sure the visit_tparam cannot override some module level item.
            scopes.enter_scope(|scopes| {
                scopes.set_is_test(
                    provider.found_in_test()
                        || scopes.module_is_test(addr, module_name).unwrap_or_default(),
                );
                scopes.set_is_spec(is_spec_module || is_spec);

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
                    ret_type_unresolved: s.return_type.clone(),
                    is_spec,
                    vis: f.visibility.clone(),
                    addr_and_name: AddrAndModuleName {
                        addr: addr.clone(),
                        name: ModuleName(Spanned {
                            loc: UNKNOWN_LOC,
                            value: module_name,
                        }),
                    },
                    is_test: attributes_has_test(&f.attributes),
                });
                let item = ItemOrAccess::Item(item);
                visitor.handle_item_or_access(modules, scopes, &item);
                scopes.enter_top_item(
                    self,
                    addr,
                    module_name,
                    f.name.value(),
                    item,
                    is_spec_module,
                );
            });
        };

        provider.with_function(|addr, module_name, f| {
            // This clone scope make sure we can visit module level item.
            let _guard = scopes.clone_scope_and_enter(addr, module_name, false);
            scopes.set_is_test(
                provider.found_in_test()
                    || scopes.module_is_test(addr, module_name).unwrap_or_default()
                    || attributes_has_test(&f.attributes).is_test(),
            );
            enter_function(self, f, scopes, visitor, addr, module_name, false, false);
        });

        // visit use decl again.
        provider.with_use_decl(|addr, module_name, u, is_spec_module| {
            scopes.set_current_addr_and_module_name(addr, module_name);
            scopes.set_is_test(
                provider.found_in_test()
                    || scopes.module_is_test(addr, module_name).unwrap_or_default(),
            );
            scopes.set_is_spec(is_spec_module);
            self.visit_use_decl(
                Some((addr, module_name)),
                u,
                scopes,
                Some(visitor),
                is_spec_module,
            );
        });

        provider.with_friend(|addr, module_name, f| {
            scopes.set_current_addr_and_module_name(addr, module_name);
            self.visit_friend(f, addr, module_name, scopes, visitor);
        });

        if !visitor.visit_fun_or_spec_body() {
            //
            return;
        }
        // visit function body.
        provider.with_function(|addr, module_name, f| {
            scopes.set_current_addr_and_module_name(addr, module_name);
            scopes.set_is_test(
                provider.found_in_test()
                    || scopes.module_is_test(addr, module_name).unwrap_or_default()
                    || attributes_has_test(&f.attributes).is_test(),
            );
            let range = self.convert_loc_range(&f.loc);
            if range.is_none() {
                return;
            }
            if !visitor.function_or_spec_body_should_visit(range.as_ref().unwrap()) {
                return;
            }
            let _guard = scopes.clone_scope_and_enter(addr, module_name, false);
            self.visit_function(f, scopes, visitor);
        });
        // enter schema.
        provider.with_spec_schema(|addr, module_name, name, spec, is_spec_module| {
            let item = ItemOrAccess::Item(Item::SpecSchema(
                name.clone(),
                self.collect_spec_schema_fields(scopes, &spec.value.members),
            ));
            visitor.handle_item_or_access(self, scopes, &item);
            scopes.enter_top_item(self, addr, module_name, name.value, item, is_spec_module);
        });

        provider.with_spec(|addr, module_name, spec, is_spec_module| {
            scopes.set_current_addr_and_module_name(addr, module_name);
            scopes.set_is_spec(true);
            match &spec.value.target.value {
                SpecBlockTarget_::Module => {}
                _ => return,
            };
            for m in spec.value.members.iter() {
                match &m.value {
                    SpecBlockMember_::Function {
                        uninterpreted: _uninterpreted,
                        name,
                        signature,
                        body,
                    } => {
                        let f = Function {
                            attributes: vec![],
                            loc: m.loc,
                            visibility: Visibility::Internal,
                            entry: None,
                            signature: signature.clone(),
                            acquires: vec![],
                            name: name.clone(),
                            body: body.clone(),
                        };
                        enter_function(
                            self,
                            &f,
                            scopes,
                            visitor,
                            addr,
                            module_name,
                            is_spec_module,
                            true,
                        );
                    }
                    _ => {}
                }
            }
        });

        provider.with_spec(|addr, module_name, spec, is_spec_module| {
            scopes.set_current_addr_and_module_name(addr, module_name);
            scopes.set_is_test(false);
            scopes.set_is_spec(true);
            let range = self.convert_loc_range(&spec.loc);
            if range.is_none() {
                return;
            }
            if !visitor.function_or_spec_body_should_visit(range.as_ref().unwrap()) {
                return;
            }
            let _guard = scopes.clone_scope_and_enter(addr, module_name, is_spec_module);
            self.visit_spec(spec, scopes, visitor);
        });
    }

    pub(crate) fn visit_scripts(
        &self,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
        provider: impl AstProvider,
    ) {
        provider.with_script(|script| {
            scopes.enter_scope(|scopes| {
                for u in script.uses.iter() {
                    self.visit_use_decl(None, u, scopes, Some(visitor), false);
                    if visitor.finished() {
                        return;
                    }
                }
                for c in script.constants.iter() {
                    self.visit_const(None, c, scopes, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
                for c in script.specs.iter() {
                    self.visit_spec(c, scopes, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
                self.visit_function(&script.function, scopes, visitor);
            })
        });
    }

    pub fn visit_spec(&self, spec: &SpecBlock, scopes: &Scopes, visitor: &mut dyn ScopeVisitor) {
        let _guard = scopes.enter_scope_guard(Scope::default());
        match &spec.value.target.value {
            SpecBlockTarget_::Code => {
                // Nothing to do here.
            }
            SpecBlockTarget_::Module => {
                // TODO
                // here should to jump to module definition.
                // But We don't have the loc to the `module` key words.
            }
            SpecBlockTarget_::Member(m, signature) => {
                let chain = Spanned {
                    loc: m.loc,
                    value: NameAccessChain_::One(Spanned {
                        loc: m.loc,
                        value: m.value,
                    }),
                };
                let (item_ret, _) = scopes.find_name_chain_item(&chain, self);
                {
                    // TODO this may not be expr.
                    // You can write spec for struct.
                    let item = ItemOrAccess::Access(Access::SpecFor(
                        m.clone(),
                        Box::new(item_ret.clone().unwrap_or_default()),
                    ));
                    visitor.handle_item_or_access(self, scopes, &item);
                    if visitor.finished() {
                        return;
                    }
                }

                if let Some(item_ret) = item_ret.as_ref() {
                    match &item_ret {
                        Item::Fun(x) => {
                            for (var, ty) in x.parameters.iter() {
                                scopes.enter_item(
                                    self,
                                    var.0.value,
                                    Item::Parameter(var.clone(), ty.clone()),
                                );
                            }
                            for (var, ty) in x.type_parameters.iter() {
                                scopes.enter_types(
                                    self,
                                    var.value,
                                    Item::TParam(var.clone(), ty.clone()),
                                );
                            }
                            // enter result.
                            let false_multi = vec![x.ret_type_unresolved.clone()];
                            match x.ret_type.as_ref() {
                                ResolvedType::Multiple(tys) => {
                                    let mut index = 1;
                                    for (ty1, ty2) in tys.iter().zip(
                                        match &x.ret_type_unresolved.value {
                                            Type_::Multiple(tys2) => tys2,
                                            _ => &false_multi,
                                        }
                                        .iter(),
                                    ) {
                                        let s = Symbol::from(format!("result_{}", index).as_str());
                                        scopes.enter_item(
                                            self,
                                            s,
                                            Item::Var(
                                                Var(Spanned {
                                                    loc: ty2.loc,
                                                    value: s,
                                                }),
                                                ty1.clone(),
                                            ),
                                        );
                                        index = index + 1;
                                    }
                                }

                                _ => {
                                    scopes.enter_item(
                                        self,
                                        Symbol::from("result"),
                                        Item::Var(
                                            Var(Spanned {
                                                loc: x.ret_type_unresolved.loc,
                                                value: Symbol::from("result"),
                                            }),
                                            *x.ret_type.clone(),
                                        ),
                                    );
                                }
                            }
                        }
                        Item::Struct(x) => {
                            for f in x.fields.iter() {
                                scopes.enter_item(
                                    self,
                                    f.0.value(),
                                    Item::Var(Var(f.0 .0.clone()), f.1.clone()),
                                );
                            }
                        }
                        Item::StructNameRef(ItemStructNameRef {
                            addr,
                            module_name,
                            name,
                            ..
                        }) => {
                            log::error!(
                                "struct name ref not handle:{} {} {}",
                                addr,
                                module_name.as_str(),
                                name.0.value.as_str()
                            );
                        }
                        _ => {}
                    }
                }
                // TODO why spec have function signature.
                // 看起来是重复定义，必须和函数的定义一模一样。
                if let Some(signature) = signature {
                    self.visit_signature(signature.as_ref(), scopes, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
            SpecBlockTarget_::Schema(_, type_parameters) => {
                for t in type_parameters.iter() {
                    self.visit_tparam(t, scopes, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            } // enter parameter and ret.
        }

        for u in spec.value.uses.iter() {
            self.visit_use_decl(
                None,
                u,
                scopes,
                Some(visitor),
                true, /*  here false or true doesn't matter. */
            );
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

    /// Collect field name like a in  spec schema IncrementAborts {
    ///                   a: address;
    /// }
    pub(crate) fn collect_spec_schema_fields(
        &self,
        scopes: &Scopes,
        members: &Vec<SpecBlockMember>,
    ) -> HashMap<Symbol, (Name, ResolvedType)> {
        let mut ret = HashMap::new();
        for member in members.iter() {
            match &member.value {
                SpecBlockMember_::Variable {
                    is_global: _is_global,
                    name,
                    type_parameters: _type_parameters,
                    type_,
                    init,
                } => {
                    let mut ty = scopes.resolve_type(type_, self);
                    if ty.is_err() && init.is_some() {
                        ty = self.get_expr_type(init.as_ref().unwrap(), scopes);
                    }
                    ret.insert(name.value, (name.clone(), ty));
                }
                _ => {}
            }
        }
        ret
    }

    pub fn visit_spec_member(
        &self,
        member: &SpecBlockMember,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        log::trace!("visit spec member:{:?}", member);
        match &member.value {
            SpecBlockMember_::Condition {
                kind,
                properties: _properties,
                exp,
                additional_exps,
            } => {
                let empty = vec![];
                let type_parameters = get_spec_condition_type_parameters(kind).unwrap_or(&empty);
                scopes.enter_scope(|scopes: &Scopes| {
                    // enter type parameters.
                    for (name, ab) in type_parameters.iter() {
                        let item = ItemOrAccess::Item(Item::TParam(name.clone(), ab.clone()));
                        visitor.handle_item_or_access(self, scopes, &item);
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
                uninterpreted: _uninterpreted,
                name,
                signature,
                body,
            } => {
                self.visit_signature(signature, scopes, visitor);
                if visitor.finished() {
                    return;
                }
                let _guard = scopes.enter_scope_guard(Scope::default());
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
                    ret_type_unresolved: signature.return_type.clone(),
                    is_spec: true,
                    vis: Visibility::Internal,
                    addr_and_name: scopes.get_current_addr_and_module_name(),
                    is_test: IsFunTest::No,
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
                is_global: _is_global,
                name,
                type_parameters: _type_parameters,
                type_: ty,
                init,
            } => {
                self.visit_type_apply(ty, scopes, visitor);
                if visitor.finished() {
                    return;
                }
                let ty = scopes.resolve_type(&ty, self);
                let item = ItemOrAccess::Item(Item::Var(Var(name.clone()), ty));
                visitor.handle_item_or_access(self, scopes, &item);
                scopes.enter_item(self, name.value, item);
                if let Some(init) = init {
                    self.visit_expr(init, scopes, visitor);
                }
            }

            SpecBlockMember_::Let {
                name,
                post_state: _post_state,
                def,
            } => {
                let ty = self.get_expr_type(&def, scopes);
                let item = ItemOrAccess::Item(Item::Var(Var(name.clone()), ty));
                visitor.handle_item_or_access(self, scopes, &item);
                scopes.enter_item(self, name.value, item);
                self.visit_expr(def, scopes, visitor);
            }

            SpecBlockMember_::Update { lhs, rhs } => {
                self.visit_expr(&lhs, scopes, visitor);
                self.visit_expr(&rhs, scopes, visitor);
            }
            SpecBlockMember_::Include {
                properties: _properties,
                exp,
            } => {
                // TODO handle _properties
                // TODO.
                match &exp.value {
                    Exp_::Name(chain, type_args) => {
                        let (item_ret, module_ret) = scopes.find_name_chain_item(chain, self);
                        let item = ItemOrAccess::Access(Access::ExprAccessChain(
                            chain.clone(),
                            module_ret,
                            Box::new(item_ret.unwrap_or_default()),
                        ));
                        visitor.handle_item_or_access(self, scopes, &item);
                        if visitor.finished() {
                            return;
                        }
                        if let Some(type_args) = type_args {
                            for t in type_args.iter() {
                                self.visit_type_apply(t, scopes, visitor);
                                if visitor.finished() {
                                    return;
                                }
                            }
                        }
                    }
                    Exp_::Pack(chain, type_args, fields) => {
                        let (item_ret, module_ret) = scopes.find_name_chain_item(chain, self);
                        let item = ItemOrAccess::Access(Access::ExprAccessChain(
                            chain.clone(),
                            module_ret,
                            Box::new(item_ret.clone().unwrap_or_default()),
                        ));
                        visitor.handle_item_or_access(self, scopes, &item);
                        if visitor.finished() {
                            return;
                        }
                        if let Some(type_args) = type_args {
                            for t in type_args.iter() {
                                self.visit_type_apply(t, scopes, visitor);
                                if visitor.finished() {
                                    return;
                                }
                            }
                        }
                        {
                            let all_fields = item_ret
                                .as_ref()
                                .map(|x| match x {
                                    Item::SpecSchema(_, x) => Some(x.clone()),
                                    _ => None,
                                })
                                .flatten()
                                .unwrap_or(Default::default());

                            for (f, e) in fields.iter() {
                                // TODO can jump to the schema where define this field??.
                                self.visit_expr(e, scopes, visitor);
                                if visitor.finished() {
                                    return;
                                }

                                if let Some((f2, ty)) = all_fields.get(&f.value()) {
                                    let item =
                                        ItemOrAccess::Access(Access::AccessFiled(AccessFiled {
                                            from: f.clone(),
                                            to: Field(f2.clone()),
                                            ty: ty.clone(),
                                            all_fields: all_fields.clone(),
                                            item: None,
                                        }));
                                    visitor.handle_item_or_access(self, scopes, &item);
                                    if visitor.finished() {
                                        return;
                                    }
                                } else {
                                    let item =
                                        ItemOrAccess::Access(Access::AccessFiled(AccessFiled {
                                            from: f.clone(),
                                            to: f.clone(),
                                            ty: ResolvedType::UnKnown,
                                            all_fields: all_fields.clone(),
                                            item: None,
                                        }));
                                    visitor.handle_item_or_access(self, scopes, &item);
                                    if visitor.finished() {
                                        return;
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                };
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
                    let (item_ret, _) = scopes.find_name_chain_item(&rule, self);
                    match &item_ret {
                        Some(x) => match x {
                            Item::SpecSchema(name, _) => {
                                let item = ItemOrAccess::Access(Access::IncludeSchema(
                                    rule.clone(),
                                    name.clone(),
                                ));
                                visitor.handle_item_or_access(self, scopes, &item);
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
                                let chain = Spanned {
                                    loc: name.loc,
                                    value: NameAccessChain_::One(name.clone()),
                                };
                                let (item_ret, module_ret) =
                                    scopes.find_name_chain_item(&chain, self);
                                if let Some(x) = item_ret {
                                    let item = ItemOrAccess::Access(Access::ExprAccessChain(
                                        chain.clone(),
                                        module_ret,
                                        Box::new(x),
                                    ));
                                    visitor.handle_item_or_access(self, scopes, &item);
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
                for p in properties.iter() {
                    let item = ItemOrAccess::Access(Access::PragmaProperty(p.clone()));
                    visitor.handle_item_or_access(self, scopes, &item);
                    if visitor.finished() {
                        return;
                    }
                }
            }
        }
    }

    /// Entrance for `ScopeVisitor` base on analyze.
    pub fn run_full_visitor(&self, visitor: &mut dyn ScopeVisitor) {
        log::info!("run visitor for {} ", visitor);
        // visit should `rev`.
        let manifests: Vec<_> = self
            .manifest_paths
            .iter()
            .rev()
            .map(|x| x.clone())
            .collect();
        for m in manifests.iter() {
            self.visit_modules_or_tests(
                &self.scopes,
                visitor,
                ModulesAstProvider::new(self, m.clone(), SourcePackageLayout::Sources),
            );
            if visitor.finished() {
                return;
            }
            self.visit_modules_or_tests(
                &self.scopes,
                visitor,
                ModulesAstProvider::new(self, m.clone(), SourcePackageLayout::Tests),
            );
            if visitor.finished() {
                return;
            }
            self.visit_scripts(
                &self.scopes,
                visitor,
                ModulesAstProvider::new(self, m.clone(), SourcePackageLayout::Scripts),
            );
        }
    }

    pub(crate) fn get_defs(
        &self,
        manifest: &PathBuf,
        filename: &PathBuf,
        layout: SourcePackageLayout,
    ) -> VecDefAstProvider {
        let defs = if layout == SourcePackageLayout::Sources {
            self.modules
                .get(manifest)
                .unwrap()
                .sources
                .get(filename)
                .unwrap()
        } else if layout == SourcePackageLayout::Tests {
            self.modules
                .get(manifest)
                .unwrap()
                .tests
                .get(filename)
                .unwrap()
        } else if layout == SourcePackageLayout::Scripts {
            self.modules
                .get(manifest)
                .unwrap()
                .scripts
                .get(filename)
                .unwrap()
        } else {
            unreachable!()
        };
        VecDefAstProvider::new(defs, self, layout)
    }
    pub fn run_visitor_for_file(
        &self,
        visitor: &mut dyn ScopeVisitor,
        manifest: &PathBuf,
        filename: &PathBuf,
        layout: SourcePackageLayout,
    ) {
        log::info!("run visitor part for {} ", visitor);
        let provider = self.get_defs(manifest, filename, layout);
        self.visit_modules_or_tests(&self.scopes, visitor, provider.clone());
        self.visit_scripts(&self.scopes, visitor, provider);
    }

    pub(crate) fn visit_const(
        &self,
        enter_top: Option<(AccountAddress, Symbol)>,
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

        let item = ItemOrAccess::Item(Item::Const(ItemConst {
            name: c.name.clone(),
            ty,
            is_test: attributes_has_test(&c.attributes).is_test(),
        }));
        visitor.handle_item_or_access(self, scopes, &item);
        let item: Item = item.into();
        if let Some((address, module)) = enter_top {
            scopes.enter_top_item(self, address, module, c.name.value(), item.clone(), false);
        } else {
            scopes.enter_item(self, c.name.value(), item);
        }
    }
    pub(crate) fn get_module_addr(
        &self,
        addr: Option<LeadingNameAccess>,
        m: &ModuleDefinition,
    ) -> AccountAddress {
        match addr {
            Some(x) => match x.value {
                LeadingNameAccess_::AnonymousAddress(x) => x.bytes,
                LeadingNameAccess_::Name(name) => self.name_to_addr_impl(name.value),
            },
            None => match m.address {
                Some(x) => match x.value {
                    LeadingNameAccess_::AnonymousAddress(x) => x.bytes,
                    LeadingNameAccess_::Name(name) => self.name_to_addr_impl(name.value),
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
                self.visit_use_decl(None, u, scopes, Some(visitor), false);
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
        if let Some(expr) = expr {
            self.visit_expr(expr.as_ref(), scopes, visitor);
            if visitor.finished() {
                return;
            }
        }
        let ty = if let Some(ty) = ty {
            self.visit_type_apply(ty, scopes, visitor);
            if visitor.finished() {
                return;
            }
            scopes.resolve_type(ty, self)
        } else if let Some(expr) = expr {
            let ty = self.get_expr_type(expr, scopes);
            ty
        } else {
            ResolvedType::UnKnown
        };
        for (index, bind) in bind_list.value.iter().enumerate() {
            let ty = ty.nth_ty(index);
            let unknown = ResolvedType::UnKnown;
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
                visitor.handle_item_or_access(self, scopes, &item);
                if visitor.finished() {
                    return;
                }
                scopes.enter_item(self, var.0.value, item);
                return;
            }
            Bind_::Unpack(chain, tys, field_binds) => {
                self.visit_type_apply(
                    &Spanned {
                        loc: chain.loc,
                        value: Type_::Apply(chain.clone(), vec![]),
                    },
                    scopes,
                    visitor,
                );
                let (struct_ty, _) = scopes.find_name_chain_item(chain, self);
                let struct_ty = struct_ty.unwrap_or_default().to_type().unwrap_or_default();
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
                        name: _name,
                        type_parameters,
                        type_parameters_ins: _type_parameters_ins,
                        fields: _fields,
                        is_test: _is_test,
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
                            let field_and_ty = struct_ty.find_filed_by_name(field.0.value);
                            {
                                let item = ItemOrAccess::Access(Access::AccessFiled(AccessFiled {
                                    from: field.clone(),
                                    to: if let Some(x) = field_and_ty {
                                        x.0.clone()
                                    } else {
                                        field.clone()
                                    },
                                    ty: if let Some(x) = field_and_ty {
                                        x.1.clone()
                                    } else {
                                        ResolvedType::UnKnown
                                    },
                                    all_fields: struct_ty.all_fields(),
                                    item: None,
                                }));
                                visitor.handle_item_or_access(self, scopes, &item);
                                if visitor.finished() {
                                    return;
                                }
                            }
                            if let Some(field_ty) = field_and_ty {
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
                let ty = scopes.resolve_type(ty, self);
                let (_, module) = scopes.find_name_chain_item(chain, self);
                let item = ItemOrAccess::Access(Access::ApplyType(
                    chain.as_ref().clone(),
                    module.map(|x| x.name.clone()),
                    Box::new(ty),
                ));
                visitor.handle_item_or_access(self, scopes, &item);
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
                    visitor.handle_item_or_access(self, scopes, &item);
                }
            }
            Exp_::Move(var) | Exp_::Copy(var) => {
                let item = scopes.find_var(var.value());
                let item = ItemOrAccess::Access(Access::ExprVar(
                    var.clone(),
                    Box::new(item.unwrap_or_default()),
                ));
                visitor.handle_item_or_access(self, scopes, &item);
            }
            Exp_::Name(chain, tys) => {
                // let's try.
                if let Some(tys) = tys {
                    for ty in tys.iter() {
                        self.visit_type_apply(ty, scopes, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                }
                let (item, module) = scopes.find_name_chain_item(chain, self);
                let item = ItemOrAccess::Access(Access::ExprAccessChain(
                    chain.clone(),
                    module,
                    Box::new(item.unwrap_or_default()),
                ));
                visitor.handle_item_or_access(self, scopes, &item);
                if visitor.finished() {
                    return;
                }
            }

            Exp_::Call(ref chain, is_macro, ref types, ref exprs) => {
                if *is_macro {
                    let c = MacroCall::from_chain(chain).unwrap_or_default();
                    let item = ItemOrAccess::Access(Access::MacroCall(c, chain.clone()));
                    visitor.handle_item_or_access(self, scopes, &item);
                } else {
                    let (item, module) = scopes.find_name_chain_item(chain, self);
                    let item = ItemOrAccess::Access(Access::ExprAccessChain(
                        chain.clone(),
                        module,
                        Box::new(item.unwrap_or_default()),
                    ));
                    visitor.handle_item_or_access(self, scopes, &item);
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
                self.visit_type_apply(
                    &Spanned {
                        loc: chain.loc,
                        value: Type_::Apply(Box::new(chain.clone()), vec![]),
                    },
                    scopes,
                    visitor,
                );
                if visitor.finished() {
                    return;
                }
                let (ty, _) = scopes.find_name_chain_item(chain, self);
                let ty = ty.unwrap_or_default().to_type().unwrap_or_default();
                if let Some(types) = types {
                    for t in types.iter() {
                        self.visit_type_apply(t, scopes, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                }
                for f in fields.iter() {
                    let field_type = ty.find_filed_by_name(f.0.value());
                    let all_fields = ty.all_fields();
                    let item = match &f.1.value {
                        Exp_::Name(chain, _) => match &chain.value {
                            NameAccessChain_::One(x) => {
                                if x.value.as_str() == f.0.value().as_str() {
                                    let (item, _) = scopes.find_name_chain_item(chain, self);
                                    item
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        },
                        _ => None,
                    };
                    if let Some(field_type) = field_type {
                        let item = ItemOrAccess::Access(Access::AccessFiled(AccessFiled {
                            from: f.0.clone(),
                            to: field_type.0.clone(),
                            ty: field_type.1.clone(),
                            all_fields,
                            item,
                        }));
                        visitor.handle_item_or_access(self, scopes, &item);
                    } else {
                        let item = ItemOrAccess::Access(Access::AccessFiled(AccessFiled {
                            from: f.0.clone(),
                            to: f.0.clone(),
                            ty: ResolvedType::UnKnown,
                            all_fields,
                            item,
                        }));
                        visitor.handle_item_or_access(self, scopes, &item);
                    }
                    if visitor.finished() {
                        return;
                    }
                    self.visit_expr(&f.1, scopes, visitor);
                    if visitor.finished() {
                        return;
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
                                    log::error!("bind the wrong type:{}", ty);
                                    ty
                                };
                                let item = ItemOrAccess::Item(Item::Parameter(var.clone(), ty));
                                visitor.handle_item_or_access(self, scopes, &item);
                                scopes.enter_item(self, var.value(), item);
                            }
                            _ => {
                                // Not supported,according to the msl spec.
                            }
                        }
                        self.visit_expr(expr, scopes, visitor);
                        if visitor.finished() {
                            return;
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
                visitor.handle_item_or_access(self, scopes, &item);
            }
            Exp_::Continue => {
                let item = ItemOrAccess::Access(Access::KeyWords("continue"));
                visitor.handle_item_or_access(self, scopes, &item);
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
                let all_fields = struct_ty.all_fields();
                if let Some(def_field) = struct_ty.find_filed_by_name(field.value) {
                    let item = ItemOrAccess::Access(Access::AccessFiled(AccessFiled {
                        from: Field(field.clone()),
                        to: def_field.0.clone(),
                        ty: def_field.1.clone(),
                        all_fields,
                        item: None,
                    }));
                    visitor.handle_item_or_access(self, scopes, &item);
                } else {
                    let item = ItemOrAccess::Access(Access::AccessFiled(AccessFiled {
                        from: Field(field.clone()),
                        to: Field(field.clone()),
                        ty: ResolvedType::UnKnown,
                        all_fields,
                        item: None,
                    }));
                    visitor.handle_item_or_access(self, scopes, &item);
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
                let old = scopes.set_is_spec(true);
                self.visit_spec(spec, scopes, visitor);
                scopes.set_is_spec(old);
            }
            Exp_::UnresolvedError => {
                //
            }
        }
    }

    pub(crate) fn visit_friend(
        &self,
        friend_decl: &FriendDecl,
        addr: AccountAddress,
        module_name: Symbol,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        match &friend_decl.friend.value {
            NameAccessChain_::One(x) => {
                // This is absolute wrong syntax,But can be use for completion.
                let item = ItemOrAccess::Access(Access::Friend(
                    friend_decl.friend.clone(),
                    ModuleName(Spanned {
                        loc: x.loc,
                        value: Symbol::from("uOZKbQXVWi"),
                    }),
                ));
                visitor.handle_item_or_access(self, scopes, &item);
            }

            NameAccessChain_::Two(x, name) => {
                let addr_friend = match &x.value {
                    LeadingNameAccess_::AnonymousAddress(x) => x.bytes,
                    LeadingNameAccess_::Name(name) => self.name_to_addr_impl(name.value),
                };
                let m = scopes.resolve_friend(addr_friend, name.value);
                let m = match m {
                    Some(x) => x,
                    None => ModuleName(name.clone()),
                };
                let item = ItemOrAccess::Access(Access::Friend(friend_decl.friend.clone(), m));
                visitor.handle_item_or_access(self, scopes, &item);
                scopes.insert_friend(addr, module_name, (addr_friend, name.value));
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
        is_spec_module: bool,
    ) {
        let mut dummy = DummyVisitor;
        let visitor = visitor.unwrap_or(&mut dummy);
        let get_addr = |module: &ModuleIdent| -> AccountAddress {
            match &module.value.address.value {
                LeadingNameAccess_::AnonymousAddress(num) => num.bytes,
                LeadingNameAccess_::Name(name) => self.name_to_addr_impl(name.value),
            }
        };
        let get_module = |module: &ModuleIdent| -> Option<Rc<RefCell<ModuleScope>>> {
            let module_scope = scopes.visit_address(|top| -> Option<Rc<RefCell<ModuleScope>>> {
                let x = top
                    .address
                    .get(&get_addr(module))?
                    .modules
                    .get(&module.value.module.0.value)?
                    .clone();
                Some(x)
            });
            let module_scope = match module_scope {
                Some(x) => x,
                None => return None,
            };
            Some(module_scope)
        };

        match &use_decl.use_ {
            Use::Module(module, alias) => {
                let module_scope = get_module(module);
                let module_scope = module_scope.unwrap_or(ModuleScope::new_module_name(
                    get_addr(module),
                    module.value.module.clone(),
                ));
                let item = ItemOrAccess::Item(Item::UseModule(
                    module.clone(),
                    alias.clone(),
                    module_scope,
                    None,
                ));
                visitor.handle_item_or_access(self, scopes, &item);
                if visitor.finished() {
                    return;
                }
                let name = if let Some(alias) = alias {
                    alias.value()
                } else {
                    module.value.module.value()
                };
                if let Some((addr, module_name)) = is_global {
                    scopes.enter_top_item(self, addr, module_name, name, item, is_spec_module);
                } else {
                    scopes.enter_item(self, name, item);
                }
            }

            Use::Members(module, members) => {
                let module_scope = get_module(module);
                let module_scope = module_scope.unwrap_or(ModuleScope::new_module_name(
                    get_addr(module),
                    module.value.module.clone(),
                ));
                let garbage = vec![(
                    Name {
                        loc: module.loc,
                        value: Symbol::from("BRKuUAoEna"),
                    },
                    None,
                )];
                for (member, alias) in if members.len() > 0 {
                    members.iter()
                } else {
                    // So can find module definition in statement like this.
                    // use std::vector::{}
                    garbage.iter()
                } {
                    if member.value.as_str() == "Self" {
                        // Special handle for Self.
                        let item = ItemOrAccess::Item(Item::UseModule(
                            module.clone(),
                            // Here is special .
                            alias.clone().map(|x| ModuleName(x)),
                            module_scope.clone(),
                            Some(member.clone()),
                        ));
                        visitor.handle_item_or_access(self, scopes, &item);
                        if visitor.finished() {
                            return;
                        }
                        let name = if let Some(alias) = alias {
                            alias.value
                        } else {
                            module.value.module.value()
                        };
                        if let Some((addr, module_name)) = is_global {
                            scopes.enter_top_item(
                                self,
                                addr,
                                module_name,
                                name,
                                item,
                                is_spec_module,
                            );
                        } else {
                            scopes.enter_item(self, name, item);
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
                    visitor.handle_item_or_access(self, scopes, &item);
                    if visitor.finished() {
                        return;
                    }
                    if let Some((addr, module_name)) = is_global {
                        scopes.enter_top_item(
                            self,
                            addr,
                            module_name,
                            name.value,
                            item,
                            is_spec_module,
                        );
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
