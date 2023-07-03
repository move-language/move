// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use super::{item::*, project::*, project_context::*, scope::*, types::*};
use crate::utils::discover_manifest_and_kind;
use move_command_line_common::files::FileHash;
use move_compiler::{
    parser::ast::*,
    shared::{Identifier, Name},
};
use move_core_types::account_address::*;
use move_ir_types::location::*;
use move_package::source_package::layout::SourcePackageLayout;
use move_symbol_pool::Symbol;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    path::PathBuf,
    rc::Rc,
    vec,
};

impl Project {
    /// Collect field name like a in  spec schema IncrementAborts {
    ///                   a: address;
    /// }
    pub(crate) fn collect_spec_schema_fields(
        &self,
        project_context: &ProjectContext,
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
                    let mut ty = project_context.resolve_type(type_, self);
                    if ty.is_err() && init.is_some() {
                        ty = self.get_expr_type(init.as_ref().unwrap(), project_context);
                    }
                    ret.insert(name.value, (name.clone(), ty));
                }
                _ => {}
            }
        }
        ret
    }

    pub(crate) fn get_defs(
        &self,
        filepath: &PathBuf,
        call_back: impl FnOnce(VecDefAstProvider),
    ) -> anyhow::Result<()> {
        let (manifest_path, layout) = match discover_manifest_and_kind(filepath.as_path()) {
            Some(x) => x,
            None => {
                return anyhow::Result::Err(anyhow::anyhow!(
                    "manifest not found for '{:?}'",
                    filepath.as_path()
                ))
            }
        };
        let d = Default::default();
        let d2 = Default::default();
        let b = self
            .modules
            .get(&manifest_path)
            .unwrap_or(&d2)
            .as_ref()
            .borrow();
        call_back(VecDefAstProvider::new(
            if layout == SourcePackageLayout::Sources {
                b.sources.get(filepath).unwrap_or(&d)
            } else if layout == SourcePackageLayout::Tests {
                b.tests.get(filepath).unwrap_or(&d)
            } else if layout == SourcePackageLayout::Scripts {
                b.scripts.get(filepath).unwrap_or(&d)
            } else {
                unreachable!()
            },
            self,
            layout,
        ));
        anyhow::Ok(())
    }

    pub(crate) fn get_module_addr(
        &self,
        addr: Option<LeadingNameAccess>,
        m: &ModuleDefinition,
    ) -> AccountAddress {
        match addr {
            Some(x) => match x.value {
                LeadingNameAccess_::AnonymousAddress(x) => x.into_inner(),
                LeadingNameAccess_::Name(name) => self.name_to_addr_impl(name.value),
            },
            None => match m.address {
                Some(x) => match x.value {
                    LeadingNameAccess_::AnonymousAddress(x) => x.into_inner(),
                    LeadingNameAccess_::Name(name) => self.name_to_addr_impl(name.value),
                },
                None => *ERR_ADDRESS,
            },
        }
    }

    /// Entrance for `ItemOrAccessHandler` base on analyze.
    pub fn run_full_visitor(&self, visitor: &mut dyn ItemOrAccessHandler) {
        log::info!("run visitor for {} ", visitor);
        self.project_context.clear_scopes_and_addresses();

        // visit should `rev`.
        let manifests: Vec<_> = self
            .manifest_paths
            .iter()
            .rev()
            .map(|x| x.clone())
            .collect();
        for m in manifests.iter() {
            self.visit(
                &self.project_context,
                visitor,
                ModulesAstProvider::new(self, m.clone(), SourcePackageLayout::Sources),
                true,
            );
            if visitor.finished() {
                return;
            }
            self.visit(
                &self.project_context,
                visitor,
                ModulesAstProvider::new(self, m.clone(), SourcePackageLayout::Tests),
                true,
            );
            if visitor.finished() {
                return;
            }
            self.visit_scripts(
                &self.project_context,
                visitor,
                ModulesAstProvider::new(self, m.clone(), SourcePackageLayout::Scripts),
            );
        }
    }

    pub fn run_visitor_for_file(
        &self,
        visitor: &mut dyn ItemOrAccessHandler,
        filepath: &PathBuf,
        enter_import: bool,
    ) -> anyhow::Result<()> {
        log::info!("run visitor part for {} ", visitor);
        self.get_defs(filepath, |provider| {
            self.visit(
                &self.project_context,
                visitor,
                provider.clone(),
                enter_import,
            );
            self.visit_scripts(&self.project_context, visitor, provider);
        })
    }

    fn try_fix_local_var_and_visit_lambda(
        &self,
        value: &Exp,
        ty: &ResolvedType,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        let u_ty = ResolvedType::UnKnown;
        let x = match &value.value {
            Exp_::Name(name, _) => match &name.value {
                NameAccessChain_::One(name) => {
                    self.project_context.try_fix_local_var_ty(name.value, ty)
                }
                NameAccessChain_::Two(_, _) => None,
                NameAccessChain_::Three(_, _) => None,
            },
            Exp_::Lambda(b, e) => Some(LambdaExp {
                bind_list: b.clone(),
                exp: e.as_ref().clone(),
            }),
            _ => None,
        };
        if let Some(lambda) = x {
            match ty {
                ResolvedType::Lambda {
                    args,
                    ret_ty: _ret_ty,
                } => {
                    let _guard = self.project_context.enter_scope_guard(Scope::default());
                    for (index, b) in lambda.bind_list.value.iter().enumerate() {
                        self.visit_bind(
                            b,
                            match args.get(index) {
                                Some(x) => x,
                                None => &u_ty,
                            },
                            &self.project_context,
                            visitor,
                            None,
                            false,
                        );
                        if visitor.finished() {
                            return;
                        }
                    }
                    // visit expr body
                    self.visit_expr(&lambda.exp, &self.project_context, visitor);
                }
                _ => {}
            }
        }
    }

    pub fn visit(
        &self,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
        provider: impl AstProvider,
        enter_import: bool,
    ) {
        project_context.set_access_env(Default::default());
        let mut all_spec_module = HashSet::new();
        provider.with_module(|addr, module_def| {
            let item = ItemOrAccess::Item(Item::ModuleName(ItemModuleName {
                name: module_def.name,
            }));
            visitor.handle_item_or_access(self, project_context, &item);
            if !module_def.is_spec_module {
                project_context.set_up_module(
                    addr,
                    module_def.name,
                    provider.found_in_test()
                        || attributes_has_test(&module_def.attributes).is_test(),
                );
            } else {
                all_spec_module.insert((addr, module_def.name));
            }
        });
        // module created
        let mut spec_module_created = HashSet::new();
        project_context.visit_address(|address| {
            for (addr, module_name) in all_spec_module.iter() {
                let created = address
                    .address
                    .get(addr)
                    .map(|mm| mm.modules.get(&module_name.0.value).is_some())
                    .unwrap_or(false);
                if created {
                    spec_module_created.insert((*addr, module_name.clone()));
                }
            }
        });
        for (addr, module_name) in all_spec_module.into_iter() {
            // skip if created.
            if spec_module_created.contains(&(addr.clone(), module_name.clone())) == false {
                project_context.set_up_module(addr, module_name, false);
            }
        }

        provider.with_const(|addr, name, c| {
            self.visit_const(Some((addr, name)), c, project_context, visitor);
        });
        provider.with_struct(|addr, module_name, c| {
            let item = Item::StructNameRef(ItemStructNameRef {
                addr,
                module_name,
                name: c.name.clone(),
                type_parameters: c.type_parameters.clone(),
                is_test: attributes_has_test(&c.attributes).is_test(),
            });
            project_context.enter_top_item(self, addr, module_name, c.name.0.value, item, false);
        });

        provider.with_use_decl(|addr, module_name, u, is_spec_module| {
            self.visit_use_decl(
                Some((addr, module_name)),
                u,
                project_context,
                None,
                is_spec_module,
                enter_import,
            )
        });

        provider.with_struct(|addr, module_name, s| {
            let _guard = project_context.clone_scope_and_enter(addr, module_name, false);
            project_context.enter_scope(|scopes| {
                scopes.set_access_env(
                    if provider.found_in_test()
                        || scopes.module_is_test(addr, module_name).unwrap_or_default()
                        || attributes_has_test(&s.attributes).is_test()
                    {
                        AccessEnv::Test
                    } else {
                        Default::default()
                    },
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
                    addr,
                    module_name,
                }));
                visitor.handle_item_or_access(self, scopes, &item);
                scopes.enter_top_item(self, addr, module_name, s.name.value(), item, false)
            });
        });

        let enter_function = |modules: &Project,
                              f: &Function,
                              project_context: &ProjectContext,
                              visitor: &mut dyn ItemOrAccessHandler,
                              addr: AccountAddress,
                              module_name: Symbol,
                              is_spec_module: bool,
                              is_spec: bool| {
            // This enter scope make sure the visit_tparam cannot override some module level item.
            project_context.enter_scope(|scopes| {
                scopes.set_access_env(
                    if provider.found_in_test()
                        || scopes.module_is_test(addr, module_name).unwrap_or_default()
                        || attributes_has_test(&f.attributes).is_test()
                    {
                        AccessEnv::Test
                    } else if is_spec_module || is_spec {
                        AccessEnv::Spec
                    } else {
                        Default::default()
                    },
                );

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
                    addr_and_name: AddrAndModuleName {
                        addr: addr.clone(),
                        name: ModuleName(Spanned {
                            loc: Loc::new(FileHash::empty(), 0, 0),
                            value: module_name,
                        }),
                    },
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
            let _guard = project_context.clone_scope_and_enter(addr, module_name, false);
            enter_function(
                self,
                f,
                project_context,
                visitor,
                addr,
                module_name,
                false,
                false,
            );
        });

        // enter schema.
        provider.with_spec_schema(|addr, module_name, name, spec, is_spec_module| {
            project_context.set_current_addr_and_module_name(addr, module_name);
            project_context.set_access_env(AccessEnv::Spec);
            let item = ItemOrAccess::Item(Item::SpecSchema(
                name.clone(),
                self.collect_spec_schema_fields(project_context, &spec.value.members),
            ));

            visitor.handle_item_or_access(self, project_context, &item);
            project_context.enter_top_item(
                self,
                addr,
                module_name,
                name.value,
                item,
                is_spec_module,
            );
        });

        // visit use decl again.
        provider.with_use_decl(|addr, module_name, u, is_spec_module| {
            project_context.set_current_addr_and_module_name(addr, module_name);
            project_context.set_access_env(if is_spec_module {
                AccessEnv::Spec
            } else if provider.found_in_test()
                || project_context
                    .module_is_test(addr, module_name)
                    .unwrap_or_default()
            {
                AccessEnv::Test
            } else {
                Default::default()
            });
            self.visit_use_decl(
                Some((addr, module_name)),
                u,
                project_context,
                Some(visitor),
                is_spec_module,
                false,
            );
        });

        provider.with_friend(|addr, module_name, f| {
            project_context.set_current_addr_and_module_name(addr, module_name);
            self.visit_friend(f, addr, module_name, project_context, visitor);
        });
        provider.with_spec(|addr, module_name, spec, is_spec_module| {
            project_context.set_current_addr_and_module_name(addr, module_name);
            project_context.set_access_env(AccessEnv::Spec);
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
                            project_context,
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

        if !visitor.visit_fun_or_spec_body() {
            //
            return;
        }

        // visit function body.
        provider.with_function(|addr, module_name, f| {
            project_context.set_current_addr_and_module_name(addr, module_name);
            project_context.set_access_env(
                if provider.found_in_test()
                    || project_context
                        .module_is_test(addr, module_name)
                        .unwrap_or_default()
                    || attributes_has_test(&f.attributes).is_test()
                {
                    AccessEnv::Test
                } else {
                    Default::default()
                },
            );
            let range = self.convert_loc_range(&f.loc);
            if range.is_none() {
                return;
            }
            if !visitor.function_or_spec_body_should_visit(range.as_ref().unwrap()) {
                return;
            }
            let _guard = project_context.clone_scope_and_enter(addr, module_name, false);
            log::info!("provider.with_function range = {:?}", range);
            self.visit_function(f, project_context, visitor);
        });

        provider.with_spec(|addr, module_name, spec, _is_spec_module| {
            project_context.set_current_addr_and_module_name(addr, module_name);
            project_context.set_access_env(AccessEnv::Spec);
            let range = self.convert_loc_range(&spec.loc);
            if range.is_none() {
                return;
            }
            if !visitor.function_or_spec_body_should_visit(range.as_ref().unwrap()) {
                return;
            }
            let _guard = project_context.clone_scope_and_enter(addr, module_name, true);
            self.visit_spec(spec, project_context, visitor);
        });
    }

    pub(crate) fn visit_bind(
        &self,
        bind: &Bind,
        infer_ty: &ResolvedType,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
        expr: Option<&Exp>,
        has_decl_ty: bool,
    ) {
        log::info!("visit_bind:{:?}", bind);
        match &bind.value {
            Bind_::Var(var) => {
                let item = ItemOrAccess::Item(Item::Var {
                    has_decl_ty,
                    var: var.clone(),
                    ty: infer_ty.clone(),
                    lambda: expr
                        .map(|x| match &x.value {
                            Exp_::Lambda(b, e) => Some(LambdaExp {
                                bind_list: b.clone(),
                                exp: e.as_ref().clone(),
                            }),
                            _ => None,
                        })
                        .flatten()
                        .map(|x| x.clone()),
                });
                visitor.handle_item_or_access(self, project_context, &item);
                if visitor.finished() {
                    return;
                }
                project_context.enter_item(self, var.0.value, item);
                return;
            }
            Bind_::Unpack(chain, tys, field_binds) => {
                self.visit_type_apply(
                    &Spanned {
                        loc: chain.loc,
                        value: Type_::Apply(chain.clone(), vec![]),
                    },
                    project_context,
                    visitor,
                );
                let (struct_ty, _) = project_context.find_name_chain_item(chain, self);
                let struct_ty = struct_ty.unwrap_or_default().to_type().unwrap_or_default();
                if let Some(tys) = tys {
                    for t in tys.iter() {
                        self.visit_type_apply(t, project_context, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                }
                let mut struct_item = struct_ty.struct_ref_to_struct(project_context);
                let struct_item = if let Some(tys) = tys {
                    let tys: Vec<_> = tys
                        .iter()
                        .map(|x| project_context.resolve_type(x, self))
                        .collect();
                    let mut m = HashMap::new();
                    struct_item
                        .type_parameters
                        .iter()
                        .zip(tys.iter())
                        .for_each(|(t, ty)| {
                            m.insert(t.name.value, ty.clone());
                        });
                    struct_item.bind_type_parameter(Some(&m));
                    struct_item
                } else {
                    // use
                    infer_ty.clone().struct_ref_to_struct(project_context)
                };

                for (field, bind) in field_binds.iter() {
                    let field_and_ty = struct_item.find_filed_by_name(field.0.value);
                    let field_ty = if let Some(x) = field_and_ty {
                        if infer_ty.is_ref() {
                            ResolvedType::new_ref(false, x.1.clone())
                        } else {
                            x.1.clone()
                        }
                    } else {
                        ResolvedType::UnKnown
                    };
                    {
                        let item = ItemOrAccess::Access(Access::AccessFiled(AccessFiled {
                            from: field.clone(),
                            to: if let Some(x) = field_and_ty {
                                x.0.clone()
                            } else {
                                field.clone()
                            },
                            ty: field_ty.clone(),
                            item: None,
                        }));
                        visitor.handle_item_or_access(self, project_context, &item);
                        if visitor.finished() {
                            return;
                        }
                    }
                    self.visit_bind(bind, &field_ty, project_context, visitor, None, true);
                }
            }
        }
    }

    pub(crate) fn visit_bind_list(
        &self,
        bind_list: &BindList,
        ty: &Option<Type>,
        expr: Option<&Box<Exp>>,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        if let Some(expr) = expr {
            self.visit_expr(expr.as_ref(), project_context, visitor);
            if visitor.finished() {
                return;
            }
        }
        let has_decl_type = ty.is_some();
        let ty = if let Some(ty) = ty {
            self.visit_type_apply(ty, project_context, visitor);
            if visitor.finished() {
                return;
            }
            project_context.resolve_type(ty, self)
        } else if let Some(expr) = expr {
            let ty = self.get_expr_type(expr, project_context);
            ty
        } else {
            ResolvedType::UnKnown
        };

        for (index, bind) in bind_list.value.iter().enumerate() {
            let ty = ty.nth_ty(index);
            let unknown = ResolvedType::UnKnown;
            let ty = ty.unwrap_or(&unknown);
            self.visit_bind(
                bind,
                ty,
                project_context,
                visitor,
                match expr {
                    Some(x) => match &x.as_ref().value {
                        Exp_::ExpList(es) => es.get(index),
                        _ => Some(x.as_ref()),
                    },
                    None => None,
                },
                has_decl_type,
            );
            if visitor.finished() {
                return;
            }
        }
    }

    pub(crate) fn visit_block(
        &self,
        seq: &Sequence,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        project_context.enter_scope(|scopes| {
            for u in seq.0.iter() {
                self.visit_use_decl(None, u, scopes, Some(visitor), false, true);
                if visitor.finished() {
                    return;
                }
            }
            for s in seq.1.iter() {
                log::trace!("visit_block sequence_item = {:?}", s);
                self.visit_sequence_item(s, scopes, visitor);
                if visitor.finished() {
                    return;
                }
            }
            if let Some(ref exp) = seq.3.as_ref() {
                match exp.value {
                    Exp_::UnaryExp(_, _) => {
                        return;
                    }
                    Exp_::BinopExp(_, _, _) => {
                        return;
                    }
                    Exp_::Dot(_, _) => {
                        return;
                    }
                    _ => {}
                }

                log::trace!("visit_block exp = {:?}", exp);
                self.visit_expr(exp, scopes, visitor);
            }
        });
    }

    pub(crate) fn visit_const(
        &self,
        enter_top: Option<(AccountAddress, Symbol)>,
        c: &Constant,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        self.visit_type_apply(&c.signature, project_context, visitor);
        if visitor.finished() {
            return;
        }
        // const can only be declared at top scope
        let ty = project_context.resolve_type(&c.signature, self);

        let item = ItemOrAccess::Item(Item::Const(ItemConst {
            name: c.name.clone(),
            ty,
        }));
        visitor.handle_item_or_access(self, project_context, &item);
        let item: Item = item.into();
        if let Some((address, module)) = enter_top {
            project_context.enter_top_item(
                self,
                address,
                module,
                c.name.value(),
                item.clone(),
                false,
            );
        } else {
            project_context.enter_item(self, c.name.value(), item);
        }
    }

    pub(crate) fn visit_expr(
        &self,
        exp: &Exp,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        log::trace!("visit_expr:{:?}", exp);
        if visitor.need_expr_type() {
            let ty = self.get_expr_type(exp, project_context);
            visitor.handle_expr_typ(exp, ty);
        }

        let handle_dot = |e: &Exp,
                          field: &Name,
                          project_context: &ProjectContext,
                          visitor: &mut dyn ItemOrAccessHandler,
                          _has_ref: Option<bool>| {
            log::trace!("handle_dot({})", field);
            // self.visit_expr(e, project_context, visitor);
            if visitor.finished() {
                return;
            }
            log::trace!(
                "handle_dot --> inlay_hint.handle_item_or_access({}) continue",
                field
            );
            let struct_ty = self.get_expr_type(e, project_context);
            let struct_ty = match &struct_ty {
                ResolvedType::Ref(_, ty) => ty.as_ref(),
                _ => &struct_ty,
            };
            let struct_ty = struct_ty.struct_ref_to_struct(project_context);
            if let Some(def_field) = struct_ty.find_filed_by_name(field.value) {
                let item = ItemOrAccess::Access(Access::AccessFiled(AccessFiled {
                    from: Field(field.clone()),
                    to: def_field.0.clone(),
                    ty: def_field.1.clone(),
                    item: None,
                }));
                visitor.handle_item_or_access(self, project_context, &item);
            } else {
                let item = ItemOrAccess::Access(Access::AccessFiled(AccessFiled {
                    from: Field(field.clone()),
                    to: Field(field.clone()),
                    ty: ResolvedType::UnKnown,
                    item: None,
                }));
                visitor.handle_item_or_access(self, project_context, &item);
            }
        };

        match &exp.value {
            Exp_::Value(ref v) => {
                if let Some(name) = get_name_from_value(v) {
                    let item = ItemOrAccess::Access(Access::ExprAddressName(name.clone()));
                    visitor.handle_item_or_access(self, project_context, &item);
                }
            }
            Exp_::Move(var) | Exp_::Copy(var) => {
                let item = project_context.find_var(var.value());
                let item = ItemOrAccess::Access(Access::ExprVar(
                    var.clone(),
                    Box::new(item.unwrap_or_default()),
                ));
                visitor.handle_item_or_access(self, project_context, &item);
            }
            Exp_::Name(chain, tys) => {
                // let's try.
                if let Some(tys) = tys {
                    for ty in tys.iter() {
                        log::trace!("process Exp_::Name, ty = {:?}", ty);
                        self.visit_type_apply(ty, project_context, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                }
                log::trace!("process Exp_::Name, chain = {}", chain);
                let (item, module) = project_context.find_name_chain_item(chain, self);
                let item = ItemOrAccess::Access(Access::ExprAccessChain(
                    chain.clone(),
                    module,
                    Box::new(item.unwrap_or_default()),
                ));
                log::trace!("process Exp_::Name, item = {}", item);
                visitor.handle_item_or_access(self, project_context, &item);
                if visitor.finished() {
                    return;
                }
            }

            Exp_::Call(ref chain, is_macro, ref types, ref exprs) => {
                if *is_macro {
                    let c = MacroCall::from_chain(chain).unwrap_or_default();
                    let item = ItemOrAccess::Access(Access::MacroCall(c, chain.clone()));
                    visitor.handle_item_or_access(self, project_context, &item);
                } else {
                    let (item, module) = project_context.find_name_chain_item(chain, self);
                    if visitor.need_call_pair() {
                        match item.clone().unwrap_or_default() {
                            Item::Fun(_f) => {
                                let addr = project_context.get_current_addr_and_module_name();
                                visitor.handle_call_pair(
                                    FunID {
                                        addr: addr.addr.clone(),
                                        addr_name: "".to_string(), // TODO
                                        module_name: addr.name.0.value.clone(),
                                        function_name: get_name_chain_last_name(chain).value,
                                    },
                                    FunID {
                                        addr: _f.addr_and_name.addr.clone(),
                                        addr_name: "".to_string(), // TODO
                                        module_name: _f.addr_and_name.name.0.value,
                                        function_name: _f.name.0.value,
                                    },
                                );
                            }
                            _ => {}
                        }
                    }

                    // try visit lambda expr.
                    match self
                        .initialize_fun_call(project_context, chain, types, exprs)
                        .unwrap_or_default()
                    {
                        // TODO we maybe need infer type parameter first.
                        ResolvedType::Fun(x) => {
                            let unkown = (
                                Var(Spanned {
                                    loc: Loc::new(FileHash::empty(), 0, 0),
                                    value: Symbol::from(""),
                                }),
                                ResolvedType::UnKnown,
                            );
                            for (index, e) in exprs.value.iter().enumerate() {
                                let ty = x.parameters.get(index).unwrap_or(&unkown);
                                self.try_fix_local_var_and_visit_lambda(e, &ty.1, visitor);
                                if visitor.need_para_arg_pair() {
                                    visitor.handle_para_arg_pair(self, ty.0 .0.clone(), e);
                                }
                            }
                        }
                        _ => {}
                    }
                    let item = ItemOrAccess::Access(Access::ExprAccessChain(
                        chain.clone(),
                        module,
                        Box::new(item.unwrap_or_default()),
                    ));
                    log::trace!("process Exp_::Call, item = {}", item);
                    visitor.handle_item_or_access(self, project_context, &item);
                    if visitor.finished() {
                        return;
                    }
                }
                if let Some(ref types) = types {
                    for t in types.iter() {
                        log::trace!("process Exp_::Call, t = {:?}", t);
                        self.visit_type_apply(t, project_context, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                }
                for expr in exprs.value.iter() {
                    log::trace!("process Exp_::Call, expr = {:?}", expr);
                    self.visit_expr(expr, project_context, visitor);
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
                    project_context,
                    visitor,
                );
                if visitor.finished() {
                    return;
                }
                let (struct_item, _) = project_context.find_name_chain_item(chain, self);
                let mut struct_item = match struct_item {
                    Some(x) => match x {
                        Item::Struct(x) => x,
                        _ => {
                            return;
                        }
                    },
                    None => return,
                };
                if let Some(types) = types {
                    for t in types.iter() {
                        self.visit_type_apply(t, project_context, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                    let types: Vec<_> = types
                        .iter()
                        .map(|ty| project_context.resolve_type(ty, self))
                        .collect();
                    struct_item.type_parameters_ins = types;
                    struct_item.bind_type_parameter(None);
                }
                for f in fields.iter() {
                    let field_type = struct_item.find_filed_by_name(f.0.value());
                    let item = match &f.1.value {
                        Exp_::Name(chain, _) => match &chain.value {
                            NameAccessChain_::One(x) => {
                                if x.value == f.0.value() && x.loc == f.0.loc() {
                                    let (item, _) =
                                        project_context.find_name_chain_item(chain, self);
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
                            item,
                        }));
                        visitor.handle_item_or_access(self, project_context, &item);
                    } else {
                        let item = ItemOrAccess::Access(Access::AccessFiled(AccessFiled {
                            from: f.0.clone(),
                            to: f.0.clone(),
                            ty: ResolvedType::UnKnown,
                            item,
                        }));
                        visitor.handle_item_or_access(self, project_context, &item);
                    }
                    if visitor.finished() {
                        return;
                    }
                    self.visit_expr(&f.1, project_context, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }

            Exp_::Vector(_loc, ref ty, ref exprs) => {
                if let Some(ty) = ty {
                    for t in ty.iter() {
                        self.visit_type_apply(t, project_context, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                }
                for e in exprs.value.iter() {
                    self.visit_expr(e, project_context, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
            Exp_::IfElse(condition, then_, else_) => {
                self.visit_expr(condition, project_context, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_expr(then_, project_context, visitor);
                if visitor.finished() {
                    return;
                }
                if let Some(else_) = else_ {
                    self.visit_expr(else_.as_ref(), project_context, visitor);
                }
            }
            Exp_::While(condition, body) => {
                self.visit_expr(condition, project_context, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_expr(body.as_ref(), project_context, visitor);
            }
            Exp_::Loop(e) => {
                self.visit_expr(e.as_ref(), project_context, visitor);
            }
            Exp_::Block(b) => self.visit_block(b, project_context, visitor),
            Exp_::Lambda(_, _) => {
                // TODO have lambda expression in ast structure.
                // But I don't find in msl spec.
                log::error!("lambda expression in ast.");
            }

            Exp_::Quant(_, binds, bodies, where_, result) => {
                // TODO look list t can be use a type alias.
                // forall t: type, addr: address where exists<R<t>>(addr): exists<T<t>>(addr)
                project_context.enter_scope(|scopes| {
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
                    self.visit_expr(e, project_context, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
            Exp_::Unit => {
                // Nothing.
            }
            Exp_::Assign(left, right) => {
                self.visit_expr(left, project_context, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_expr(right, project_context, visitor);
                let ty = self.get_expr_type(right.as_ref(), project_context);
                self.try_fix_local_var_and_visit_lambda(left, &ty, visitor);
            }
            Exp_::Return(e) => {
                if let Some(e) = e {
                    self.visit_expr(e, project_context, visitor);
                }
            }
            Exp_::Abort(e) => self.visit_expr(e.as_ref(), project_context, visitor),
            Exp_::Break => {
                let item = ItemOrAccess::Access(Access::KeyWords("break"));
                visitor.handle_item_or_access(self, project_context, &item);
            }
            Exp_::Continue => {
                let item = ItemOrAccess::Access(Access::KeyWords("continue"));
                visitor.handle_item_or_access(self, project_context, &item);
            }
            Exp_::Dereference(x) => {
                self.visit_expr(x.as_ref(), project_context, visitor);
            }
            Exp_::UnaryExp(_, e) => {
                self.visit_expr(e.as_ref(), project_context, visitor);
            }
            Exp_::BinopExp(left, _, right) => {
                self.visit_expr(left, project_context, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_expr(right, project_context, visitor);
            }
            Exp_::Borrow(is_mut, e) => match &e.value {
                Exp_::Dot(e, f) => {
                    handle_dot(&e, f, project_context, visitor, Some(*is_mut));
                }
                _ => {
                    self.visit_expr(e.as_ref(), project_context, visitor);
                }
            },
            Exp_::Dot(e, field) => {
                log::trace!("process Exp_::Dot, field = {}", field);
                handle_dot(&e, field, project_context, visitor, None);
            }
            Exp_::Index(e, index) => {
                self.visit_expr(e.as_ref(), project_context, visitor);
                self.visit_expr(index.as_ref(), project_context, visitor)
            }
            Exp_::Cast(e, ty) => {
                self.visit_expr(e.as_ref(), project_context, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_type_apply(ty, project_context, visitor);
            }
            Exp_::Annotate(e, ty) => {
                self.visit_expr(e.as_ref(), project_context, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_type_apply(ty, project_context, visitor);
            }
            Exp_::Spec(spec) => {
                let old = project_context.set_access_env(AccessEnv::Spec);
                self.visit_spec(spec, project_context, visitor);
                project_context.set_access_env(old);
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
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
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
                visitor.handle_item_or_access(self, project_context, &item);
            }

            NameAccessChain_::Two(x, name) => {
                let addr_friend = match &x.value {
                    LeadingNameAccess_::AnonymousAddress(x) => x.into_inner(),
                    LeadingNameAccess_::Name(name) => self.name_to_addr_impl(name.value),
                };
                let m = project_context.resolve_friend(addr_friend, name.value);
                let m = match m {
                    Some(x) => x,
                    None => ModuleName(name.clone()),
                };
                let item = ItemOrAccess::Access(Access::Friend(friend_decl.friend.clone(), m));
                visitor.handle_item_or_access(self, project_context, &item);
                project_context.insert_friend(addr, module_name, (addr_friend, name.value));
            }

            NameAccessChain_::Three(_, _) => {
                log::error!("access friend three")
            }
        }
    }

    ///
    pub(crate) fn visit_function(
        &self,
        function: &Function,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        return project_context.enter_scope(|s| {
            self.visit_signature(&function.signature, s, visitor);
            if visitor.finished() {
                return;
            }
            for v in function.acquires.iter() {
                let ty = &Spanned {
                    loc: v.loc,
                    value: Type_::Apply(Box::new(v.clone()), vec![]),
                };
                log::trace!("visit_function, ty = {:?}", ty);
                self.visit_type_apply(&ty, project_context, visitor);
                if visitor.finished() {
                    return;
                }
            }
            log::info!(
                "visit_function, function.body.value = {:?}",
                function.body.value
            );
            match function.body.value {
                FunctionBody_::Native => {}
                FunctionBody_::Defined(ref seq) => self.visit_block(seq, project_context, visitor),
            }
        });
    }

    pub(crate) fn visit_scripts(
        &self,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
        provider: impl AstProvider,
    ) {
        provider.with_script(|script| {
            project_context.enter_scope(|scopes| {
                scopes.set_access_env(AccessEnv::default());
                for u in script.uses.iter() {
                    self.visit_use_decl(None, u, scopes, Some(visitor), false, true);
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

    pub(crate) fn visit_sequence_item(
        &self,
        seq: &SequenceItem,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        match seq.value {
            SequenceItem_::Seq(ref e) => {
                self.visit_expr(e, project_context, visitor);
                if visitor.finished() {
                    return;
                }
            }
            SequenceItem_::Declare(ref list, ref ty) => {
                self.visit_bind_list(list, ty, None, project_context, visitor);
                if visitor.finished() {
                    return;
                }
            }
            SequenceItem_::Bind(ref list, ref ty, ref expr) => {
                self.visit_bind_list(list, ty, Some(expr), project_context, visitor);
                if visitor.finished() {
                    return;
                }
            }
        }
    }
    pub fn visit_spec(
        &self,
        spec: &SpecBlock,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        let _guard = project_context.enter_scope_guard(Scope::default());
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
                let (item_ret, _) = project_context.find_name_chain_item(&chain, self);
                {
                    // TODO this may not be expr.
                    // You can write spec for struct.
                    let item = ItemOrAccess::Access(Access::SpecFor(
                        m.clone(),
                        Box::new(item_ret.clone().unwrap_or_default()),
                    ));
                    visitor.handle_item_or_access(self, project_context, &item);
                    if visitor.finished() {
                        return;
                    }
                }

                if let Some(item_ret) = item_ret.as_ref() {
                    match &item_ret {
                        Item::Fun(x) => {
                            for (var, ty) in x.parameters.iter() {
                                project_context.enter_item(
                                    self,
                                    var.0.value,
                                    Item::Parameter(var.clone(), ty.clone()),
                                );
                            }
                            for (var, ty) in x.type_parameters.iter() {
                                project_context.enter_types(
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
                                        project_context.enter_item(
                                            self,
                                            s,
                                            Item::Var {
                                                var: Var(Spanned {
                                                    loc: ty2.loc,
                                                    value: s,
                                                }),
                                                ty: ty1.clone(),
                                                lambda: None,
                                                has_decl_ty: true,
                                            },
                                        );
                                        index = index + 1;
                                    }
                                }

                                _ => {
                                    project_context.enter_item(
                                        self,
                                        Symbol::from("result"),
                                        Item::Var {
                                            var: Var(Spanned {
                                                loc: x.ret_type_unresolved.loc,
                                                value: Symbol::from("result"),
                                            }),
                                            ty: *x.ret_type.clone(),
                                            lambda: None,
                                            has_decl_ty: true,
                                        },
                                    );
                                }
                            }
                        }
                        Item::Struct(x) => {
                            for f in x.fields.iter() {
                                project_context.enter_item(
                                    self,
                                    f.0.value(),
                                    Item::Var {
                                        var: Var(f.0 .0.clone()),
                                        ty: f.1.clone(),
                                        lambda: None,
                                        has_decl_ty: true,
                                    },
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
                    self.visit_signature(signature.as_ref(), project_context, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
            SpecBlockTarget_::Schema(_, type_parameters) => {
                for t in type_parameters.iter() {
                    self.visit_tparam(t, project_context, visitor);
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
                project_context,
                Some(visitor),
                true, /*  here false or true doesn't matter. */
                true,
            );
            if visitor.finished() {
                return;
            }
        }
        for m in spec.value.members.iter() {
            self.visit_spec_member(m, project_context, visitor);
            if visitor.finished() {
                return;
            }
        }
    }
    pub fn visit_spec_member(
        &self,
        member: &SpecBlockMember,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
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
                project_context.enter_scope(|project_context: &ProjectContext| {
                    // enter type parameters.
                    for (name, ab) in type_parameters.iter() {
                        let item = ItemOrAccess::Item(Item::TParam(name.clone(), ab.clone()));
                        visitor.handle_item_or_access(self, project_context, &item);
                        project_context.enter_item(self, name.value, item);
                    }
                    self.visit_expr(exp, project_context, visitor);
                    if visitor.finished() {
                        return;
                    }
                    for exp in additional_exps.iter() {
                        self.visit_expr(exp, project_context, visitor);
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
                self.visit_signature(signature, project_context, visitor);
                if visitor.finished() {
                    return;
                }
                let _guard = project_context.enter_scope_guard(Scope::default());
                for t in signature.type_parameters.iter() {
                    self.visit_tparam(t, project_context, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
                let parameter: Vec<_> = signature
                    .parameters
                    .iter()
                    .map(|(var, ty)| (var.clone(), project_context.resolve_type(ty, self)))
                    .collect();

                let ret_ty = project_context.resolve_type(&signature.return_type, self);
                let item = Item::Fun(ItemFun {
                    name: name.clone(),
                    type_parameters: signature.type_parameters.clone(),
                    parameters: parameter.clone(),
                    ret_type: Box::new(ret_ty),
                    ret_type_unresolved: signature.return_type.clone(),
                    addr_and_name: project_context.get_current_addr_and_module_name(),
                });
                project_context.enter_item(self, name.value(), item);
                for (var, ty) in parameter {
                    project_context.enter_item(self, var.value(), Item::Parameter(var, ty));
                }
                // visit function body.
                match &body.value {
                    FunctionBody_::Defined(s) => self.visit_block(s, project_context, visitor),
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
                self.visit_type_apply(ty, project_context, visitor);
                if visitor.finished() {
                    return;
                }
                let ty = project_context.resolve_type(&ty, self);
                let item = ItemOrAccess::Item(Item::Var {
                    var: Var(name.clone()),
                    ty,
                    lambda: None,
                    has_decl_ty: true,
                });
                visitor.handle_item_or_access(self, project_context, &item);
                project_context.enter_item(self, name.value, item);
                if let Some(init) = init {
                    self.visit_expr(init, project_context, visitor);
                }
            }

            SpecBlockMember_::Let {
                name,
                post_state: _post_state,
                def,
            } => {
                let ty = self.get_expr_type(&def, project_context);
                let item = ItemOrAccess::Item(Item::Var {
                    var: Var(name.clone()),
                    ty,
                    lambda: None,
                    has_decl_ty: true,
                });
                visitor.handle_item_or_access(self, project_context, &item);
                project_context.enter_item(self, name.value, item);
                self.visit_expr(def, project_context, visitor);
            }

            SpecBlockMember_::Update { lhs, rhs } => {
                self.visit_expr(&lhs, project_context, visitor);
                self.visit_expr(&rhs, project_context, visitor);
            }
            SpecBlockMember_::Include {
                properties: _properties,
                exp,
            } => {
                // TODO handle _properties
                // TODO.
                match &exp.value {
                    Exp_::Name(chain, type_args) => {
                        let (item_ret, _module_ret) =
                            project_context.find_name_chain_item(chain, self);
                        let item = ItemOrAccess::Access(Access::IncludeSchema(
                            chain.clone(),
                            Box::new(item_ret.unwrap_or_default()),
                        ));
                        visitor.handle_item_or_access(self, project_context, &item);
                        if visitor.finished() {
                            return;
                        }
                        if let Some(type_args) = type_args {
                            for t in type_args.iter() {
                                self.visit_type_apply(t, project_context, visitor);
                                if visitor.finished() {
                                    return;
                                }
                            }
                        }
                    }
                    Exp_::Pack(chain, type_args, fields) => {
                        let (item_ret, _module_ret) =
                            project_context.find_name_chain_item(chain, self);
                        let item = ItemOrAccess::Access(Access::IncludeSchema(
                            chain.clone(),
                            Box::new(item_ret.clone().unwrap_or_default()),
                        ));
                        visitor.handle_item_or_access(self, project_context, &item);
                        if visitor.finished() {
                            return;
                        }
                        if let Some(type_args) = type_args {
                            for t in type_args.iter() {
                                self.visit_type_apply(t, project_context, visitor);
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
                                self.visit_expr(e, project_context, visitor);
                                if visitor.finished() {
                                    return;
                                }

                                if let Some((f2, ty)) = all_fields.get(&f.value()) {
                                    let item =
                                        ItemOrAccess::Access(Access::AccessFiled(AccessFiled {
                                            from: f.clone(),
                                            to: Field(f2.clone()),
                                            ty: ty.clone(),
                                            item: None,
                                        }));
                                    visitor.handle_item_or_access(self, project_context, &item);
                                    if visitor.finished() {
                                        return;
                                    }
                                } else {
                                    let item =
                                        ItemOrAccess::Access(Access::AccessFiled(AccessFiled {
                                            from: f.clone(),
                                            to: f.clone(),
                                            ty: ResolvedType::UnKnown,
                                            item: None,
                                        }));
                                    visitor.handle_item_or_access(self, project_context, &item);
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
                    Exp_::Name(chain, t) => {
                        if let Some(ts) = t {
                            for ty in ts.iter() {
                                self.visit_type_apply(ty, project_context, visitor);
                                if visitor.finished() {
                                    return;
                                }
                            }
                        };
                        Some(chain)
                    }
                    _ => None,
                };
                if let Some(rule) = rule {
                    let (item_ret, _) = project_context.find_name_chain_item(&rule, self);
                    let item_ret = item_ret.unwrap_or_default();
                    let item = ItemOrAccess::Access(Access::IncludeSchema(
                        rule.clone(),
                        Box::new(item_ret),
                    ));
                    visitor.handle_item_or_access(self, project_context, &item);
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
                                let (item_ret, _module_ret) =
                                    project_context.find_name_chain_item(&chain, self);
                                let item_ret = item_ret.unwrap_or_default();
                                let item = ItemOrAccess::Access(Access::ApplySchemaTo(
                                    chain.clone(),
                                    Box::new(item_ret),
                                ));
                                visitor.handle_item_or_access(self, project_context, &item);
                                if visitor.finished() {
                                    return;
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
                    visitor.handle_item_or_access(self, project_context, &item);
                    if visitor.finished() {
                        return;
                    }
                }
            }
        }
    }

    pub(crate) fn visit_type_apply(
        &self,
        ty: &Type,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        match &ty.value {
            Type_::Apply(chain, types) => {
                let ty = project_context.resolve_type(ty, self);
                let (_, module) = project_context.find_name_chain_item(chain, self);
                let item = ItemOrAccess::Access(Access::ApplyType(
                    chain.as_ref().clone(),
                    module.map(|x| x.name.clone()),
                    Box::new(ty),
                ));
                visitor.handle_item_or_access(self, project_context, &item);
                if visitor.finished() {
                    return;
                }
                for t in types.iter() {
                    self.visit_type_apply(t, project_context, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
            Type_::Ref(_, ty) => self.visit_type_apply(ty, project_context, visitor),
            Type_::Fun(args, ret_ty) => {
                for a in args.iter() {
                    self.visit_type_apply(a, project_context, visitor);
                }
                self.visit_type_apply(ret_ty.as_ref(), project_context, visitor);
            }
            Type_::Unit => {}
            Type_::Multiple(types) => {
                for t in types.iter() {
                    self.visit_type_apply(t, project_context, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
        }
    }

    pub(crate) fn visit_use_decl(
        &self,
        is_global: Option<(AccountAddress, Symbol)>,
        use_decl: &UseDecl,
        project_context: &ProjectContext,
        visitor: Option<&mut dyn ItemOrAccessHandler>,
        is_spec_module: bool,
        enter_import: bool,
    ) {
        let mut dummy = DummyHandler;
        let visitor = visitor.unwrap_or(&mut dummy);
        let get_addr = |module: &ModuleIdent| -> AccountAddress {
            match &module.value.address.value {
                LeadingNameAccess_::AnonymousAddress(num) => num.into_inner(),
                LeadingNameAccess_::Name(name) => self.name_to_addr_impl(name.value),
            }
        };
        let get_module = |module: &ModuleIdent| -> Option<Rc<RefCell<ModuleScope>>> {
            let module_scope =
                project_context.visit_address(|top| -> Option<Rc<RefCell<ModuleScope>>> {
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
                let item = ItemOrAccess::Item(Item::Use(vec![ItemUse::Module(ItemUseModule {
                    module_ident: module.clone(),
                    alias: alias.clone(),
                    members: module_scope,
                    s: None,
                    is_test: attributes_has_test(&use_decl.attributes) == AttrTest::TestOnly,
                })]));
                visitor.handle_item_or_access(self, project_context, &item);
                if visitor.finished() {
                    return;
                }
                let name = if let Some(alias) = alias {
                    alias.value()
                } else {
                    module.value.module.value()
                };
                if enter_import {
                    if let Some((addr, module_name)) = is_global {
                        project_context.enter_top_use_item(
                            self,
                            addr,
                            module_name,
                            name,
                            item,
                            is_spec_module,
                        );
                    } else {
                        project_context.enter_use_item(self, name, item);
                    }
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
                        let item =
                            ItemOrAccess::Item(Item::Use(vec![ItemUse::Module(ItemUseModule {
                                module_ident: module.clone(),
                                alias: alias.clone().map(|x| ModuleName(x)),
                                members: module_scope.clone(),
                                s: Some(member.clone()),
                                is_test: attributes_has_test(&use_decl.attributes)
                                    == AttrTest::TestOnly,
                            })]));
                        visitor.handle_item_or_access(self, project_context, &item);
                        if visitor.finished() {
                            return;
                        }
                        let name = if let Some(alias) = alias {
                            alias.value
                        } else {
                            module.value.module.value()
                        };
                        if let Some((addr, module_name)) = is_global {
                            project_context.enter_top_use_item(
                                self,
                                addr,
                                module_name,
                                name,
                                item,
                                is_spec_module,
                            );
                        } else {
                            project_context.enter_use_item(self, name, item);
                        }
                        continue;
                    }
                    let name = if let Some(alias) = alias {
                        alias.clone()
                    } else {
                        member.clone()
                    };
                    let item = ItemOrAccess::Item(Item::Use(vec![ItemUse::Item(ItemUseItem {
                        module_ident: module.clone(),
                        name: member.clone(),
                        alias: alias.clone(),
                        members: module_scope.clone(),
                        is_test: attributes_has_test(&use_decl.attributes) == AttrTest::TestOnly,
                    })]));
                    visitor.handle_item_or_access(self, project_context, &item);
                    if visitor.finished() {
                        return;
                    }
                    if enter_import {
                        if let Some((addr, module_name)) = is_global {
                            project_context.enter_top_use_item(
                                self,
                                addr,
                                module_name,
                                name.value,
                                item,
                                is_spec_module,
                            );
                        } else {
                            project_context.enter_use_item(self, name.value, item);
                        }
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
