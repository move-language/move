// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use super::{item::*, project::*, scope::*, types::*, utils::*};
use move_command_line_common::files::FileHash;
use move_compiler::parser::ast::*;
use move_core_types::account_address::AccountAddress;
use move_ir_types::location::*;
use move_symbol_pool::Symbol;
use std::{
    borrow::BorrowMut,
    cell::{Cell, RefCell},
    rc::Rc,
};

#[derive(Clone)]
pub struct ProjectContext {
    scopes: Rc<RefCell<Vec<Scope>>>,
    pub(crate) addresses: RefCell<Addresses>,
    pub(crate) addr_and_name: RefCell<AddrAndModuleName>,
    pub(crate) access_env: Cell<AccessEnv>,
}

#[derive(Clone, Copy, PartialEq)]
pub enum AccessEnv {
    Move,
    Test,
    Spec,
}
impl ProjectContext {
    pub(crate) fn clear_scopes_and_addresses(&self) {
        let d = Self::default();
        *self.scopes.as_ref().borrow_mut() = d.scopes.as_ref().borrow().clone();
        *self.addresses.borrow_mut() = Default::default();
    }
}

impl Default for ProjectContext {
    fn default() -> Self {
        let x = Self {
            scopes: Default::default(),
            addresses: Default::default(),
            addr_and_name: RefCell::new(AddrAndModuleName {
                addr: *ERR_ADDRESS,
                name: ModuleName(Spanned {
                    loc: Loc::new(FileHash::empty(), 0, 0),
                    value: Symbol::from("_"),
                }),
            }),
            access_env: Cell::new(Default::default()),
        };
        let s = Scope::default();
        x.scopes.as_ref().borrow_mut().push(s);
        x.enter_build_in();
        x
    }
}

impl Default for AccessEnv {
    fn default() -> Self {
        Self::Move
    }
}

impl ProjectContext {
    pub(crate) fn new() -> Self {
        Self::default()
    }
    /// try fix a local var type
    /// for syntax like
    /// ```move
    /// let x ;
    /// ...
    /// x = 1  // fix can happen here.
    /// ...
    /// ```
    /// This function also return a option lambda expr associate with
    /// like syntax
    /// ```move
    /// let add : |u8 , u8| u8
    /// add = |x , y | x + y
    /// ```
    #[must_use]
    pub(crate) fn try_fix_local_var_ty(
        &self,
        name: Symbol,
        tye: &ResolvedType,
    ) -> Option<LambdaExp> {
        let mut b = self.scopes.as_ref().borrow_mut();
        let mut ret = None;
        {
            let mut fixed = false;
            b.iter_mut().rev().for_each(|x| {
                if !fixed {
                    if let Some(item) = x.items.get_mut(&name) {
                        match item {
                            Item::Var { ty, .. } | Item::Parameter(_, ty) => {
                                if ty.is_err() {
                                    *ty = tye.clone();
                                    fixed = true;
                                }
                            }
                            _ => {}
                        }
                    }
                }
            });
        }
        {
            let mut fixed = false;
            b.iter_mut().rev().for_each(|x| {
                if !fixed {
                    if let Some(item) = x.items.get_mut(&name) {
                        match item {
                            Item::Var { ty, lambda, .. } => {
                                if ty.is_err() {
                                    *ty = tye.clone();
                                    fixed = true;
                                    // try visit lambda expr
                                    ret = lambda.clone();
                                }
                            }
                            _ => {}
                        }
                    }
                }
            });
        }
        ret
    }

    pub(crate) fn set_current_addr_and_module_name(&self, addr: AccountAddress, name: Symbol) {
        self.addr_and_name.borrow_mut().addr = addr;
        self.addr_and_name.borrow_mut().name = ModuleName(Spanned {
            loc: Loc::new(FileHash::empty(), 0, 0),
            value: name,
        });
    }

    pub(crate) fn module_is_test(&self, addr: AccountAddress, name: Symbol) -> Option<bool> {
        Some(
            self.addresses
                .borrow()
                .address
                .get(&addr)?
                .modules
                .get(&name)?
                .as_ref()
                .borrow()
                .is_test,
        )
    }

    pub(crate) fn get_current_addr_and_module_name(&self) -> AddrAndModuleName {
        self.addr_and_name.borrow().clone()
    }

    pub(crate) fn set_up_module(
        &self,
        addr: AccountAddress,
        module_name: ModuleName,
        is_test: bool,
    ) {
        log::info!(
            "set up module,addr:0x{:?} module_name:{:?}",
            addr.short_str_lossless(),
            module_name
        );
        if self.addresses.borrow().address.get(&addr).is_none() {
            self.addresses
                .borrow_mut()
                .address
                .insert(addr, Default::default());
        }
        let name_and_addr = AddrAndModuleName {
            name: module_name.clone(),
            addr: addr.clone(),
        };

        if let Some(scope) = self
            .addresses
            .borrow_mut()
            .address
            .get_mut(&addr)
            .unwrap()
            .modules
            .get_mut(&module_name.0.value)
        {
            scope.as_ref().borrow_mut().name_and_addr = name_and_addr;
            scope.as_ref().borrow_mut().friends = Default::default();
            return;
        }

        self.addresses
            .borrow_mut()
            .address
            .get_mut(&addr)
            .unwrap()
            .modules
            .insert(
                module_name.0.value,
                Rc::new(RefCell::new(ModuleScope::new(name_and_addr, is_test))),
            );
    }

    pub(crate) fn insert_friend(
        &self,
        addr: AccountAddress,
        module_name: Symbol,
        friend: (AccountAddress, Symbol),
    ) {
        self.visit_address(|x| {
            x.address
                .get(&addr)
                .unwrap()
                .modules
                .get(&module_name)
                .unwrap()
                .as_ref()
                .borrow_mut()
                .friends
                .insert(friend)
        });
    }

    pub(crate) fn query_item<R>(
        &self,
        addr: AccountAddress,
        module_name: Symbol,
        item_name: Symbol,
        x: impl FnOnce(&Item) -> R,
    ) -> Option<R> {
        Some(x(self
            .addresses
            .borrow()
            .address
            .get(&addr)?
            .modules
            .get(&module_name)?
            .as_ref()
            .borrow()
            .module
            .items
            .get(&item_name)?))
    }

    pub(crate) fn enter_build_in(&self) {
        self.scopes
            .as_ref()
            .borrow_mut()
            .first_mut()
            .unwrap()
            .enter_build_in();
    }
    pub(crate) fn enter_scope<R>(&self, call_back: impl FnOnce(&ProjectContext) -> R) -> R {
        let s = Scope::default();
        self.scopes.as_ref().borrow_mut().push(s);
        let _guard = ScopesGuarder::new(self.clone());
        let r = call_back(self);
        r
    }

    // Enter
    pub(crate) fn enter_item(
        &self,
        convert_loc: &dyn ConvertLoc,
        name: Symbol,
        item: impl Into<Item>,
    ) {
        let item = item.into();
        let loc = item.def_loc();
        let loc = convert_loc
            .convert_loc_range(&loc)
            .unwrap_or(FileRange::unknown());
        log::trace!("{}", loc);
        log::trace!("enter scope name:{:?} item:{}", name, item);
        self.scopes
            .as_ref()
            .borrow_mut()
            .last_mut()
            .unwrap()
            .enter_item(name, item);
    }
    pub(crate) fn enter_use_item(
        &self,
        convert_loc: &dyn ConvertLoc,
        name: Symbol,
        item: impl Into<Item>,
    ) {
        let item = item.into();
        let loc = item.def_loc();
        let loc = convert_loc
            .convert_loc_range(&loc)
            .unwrap_or(FileRange::unknown());
        log::trace!("{}", loc);
        log::trace!("enter scope name:{:?} item:{}", name, item);
        self.scopes
            .as_ref()
            .borrow_mut()
            .last_mut()
            .unwrap()
            .enter_use_item(name, item);
    }

    pub(crate) fn enter_types(
        &self,
        convert_loc: &dyn ConvertLoc,
        name: Symbol,
        item: impl Into<Item>,
    ) {
        let item = item.into();
        let loc = item.def_loc();
        let loc = convert_loc
            .convert_loc_range(&loc)
            .unwrap_or(FileRange::unknown());
        log::trace!("{}", loc);
        log::trace!("enter scope name:{:?} item:{}", name, item);
        self.scopes
            .as_ref()
            .borrow_mut()
            .last_mut()
            .unwrap()
            .enter_types(name, item);
    }

    pub(crate) fn enter_top_item(
        &self,
        convert_loc: &dyn ConvertLoc,
        address: AccountAddress,
        module: Symbol,
        item_name: Symbol,
        item: impl Into<Item>,
        is_spec_module: bool,
    ) {
        let item: Item = item.into();
        let loc = item.def_loc();
        let loc = convert_loc
            .convert_loc_range(&loc)
            .unwrap_or(FileRange::unknown());
        log::trace!("{}", loc);
        log::trace!(
            "enter top scope address:0x{:?} module:{:?} name:{:?} item:{}",
            address.short_str_lossless(),
            module,
            item_name,
            item,
        );
        if is_spec_module {
            self.addresses
                .borrow_mut()
                .address
                .get_mut(&address)
                .unwrap()
                .modules
                .get_mut(&module)
                .unwrap()
                .as_ref()
                .borrow_mut()
                .borrow_mut()
                .spec
                .enter_item(item_name, item.clone());
        } else {
            self.addresses
                .borrow_mut()
                .address
                .get_mut(&address)
                .unwrap()
                .modules
                .get_mut(&module)
                .unwrap()
                .as_ref()
                .borrow_mut()
                .borrow_mut()
                .module
                .enter_item(item_name, item.clone());
        }
    }

    pub(crate) fn enter_top_use_item(
        &self,
        convert_loc: &dyn ConvertLoc,
        address: AccountAddress,
        module: Symbol,
        item_name: Symbol,
        item: impl Into<Item>,
        is_spec_module: bool,
    ) {
        let item: Item = item.into();
        let loc = item.def_loc();
        let loc = convert_loc
            .convert_loc_range(&loc)
            .unwrap_or(FileRange::unknown());
        log::trace!("{}", loc);
        log::trace!(
            "enter top scope address:0x{:?} module:{:?} name:{:?} item:{}",
            address.short_str_lossless(),
            module,
            item_name,
            item,
        );
        if is_spec_module {
            self.addresses
                .borrow_mut()
                .address
                .get_mut(&address)
                .unwrap()
                .modules
                .get_mut(&module)
                .unwrap()
                .as_ref()
                .borrow_mut()
                .borrow_mut()
                .spec
                .enter_use_item(item_name, item.clone());
        } else {
            self.addresses
                .borrow_mut()
                .address
                .get_mut(&address)
                .unwrap()
                .modules
                .get_mut(&module)
                .unwrap()
                .as_ref()
                .borrow_mut()
                .borrow_mut()
                .module
                .enter_use_item(item_name, item.clone());
        }
    }

    /// Visit all scope from inner to outer.
    pub(crate) fn inner_first_visit(
        &self,
        mut visitor: impl FnMut(&Scope) -> bool, /*  stop??? */
    ) {
        for s in self.scopes.as_ref().borrow().iter().rev() {
            if visitor(s) {
                return;
            }
        }
    }

    /// If none of item enter could be None.
    fn clone_scope(
        &self,
        addr: AccountAddress,
        module_name: Symbol,
        is_spec: bool,
    ) -> Option<Scope> {
        Some({
            let x = self
                .addresses
                .borrow()
                .address
                .get(&addr)?
                .modules
                .get(&module_name)?
                .as_ref()
                .borrow()
                .clone();
            if is_spec {
                x.clone_spec_scope()
            } else {
                x.clone_module_scope()
            }
        })
    }

    pub(crate) fn clone_scope_and_enter(
        &self,
        addr: AccountAddress,
        module_name: Symbol,
        is_spec: bool,
    ) -> ScopesGuarder {
        self.enter_scope_guard(
            self.clone_scope(addr, module_name, is_spec)
                .unwrap_or(Default::default()),
        )
    }

    pub(crate) fn set_access_env(&self, x: AccessEnv) -> AccessEnv {
        self.access_env.replace(x)
    }

    pub(crate) fn find_var_type(&self, name: Symbol) -> ResolvedType {
        let mut ret = None;
        self.inner_first_visit(|s| {
            if let Some(v) = s.items.get(&name) {
                match v {
                    Item::Parameter(_, ty) | Item::Var { ty, .. } => {
                        ret = Some(ty.clone());
                        return true;
                    }
                    _ => {}
                }
            };
            false
        });
        return ResolvedType::UnKnown;
    }

    pub(crate) fn find_name_chain_item(
        &self,
        chain: &NameAccessChain,
        name_to_addr: &impl Name2Addr,
    ) -> (
        Option<Item>,
        Option<AddrAndModuleName>, /* with a possible module loc returned  */
    ) {
        let mut item_ret = None;
        let mut module_scope = None;
        match &chain.value {
            NameAccessChain_::One(name) => {
                self.inner_first_visit(|s| {
                    if let Some(v) = if let Some(x) = s.items.get(&name.value) {
                        Some(x)
                    } else {
                        s.uses.get(&name.value)
                    } {
                        match v {
                            Item::Use(x) => {
                                for x in x.iter() {
                                    match x {
                                        ItemUse::Module(_) => {}
                                        ItemUse::Item(_) => {
                                            item_ret = Some(v.clone());
                                            return true;
                                        }
                                    }
                                }
                            }
                            _ => {
                                item_ret = Some(v.clone());
                                return true;
                            }
                        }
                    }
                    false
                });
            }
            NameAccessChain_::Two(name, member) => {
                match name.value {
                    LeadingNameAccess_::Name(name) => {
                        self.inner_first_visit(|s| {
                            if let Some(v) = s.uses.get(&name.value) {
                                match v {
                                    Item::Use(x) => {
                                        for x in x.iter() {
                                            match x {
                                                ItemUse::Module(ItemUseModule {
                                                    members, ..
                                                }) => {
                                                    if let Some(item) = members
                                                        .as_ref()
                                                        .borrow()
                                                        .module
                                                        .items
                                                        .get(&member.value)
                                                    {
                                                        module_scope = Some(
                                                            members
                                                                .as_ref()
                                                                .borrow()
                                                                .name_and_addr
                                                                .clone(),
                                                        );
                                                        item_ret = Some(item.clone());
                                                        // make inner_first_visit stop.
                                                        return true;
                                                    }
                                                    if let Some(item) = members
                                                        .as_ref()
                                                        .borrow()
                                                        .spec
                                                        .items
                                                        .get(&member.value)
                                                    {
                                                        module_scope = Some(
                                                            members
                                                                .as_ref()
                                                                .borrow()
                                                                .name_and_addr
                                                                .clone(),
                                                        );
                                                        item_ret = Some(item.clone());
                                                        // make inner_first_visit stop.
                                                        return true;
                                                    }
                                                }
                                                ItemUse::Item(_) => {}
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            false
                        });
                    }
                    LeadingNameAccess_::AnonymousAddress(addr) => {
                        let x = self.visit_address(|x| -> Option<AddrAndModuleName> {
                            Some(
                                x.address
                                    .get(&addr.into_inner())?
                                    .modules
                                    .get(&member.value)?
                                    .as_ref()
                                    .borrow()
                                    .name_and_addr
                                    .clone(),
                            )
                        });
                        module_scope = x;
                    }
                }
            }
            NameAccessChain_::Three(chain_two, member) => self.visit_address(|top| {
                let modules = top.address.get(&match &chain_two.value.0.value {
                    LeadingNameAccess_::AnonymousAddress(x) => x.into_inner(),
                    LeadingNameAccess_::Name(name) => name_to_addr.name_2_addr(name.value),
                });
                if modules.is_none() {
                    return;
                }
                let modules = modules.unwrap();
                let module = modules.modules.get(&chain_two.value.1.value);
                if module.is_none() {
                    return;
                }
                let module = module.unwrap();
                module_scope = Some(module.as_ref().borrow().name_and_addr.clone());
                if let Some(item) = module.as_ref().borrow().module.items.get(&member.value) {
                    item_ret = Some(item.clone());
                    return;
                } else if let Some(item) = module.as_ref().borrow().spec.items.get(&member.value) {
                    item_ret = Some(item.clone());
                    return;
                }
            }),
        }
        return (item_ret, module_scope);
    }

    pub(crate) fn find_name_chain_ty(
        &self,
        chain: &NameAccessChain,
        name_to_addr: &impl Name2Addr,
    ) -> (
        Option<ResolvedType>,
        Option<AddrAndModuleName>, /* with a possible module loc returned  */
    ) {
        let mut item_ret = None;
        let mut module_scope = None;
        match &chain.value {
            NameAccessChain_::One(name) => {
                self.inner_first_visit(|s| {
                    if let Some(v) = s.types.get(&name.value) {
                        if let Some(t) = v.to_type() {
                            item_ret = Some(t);
                            return true;
                        }
                    }

                    if let Some(v) = s.items.get(&name.value) {
                        if let Some(t) = v.to_type() {
                            item_ret = Some(t);
                            return true;
                        }
                    }
                    if let Some(v) = s.uses.get(&name.value) {
                        if let Some(t) = v.to_type() {
                            item_ret = Some(t);
                            return true;
                        }
                    }
                    false
                });
            }
            NameAccessChain_::Two(name, member) => match name.value {
                LeadingNameAccess_::Name(name) => {
                    self.inner_first_visit(|s| {
                        if let Some(v) = s.uses.get(&name.value) {
                            match v {
                                Item::Use(x) => {
                                    for x in x.iter() {
                                        match x {
                                            ItemUse::Module(ItemUseModule { members, .. }) => {
                                                if let Some(item) = members
                                                    .as_ref()
                                                    .borrow()
                                                    .module
                                                    .items
                                                    .get(&member.value)
                                                {
                                                    module_scope = Some(
                                                        members
                                                            .as_ref()
                                                            .borrow()
                                                            .name_and_addr
                                                            .clone(),
                                                    );
                                                    if let Some(t) = item.to_type() {
                                                        item_ret = Some(t);
                                                        return true;
                                                    }
                                                }
                                            }
                                            ItemUse::Item(_) => {}
                                        }
                                    }
                                }

                                _ => {}
                            }
                        }
                        false
                    });
                }
                LeadingNameAccess_::AnonymousAddress(addr) => {
                    let x = self.visit_address(|x| -> Option<AddrAndModuleName> {
                        Some(
                            x.address
                                .get(&addr.into_inner())?
                                .modules
                                .get(&member.value)?
                                .as_ref()
                                .borrow()
                                .name_and_addr
                                .clone(),
                        )
                    });
                    module_scope = x;
                }
            },
            NameAccessChain_::Three(chain_two, member) => self.visit_address(|top| {
                let modules = top.address.get(&match &chain_two.value.0.value {
                    LeadingNameAccess_::AnonymousAddress(x) => x.into_inner(),
                    LeadingNameAccess_::Name(name) => name_to_addr.name_2_addr(name.value),
                });
                if modules.is_none() {
                    return;
                }
                let modules = modules.unwrap();
                let module = modules.modules.get(&chain_two.value.1.value);
                if module.is_none() {
                    return;
                }
                let module = module.unwrap();
                module_scope = Some(module.as_ref().borrow().name_and_addr.clone());
                if let Some(item) = module.as_ref().borrow().module.items.get(&member.value) {
                    if let Some(t) = item.to_type() {
                        item_ret = Some(t);
                    }
                    return;
                } else if let Some(item) = module.as_ref().borrow().spec.items.get(&member.value) {
                    if let Some(t) = item.to_type() {
                        item_ret = Some(t);
                    }
                    return;
                }
            }),
        }
        return (item_ret, module_scope);
    }

    pub(crate) fn find_var(&self, name: Symbol) -> Option<Item> {
        let mut r = None;
        self.inner_first_visit(|scope| {
            if let Some(item) = scope.items.get(&name) {
                match item {
                    Item::Var { .. } | Item::Parameter(_, _) => {
                        r = Some(item.clone());
                        return true;
                    }
                    _ => {}
                }
            }
            false
        });
        r
    }

    pub(crate) fn visit_address<R>(&self, x: impl FnOnce(&Addresses) -> R) -> R {
        x(&self.addresses.borrow())
    }

    #[must_use]
    pub(crate) fn enter_scope_guard(&self, s: Scope) -> ScopesGuarder {
        self.scopes.as_ref().borrow_mut().push(s);
        ScopesGuarder::new(self.clone())
    }

    pub(crate) fn resolve_type(&self, ty: &Type, name_to_addr: &impl Name2Addr) -> ResolvedType {
        let r = match &ty.value {
            Type_::Apply(ref chain, types) => {
                // Special handle for vector.
                let types: Vec<_> = types
                    .iter()
                    .map(|ty| self.resolve_type(ty, name_to_addr))
                    .collect();
                match &chain.value {
                    NameAccessChain_::One(name) => {
                        if name.value.as_str() == "vector" {
                            let e_ty = types.get(0).unwrap_or(&UNKNOWN_TYPE).clone();
                            return ResolvedType::new_vector(e_ty);
                        }
                    }
                    _ => {}
                }
                let (chain_ty, _) = self.find_name_chain_ty(chain, name_to_addr);
                let mut chain_ty = chain_ty.unwrap_or_default();
                let chain_ty = match &mut chain_ty {
                    ResolvedType::Struct(
                        ItemStructNameRef {
                            type_parameters: _type_parameters,
                            ..
                        },
                        m,
                    ) => {
                        let _ = std::mem::replace(m, types);
                        chain_ty
                    }
                    _ => chain_ty,
                };
                return chain_ty;
            }
            Type_::Ref(m, ref b) => {
                ResolvedType::Ref(*m, Box::new(self.resolve_type(b.as_ref(), name_to_addr)))
            }
            Type_::Fun(args, ret_ty) => {
                let args: Vec<_> = args
                    .iter()
                    .map(|x| self.resolve_type(x, name_to_addr))
                    .collect();
                let ret_ty = self.resolve_type(ret_ty.as_ref(), name_to_addr);
                ResolvedType::Lambda {
                    args,
                    ret_ty: Box::new(ret_ty),
                }
            }

            Type_::Unit => ResolvedType::Unit,
            Type_::Multiple(ref types) => {
                let types: Vec<_> = types
                    .iter()
                    .map(|v| self.resolve_type(v, name_to_addr))
                    .collect();
                ResolvedType::Multiple(types)
            }
        };
        r
    }

    pub(crate) fn delete_module_items(
        &self,
        addr: AccountAddress,
        module_name: Symbol,
        is_spec_module: bool,
    ) {
        let delete_module_items = || -> Option<()> {
            if is_spec_module {
                self.addresses
                    .borrow_mut()
                    .address
                    .get_mut(&addr)?
                    .modules
                    .get_mut(&module_name)?
                    .as_ref()
                    .borrow_mut()
                    .spec
                    .items
                    .clear();
                self.addresses
                    .borrow_mut()
                    .address
                    .get_mut(&addr)?
                    .modules
                    .get_mut(&module_name)?
                    .as_ref()
                    .borrow_mut()
                    .spec
                    .uses
                    .clear();
                return None;
            } else {
                self.addresses
                    .borrow_mut()
                    .address
                    .get_mut(&addr)?
                    .modules
                    .get_mut(&module_name)?
                    .as_ref()
                    .borrow_mut()
                    .module
                    .items
                    .clear();
                self.addresses
                    .borrow_mut()
                    .address
                    .get_mut(&addr)?
                    .modules
                    .get_mut(&module_name)?
                    .as_ref()
                    .borrow_mut()
                    .module
                    .uses
                    .clear();
                self.addresses
                    .borrow_mut()
                    .address
                    .get_mut(&addr)?
                    .modules
                    .get_mut(&module_name)?
                    .as_ref()
                    .borrow_mut()
                    .friends
                    .clear();
                return None;
            }
        };
        delete_module_items();
    }

    pub(crate) fn resolve_friend(&self, addr: AccountAddress, name: Symbol) -> Option<ModuleName> {
        self.visit_address(|x| {
            Some(
                x.address
                    .get(&addr)?
                    .modules
                    .get(&name)?
                    .as_ref()
                    .borrow()
                    .name_and_addr
                    .name
                    .clone(),
            )
        })
    }
}

/// RAII type pop on when enter a scope.
#[must_use]
pub(crate) struct ScopesGuarder(Rc<RefCell<Vec<Scope>>>);

impl ScopesGuarder {
    pub(crate) fn new(s: ProjectContext) -> Self {
        Self(s.scopes.clone())
    }
}

impl Drop for ScopesGuarder {
    fn drop(&mut self) {
        self.0.as_ref().borrow_mut().pop().unwrap();
    }
}
