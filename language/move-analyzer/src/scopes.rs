#![allow(dead_code)]

use super::item::*;
use super::modules::*;
use super::scope::*;
use super::types::*;
use super::utils::*;
use move_compiler::parser::ast::*;
use move_core_types::account_address::AccountAddress;
use move_symbol_pool::Symbol;
use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Scopes {
    scopes: Rc<RefCell<Vec<Scope>>>,
    pub(crate) addresses: RefCell<Addresses>,
}
impl Scopes {
    pub(crate) fn new() -> Self {
        let x = Self {
            scopes: Default::default(),
            addresses: Default::default(),
        };
        let s = Scope::default();
        x.scopes.as_ref().borrow_mut().push(s);
        x.enter_build_in();
        x
    }
    pub(crate) fn set_up_module(&self, addr: AccountAddress, module_name: ModuleName) {
        log::info!(
            "set up module,addr:{:?} module_name:{:?}",
            addr,
            module_name
        );
        if self.addresses.borrow().address.get(&addr).is_none() {
            self.addresses
                .borrow_mut()
                .address
                .insert(addr, Default::default());
        }
        let s = ModuleScope::new(ModuleNameAndAddr {
            name: module_name.clone(),
            addr: addr.clone(),
        });
        if let Some(scope) = self
            .addresses
            .borrow_mut()
            .address
            .get_mut(&addr)
            .unwrap()
            .modules
            .get_mut(&module_name.0.value)
        {
            scope.as_ref().borrow_mut().module.module_name_and_addr = s.module.module_name_and_addr;
            return;
        }
        self.addresses
            .borrow_mut()
            .address
            .get_mut(&addr)
            .unwrap()
            .modules
            .insert(module_name.0.value, Rc::new(RefCell::new(s)));
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
    pub(crate) fn enter_scope<R>(&self, call_back: impl FnOnce(&Scopes) -> R) -> R {
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
        log::info!("{}", loc);
        log::info!(
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
                .items
                .insert(item_name, item.clone());
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
                .items
                .insert(item_name, item.clone());
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
        is_spec_module: bool,
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
            if is_spec_module {
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
        is_spec_module: bool,
    ) -> ScopesGuarder {
        self.enter_scope_guard(
            self.clone_scope(addr, module_name, is_spec_module)
                .unwrap_or(Default::default()),
        )
    }

    ///
    pub(crate) fn setup_scope(&self, f: impl FnOnce(&mut Scope)) {
        let mut x = self.scopes.as_ref().borrow_mut();
        let x = x.last_mut().unwrap();
        f(x);
    }

    pub(crate) fn under_spec(&self) -> bool {
        let mut r = false;
        self.inner_first_visit(|s| {
            if s.is_spec {
                r = true;
                return true;
            }
            false
        });
        r
    }

    pub(crate) fn find_var_type(&self, name: Symbol) -> ResolvedType {
        let mut ret = None;
        self.inner_first_visit(|s| {
            if let Some(v) = s.items.get(&name) {
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
        return ResolvedType::UnKnown;
    }

    pub(crate) fn find_name_chain_item(
        &self,
        chain: &NameAccessChain,
        name_to_addr: &dyn Name2Addr,
    ) -> (
        Option<Item>,
        Option<ModuleNameAndAddr>, /* with a possible module loc returned  */
    ) {
        let under_spec = self.under_spec();

        let mut item_ret = None;
        let mut module_scope = None;
        match &chain.value {
            NameAccessChain_::One(name) => {
                self.inner_first_visit(|s| {
                    if let Some(v) = s.items.get(&name.value) {
                        item_ret = Some(v.clone());
                        return true;
                    }
                    false
                });
            }
            NameAccessChain_::Two(name, member) => {
                match name.value {
                    LeadingNameAccess_::Name(name) => {
                        self.inner_first_visit(|s| {
                            if let Some(v) = s.items.get(&name.value) {
                                match v {
                                    Item::UseModule(_, _, members, _) => {
                                        if let Some(item) = members
                                            .as_ref()
                                            .borrow()
                                            .module
                                            .items
                                            .get(&member.value)
                                        {
                                            module_scope = members
                                                .as_ref()
                                                .borrow()
                                                .module
                                                .module_name_and_addr
                                                .clone();
                                            item_ret = Some(item.clone());
                                            // make inner_first_visit stop.
                                            return true;
                                        }
                                        if under_spec {
                                            if let Some(item) = members
                                                .as_ref()
                                                .borrow()
                                                .spec
                                                .items
                                                .get(&member.value)
                                            {
                                                module_scope = members
                                                    .as_ref()
                                                    .borrow()
                                                    .module
                                                    .module_name_and_addr
                                                    .clone();
                                                item_ret = Some(item.clone());
                                                // make inner_first_visit stop.
                                                return true;
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
                        let x = self.visit_address(|x| -> Option<ModuleNameAndAddr> {
                            x.address
                                .get(&addr.bytes)?
                                .modules
                                .get(&member.value)?
                                .as_ref()
                                .borrow()
                                .module
                                .module_name_and_addr
                                .clone()
                        });
                        module_scope = x;
                    }
                }
            }
            NameAccessChain_::Three(chain_two, member) => self.visit_address(|top| {
                let modules = top.address.get(&match &chain_two.value.0.value {
                    LeadingNameAccess_::AnonymousAddress(x) => x.bytes,
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
                module_scope = module.as_ref().borrow().module.module_name_and_addr.clone();
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

    pub(crate) fn find_name_chain_type<'a>(
        &self,
        chain: &NameAccessChain,
        name_to_addr: &dyn Name2Addr,
    ) -> ResolvedType {
        let failed = ResolvedType::new_unknown();
        match &chain.value {
            NameAccessChain_::One(name) => {
                let mut r = None;
                self.inner_first_visit(|s| {
                    if let Some(v) = s.items.get(&name.value) {
                        r = v.to_type();
                        if r.is_some() {
                            return true;
                        }
                    }
                    if let Some(v) = s.types.get(&name.value) {
                        r = v.to_type();
                        if r.is_some() {
                            return true;
                        }
                    }
                    false
                });
                r.unwrap_or(failed)
            }

            NameAccessChain_::Two(name, member) => {
                // first find this name.
                let mut r = None;
                match name.value {
                    LeadingNameAccess_::Name(name) => {
                        self.inner_first_visit(|s| {
                            if let Some(v) = s.items.get(&name.value) {
                                match v {
                                    Item::UseModule(_, _, members, _) => {
                                        if let Some(item) = members
                                            .as_ref()
                                            .borrow()
                                            .module
                                            .items
                                            .get(&member.value)
                                        {
                                            if let Some(ty) = item.to_type() {
                                                r = Some(ty);
                                                return true; // make inner_first_visit stop.
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            false
                        });
                    }
                    LeadingNameAccess_::AnonymousAddress(_) => {
                        r = Some(ResolvedType::new_unknown());
                    }
                }
                r.unwrap_or(failed)
            }
            NameAccessChain_::Three(chain_two, member) => self.visit_address(|top| {
                let modules = top.address.get(&match &chain_two.value.0.value {
                    LeadingNameAccess_::AnonymousAddress(x) => x.bytes,
                    LeadingNameAccess_::Name(name) => name_to_addr.name_2_addr(name.value),
                });
                if modules.is_none() {
                    return failed;
                }
                let modules = modules.unwrap();
                let module = modules.modules.get(&chain_two.value.1.value);
                if module.is_none() {
                    return failed;
                }
                let module = module.unwrap();
                if let Some(item) = module.as_ref().borrow().module.items.get(&member.value) {
                    item.to_type().unwrap_or(failed)
                } else {
                    failed
                }
            }),
        }
    }

    pub(crate) fn visit_address<R>(&self, x: impl FnOnce(&Addresses) -> R) -> R {
        x(&self.addresses.borrow())
    }

    #[must_use]
    pub(crate) fn enter_scope_guard(&self, s: Scope) -> ScopesGuarder {
        self.scopes.as_ref().borrow_mut().push(s);
        ScopesGuarder::new(self.clone())
    }

    pub(crate) fn resolve_type(&self, ty: &Type, name_to_addr: &dyn Name2Addr) -> ResolvedType {
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
                    NameAccessChain_::Two(_, _) => {}
                    NameAccessChain_::Three(_, _) => {}
                }
                let mut chain_ty = self.find_name_chain_type(chain, name_to_addr);
                let chain_ty = match &mut chain_ty {
                    ResolvedType::Struct(x) => {
                        let mut m = HashMap::new();
                        for (name, ty) in x.type_parameters.iter().zip(types.iter()) {
                            m.insert(name.name.value, ty.clone());
                        }
                        let mut x = chain_ty.clone();
                        x.bind_type_parameter(&m, self);
                        x
                    }
                    ResolvedType::StructRef(_, _, _, type_paras, m) => {
                        for (name, ty) in type_paras.iter().zip(types.iter()) {
                            m.insert(name.name.value, ty.clone());
                        }
                        chain_ty
                    }
                    _ => chain_ty,
                };
                return chain_ty;
            }
            Type_::Ref(m, ref b) => {
                ResolvedType::Ref(*m, Box::new(self.resolve_type(b.as_ref(), name_to_addr)))
            }
            Type_::Fun(_, _) => {
                log::error!("fun is not first class type.");
                ResolvedType::UnKnown
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
    ) -> bool // deleted ???
    {
        let delete_module_items = || {
            if is_spec_module {
                Some(
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
                        .clear(),
                )
            } else {
                Some(
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
                        .clear(),
                )
            }
        };
        delete_module_items().map(|_| true).unwrap_or_default()
    }

    #[allow(dead_code)]
    pub(crate) fn current_scope_mut<R>(&self, x: impl FnOnce(&mut Scope) -> R) -> R {
        let mut s = self.scopes.as_ref().borrow_mut();
        x(s.last_mut().unwrap())
    }
    pub(crate) fn resolve_friend(&self, addr: AccountAddress, name: Symbol) -> Option<ModuleName> {
        self.visit_address(|x| {
            x.address
                .get(&addr)?
                .modules
                .get(&name)?
                .as_ref()
                .borrow()
                .module
                .module_name_and_addr
                .as_ref()
                .map(|x| x.name.clone())
        })
    }

    /// Collect all spec schema.
    pub(crate) fn collect_all_spec_schema(&self) -> Vec<Item> {
        let mut ret = Vec::new();
        self.inner_first_visit(|scope| {
            for (_, item) in scope.items.iter() {
                match item {
                    Item::SpecSchema(_, _) => {
                        ret.push(item.clone());
                    }
                    _ => {}
                }
            }
            false
        });
        ret
    }

    pub(crate) fn collect_all_spec_target(&self) -> Vec<Item> {
        let mut ret = Vec::new();
        self.inner_first_visit(|scope| {
            for (_, item) in scope.items.iter() {
                match item {
                    Item::Struct(_) | Item::StructNameRef(_, _, _, _) | Item::Fun(_) => {
                        ret.push(item.clone());
                    }
                    _ => {}
                }
            }
            false
        });
        ret
    }

    /// Collect type item in all nest scopes.
    pub(crate) fn collect_all_type_items(&self) -> Vec<Item> {
        let mut ret = Vec::new();
        let item_ok = |item: &Item| -> bool {
            match item {
                Item::TParam(_, _) => true,
                Item::Struct(_) | Item::StructNameRef(_, _, _, _) => true,
                Item::BuildInType(_) => true,
                Item::UseMember(_, _, _, _) | Item::UseModule(_, _, _, _) => true,
                _ => false,
            }
        };

        self.inner_first_visit(|scope| {
            for (_, item) in scope.types.iter().chain(scope.items.iter()) {
                match item {
                    Item::UseMember(_, name, _, rc) => {
                        // TODO this could be a type like struct.
                        // do a query to if if this is a type.
                        let x = rc
                            .as_ref()
                            .borrow()
                            .module
                            .items
                            .get(&name.value)
                            .map(|item| item_ok(item));
                        if x.unwrap_or(true) {
                            ret.push(item.clone());
                        }
                    }
                    _ => {
                        if item_ok(item) {
                            ret.push(item.clone());
                        }
                    }
                };
            }
            false
        });
        ret
    }

    /// Collect all expr name items.
    ///
    pub(crate) fn collect_all_var_items(&self) -> Vec<Item> {
        let mut ret = Vec::new();
        self.inner_first_visit(|scope| {
            for (_, item) in scope.types.iter().chain(scope.items.iter()) {
                match item {
                    // collect var parameter and const.
                    Item::Var(_, _) | Item::Parameter(_, _) | Item::Const(_, _) => {
                        ret.push(item.clone());
                    }
                    _ => {}
                };
            }
            false
        });
        ret
    }
    /// Collect all item in nest scopes.
    pub(crate) fn collect_items(&self, x: impl Fn(&Item, bool) -> bool) -> Vec<Item> {
        let under_spec = self.under_spec();
        let mut ret = Vec::new();
        self.inner_first_visit(|scope| {
            for (_, item) in scope.types.iter().chain(scope.items.iter()) {
                if x(item, under_spec) {
                    ret.push(item.clone());
                } else {
                    // eprintln!("item skiped:{}", item);
                }
            }
            false
        });
        ret
    }

    /// Collect all import modules.
    /// like use 0x1::vector.
    pub(crate) fn collect_imported_modules(&self) -> Vec<Item> {
        let mut ret = Vec::new();
        self.inner_first_visit(|scope| {
            for (_, item) in scope.types.iter().chain(scope.items.iter()) {
                match item {
                    Item::UseModule(_, _, _, _) => {
                        ret.push(item.clone());
                    }
                    _ => {}
                };
            }
            false
        });
        ret
    }

    /// Collect all items in a module like 0x1::vector.
    pub(crate) fn collect_use_module_items(
        &self,
        name: &LeadingNameAccess,
        select_item: impl Fn(&Item, bool) -> bool,
    ) -> Vec<Item> {
        let under_spec = self.under_spec();
        let mut ret = Vec::new();
        let name = match &name.value {
            LeadingNameAccess_::AnonymousAddress(addr) => {
                log::error!("addr:{:?} should not be here.", addr);
                return ret;
            }
            LeadingNameAccess_::Name(name) => name.value,
        };
        self.inner_first_visit(|scope| {
            for (name2, item) in scope.types.iter().chain(scope.items.iter()) {
                match item {
                    Item::UseModule(_, _, s, _) => {
                        if name == *name2 {
                            s.borrow().module.items.iter().for_each(|(_, item)| {
                                if select_item(item, under_spec) {
                                    ret.push(item.clone());
                                }
                            });
                            s.borrow().spec.items.iter().for_each(|(_, item)| {
                                if select_item(item, under_spec) {
                                    ret.push(item.clone());
                                }
                            });
                            return true;
                        };
                    }
                    _ => {}
                };
            }
            false
        });
        ret
    }

    /// Collect all module names in a addr.
    /// like module 0x1::vector{ ... }
    pub(crate) fn collect_modules(&self, addr: &AccountAddress) -> Vec<ModuleName> {
        let empty = Default::default();
        let mut ret = Vec::new();
        self.visit_address(|x| {
            x.address
                .get(addr)
                .unwrap_or(&empty)
                .modules
                .iter()
                .for_each(|(_, x)| {
                    if let Some(name) = x.borrow().module.module_name_and_addr.clone() {
                        ret.push(name.name.clone());
                    }
                })
        });
        ret
    }

    /// Collect all module names in a addr.
    /// like module 0x1::vector{ ... }
    pub(crate) fn collect_modules_items(
        &self,
        addr: &AccountAddress,
        module_name: Symbol,
        filter: impl Fn(&Item, bool) -> bool,
    ) -> Vec<Item> {
        let under_spec = self.under_spec();
        let empty = Default::default();
        let empty2 = Default::default();
        let mut ret = Vec::new();
        self.visit_address(|x| {
            x.address
                .get(addr)
                .unwrap_or(&empty)
                .modules
                .get(&module_name)
                .unwrap_or(&empty2)
                .borrow()
                .module
                .items
                .iter()
                .for_each(|(_, x)| {
                    if filter(x, under_spec) {
                        ret.push(x.clone())
                    }
                });

            if under_spec {
                x.address
                    .get(addr)
                    .unwrap_or(&empty)
                    .modules
                    .get(&module_name)
                    .unwrap_or(&empty2)
                    .borrow()
                    .spec
                    .items
                    .iter()
                    .for_each(|(_, x)| {
                        if filter(x, under_spec) {
                            ret.push(x.clone())
                        }
                    });
            }
        });
        ret
    }
}

/// RAII type pop on when enter a scope.
#[must_use]
pub(crate) struct ScopesGuarder(Scopes);

impl ScopesGuarder {
    pub(crate) fn new(s: Scopes) -> Self {
        Self(s)
    }
}

impl Drop for ScopesGuarder {
    fn drop(&mut self) {
        self.0.scopes.as_ref().borrow_mut().pop().unwrap();
    }
}
