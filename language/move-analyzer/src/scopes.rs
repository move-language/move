#![allow(dead_code)]
use super::item::*;
use super::modules::*;
use super::scope::*;
use super::types::*;
use super::utils::*;
use move_compiler::{parser::ast::*, shared::*};
use move_core_types::account_address::AccountAddress;
use move_core_types::effects::Op;
use move_ir_types::location::Loc;
use move_symbol_pool::Symbol;

use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::rc::Rc;
#[derive(Clone)]
pub struct Scopes {
    scopes: Rc<RefCell<Vec<Scope>>>,
    pub(crate) addresses: RefCell<Addresses>,
}

impl Scopes {
    pub(crate) fn new() -> Self {
        let x = Scopes {
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
        let mut s = Scope::default();
        s.module_ = Some(ModuleScope {
            name: module_name.clone(),
            addr: addr.clone(),
        });

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
        log::info!("{}", loc);
        log::info!("enter scope name:{:?} item:{}", name, item);
        self.scopes
            .as_ref()
            .borrow_mut()
            .last_mut()
            .unwrap()
            .enter_item(name, item);
    }

    pub(crate) fn enter_top_item(
        &self,
        convert_loc: &dyn ConvertLoc,
        address: AccountAddress,
        module: Symbol,
        item_name: Symbol,
        item: impl Into<Item>,
    ) {
        let item: Item = item.into();
        let loc = item.def_loc();
        let loc = convert_loc
            .convert_loc_range(&loc)
            .unwrap_or(FileRange::unknown());
        log::info!("{}", loc);
        log::info!(
            "enter top scope address:{:?} module:{:?} name:{:?} item:{}",
            address,
            module,
            item_name,
            item,
        );
        if !self.addresses.borrow().address.contains_key(&address) {
            self.addresses
                .borrow_mut()
                .address
                .insert(address, Default::default());
        }
        if !self
            .addresses
            .borrow()
            .address
            .get(&address)
            .unwrap()
            .modules
            .contains_key(&module)
        {
            self.addresses
                .borrow_mut()
                .address
                .get_mut(&address)
                .unwrap()
                .modules
                .insert(module, Default::default());
        }

        // finally, OK to borrow.
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
            .items
            .insert(item_name, item.clone());
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
    fn clone_scope(&self, addr: AccountAddress, module_name: Symbol) -> Option<Scope> {
        Some(
            self.addresses
                .borrow()
                .address
                .get(&addr)?
                .modules
                .get(&module_name)?
                .as_ref()
                .borrow()
                .clone(),
        )
    }
    pub(crate) fn clone_scope_and_enter(
        &self,
        addr: AccountAddress,
        module_name: Symbol,
    ) -> ScopesGuarder {
        self.enter_scope_guard(
            self.clone_scope(addr, module_name)
                .unwrap_or(Default::default()),
        )
    }

    ///
    pub(crate) fn setup_scope(&self, f: impl FnOnce(&mut Scope)) {
        let mut x = self.scopes.as_ref().borrow_mut();
        let x = x.last_mut().unwrap();
        f(x);
    }

    pub(crate) fn under_function(&self) -> Option<()> {
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

    pub(crate) fn under_spec(&self) -> Option<()> {
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

    pub(crate) fn find_name_chain_type<'a>(
        &self,
        chain: &NameAccessChain,
        item_ret: &mut Option<Item>,
        name_to_addr: &dyn Name2Addr,
        accept_tparam: bool,
    ) -> ResolvedType {
        let failed = ResolvedType::new_unknown();
        match &chain.value {
            NameAccessChain_::One(name) => {
                let mut r = None;
                self.inner_first_visit(|s| {
                    if let Some(v) = s.items.get(&name.value) {
                        r = v.to_type(accept_tparam);
                        if r.is_some() {
                            let _ = std::mem::replace(item_ret, Some(v.clone()));
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
                                    Item::UseModule(_, _, members) => {
                                        if let Some(item) =
                                            members.as_ref().borrow().items.get(&member.value)
                                        {
                                            let _ = std::mem::replace(item_ret, Some(item.clone()));
                                            if let Some(ty) = item.to_type(false) {
                                                r = Some(ty);
                                                let _ =
                                                    std::mem::replace(item_ret, Some(item.clone()));
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
                        unreachable!()
                    }
                }
                r.unwrap_or(failed)
            }
            NameAccessChain_::Three(chain_two, member) => self.visit_address(|top| {
                let modules = top.address.get(&match &chain_two.value.0.value {
                    LeadingNameAccess_::AnonymousAddress(x) => x.bytes,
                    LeadingNameAccess_::Name(name) => name_to_addr.convert(name.value),
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
                if let Some(item) = module.as_ref().borrow().items.get(&member.value) {
                    let _ = std::mem::replace(item_ret, Some(item.clone()));
                    item.to_type(false).unwrap_or(failed)
                } else {
                    failed
                }
            }),
        }
    }

    pub(crate) fn visit_address<R>(&self, x: impl FnOnce(&Addresses) -> R) -> R {
        x(&self.addresses.borrow())
    }

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

                let mut chain_ty = self.find_name_chain_type(chain, &mut None, name_to_addr, true);
                // Todo bind

                let names = chain_ty.collect_type_parameter_name(self);
                let mut m = std::collections::hash_map::HashMap::new();
                for (name, ty) in names.iter().zip(types.iter()) {
                    m.insert(name.clone(), ty.clone());
                }
                chain_ty.bind_type_parameter(&m, self);

                return chain_ty;
            }
            Type_::Ref(m, ref b) => {
                ResolvedType::Ref(*m, Box::new(self.resolve_type(b.as_ref(), name_to_addr)))
            }
            Type_::Fun(ref parameters, ref ret) => {
                let parameters: Vec<_> = parameters
                    .iter()
                    .map(|v| {
                        (
                            Var(Name::new(todo!(), Symbol::from("_"))),
                            self.resolve_type(v, name_to_addr),
                        )
                    })
                    .collect();
                let ret = self.resolve_type(ret.as_ref(), name_to_addr);
                ResolvedType::Fun(ItemFunction {
                    name: todo!(),
                    type_parameters: vec![],
                    parameters,
                    ret_type: Box::new(ret),
                })
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
                .module_
                .as_ref()
                .map(|x| x.name.clone())
        })
    }
}

/// RAII type pop on when enter a scope.
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
