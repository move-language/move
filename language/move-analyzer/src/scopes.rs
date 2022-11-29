#![allow(dead_code)]
use super::item::*;
use super::modules::*;
use super::scope::*;
use super::types::*;
use super::utils::*;
use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::rc::Rc;

use move_compiler::{parser::ast::*, shared::*};
use move_core_types::account_address::AccountAddress;
use move_ir_types::location::Loc;
use move_ir_types::location::Spanned;
use move_symbol_pool::Symbol;

#[derive(Clone)]
pub struct Scopes {
    scopes: Rc<RefCell<Vec<Scope>>>,
}

impl Scopes {
    pub(crate) fn new() -> Self {
        let x = Scopes {
            scopes: Default::default(),
        };
        let s = Scope::new_top();
        x.scopes.as_ref().borrow_mut().push(s);
        x.enter_build_in();
        x
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

    pub(crate) fn enter_scope_guard(&self) -> ScopesGuarder {
        let s = Scope::default();
        self.scopes.as_ref().borrow_mut().push(s);
        ScopesGuarder::new(self.clone())
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
            .convert_loc_range(loc)
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
            .convert_loc_range(loc)
            .unwrap_or(FileRange::unknown());
        log::trace!("{}", loc);
        log::trace!(
            "enter top scope address:{:?} module:{:?} name:{:?} item:{}",
            address,
            module,
            item_name,
            item,
        );
        let mut b = self.scopes.as_ref().borrow_mut();
        let mut s = b.first_mut().unwrap();
        if s.addresses.is_none() {
            s.addresses = Some(Addresses::new());
        };
        let t = s.addresses.as_mut().unwrap();
        if !t.address.contains_key(&address) {
            t.address.insert(address, Default::default());
        }
        if !t
            .address
            .get(&address)
            .unwrap()
            .modules
            .contains_key(&module)
        {
            t.address
                .get_mut(&address)
                .unwrap()
                .modules
                .insert(module, Default::default());
        }
        // finally, OK to borrow.
        t.address
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
        // A top item can be access by two ways.
        // First 0x1::xxx::yyy
        // Second the in current module,Can use a plain name to access then like  foo().
        // drop b cannot borrow more than once.
        drop(b);
        self.enter_item(convert_loc, item_name, item);
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
        name_to_addr: &dyn ConvertName2AccountAddress,
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
            NameAccessChain_::Two(_name, member) => {
                // first find this name.
                let mut r = None;
                self.inner_first_visit(|s| {
                    if let Some(v) = s.items.get(&member.value) {
                        r = Some(v.clone());
                        return true;
                    }
                    false
                });
                if r.is_none() {
                    return failed;
                }
                let r = r.unwrap();
                match r {
                    Item::ImportedModule(_, members) => {
                        if let Some(item) = members.as_ref().borrow().items.get(&member.value) {
                            let _ = std::mem::replace(item_ret, Some(item.clone()));
                            item.to_type(false).unwrap_or(failed)
                        } else {
                            failed
                        }
                    }
                    _ => failed,
                }
            }
            NameAccessChain_::Three(chain_two, member) => self.visit_top_scope(|top| {
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

    pub(crate) fn visit_top_scope<R>(&self, x: impl FnOnce(&Addresses) -> R) -> R {
        x(self
            .scopes
            .as_ref()
            .borrow()
            .first()
            .unwrap()
            .addresses
            .as_ref()
            .unwrap())
    }

    pub(crate) fn resolve_type(
        &self,
        ty: &Type,
        name_to_addr: &dyn ConvertName2AccountAddress,
    ) -> ResolvedType {
        let r = match &ty.value {
            Type_::Apply(ref chain, types) => {
                let chain_ty = self.find_name_chain_type(chain, &mut None, name_to_addr, true);
                let _types: Vec<_> = types
                    .iter()
                    .map(|ty| self.resolve_type(ty, name_to_addr))
                    .collect();
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
