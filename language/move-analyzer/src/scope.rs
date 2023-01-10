use super::item::*;
use super::types::*;
use crate::modules::ERR_ADDRESS;
use move_compiler::parser::ast::*;
use move_core_types::account_address::AccountAddress;
use move_ir_types::location::Spanned;

use move_symbol_pool::Symbol;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Default, Clone)]
pub struct Scope {
    pub(crate) items: HashMap<Symbol, Item>,
    pub(crate) is_spec: bool,
    /// Type parameter go into this map.
    pub(crate) types: HashMap<Symbol, Item>,
    pub(crate) info: Option<ModuleInfo>,
}

#[derive(Clone)]
pub struct ModuleInfo {
    pub(crate) friends: HashSet<(AccountAddress, Symbol)>,
}

impl ModuleInfo {
    pub(crate) fn new() -> Self {
        Self {
            friends: Default::default(),
        }
    }
}
impl ModuleInfo {
    pub(crate) fn has_friend(&self, addr: AccountAddress, name: Symbol) -> bool {
        self.friends.iter().any(|x| x.0 == addr && x.1 == name)
    }
}

impl Scope {
    pub(crate) fn has_friend(&self, addr: AccountAddress, name: Symbol) -> bool {
        let has_friend = || self.info.as_ref().map(|x| x.has_friend(addr, name));
        has_friend().unwrap_or(false)
    }
}

#[derive(Clone)]
pub struct AddrAndModuleName {
    pub(crate) addr: AccountAddress,
    pub(crate) name: ModuleName,
}
impl Eq for AddrAndModuleName {}

impl PartialEq for AddrAndModuleName {
    fn eq(&self, other: &Self) -> bool {
        self.addr == other.addr && self.name.0.value.as_str() == other.name.0.value.as_str()
    }
}

impl Scope {
    pub(crate) fn new_spec(under_spec: bool) -> Self {
        let mut x = Self::default();
        x.is_spec = true;
        if !under_spec {
            x.enter_spec_build_in();
        }
        x
    }

    pub(crate) fn new_fun() -> Self {
        Self::default()
    }
    pub(crate) fn enter_build_in(&mut self) {
        BuildInType::build_ins().iter().for_each(|x| {
            self.enter_item(Symbol::from(x.to_static_str()), Item::BuildInType(*x));
        });
        enum_iterator::all::<MoveBuildInFun>()
            .collect::<Vec<_>>()
            .iter()
            .for_each(|x| {
                self.enter_item(
                    Symbol::from(x.to_static_str()),
                    Item::MoveBuildInFun(x.clone()),
                )
            });
    }
    pub(crate) fn enter_item(&mut self, s: Symbol, item: impl Into<Item>) {
        let item = item.into();
        self.items.insert(s, item);
    }

    pub(crate) fn enter_types(&mut self, s: Symbol, item: impl Into<Item>) {
        let item = item.into();
        self.types.insert(s, item);
    }
    fn enter_spec_build_in(&mut self) {
        BuildInType::num_types().iter().for_each(|x| {
            let x = Symbol::from(format!("MAX_{}", x.to_static_str().to_uppercase()));
            self.enter_item(
                x,
                Item::Const(
                    ConstantName(Spanned {
                        loc: UNKNOWN_LOC,
                        value: x,
                    }),
                    ResolvedType::new_build_in(BuildInType::NumType),
                ),
            );
        });
        enum_iterator::all::<SpecBuildInFun>()
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|x| {
                self.enter_item(Symbol::from(x.to_static_str()), Item::SpecBuildInFun(x))
            });
    }
}

#[derive(Clone, Default)]
pub struct Addresses {
    /// address to modules
    pub(crate) address: HashMap<AccountAddress, Address>,
}

impl Addresses {
    pub fn new() -> Self {
        Self {
            address: Default::default(),
        }
    }
}

#[derive(Default, Clone)]
pub struct Address {
    /// module name to Scope.
    pub(crate) modules: HashMap<Symbol, Rc<RefCell<ModuleScope>>>,
}

#[derive(Clone)]
pub struct ModuleScope {
    pub(crate) module: Scope,
    pub(crate) spec: Scope,
    pub(crate) name_and_addr: AddrAndModuleName,
    pub(crate) friends: HashSet<(AccountAddress, Symbol)>,
}

impl Default for ModuleScope {
    fn default() -> Self {
        Self {
            module: Default::default(),
            spec: Default::default(),
            name_and_addr: AddrAndModuleName {
                addr: ERR_ADDRESS,
                name: ModuleName(Spanned {
                    loc: crate::types::UNKNOWN_LOC,
                    value: Symbol::from("_"),
                }),
            },
            friends: Default::default(),
        }
    }
}

impl ModuleScope {
    pub(crate) fn new(name_and_addr: AddrAndModuleName) -> Self {
        Self {
            module: {
                let mut s = Scope::default();
                s.info = Some(ModuleInfo::new());
                s
            },
            spec: Default::default(),
            name_and_addr,
            friends: Default::default(),
        }
    }

    fn mk_module_info(&self) -> ModuleInfo {
        ModuleInfo {
            friends: self.friends.clone(),
        }
    }

    pub(crate) fn clone_spec_scope(&self) -> Scope {
        let mut s = self.module.clone();
        s.info = Some(self.mk_module_info());
        for x in self.spec.items.iter() {
            s.enter_item(x.0.clone(), x.1.clone());
        }
        s
    }

    pub(crate) fn clone_module_scope(&self) -> Scope {
        let mut s = self.module.clone();
        s.info = Some(self.mk_module_info());
        for x in self.spec.items.iter() {
            match &x.1 {
                Item::Fun(_) | Item::SpecSchema(_, _) => {
                    s.enter_item(x.0.clone(), x.1.clone());
                }
                _ => {}
            }
        }
        s
    }

    pub(crate) fn new_module_name(
        addr: AccountAddress,
        name: ModuleName,
    ) -> Rc<RefCell<ModuleScope>> {
        Rc::new(RefCell::new(ModuleScope::new(AddrAndModuleName {
            addr,
            name,
        })))
    }
}
