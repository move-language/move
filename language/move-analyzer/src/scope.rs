use super::item::*;
use super::types::*;
use crate::modules::ERR_ADDRESS;
use move_compiler::parser::ast::*;
use move_core_types::account_address::AccountAddress;
use move_ir_types::location::Spanned;

use move_ir_types::location::Loc;
use move_symbol_pool::Symbol;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Default, Clone)]
pub struct Scope {
    pub(crate) items: HashMap<Symbol, Item>,
    /// Type parameter go into this map.
    pub(crate) types: HashMap<Symbol, Item>,
    pub(crate) last_use_loc: Option<Loc>,
}

#[derive(Clone)]
pub struct AddrAndModuleName {
    pub(crate) addr: AccountAddress,
    pub(crate) name: ModuleName,
}
impl Eq for AddrAndModuleName {}

impl PartialEq for AddrAndModuleName {
    fn eq(&self, other: &Self) -> bool {
        self.addr == other.addr && self.name.0.value == other.name.0.value
    }
}

impl Scope {
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
        self.enter_spec_build_in();
    }
    pub(crate) fn enter_item(&mut self, s: Symbol, item: impl Into<Item>) {
        let item = item.into();
        match &item {
            Item::UseMember(_, name, alias, _) => {
                if let Some(alias) = alias {
                    self.last_use_loc = Some(alias.loc);
                } else {
                    self.last_use_loc = Some(name.loc);
                }
            }
            Item::UseModule(module, alias, _, _self) => {
                if let Some(_self) = _self {
                    self.last_use_loc = Some(_self.loc);
                } else if let Some(alias) = alias {
                    self.last_use_loc = Some(alias.0.loc);
                } else {
                    self.last_use_loc = Some(module.value.module.0.loc);
                }
            }
            _ => {}
        }
        self.items.insert(s, item);
    }

    pub(crate) fn enter_types(&mut self, s: Symbol, item: impl Into<Item>) {
        let item = item.into();
        self.types.insert(s, item);
    }
    fn enter_spec_build_in(&mut self) {
        BuildInType::num_types().iter().for_each(|ty| {
            let x = Symbol::from(format!("MAX_{}", ty.to_static_str().to_uppercase()));
            self.enter_item(
                x,
                Item::SpecConst(ItemConst {
                    name: ConstantName(Spanned {
                        loc: UNKNOWN_LOC,
                        value: x,
                    }),
                    ty: ResolvedType::new_build_in(ty.clone()),
                    is_test: false,
                }),
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
    pub(crate) is_test: bool,
}

/// Used for some dummy or empty data.
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
            is_test: false,
        }
    }
}

impl ModuleScope {
    pub(crate) fn new(name_and_addr: AddrAndModuleName, is_test: bool) -> Self {
        Self {
            module: Default::default(),
            spec: Default::default(),
            name_and_addr,
            friends: Default::default(),
            is_test,
        }
    }

    fn clone_module(&self) -> Scope {
        let s = self.module.clone();
        s
    }

    pub(crate) fn clone_spec_scope(&self) -> Scope {
        let mut s = self.clone_module();
        for x in self.spec.items.iter() {
            s.enter_item(x.0.clone(), x.1.clone());
        }
        s
    }

    pub(crate) fn clone_module_scope(&self) -> Scope {
        let mut s = self.clone_module();
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
        Rc::new(RefCell::new(ModuleScope::new(
            AddrAndModuleName { addr, name },
            false,
        )))
    }
}
