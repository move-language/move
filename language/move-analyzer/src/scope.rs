#![allow(dead_code)]
use super::item::*;

use super::types::*;

use move_compiler::shared::*;
use move_compiler::{parser::ast::*, shared::*};
use move_core_types::account_address::AccountAddress;
use move_ir_types::location::Loc;
use move_symbol_pool::Symbol;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Default, Clone)]
pub struct Scope {
    pub(crate) items: HashMap<Symbol, Item>,
    pub(crate) is_function: bool,
    pub(crate) is_spec: bool,
    pub(crate) module_: Option<ModuleScope>,
}
#[derive(Clone)]
pub struct ModuleScope {
    pub(crate) addr: AccountAddress,
    pub(crate) name: ModuleName,
}
impl Scope {
    pub(crate) fn enter_build_in(&mut self) {
        self.enter_item(Symbol::from("bool"), Item::BuildInType(BuildInType::Bool));
        self.enter_item(Symbol::from("u8"), Item::BuildInType(BuildInType::U8));
        self.enter_item(Symbol::from("u64"), Item::BuildInType(BuildInType::U64));
        self.enter_item(Symbol::from("u128"), Item::BuildInType(BuildInType::U128));
        self.enter_item(
            Symbol::from("address"),
            Item::BuildInType(BuildInType::Address),
        );
    }
    pub(crate) fn enter_item(&mut self, s: Symbol, item: impl Into<Item>) {
        let item = item.into();
        self.items.insert(s, item);
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
    pub(crate) modules: HashMap<Symbol, Rc<RefCell<Scope>>>,
}
