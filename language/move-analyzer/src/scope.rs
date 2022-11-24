use super::item::*;
use super::modules::*;
use super::types::*;
use move_command_line_common::files::FileHash;
use move_compiler::{
    parser::{
        ast::*,
        lexer::{Lexer, Tok},
    },
    shared::*,
    CommentMap,
};
use move_ir_types::location::{Loc, Spanned};
use move_symbol_pool::Symbol;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Default, Debug, Clone)]
pub struct Scope {
    pub(crate) items: HashMap<Symbol, Item>,
    pub(crate) is_function: bool,
    pub(crate) is_spec: bool,
    /// Top level scope have this structure.
    pub(crate) addresses: Option<Addresses>,
}

impl Scope {
    pub(crate) fn new_top() -> Self {
        Self {
            items: Default::default(),
            is_function: false,
            is_spec: false,
            addresses: Some(Addresses::new()),
        }
    }
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
    pub(crate) fn enter_item(&mut self, s: Symbol, item: Item) {
        self.items.insert(s, item);
    }
}

#[derive(Debug, Clone)]
pub struct Addresses {
    /// address to modules
    pub(crate) address: HashMap<NumericalAddress, Address>,
}

impl Addresses {
    pub fn new() -> Self {
        Self {
            address: Default::default(),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Address {
    /// module name to Scope.
    pub(crate) modules: HashMap<Symbol, Rc<RefCell<Scope>>>,
}
