#![allow(dead_code)]
use super::item::*;
use super::types::*;
use move_compiler::parser::ast::*;
use move_core_types::account_address::AccountAddress;
use move_ir_types::location::*;
use move_symbol_pool::Symbol;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Default, Clone)]
pub struct Scope {
    pub(crate) items: HashMap<Symbol, Item>,
    pub(crate) is_spec: bool,
    pub(crate) module_name_and_addr: Option<ModuleNameAndAddr>,
    /// Type parameter go into this map.
    pub(crate) types: HashMap<Symbol, Item>,
}

#[derive(Default, Clone)]
pub struct ModuleScope {
    pub(crate) module: Scope,
    pub(crate) spec: Scope,
}

impl ModuleScope {
    pub(crate) fn new(name_and_addr: ModuleNameAndAddr) -> Self {
        Self {
            module: {
                let mut x = Scope::default();
                x.module_name_and_addr = Some(name_and_addr);
                x
            },
            spec: Default::default(),
        }
    }
    pub(crate) fn clone_spec_scope(&self) -> Scope {
        let mut s = self.module.clone();
        for x in self.spec.items.iter() {
            s.enter_item(x.0.clone(), x.1.clone());
        }
        s
    }

    pub(crate) fn clone_module_scope(&self) -> Scope {
        let mut s = self.module.clone();
        for x in self.spec.items.iter() {
            match &x.1 {
                Item::Fun(_) => {
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
        Rc::new(RefCell::new(ModuleScope::new(ModuleNameAndAddr {
            addr,
            name,
        })))
    }
}

#[derive(Clone)]
pub struct ModuleNameAndAddr {
    pub(crate) addr: AccountAddress,
    pub(crate) name: ModuleName,
}

impl Scope {
    pub(crate) fn new_spec() -> Self {
        let mut x = Self::default();
        x.is_spec = true;
        x.enter_spec_build_in_const();
        x
    }

    pub(crate) fn new_fun() -> Self {
        Self::default()
    }

    pub(crate) fn enter_build_in(&mut self) {
        self.enter_item(Symbol::from("bool"), Item::BuildInType(BuildInType::Bool));
        self.enter_item(Symbol::from("u8"), Item::BuildInType(BuildInType::U8));
        self.enter_item(Symbol::from("u16"), Item::BuildInType(BuildInType::U16));
        self.enter_item(Symbol::from("u32"), Item::BuildInType(BuildInType::U32));
        self.enter_item(Symbol::from("u64"), Item::BuildInType(BuildInType::U64));
        self.enter_item(Symbol::from("u128"), Item::BuildInType(BuildInType::U128));
        self.enter_item(Symbol::from("u256"), Item::BuildInType(BuildInType::U256));
        self.enter_item(
            Symbol::from("signer"),
            Item::BuildInType(BuildInType::Signer),
        );
        self.enter_item(
            Symbol::from("address"),
            Item::BuildInType(BuildInType::Address),
        );
    }
    pub(crate) fn enter_item(&mut self, s: Symbol, item: impl Into<Item>) {
        let item = item.into();
        self.items.insert(s, item);
    }

    pub(crate) fn enter_types(&mut self, s: Symbol, item: impl Into<Item>) {
        let item = item.into();
        self.types.insert(s, item);
    }

    fn enter_spec_build_in_const(&mut self) {
        {
            let x = Symbol::from("MAX_U8");
            self.enter_item(
                x,
                Item::Const(
                    ConstantName(Spanned {
                        loc: UNKNOWN_LOC,
                        value: x,
                    }),
                    ResolvedType::new_build_in(BuildInType::NumType),
                ),
            )
        }
        {
            let x = Symbol::from("MAX_U16");
            self.enter_item(
                x,
                Item::Const(
                    ConstantName(Spanned {
                        loc: UNKNOWN_LOC,
                        value: x,
                    }),
                    ResolvedType::new_build_in(BuildInType::NumType),
                ),
            )
        }
        {
            let x = Symbol::from("MAX_U32");
            self.enter_item(
                x,
                Item::Const(
                    ConstantName(Spanned {
                        loc: UNKNOWN_LOC,
                        value: x,
                    }),
                    ResolvedType::new_build_in(BuildInType::NumType),
                ),
            )
        }

        {
            let x = Symbol::from("MAX_U64");
            self.enter_item(
                x,
                Item::Const(
                    ConstantName(Spanned {
                        loc: UNKNOWN_LOC,
                        value: x,
                    }),
                    ResolvedType::new_build_in(BuildInType::NumType),
                ),
            )
        }
        {
            let x = Symbol::from("MAX_U128");
            self.enter_item(
                x,
                Item::Const(
                    ConstantName(Spanned {
                        loc: UNKNOWN_LOC,
                        value: x,
                    }),
                    ResolvedType::new_build_in(BuildInType::NumType),
                ),
            )
        }
        {
            let x = Symbol::from("MAX_U256");
            self.enter_item(
                x,
                Item::Const(
                    ConstantName(Spanned {
                        loc: UNKNOWN_LOC,
                        value: x,
                    }),
                    ResolvedType::new_build_in(BuildInType::NumType),
                ),
            )
        }
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
