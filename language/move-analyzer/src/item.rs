// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use super::{scope::*, types::*};
use enum_iterator::Sequence;
use move_compiler::{
    parser::ast::*,
    shared::{Identifier, TName, *},
};
use move_core_types::account_address::AccountAddress;

use move_command_line_common::files::FileHash;
use move_ir_types::location::Loc;
use move_symbol_pool::Symbol;
use std::{cell::RefCell, collections::HashMap, rc::Rc, str::FromStr};

#[derive(Clone)]
pub struct ItemStruct {
    pub(crate) name: StructName,
    pub(crate) type_parameters: Vec<StructTypeParameter>,
    pub(crate) type_parameters_ins: Vec<ResolvedType>,
    pub(crate) fields: Vec<(Field, ResolvedType)>, /* TODO If this length is zero,maybe a native. */
    pub(crate) is_test: bool,
    pub(crate) addr: AccountAddress,
    pub(crate) module_name: Symbol,
}

impl ItemStruct {
    pub(crate) fn to_struct_ref(&self) -> ItemStructNameRef {
        ItemStructNameRef {
            addr: self.addr,
            module_name: self.module_name,
            name: self.name,
            type_parameters: self.type_parameters.clone(),
            is_test: self.is_test,
        }
    }
    pub(crate) fn find_filed_by_name(&self, name: Symbol) -> Option<&(Field, ResolvedType)> {
        for f in self.fields.iter() {
            if f.0 .0.value.as_str() == name.as_str() {
                return Some(f);
            }
        }
        None
    }
}

impl ItemStruct {
    pub(crate) fn bind_type_parameter(
        &mut self,
        types: Option<&HashMap<Symbol, ResolvedType>>, //  types maybe infered from somewhere or use sepcified.
    ) {
        let mut m = HashMap::new();

        debug_assert!(
            self.type_parameters_ins.len() == 0
                || self.type_parameters_ins.len() == self.type_parameters.len()
        );
        let types = if let Some(types) = types {
            self.type_parameters.iter().for_each(|x| {
                self.type_parameters_ins.push(
                    types
                        .get(&x.name.value)
                        .map(|x| x.clone())
                        .unwrap_or_default(),
                )
            });
            types
        } else {
            for (k, v) in self
                .type_parameters
                .iter()
                .zip(self.type_parameters_ins.iter())
            {
                m.insert(k.name.value, v.clone());
            }
            &m
        };
        self.fields
            .iter_mut()
            .for_each(|f| f.1.bind_type_parameter(types));
    }
}

impl std::fmt::Display for ItemStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name.value().as_str())
    }
}

#[derive(Clone)]
pub enum Item {
    Parameter(Var, ResolvedType),
    Const(ItemConst),
    Var {
        var: Var,
        ty: ResolvedType,
        lambda: Option<LambdaExp>,
        has_decl_ty: bool,
    },
    Field(Field, ResolvedType),
    Struct(ItemStruct),
    StructNameRef(ItemStructNameRef),
    Fun(ItemFun),
    MoveBuildInFun(MoveBuildInFun),
    SpecBuildInFun(SpecBuildInFun),
    SpecConst(ItemConst),
    /// build in types.
    BuildInType(BuildInType),
    TParam(Name, Vec<Ability>),
    SpecSchema(Name, HashMap<Symbol, (Name, ResolvedType)>),
    /// a module name in 0x1111::module_name
    ModuleName(ItemModuleName),
    Use(Vec<ItemUse>),
    Dummy,
}

#[derive(Clone)]
pub struct LambdaExp {
    pub(crate) bind_list: BindList,
    pub(crate) exp: Exp,
}

#[derive(Clone)]
pub enum ItemUse {
    Module(ItemUseModule),
    Item(ItemUseItem),
}

#[derive(Clone)]
pub struct ItemUseModule {
    pub(crate) module_ident: ModuleIdent,         // 0x111::xxxx
    pub(crate) alias: Option<ModuleName>,         // alias
    pub(crate) members: Rc<RefCell<ModuleScope>>, // module scope.
    pub(crate) s: Option<Name>,                   // Option Self
    #[allow(dead_code)]
    pub(crate) is_test: bool,
}

#[derive(Clone)]
pub struct ItemUseItem {
    pub(crate) module_ident: ModuleIdent, /* access name */
    pub(crate) name: Name,
    pub(crate) alias: Option<Name>, /* alias  */
    pub(crate) members: Rc<RefCell<ModuleScope>>,
    #[allow(dead_code)]
    pub(crate) is_test: bool,
}

#[derive(Clone)]
pub struct ItemModuleName {
    pub(crate) name: ModuleName,
}

#[derive(Clone)]
pub struct ItemStructNameRef {
    pub(crate) addr: AccountAddress,
    pub(crate) module_name: Symbol,
    pub(crate) name: StructName,
    pub(crate) type_parameters: Vec<StructTypeParameter>,
    pub(crate) is_test: bool,
}

#[derive(Clone)]
pub struct ItemFun {
    pub(crate) name: FunctionName,
    pub(crate) type_parameters: Vec<(Name, Vec<Ability>)>,
    pub(crate) parameters: Vec<(Var, ResolvedType)>,
    pub(crate) ret_type: Box<ResolvedType>,
    pub(crate) ret_type_unresolved: Type,
    pub(crate) addr_and_name: AddrAndModuleName,
}

#[derive(Clone, Copy, PartialEq)]
pub enum AttrTest {
    No,
    Test,
    TestOnly,
}

impl AttrTest {
    pub(crate) fn is_test(self) -> bool {
        self == Self::Test || self == Self::TestOnly
    }
}

impl std::fmt::Display for ItemFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun {}", self.name.value().as_str())?;
        if self.type_parameters.len() > 0 {
            write!(f, "<")?;
            let last = self.type_parameters.len() - 1;
            for (index, (name, _)) in self.type_parameters.iter().enumerate() {
                write!(
                    f,
                    "{}{}",
                    name.value.as_str(),
                    if index < last { "," } else { "" }
                )?;
            }
            write!(f, ">")?;
        }
        write!(f, "(")?;
        if self.parameters.len() > 0 {
            let last = self.parameters.len() - 1;
            for (index, (name, t)) in self.parameters.iter().enumerate() {
                write!(
                    f,
                    "{}:{}{}",
                    name.value().as_str(),
                    t,
                    if index < last { "," } else { "" }
                )?;
            }
        }

        write!(f, ")")?;
        if !self.ret_type.as_ref().is_unit() {
            write!(f, ":{}", self.ret_type.as_ref())?;
        }
        Ok(())
    }
}

impl Item {
    ///   
    pub(crate) fn to_type(&self) -> Option<ResolvedType> {
        let x = match self {
            Item::TParam(name, ab) => ResolvedType::TParam(name.clone(), ab.clone()),
            Item::Struct(x) => ResolvedType::Struct(x.to_struct_ref(), Default::default()),
            Item::StructNameRef(x) => ResolvedType::Struct(x.clone(), Default::default()),
            Item::BuildInType(b) => ResolvedType::BuildInType(*b),
            Item::Parameter(_, ty) | Item::Var { ty, .. } | Item::Const(ItemConst { ty, .. }) => {
                ty.clone()
            }
            Item::Field(_, ty) => ty.clone(),
            Item::Fun(x) => ResolvedType::Fun(x.clone()),
            Item::Use(x) => {
                for x in x.iter() {
                    match x {
                        ItemUse::Module(_) => {}
                        ItemUse::Item(ItemUseItem { members, name, .. }) => {
                            return members
                                .as_ref()
                                .borrow()
                                .module
                                .items
                                .get(&name.value)
                                .map(|i| i.to_type())
                                .flatten();
                        }
                    }
                }
                return None;
            }
            Item::Dummy => return None,
            Item::SpecSchema(_, _) => return None,
            Item::ModuleName(_) => return None,
            Item::MoveBuildInFun(_) => return None,
            Item::SpecBuildInFun(_) => return None,
            Item::SpecConst(ItemConst { ty, .. }) => ty.clone(),
        };
        Some(x)
    }

    pub(crate) fn def_loc(&self) -> Loc {
        match self {
            Item::Parameter(var, _) => var.loc(),
            Item::Use(x) => {
                for x in x.iter() {
                    match x {
                        ItemUse::Module(ItemUseModule { members, .. }) => {
                            return members.borrow().name_and_addr.name.loc();
                        }
                        ItemUse::Item(ItemUseItem { members, name, .. }) => {
                            if let Some(t) = members
                                .borrow()
                                .module
                                .items
                                .get(&name.value)
                                .map(|u| u.def_loc())
                            {
                                return t;
                            } else {
                                return members
                                    .borrow()
                                    .spec
                                    .items
                                    .get(&name.value)
                                    .map(|u| u.def_loc())
                                    .unwrap_or(Loc::new(FileHash::empty(), 0, 0));
                            }
                        }
                    }
                }
                return Loc::new(FileHash::empty(), 0, 0);
            }
            Item::Struct(x) => x.name.loc(),
            Item::BuildInType(_) => Loc::new(FileHash::empty(), 0, 0),
            Item::TParam(name, _) => name.loc,
            Item::Const(ItemConst { name, .. }) => name.loc(),
            Item::StructNameRef(ItemStructNameRef { name, .. }) => name.0.loc,
            Item::Fun(f) => f.name.0.loc,
            Item::Var { var: name, .. } => name.loc(),
            Item::Field(f, _) => f.loc(),
            Item::Dummy => Loc::new(FileHash::empty(), 0, 0),
            Item::SpecSchema(name, _) => name.loc,
            Item::ModuleName(ItemModuleName { name, .. }) => name.loc(),
            Item::MoveBuildInFun(_) => Loc::new(FileHash::empty(), 0, 0),
            Item::SpecBuildInFun(_) => Loc::new(FileHash::empty(), 0, 0),
            Item::SpecConst(_) => Loc::new(FileHash::empty(), 0, 0),
        }
    }

    pub(crate) fn is_build_in(&self) -> bool {
        match self {
            Item::BuildInType(_) => true,
            Item::SpecBuildInFun(_) => true,
            Item::MoveBuildInFun(_) => true,
            _ => false,
        }
    }
}

impl Default for Item {
    fn default() -> Self {
        Self::Dummy
    }
}

#[derive(Clone)]
pub struct ItemConst {
    pub(crate) name: ConstantName,
    pub(crate) ty: ResolvedType,
}

#[derive(Clone, Copy, Debug)]
pub enum MacroCall {
    Assert,
}

impl MacroCall {
    pub(crate) fn from_chain(chain: &NameAccessChain) -> Option<Self> {
        match &chain.value {
            NameAccessChain_::One(name) => return Self::from_symbol(name.value),
            NameAccessChain_::Two(_, _) => return None,
            NameAccessChain_::Three(_, _) => return None,
        }
    }
    pub(crate) fn from_symbol(s: Symbol) -> Option<Self> {
        match s.as_str() {
            "assert" => Some(Self::Assert),
            _ => return None,
        }
    }
}

impl Default for MacroCall {
    fn default() -> Self {
        Self::Assert
    }
}

/// Get the last name of a access chain.
pub(crate) fn get_name_chain_last_name(x: &NameAccessChain) -> &Name {
    match &x.value {
        move_compiler::parser::ast::NameAccessChain_::One(name)
        | move_compiler::parser::ast::NameAccessChain_::Two(_, name)
        | move_compiler::parser::ast::NameAccessChain_::Three(_, name) => name,
    }
}

impl std::fmt::Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Item::Parameter(var, t) => {
                write!(f, "{}:{}", var.0.value.as_str(), t)
            }
            Item::ModuleName(ItemModuleName { name, .. }) => {
                write!(f, "{}", name.value().as_str())
            }
            Item::Use(x) => Ok(for x in x.iter() {
                match x {
                    ItemUse::Module(ItemUseModule { module_ident, .. }) => {
                        write!(f, "use {:?} {}", module_ident, "_")?;
                    }
                    ItemUse::Item(ItemUseItem {
                        module_ident,
                        name,
                        alias,
                        ..
                    }) => {
                        write!(
                            f,
                            "use {:?}::{:?} {}",
                            module_ident,
                            name,
                            if let Some(alias) = alias {
                                format!(" as {}", alias.value.as_str())
                            } else {
                                String::from_str("").unwrap()
                            },
                        )?;
                    }
                }
            }),

            Item::Const(ItemConst { name, ty, .. }) => {
                write!(f, "{}:{}", name.0.value.as_str(), ty)
            }
            Item::SpecConst(ItemConst { name, ty, .. }) => {
                write!(f, "{}:{}", name.0.value.as_str(), ty)
            }
            Item::Struct(s) => {
                write!(f, "{}", s)
            }
            Item::StructNameRef(ItemStructNameRef { name, .. }) => {
                write!(f, "{}", name.value().as_str())
            }
            Item::Fun(x) => write!(f, "{}", x),
            Item::BuildInType(x) => {
                write!(f, "{}", x.to_static_str())
            }
            Item::TParam(tname, abilities) => {
                write!(f, "{}:", tname.value.as_str())?;
                for i in 0..abilities.len() {
                    let x = abilities.get(i).unwrap();
                    write!(f, "{:?},", x.value)?;
                }
                std::result::Result::Ok(())
            }
            Item::Var { var, ty, .. } => {
                write!(f, "{}:{}", var.0.value.as_str(), ty)
            }
            Item::Field(x, ty) => {
                write!(f, "{}:{}", x.0.value.as_str(), ty)
            }
            Item::Dummy => {
                write!(f, "dummy")
            }
            Item::SpecSchema(name, _) => {
                write!(f, "{}", name.value.as_str())
            }
            Item::MoveBuildInFun(x) => write!(f, "move_build_in_fun {}", x.to_static_str()),
            Item::SpecBuildInFun(x) => write!(f, "spec_build_in_fun {}", x.to_static_str()),
        }
    }
}

#[derive(Clone)]
pub enum Access {
    ApplyType(NameAccessChain, Option<ModuleName>, Box<ResolvedType>),
    ExprVar(Var, Box<Item>),
    ExprAccessChain(
        NameAccessChain,
        Option<AddrAndModuleName>,
        Box<Item>, /* The item that you want to access.  */
    ),
    // Maybe the same as ExprName.
    ExprAddressName(Name),
    AccessFiled(AccessFiled),
    ///////////////
    /// key words
    KeyWords(&'static str),
    /////////////////
    /// Marco call
    MacroCall(MacroCall, NameAccessChain),
    Friend(NameAccessChain, ModuleName),

    ApplySchemaTo(
        NameAccessChain, // Apply a schema to a item.
        Box<Item>,
    ),
    IncludeSchema(NameAccessChain, Box<Item>),
    PragmaProperty(PragmaProperty),
    SpecFor(Name, Box<Item>),
}

#[derive(Clone)]
pub struct AccessFiled {
    pub(crate) from: Field,
    pub(crate) to: Field,
    #[allow(dead_code)]
    pub(crate) ty: ResolvedType,
    pub(crate) item: Option<Item>,
}

impl std::fmt::Display for Access {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Access::ApplyType(a, _, x) => {
                write!(f, "apply type {:?}->{}", a.value, x)
            }

            Access::ExprVar(var, item) => {
                write!(f, "expr {}->{}", var.borrow().1.as_str(), item)
            }
            Access::ExprAccessChain(chain, _, item) => {
                write!(f, "expr {:?}->{}", chain, item)
            }
            Access::ExprAddressName(chain) => {
                write!(f, "{:?}", chain)
            }
            Access::AccessFiled(AccessFiled { from, to, .. }) => {
                write!(f, "access_field {:?}->{:?}", from, to)
            }
            Access::KeyWords(k) => write!(f, "{}", *k),
            Access::MacroCall(macro_, _) => write!(f, "{:?}", macro_),
            Access::Friend(name, item) => {
                write!(
                    f,
                    "friend {}->{}",
                    get_name_chain_last_name(name).value.as_str(),
                    item
                )
            }
            Access::ApplySchemaTo(name, item) => {
                write!(
                    f,
                    "apply schema to {}->{}",
                    get_name_chain_last_name(name).value.as_str(),
                    item
                )
            }
            Access::SpecFor(name, _) => {
                write!(f, "spec for {}", name.value.as_str())
            }
            Access::PragmaProperty(x) => {
                write!(
                    f,
                    "{}{}",
                    x.value.name.value.as_str(),
                    if let Some(_value) = &x.value.value {
                        // TODO. actual.ame
                        String::from("...")
                    } else {
                        String::from("...")
                    }
                )
            }
            Access::IncludeSchema(name, _def) => {
                write!(
                    f,
                    "include {}",
                    get_name_chain_last_name(name).value.as_str()
                )
            }
        }
    }
}

impl Access {
    pub(crate) fn access_def_loc(&self) -> (Loc /* access loc */, Loc /* def loc */) {
        match self {
            Access::ApplyType(name, _, x) => {
                (get_name_chain_last_name(name).loc, x.as_ref().def_loc())
            }
            Access::ExprVar(var, x) => (var.loc(), x.def_loc()),
            Access::ExprAccessChain(name, _, item) => {
                (get_name_chain_last_name(name).loc, item.as_ref().def_loc())
            }

            Access::ExprAddressName(_) => (
                Loc::new(FileHash::empty(), 0, 0),
                Loc::new(FileHash::empty(), 0, 0),
            ),
            Access::AccessFiled(AccessFiled { from, to, .. }) => (from.loc(), to.loc()),
            Access::KeyWords(_) => (
                Loc::new(FileHash::empty(), 0, 0),
                Loc::new(FileHash::empty(), 0, 0),
            ),
            Access::MacroCall(_, chain) => (chain.loc, chain.loc),
            Access::Friend(name, item) => (get_name_chain_last_name(name).loc.clone(), item.loc()),
            Access::ApplySchemaTo(chain, x) => {
                (get_name_chain_last_name(chain).loc.clone(), x.def_loc())
            }
            Access::PragmaProperty(x) => (x.loc, x.loc),
            Access::SpecFor(name, item) => (name.loc, item.as_ref().def_loc()),
            Access::IncludeSchema(a, d) => (get_name_chain_last_name(a).loc, d.def_loc()),
        }
    }

    /// Get loc
    pub(crate) fn access_module(&self) -> Option<(Loc, Loc)> {
        match self {
            Self::ExprAccessChain(chain, Option::Some(module), _) => match &chain.value {
                NameAccessChain_::One(_) => return None,
                NameAccessChain_::Two(m, _) => Some((m.loc, module.name.loc())),
                NameAccessChain_::Three(x, _) => Some((x.value.1.loc, module.name.loc())),
            },

            Self::ApplyType(chain, Option::Some(module), _) => match &chain.value {
                NameAccessChain_::One(_) => return None,
                NameAccessChain_::Two(m, _) => Some((m.loc, module.loc())),
                NameAccessChain_::Three(x, _) => Some((x.value.1.loc, module.loc())),
            },

            _ => None,
        }
    }
}

#[derive(Clone)]
pub enum ItemOrAccess {
    Item(Item),
    Access(Access),
}

impl ItemOrAccess {
    pub(crate) fn is_local(&self) -> bool {
        let item_is_local = |x: &Item| match x {
            Item::Var { .. } => true,
            _ => false,
        };
        match self {
            ItemOrAccess::Item(item) => item_is_local(item),
            ItemOrAccess::Access(access) => match access {
                Access::ExprVar(_, item) | Access::ExprAccessChain(_, _, item) => {
                    item_is_local(item.as_ref())
                }
                _ => false,
            },
        }
    }
}

impl Into<Item> for ItemOrAccess {
    fn into(self) -> Item {
        match self {
            Self::Item(x) => x,
            _ => unreachable!(),
        }
    }
}

impl Into<Access> for ItemOrAccess {
    fn into(self) -> Access {
        match self {
            Self::Access(x) => x,
            _ => unreachable!(),
        }
    }
}

impl std::fmt::Display for ItemOrAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Access(a) => a.fmt(f),
            Self::Item(x) => x.fmt(f),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Sequence)]
pub enum MoveBuildInFun {
    MoveTo,
    MoveFrom,
    BorrowGlobalMut,
    BorrowGlobal,
    Exits,
}

impl MoveBuildInFun {
    pub(crate) fn to_static_str(self) -> &'static str {
        match self {
            MoveBuildInFun::MoveTo => "move_to",
            MoveBuildInFun::MoveFrom => "move_from",
            MoveBuildInFun::BorrowGlobalMut => "borrow_global_mut",
            MoveBuildInFun::BorrowGlobal => "borrow_global",
            MoveBuildInFun::Exits => "exists",
        }
    }
}

impl std::fmt::Display for MoveBuildInFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Sequence)]
pub enum SpecBuildInFun {
    Exists,
    Global,
    Len,
    Update,
    Vec,
    Concat,
    Contains,
    IndexOf,
    Range,
    InRange,
    UpdateField,
    Old,
    TRACE,
}

impl SpecBuildInFun {
    pub(crate) fn to_static_str(self) -> &'static str {
        match self {
            Self::Exists => "exists",
            Self::Global => "global",
            Self::Len => "len",
            Self::Update => "update",
            Self::Vec => "vec",
            Self::Concat => "concat",
            Self::Contains => "contains",
            Self::IndexOf => "index_of",
            Self::Range => "range",
            Self::InRange => "in_range",
            Self::UpdateField => "update_field",
            Self::Old => "old",
            Self::TRACE => "TRACE",
        }
    }
}

impl std::fmt::Display for SpecBuildInFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}
