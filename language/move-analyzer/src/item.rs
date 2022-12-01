use crate::modules;

use super::scope::*;
use super::types::*;

use move_compiler::shared::Identifier;
use move_compiler::shared::TName;
use move_compiler::{parser::ast::*, shared::*};
use move_core_types::account_address::AccountAddress;
use move_ir_types::location::Loc;
use move_symbol_pool::Symbol;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Pointer;
use std::rc::Rc;

#[derive(Clone)]
pub struct ItemStruct {
    pub(crate) name: StructName,
    pub(crate) type_parameters: Vec<StructTypeParameter>,
    pub(crate) type_parameters_ins: Vec<ResolvedType>,
    pub(crate) fields: Vec<(Field, ResolvedType)>, /* TODO If this length is zero,maybe a native. */
}

impl std::fmt::Display for ItemStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "struct {}", self.name.value().as_str())?;
        write!(f, "{{")?;
        if self.type_parameters.len() > 0 {
            write!(f, "<")?;
            for (index, t) in self.type_parameters.iter().enumerate() {
                write!(
                    f,
                    "{}{}",
                    t.name.value.as_str(),
                    if let Some(ins) = self.type_parameters_ins.get(index) {
                        format!("->{}", ins)
                    } else {
                        format!(":{:?}", t.constraints)
                    }
                )?;
            }
            write!(f, ">")?;
        }
        write!(f, "}}")
    }
}

#[derive(Clone)]
pub enum Item {
    /////////////////////////////
    /// VALUE types
    Parameter(Var, ResolvedType),
    ImportedModule(ModuleIdent, Rc<RefCell<Scope>>),
    ImportedMember(
        Name,   /* access name */
        Symbol, /* name in the module */
        Rc<RefCell<Scope>>,
    ),
    Const(ConstantName, ResolvedType),
    Var(Var, ResolvedType),
    Field(Field, ResolvedType),

    /////////////////////////
    /// TYPE types
    Struct(ItemStruct),
    StructNameRef(
        AccountAddress,
        Symbol, // module name.
        StructName,
    ),
    Fun(ItemFunction),

    /// build in types.
    BuildInType(BuildInType),
    /// Here are all definition.
    TParam(Name, Vec<Ability>),

    Dummy,
}

#[derive(Clone)]
pub struct ItemFunction {
    pub(crate) name: FunctionName,
    pub(crate) type_parameters: Vec<(Name, Vec<Ability>)>, // type parameters.
    pub(crate) parameters: Vec<(Var, ResolvedType)>,       // parameters.
    pub(crate) ret_type: Box<ResolvedType>,                // return ty
}

impl std::fmt::Display for ItemFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun {}", self.name.value().as_str())?;
        if self.type_parameters.len() > 0 {
            write!(f, "<")?;
            for (name, a) in self.type_parameters.iter() {
                write!(f, "{}:{:?},", name.value.as_str(), a)?;
            }
            write!(f, ">")?;
        }
        write!(f, "(")?;
        for (name, t) in self.parameters.iter() {
            write!(f, "{}:{},", name.value().as_str(), t)?;
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
    pub(crate) fn to_type(&self, accept_tparam: bool) -> Option<ResolvedType> {
        // TODO maybe a parameter to decide return TParam or not.
        let x = match self {
            Item::TParam(name, ab) => {
                if accept_tparam {
                    ResolvedType::TParam(name.clone(), ab.clone())
                } else {
                    return None;
                }
            }
            Item::Struct(x) => ResolvedType::Struct(x.clone()),
            Item::StructNameRef(addr, name, x) => {
                ResolvedType::StructRef(addr.clone(), name.clone(), x.clone())
            }
            Item::BuildInType(b) => ResolvedType::BuildInType(*b),
            Item::Parameter(_, ty) | Item::Var(_, ty) | Item::Const(_, ty) => ty.clone(),
            Item::Field(_, ty) => ty.clone(),
            Item::Fun(x) => ResolvedType::Fun(x.clone()),
            Item::ImportedMember(_, name, module) => {
                return module
                    .as_ref()
                    .borrow()
                    .items
                    .get(name)
                    .map(|i| i.to_type(false))
                    .flatten();
            }
            Item::ImportedModule(_, _) => return None,
            Item::Dummy => return None,
        };
        Some(x)
    }

    pub(crate) fn def_loc(&self) -> Loc {
        match self {
            Self::Parameter(var, _) => var.loc(),
            Self::ImportedMember(_, name, module) => module
                .borrow()
                .items
                .get(name)
                .map(|u| u.def_loc())
                .unwrap_or(UNKNOWN_LOC),
            Self::Struct(x) => x.name.loc(),
            Self::BuildInType(_) => UNKNOWN_LOC,
            Self::TParam(name, _) => name.loc,
            Self::Const(name, _) => name.loc(),
            Item::StructNameRef(_, _, name) => name.0.loc,
            Item::Fun(f) => f.name.0.loc,
            Item::BuildInType(_) => UNKNOWN_LOC,
            Item::ImportedModule(_, _) => UNKNOWN_LOC, // TODO maybe loc from ModuleIdent.
            Item::Var(name, _) => name.loc(),
            Item::Field(f, _) => f.loc(),
            Item::Dummy => UNKNOWN_LOC,
        }
    }

    /// New a dummy item.
    /// Can be use later by someone override data.
    /// like std::mem::replace.
    pub(crate) fn new_dummy() -> Self {
        // Data here is not important.
        Self::Dummy
    }
}

#[derive(Clone, Copy, Debug)]
pub enum MacroCall {
    Assert,
}

impl MacroCall {
    pub(crate) fn from_chain(chain: &NameAccessChain) -> Self {
        match &chain.value {
            NameAccessChain_::One(name) => Self::from_symbol(name.value),
            NameAccessChain_::Two(_, _) => unreachable!(),
            NameAccessChain_::Three(_, _) => unreachable!(),
        }
    }
    pub(crate) fn from_symbol(s: Symbol) -> Self {
        match s.as_str() {
            "assert" => Self::Assert,
            _ => unreachable!(),
        }
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
                write!(f, "parameter {}:{}", var.0.value.as_str(), t)
            }
            Item::ImportedModule(x, _item) => {
                write!(f, "use {:?} {}", x, "_")
            }
            Item::ImportedMember(name, x, module) => write!(
                f,
                "use {:?} {}",
                name,
                module.borrow().items.get(x).unwrap_or(&Item::new_dummy())
            ),
            Item::Const(name, ty) => {
                write!(f, "const {}:{}", name.0.value.as_str(), ty)
            }
            Item::Struct(s) => {
                write!(f, "{}", s)
            }
            Item::StructNameRef(_, _, name) => {
                write!(f, "struct {}", name.value().as_str())
            }
            Item::Fun(x) => write!(f, "{}", x),
            Item::BuildInType(x) => {
                write!(f, "build in '{:?}'", x)
            }
            Item::TParam(tname, abilities) => {
                write!(f, "{}:", tname.value.as_str())?;
                for i in 0..abilities.len() {
                    let x = abilities.get(i).unwrap();
                    write!(f, "{:?},", x.value)?;
                }
                std::result::Result::Ok(())
            }
            Item::Var(name, ty) => {
                write!(f, "var {}:{}", name.0.value.as_str(), ty)
            }
            Item::Field(x, ty) => {
                write!(f, "field {}:{}", x.0.value.as_str(), ty)
            }
            Item::Dummy => {
                write!(f, "dummy")
            }
        }
    }
}

pub enum Access {
    ApplyType(NameAccessChain, Box<ResolvedType>),

    ExprVar(Var, Box<Item>),
    ExprAccessChain(
        NameAccessChain,
        Box<Item>, /* The item that you want to access.  */
    ),
    // Maybe the same as ExprName.
    ExprAddressName(Name),
    AccessFiled(
        Field,        // from
        Field,        // to
        ResolvedType, //  field type
    ),
    ///////////////
    /// key words
    KeyWords(&'static str),
    /////////////////
    /// Marco call
    MacroCall(MacroCall),
}

impl std::fmt::Display for Access {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Access::ApplyType(a, x) => {
                write!(f, "apply type {:?}->{}", a.value, x)
            }

            Access::ExprVar(var, item) => {
                write!(f, "expr {}->{}", var.borrow().1.as_str(), item)
            }
            Access::ExprAccessChain(chain, item) => {
                write!(f, "expr {:?}->{}", chain, item)
            }
            Access::ExprAddressName(_) => todo!(),
            Access::AccessFiled(from, to, _) => {
                write!(f, "access_field {:?}->{:?}", from, to)
            }
            Access::KeyWords(k) => write!(f, "{}", *k),
            Access::MacroCall(macro_) => write!(f, "{:?}", macro_),
        }
    }
}

impl Access {
    pub(crate) fn access_def_loc(&self) -> (Loc /* access loc */, Loc /* def loc */) {
        match self {
            Access::ApplyType(name, x) => (name.loc, x.as_ref().def_loc()),

            Access::ExprVar(var, x) => (var.loc(), x.def_loc()),
            Access::ExprAccessChain(name, item) => {
                (get_name_chain_last_name(name).loc, item.as_ref().def_loc())
            }
            Access::ExprAddressName(_) => todo!(),
            Access::AccessFiled(a, d, _) => (a.loc(), d.loc()),
            Access::KeyWords(_) => (UNKNOWN_LOC, UNKNOWN_LOC),
            Access::MacroCall(_) => (UNKNOWN_LOC, UNKNOWN_LOC),
        }
    }
}

pub enum ItemOrAccess {
    Item(Item),
    Access(Access),
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
