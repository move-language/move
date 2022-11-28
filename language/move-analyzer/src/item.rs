use super::scope::*;
use super::types::*;

use move_compiler::shared::Identifier;
use move_compiler::{parser::ast::*, shared::*};
use move_ir_types::location::{Loc, Spanned};
use move_symbol_pool::Symbol;
use std::cell::RefCell;
use std::collections::HashMap;

use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ItemStruct {
    pub(crate) name: StructName,
    pub(crate) type_parameters: Vec<StructTypeParameter>,
    pub(crate) fields: Vec<(Field, ResolvedType)>, /* TODO If this length is zero,maybe a native. */
}

impl std::fmt::Display for ItemStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "struct {}", self.name.value().as_str())
    }
}

#[derive(Clone)]
pub enum Item {
    /////////////////////////////
    /// VALUE types
    Parameter(Var, ResolvedType),
    ImportedModule(ModuleIdent, Rc<RefCell<Scope>>),
    ImportedMember(Name, Box<Item>),
    Const(ConstantName, ResolvedType),
    Var(Var, ResolvedType),

    /////////////////////////
    /// TYPE types
    Struct(
        // StructName,
        // Vec<StructTypeParameter>,
        // Vec<(Field, ResolvedType)>, /* TODO If this length is zero,maybe a native. */
        ItemStruct,
    ),
    StructName(StructName, Rc<RefCell<HashMap<Symbol, ItemStruct>>>),
    Fun(
        FunctionName,
        Vec<(Name, Vec<Ability>)>, // type parameters.
        Vec<(Var, ResolvedType)>,  // parameters.
        Box<ResolvedType>,         // return type.
    ),

    /// build in types.
    BuildInType(BuildInType),
    /// Here are all definition.
    TParam(Name, Vec<Ability>),
}

impl Item {
    ///
    pub(crate) fn to_type(&self) -> Option<ResolvedType> {
        // TODO maybe a parameter to decide return TParam or not.
        let (loc, x) = match self {
            Item::TParam(name, ab) => (name.loc, ResolvedType_::TParam(name.clone(), ab.clone())),
            Item::Struct(x) => (x.name.loc(), ResolvedType_::Struct(x.clone())),
            Item::StructName(name, x) => (
                name.loc(),
                ResolvedType_::StructName(name.clone(), x.clone()),
            ),
            Item::BuildInType(b) => (UNKNOWN_LOC, ResolvedType_::BuildInType(*b)),
            Item::Parameter(_, ty) | Item::Var(_, ty) | Item::Const(_, ty) => {
                (UNKNOWN_LOC.clone(), ty.clone().0.value)
            }
            _ => return None,
        };
        Some(ResolvedType(Spanned { loc, value: x }))
    }

    pub(crate) fn def_loc(&self) -> &Loc {
        match self {
            Self::Parameter(var, _) => &var.borrow().0,
            Self::ImportedMember(_, item) => item.as_ref().def_loc(),
            Self::Struct(x) => x.name.borrow().0,
            Self::BuildInType(_) => &UNKNOWN_LOC,
            Self::TParam(name, _) => &name.loc,
            Self::Const(name, _) => &name.borrow().0,
            Item::StructName(name, _) => &name.0.loc,
            Item::Fun(name, _, _, _) => &name.0.loc,
            Item::BuildInType(_) => &UNKNOWN_LOC,
            Item::ImportedModule(_, _) => todo!(),
            Item::Var(name, _) => name.borrow().0,
        }
    }

    /// New a dummy item.
    /// Can be use later by some override data.
    /// like std::mem::replace.
    pub(crate) fn new_dummy() -> Self {
        // Data here is not important.
        Self::BuildInType(BuildInType::Bool)
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
            Item::ImportedMember(name, x) => write!(f, "use {:?} {}", name, x),
            Item::Const(name, ty) => {
                write!(f, "const {}:{}", name.0.value.as_str(), ty)
            }
            Item::Struct(s) => {
                write!(f, "{}", s)
            }
            Item::StructName(name, _) => {
                write!(f, "struct {}", name.value().as_str())
            }
            Item::Fun(name, _, _, _) => write!(f, "fun {}", name.value().as_str()),
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
        }
    }
}

pub enum Access {
    ApplyType(NameAccessChain, Box<ResolvedType>),
    UseMember(Name, Box<Item>),
    ExprVar(Var, Option<(Symbol, Loc, ResolvedType)>),
    ExprAccessChain(
        NameAccessChain,
        Box<Item>, /* The item that you want to access.  */
    ),
    TypeAccessChain(NameAccessChain),
    // Maybe the same as ExprName.
    ExprAddressName(Name),
    FieldInitialization(Field, ResolvedType /*  field type */),
    AccessFiled(Field, ResolvedType /*  field type */),
    ///////////////
    /// key words
    KeyWords(&'static str),
    /////////////////
    /// Marco call
    MacroCall(MacroCall),
}

impl Access {
    pub(crate) fn access_def_loc(&self) -> (&Loc /* access loc */, &Loc /* def loc */) {
        match self {
            Access::ApplyType(name, x) => (&name.loc, x.as_ref().def_loc()),
            Access::UseMember(name, x) => (&name.loc, x.as_ref().def_loc()),
            Access::ExprVar(var, x) => (
                &var.borrow().0,
                match x {
                    Some(x) => &x.1,
                    None => &UNKNOWN_LOC,
                },
            ),
            Access::ExprAccessChain(name, item) => {
                (&get_name_chain_last_name(name).loc, item.as_ref().def_loc())
            }
            Access::TypeAccessChain(_) => todo!(),
            Access::ExprAddressName(_) => todo!(),
            Access::FieldInitialization(_, _) => todo!(),
            Access::AccessFiled(_, _) => todo!(),
            Access::KeyWords(_) => todo!(),
            Access::MacroCall(_) => todo!(),
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
            Self::Access(_) => todo!(),
            Self::Item(x) => x.fmt(f),
        }
    }
}
