use super::scope::*;
use super::types::*;

use anyhow::Ok;
use move_compiler::shared::Identifier;
use move_compiler::{parser::ast::*, shared::*};
use move_ir_types::location::{Loc, Spanned};
use move_symbol_pool::Symbol;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ItemStruct {
    pub(crate) name: StructName,
    pub(crate) type_parameters: Vec<StructTypeParameter>,
    pub(crate) fields: Vec<(Field, ResolvedType)>, /* TODO If this length is zero,maybe a native. */
}

impl std::fmt::Display for ItemStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unimplemented!()
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

    ////////////////////////////////
    /// various access types.
    // A type apply.
    ApplyType(NameAccessChain, Box<ResolvedType>),
    UseMember(Name, Box<Item>),
    ExprVar(Var),
    NameAccessChain(NameAccessChain),
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

impl Item {
    ///
    pub(crate) fn to_type(&self) -> Option<ResolvedType> {
        let (loc, x) = match self {
            Item::TParam(name, ab) => (name.loc, ResolvedType_::TParam(name.clone(), ab.clone())),
            Item::Struct(x) => (x.name.borrow().0.clone(), ResolvedType_::Struct(x.clone())),
            Item::StructName(name, x) => (
                name.loc(),
                ResolvedType_::StructName(name.clone(), x.clone()),
            ),
            Item::BuildInType(b) => (UNKNOWN_LOC, ResolvedType_::BuildInType(*b)),
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
            _ => unreachable!(),
        }
    }

    pub(crate) fn debug_loc(&self) -> Option<&'_ Loc> {
        match self {
            Item::Parameter(var, _) => Some(var.borrow().0),
            Item::ImportedModule(m, _) => Some(&m.loc),
            Item::ImportedMember(_, item) => item.as_ref().debug_loc(),
            Item::Const(name, _) => Some(name.borrow().0),
            Item::Struct(x) => Some(&x.name.borrow().0),
            Item::BuildInType(_) => todo!(),
            Item::TParam(_, _) => todo!(),
            Item::ApplyType(_, _) => todo!(),
            Item::UseMember(_, _) => todo!(),
            Item::ExprVar(_) => todo!(),
            Item::NameAccessChain(_) => todo!(),
            Item::ExprAddressName(_) => todo!(),
            Item::FieldInitialization(_, _) => todo!(),
            Item::AccessFiled(_, _) => todo!(),
            Item::KeyWords(_) => todo!(),
            Item::MacroCall(_) => todo!(),
            Item::StructName(name, _) => Some(name.borrow().0),
            Item::Fun(name, _, _, _) => Some(name.borrow().0),
        }
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
pub(crate) fn get_access_chain_name(x: &NameAccessChain) -> &Name {
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
            Item::Fun(_, _, _, _) => todo!(),
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
            Item::ApplyType(_, _) => todo!(),
            Item::UseMember(_, _) => todo!(),
            Item::ExprVar(_) => todo!(),
            Item::NameAccessChain(_) => todo!(),
            Item::ExprAddressName(_) => todo!(),
            Item::FieldInitialization(_, _) => todo!(),
            Item::AccessFiled(_, _) => todo!(),
            Item::KeyWords(_) => todo!(),
            Item::MacroCall(_) => todo!(),
        }
    }
}
