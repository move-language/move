use super::scope::*;
use super::types::*;

use move_compiler::shared::Identifier;
use move_compiler::{parser::ast::*, shared::*};
use move_ir_types::location::{Loc, Spanned};
use move_symbol_pool::Symbol;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Item {
    /////////////////////////////
    /// VALUE types
    Parameter(Var, ResolvedType),
    ImportedUseModule(ModuleIdent, Rc<RefCell<Scope>>),
    ImportedMember(Box<Item>),
    Const(ConstantName, ResolvedType),

    /////////////////////////
    /// TYPE types
    Struct(Name, Vec<StructTypeParameter>, Vec<(Field, ResolvedType)>),
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
            Item::Struct(name, types, fields) => (
                name.loc,
                ResolvedType_::Struct(name.clone(), types.clone(), fields.clone()),
            ),
            Item::BuildInType(b) => (UNKNOWN_LOC, ResolvedType_::BuildInType(*b)),
            _ => return None,
        };
        Some(ResolvedType(Spanned { loc, value: x }))
    }

    pub(crate) fn def_loc(&self) -> &Loc {
        match self {
            Self::Parameter(var, _) => &var.borrow().0,
            Self::ImportedMember(item) => item.as_ref().def_loc(),
            Self::Struct(name, _, _) => &name.loc,
            Self::BuildInType(_) => &UNKNOWN_LOC,
            Self::TParam(name, _) => &name.loc,
            _ => unreachable!(),
        }
    }

    pub(crate) fn debug_loc(&self) -> Option<&'_ Loc> {
        match self {
            Item::Parameter(var, _) => Some(var.borrow().0),
            Item::ImportedUseModule(m, _) => Some(&m.loc),
            Item::ImportedMember(item) => item.as_ref().debug_loc(),
            Item::Const(name, _) => Some(name.borrow().0),
            Item::Struct(name, _, _) => Some(&name.loc),
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

// impl ShowWithModule for Item {
//     fn show_string(&self, module: &dyn ModuleServices) -> String {
//         let loc = |loc| {
//             module.convert_loc_range(loc).unwrap_or(FileRange {
//                 path: "<unknown>",
//                 line: 1,
//                 col_start: 0,
//                 col_end: 0,
//             })
//         };

//         match self {
//             Item::Parameter(var, ty) => {
//                 format!(
//                     "parameter:{} loc:{} ty:{}",
//                     var,
//                     loc(var.borrow().0),
//                     ty.show_string(module)
//                 )
//             }
//             Item::ImportedUseModule(m, _) => {
//                 format!("imported module {}")
//             }
//             Item::ImportedMember(m) => m.as_ref().show_string(module),
//             Item::Const(name, ty) => {
//                 format!(
//                     "parameter:{} loc:{} ty:{}",
//                     var,
//                     loc(name.borrow().0),
//                     ty.show_string(module)
//                 )
//             }
//             Item::Struct(name, type_paras, fields) => {
//                 let mut s = String::default();

//                 s.push("struct{")
//             }
//             Item::BuildInType(_) => {}
//             Item::TParam(_, _) => todo!(),
//             Item::ApplyType(_, _) => todo!(),
//             Item::UseMember(_, _) => todo!(),
//             Item::ExprVar(_) => todo!(),
//             Item::NameAccessChain(_) => todo!(),
//             Item::ExprAddressName(_) => todo!(),
//             Item::FieldInitialization(_, _) => todo!(),
//             Item::AccessFiled(_, _) => todo!(),
//             Item::KeyWords(_) => todo!(),
//             Item::MacroCall(_) => todo!(),
//         }
//     }
// }
