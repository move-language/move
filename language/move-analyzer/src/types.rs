#![allow(dead_code)]

use crate::item::{self, ItemFunction};
use move_command_line_common::files::FileHash;
use move_command_line_common::types;
use move_compiler::shared::Identifier;
use move_compiler::{parser::ast::*, shared::*};
use move_ir_types::location::{Loc, Spanned};
use move_symbol_pool::Symbol;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub enum ResolvedType {
    UnKnown,
    Struct(item::ItemStruct),
    StructName(StructName, Rc<RefCell<HashMap<Symbol, item::ItemStruct>>>),
    /// struct { ... }
    BuildInType(BuildInType),
    /// T : drop
    TParam(Name, Vec<Ability>),
    ApplyTParam(
        Box<ResolvedType>,
        /* two field copied from TParam  */ Name,
        Vec<Ability>,
    ),
    /// & mut ...
    Ref(bool, Box<ResolvedType>),
    /// ()
    Unit,
    /// (t1, t2, ... , tn)
    /// Used for return values and expression blocks
    Multiple(Vec<ResolvedType>),
    Fun(ItemFunction),
    Vec(Box<ResolvedType>),
    /// Can't resolve the Type,Keep the ast type.
    ResolvedFailed(Type_),
}

impl ResolvedType {
    pub(crate) fn nth_ty(&self, index: usize) -> Option<&'_ ResolvedType> {
        match self {
            ResolvedType::Multiple(x) => x.get(index),
            _ => Some(self),
        }
    }

    pub(crate) fn is_vector(&self) -> Option<&'_ Self> {
        match self {
            ResolvedType::Vec(x) => Some(x.as_ref()),
            _ => None,
        }
    }

    pub(crate) fn find_filed_by_name(&self, name: Symbol) -> Option<&'_ (Field, ResolvedType)> {
        match self {
            ResolvedType::Struct(item::ItemStruct { fields, .. }) => {
                for f in fields.iter() {
                    if f.0.value() == name {
                        return Some(f);
                    }
                }
                None
            }
            _ => None,
        }
    }

    #[inline]
    pub(crate) const fn new_struct(
        name: StructName,
        ts: Vec<StructTypeParameter>,
        fields: Vec<(Field, ResolvedType)>,
    ) -> ResolvedType {
        ResolvedType::Struct(item::ItemStruct {
            name,
            type_parameters: ts,
            fields,
        })
    }

    pub(crate) fn new_multi_repeat(one: ResolvedType, num: usize) -> Self {
        ResolvedType::Multiple((0..num).map(|_| one.clone()).collect())
    }

    #[inline]
    pub(crate) fn new_unit() -> Self {
        ResolvedType::Unit
    }
    #[inline]
    pub(crate) fn new_build_in(b: BuildInType) -> Self {
        ResolvedType::BuildInType(b)
    }

    #[inline]
    pub(crate) const fn new_unknown() -> Self {
        ResolvedType::UnKnown
    }

    #[inline]
    pub(crate) fn is_unknown(&self) -> bool {
        match self {
            ResolvedType::UnKnown => true,
            _ => false,
        }
    }
    #[inline]
    pub(crate) fn is_resolved_failed(&self) -> bool {
        match self {
            ResolvedType::ResolvedFailed(_) => true,
            _ => false,
        }
    }
    pub(crate) fn is_unit(&self) -> bool {
        match self {
            ResolvedType::Unit => true,
            _ => false,
        }
    }
    #[inline]
    pub(crate) fn new_ref(is_mut: bool, e: ResolvedType) -> Self {
        let value = ResolvedType::Ref(is_mut, Box::new(e));
        value
    }
    #[inline]
    pub(crate) fn is_err(&self) -> bool {
        self.is_resolved_failed() || self.is_unknown()
    }
    fn is_tparam(&self) -> bool {
        match self {
            ResolvedType::TParam(_, _) => true,
            _ => false,
        }
    }
    #[inline]
    pub(crate) fn is_fun(&self) -> bool {
        match self {
            ResolvedType::Fun(_) => true,
            _ => false,
        }
    }

    /// bind type parameter to concrete tpe
    pub(crate) fn bind_type_parameter(&mut self, types: &HashMap<Symbol, ResolvedType>) {
        match self {
            ResolvedType::UnKnown => {}
            ResolvedType::Struct(item::ItemStruct { ref mut fields, .. }) => {
                for i in 0..fields.len() {
                    let t = fields.get_mut(i).unwrap();
                    t.1.bind_type_parameter(types);
                }
            }
            ResolvedType::BuildInType(_) => {}
            ResolvedType::TParam(name, _) => {
                if let Some(x) = types.get(&name.value) {
                    let _ = std::mem::replace(self, x.clone());
                }
            }
            ResolvedType::Ref(_, ref mut b) => {
                b.as_mut().bind_type_parameter(types);
            }
            ResolvedType::Unit => {}
            ResolvedType::Multiple(ref mut xs) => {
                for i in 0..xs.len() {
                    let t = xs.get_mut(i).unwrap();
                    t.bind_type_parameter(types);
                }
            }
            ResolvedType::Fun(x) => {
                let xs = &mut x.parameters;
                for i in 0..xs.len() {
                    let t = xs.get_mut(i).unwrap();
                    t.1.bind_type_parameter(types);
                }
                x.ret_type.as_mut().bind_type_parameter(types);
            }
            ResolvedType::Vec(ref mut b) => {
                b.as_mut().bind_type_parameter(types);
            }
            ResolvedType::ResolvedFailed(_) => {}
            ResolvedType::ApplyTParam(_, _, _) => {
                unreachable!("called multiple times.")
            }
            ResolvedType::StructName(_, _) => {
                unimplemented!();
            }
        }
    }
}

impl ResolvedType {
    pub(crate) fn def_loc(&self) -> &Loc {
        match self {
            ResolvedType::Struct(x) => x.name.borrow().0,
            ResolvedType::TParam(name, _) => &name.loc,
            ResolvedType::BuildInType(_) => &UNKNOWN_LOC,
            _ => unreachable!("{}", self),
        }
    }
}

#[derive(Clone, Debug, Copy)]
pub enum BuildInType {
    Bool,
    U8,
    U64,
    U128,
    Address,
    /// A number type from literal.
    /// Could be u8 and ... depend on How it is used.
    NumType,
    /// https://move-book.com/advanced-topics/managing-collections-with-vectors.html?highlight=STring#hex-and-bytestring-literal-for-inline-vector-definitions
    String,
}

impl BuildInType {
    pub(crate) fn from_symbol(s: Symbol) -> Self {
        match s.as_str() {
            "u8" => Self::U8,
            "u64" => Self::U64,
            "u128" => Self::U128,
            "bool" => Self::Bool,
            "address" => Self::Address,
            _ => unreachable!(),
        }
    }
}

pub const UNKNOWN_LOC: Loc = Loc::new(FileHash::empty(), 0, 0);

impl std::fmt::Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedType::UnKnown => write!(f, "unknown"),
            ResolvedType::Struct(x) => write!(f, "{}", x),
            ResolvedType::StructName(name, _) => write!(f, "struct {}", name.value().as_str()),
            ResolvedType::BuildInType(x) => write!(f, "{:?}", x),
            ResolvedType::TParam(name, _) => {
                write!(f, "type_parameter:{}", name.value.as_str())
            }
            ResolvedType::ApplyTParam(t, name, _) => {
                write!(f, "{} as {}", name.value.as_str(), t)
            }
            ResolvedType::Ref(is_mut, ty) => {
                write!(f, "&{} {}", if *is_mut { "mut" } else { "" }, ty.as_ref())
            }
            ResolvedType::Unit => write!(f, "()"),
            ResolvedType::Multiple(m) => {
                write!(f, "(")?;
                for i in 0..m.len() {
                    let t = m.get(i).unwrap();
                    write!(f, "{}{}", if i == m.len() - 1 { "" } else { "," }, t)?;
                }
                write!(f, ")")
            }
            ResolvedType::Fun(x) => {
                write!(f, "{}", x)
            }
            ResolvedType::Vec(ty) => {
                write!(f, "vector<{}>", ty.as_ref())
            }
            ResolvedType::ResolvedFailed(ty) => {
                write!(f, "{:?}", ty)
            }
        }
    }
}
