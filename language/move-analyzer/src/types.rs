
use move_command_line_common::files::FileHash;

use move_compiler::shared::Identifier;

use move_compiler::{
    parser::{
        ast::*,
    },
    shared::*,
};

use move_ir_types::location::{Loc, Spanned};
use move_symbol_pool::Symbol;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub(crate) enum ResolvedType_ {
    UnKnown,
    Struct(Name, Vec<StructTypeParameter>, Vec<(Field, ResolvedType)>),
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
    Fun(
        Vec<(Name, Vec<Ability>)>, // type parameters.
        Vec<ResolvedType>,         // parameters.
        Box<ResolvedType>,         // return type.
    ),
    Vec(Box<ResolvedType>),
    /// Can't resolve the Type,Keep the ast type.
    ResolvedFailed(Type_),
}

#[derive(Clone, Debug)]
pub struct ResolvedType(pub(crate) Spanned<ResolvedType_>);
impl ResolvedType {
    pub(crate) fn nth_ty(&self, index: usize) -> Option<&'_ ResolvedType> {
        match &self.0.value {
            ResolvedType_::Multiple(x) => x.get(index),
            ResolvedType_::Struct(_, _, fields) => fields.get(index).map(|x| &x.1),
            _ => None,
        }
    }
    pub(crate) fn is_vector(&self) -> Option<&'_ ResolvedType> {
        match &self.0.value {
            ResolvedType_::Vec(x) => Some(x.as_ref()),
            _ => None,
        }
    }

    pub(crate) fn find_filed_by_name(&self, name: Symbol) -> Option<&'_ (Field, ResolvedType)> {
        match &self.0.value {
            ResolvedType_::Struct(_, _, fields) => {
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
    pub(crate) const fn new_unknown(loc: Loc) -> ResolvedType {
        Self(Spanned {
            loc,
            value: ResolvedType_::UnKnown,
        })
    }
    pub(crate) fn new_multi(loc: Loc, one: ResolvedType, num: usize) -> Self {
        Self(Spanned {
            loc,
            value: ResolvedType_::Multiple((0..num).map(|_| one.clone()).collect()),
        })
    }
    #[inline]
    pub(crate) fn new_unit(loc: Loc) -> Self {
        Self(Spanned {
            loc,
            value: ResolvedType_::Unit,
        })
    }
    #[inline]
    pub(crate) fn new_build_in(_b: BuildInType) -> Self {
        Self(Spanned {
            loc: UNKNOWN_LOC.clone(),
            value: ResolvedType_::Unit,
        })
    }
    #[inline]
    pub(crate) fn is_unknown(&self) -> bool {
        match &self.0.value {
            ResolvedType_::UnKnown => true,
            _ => false,
        }
    }
    #[inline]
    pub(crate) fn is_resolved_failed(&self) -> bool {
        match &self.0.value {
            ResolvedType_::ResolvedFailed(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub(crate) fn new_ref(loc: Loc, is_mut: bool, e: ResolvedType) -> Self {
        let value = ResolvedType_::Ref(is_mut, Box::new(e));
        Self(Spanned { loc, value })
    }

    #[inline]
    pub(crate) fn is_err(&self) -> bool {
        self.is_resolved_failed() || self.is_unknown()
    }
    fn is_tparam(&self) -> bool {
        match &self.0.value {
            ResolvedType_::TParam(_, _) => true,
            _ => false,
        }
    }
    #[inline]
    pub(crate) fn is_fun(&self) -> bool {
        match &self.0.value {
            ResolvedType_::Fun(_, _, _) => true,
            _ => false,
        }
    }

    /// bind type parameter to concrete tpe
    pub(crate) fn bind_type_parameter(&mut self, types: &HashMap<Symbol, ResolvedType>) {
        match &mut self.0.value {
            ResolvedType_::UnKnown => {}
            ResolvedType_::Struct(_, _, ref mut fields) => {
                for i in 0..fields.len() {
                    let t = fields.get_mut(i).unwrap();
                    t.1.bind_type_parameter(types);
                }
            }
            ResolvedType_::BuildInType(_) => {}
            ResolvedType_::TParam(name, _) => {
                if let Some(x) = types.get(&name.value) {
                    std::mem::replace(&mut self.0.value, (*x).clone().0.value);
                }
            }
            ResolvedType_::Ref(_, ref mut b) => {
                b.as_mut().bind_type_parameter(types);
            }
            ResolvedType_::Unit => {}
            ResolvedType_::Multiple(ref mut xs) => {
                for i in 0..xs.len() {
                    let t = xs.get_mut(i).unwrap();
                    t.bind_type_parameter(types);
                }
            }
            ResolvedType_::Fun(_, ref mut xs, ref mut ret) => {
                for i in 0..xs.len() {
                    let t = xs.get_mut(i).unwrap();
                    t.bind_type_parameter(types);
                }
                ret.as_mut().bind_type_parameter(types);
            }
            ResolvedType_::Vec(ref mut b) => {
                b.as_mut().bind_type_parameter(types);
            }
            ResolvedType_::ResolvedFailed(_) => {}
            ResolvedType_::ApplyTParam(_, _, _) => {
                unreachable!("called multiple times.")
            }
        }
    }
}

impl ResolvedType {
    pub(crate) fn chain_resolve_type_loc(&self) -> &Loc {
        match &self.0.value {
            ResolvedType_::Struct(name, _, _) => &name.loc,
            ResolvedType_::TParam(name, _) => &name.loc,
            _ => unreachable!(),
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
