// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use super::item::*;
use crate::{item::ItemFun, project::ERR_ADDRESS, project_context::ProjectContext};
use enum_iterator::Sequence;
use move_command_line_common::files::FileHash;
use move_compiler::{
    parser::ast::*,
    shared::{Identifier, *},
};
use move_ir_types::location::{Loc, Spanned};
use move_symbol_pool::Symbol;
use std::{collections::HashMap, fmt::Debug, vec};

#[derive(Clone)]
pub enum ResolvedType {
    UnKnown,
    Struct(ItemStructNameRef, Vec<ResolvedType>),
    BuildInType(BuildInType),
    /// T : drop
    TParam(Name, Vec<Ability>),

    /// & mut ...
    Ref(bool, Box<ResolvedType>),
    /// ()
    Unit,
    /// (t1, t2, ... , tn)
    /// Used for return values and expression blocks
    Multiple(Vec<ResolvedType>),
    Fun(ItemFun),
    Vec(Box<ResolvedType>),

    Lambda {
        args: Vec<ResolvedType>,
        ret_ty: Box<ResolvedType>,
    },

    /// Spec type
    Range,
}

impl Default for ResolvedType {
    fn default() -> Self {
        Self::UnKnown
    }
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
    pub(crate) fn is_range(&self) -> Option<()> {
        match self {
            ResolvedType::Range => Some(()),
            _ => None,
        }
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
    pub(crate) fn new_vector(ty: ResolvedType) -> Self {
        ResolvedType::Vec(Box::new(ty))
    }

    #[inline]
    pub(crate) fn is_unknown(&self) -> bool {
        match self {
            ResolvedType::UnKnown => true,
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
    pub(crate) fn new_ref(is_mut: bool, ty: ResolvedType) -> Self {
        let value = ResolvedType::Ref(is_mut, Box::new(ty));
        value
    }
    #[inline]
    pub(crate) fn is_err(&self) -> bool {
        self.is_unknown()
    }
    #[inline]
    pub(crate) fn is_ref(&self) -> bool {
        match self {
            Self::Ref(_, _) => true,
            _ => false,
        }
    }

    /// bind type parameter to concrete type
    pub(crate) fn bind_type_parameter(&mut self, types: &HashMap<Symbol, ResolvedType>) {
        match self {
            ResolvedType::UnKnown => {}
            ResolvedType::BuildInType(_) => {}
            ResolvedType::TParam(name, _) => {
                if let Some(x) = types.get(&name.value) {
                    *self = x.clone();
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

            ResolvedType::Struct(_, ts) => {
                for index in 0..ts.len() {
                    ts.get_mut(index).unwrap().bind_type_parameter(types);
                }
            }
            ResolvedType::Range => {}
            ResolvedType::Lambda { args, ret_ty } => {
                for a in args.iter_mut() {
                    a.bind_type_parameter(types);
                }
                ret_ty.bind_type_parameter(types);
            }
        }
    }
}

impl ResolvedType {
    pub(crate) fn def_loc(&self) -> Loc {
        match self {
            ResolvedType::TParam(name, _) => name.loc,
            ResolvedType::BuildInType(_) => Loc::new(FileHash::empty(), 0, 0),
            ResolvedType::Struct(ItemStructNameRef { name, .. }, _) => name.loc(),
            ResolvedType::UnKnown => Loc::new(FileHash::empty(), 0, 0),
            ResolvedType::Ref(_, _) => Loc::new(FileHash::empty(), 0, 0),
            ResolvedType::Unit => Loc::new(FileHash::empty(), 0, 0),
            ResolvedType::Multiple(_) => Loc::new(FileHash::empty(), 0, 0),
            ResolvedType::Fun(f) => f.name.0.loc,
            ResolvedType::Vec(_) => Loc::new(FileHash::empty(), 0, 0),
            ResolvedType::Range => Loc::new(FileHash::empty(), 0, 0),
            ResolvedType::Lambda { .. } => Loc::new(FileHash::empty(), 0, 0),
        }
    }
}

#[derive(Clone, Debug, Copy, Sequence)]
pub enum BuildInType {
    Bool,
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
    Address,
    /// A number type from literal.
    /// Could be u8 and ... depend on How it is used.
    NumType,
    /// https://move-book.com/advanced-topics/managing-collections-with-vectors.html?highlight=STring#hex-and-bytestring-literal-for-inline-vector-definitions
    /// alias for vector<u8>
    String,
    Signer,
}

impl BuildInType {
    pub(crate) fn to_static_str(self) -> &'static str {
        match self {
            BuildInType::U8 => "u8",
            BuildInType::U16 => "u16",
            BuildInType::U32 => "u32",
            BuildInType::U64 => "u64",
            BuildInType::U128 => "u128",
            BuildInType::U256 => "u256",
            BuildInType::Bool => "bool",
            BuildInType::Address => "address",
            BuildInType::Signer => "signer",
            BuildInType::String => "vector<u8>",
            BuildInType::NumType => "u256",
        }
    }

    pub(crate) fn num_types() -> Vec<Self> {
        vec![
            Self::U8,
            Self::U16,
            Self::U32,
            Self::U64,
            Self::U128,
            Self::U256,
        ]
    }

    /// Not all is build in.
    /// exclude String and NumType.
    pub(crate) fn build_ins() -> Vec<Self> {
        let mut x = Self::num_types();
        x.push(Self::Address);
        x.push(Self::Signer);
        x.push(Self::Bool);
        x
    }
}

impl std::fmt::Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedType::UnKnown => write!(f, "unknown"),
            ResolvedType::Struct(ItemStructNameRef { name, .. }, _) => {
                write!(f, "{}", name.value().as_str())
            }
            ResolvedType::BuildInType(x) => write!(f, "{}", x.to_static_str()),
            ResolvedType::TParam(name, _) => {
                write!(f, "{}", name.value.as_str())
            }
            ResolvedType::Ref(is_mut, ty) => {
                write!(f, "&{}{}", if *is_mut { "mut " } else { "" }, ty.as_ref())
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
                write!(f, "vector<<{}>>", ty.as_ref())
            }
            ResolvedType::Range => {
                write!(f, "range(n..m)")
            }
            ResolvedType::Lambda { args, ret_ty } => {
                write!(f, "|")?;
                if args.len() > 0 {
                    let last_index = args.len() - 1;
                    for (index, a) in args.iter().enumerate() {
                        write!(f, "{}", a)?;
                        if index != last_index {
                            write!(f, ",")?;
                        }
                    }
                }
                write!(f, "|")?;
                if matches!(ret_ty.as_ref(), ResolvedType::Unit) == false {
                    write!(f, ":")?;
                    write!(f, "{}", ret_ty)
                } else {
                    Ok(())
                }
            }
        }
    }
}

impl ResolvedType {
    pub(crate) fn struct_ref_to_struct(&self, s: &ProjectContext) -> ItemStruct {
        match self.clone() {
            Self::Struct(
                ItemStructNameRef {
                    addr,
                    module_name,
                    name,
                    type_parameters: _type_parameters,
                    is_test: _is_test,
                },
                v,
            ) => {
                s.query_item(addr, module_name, name.0.value, |x| match x {
                    Item::Struct(item) => {
                        let mut item = item.clone();
                        item.type_parameters_ins = v;
                        item.bind_type_parameter(None );
                        item
                    }
                    _ => {
                        unimplemented!()
                    }
                }) .expect("You are looking for a struct which can't be found,It is possible But should not happen.")
            }
            _ => { ItemStruct { name: StructName(Spanned { loc : Loc::new(FileHash::empty(), 0, 0) , value  :Symbol::from("")}), type_parameters: vec![ ], type_parameters_ins: vec![ ], fields: vec![ ], is_test: false , addr:  * ERR_ADDRESS, module_name: Symbol::from("") } },
        }
    }
}
