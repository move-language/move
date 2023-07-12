// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0
//! Loaded representation for runtime types.

use move_binary_format::{
    errors::{PartialVMError, PartialVMResult},
    file_format::AbilitySet,
};
use move_core_types::{
    account_address::AccountAddress,
    identifier::Identifier,
    language_storage::{StructTag, TypeTag},
    value::{MoveStructLayout, MoveTypeLayout},
    vm_status::StatusCode,
};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::convert::TryInto;

#[derive(Debug, Clone, Copy)]
pub(crate) struct WrappedAbilitySet(pub AbilitySet);

impl Serialize for WrappedAbilitySet {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.0.into_u8().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for WrappedAbilitySet {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let byte = u8::deserialize(deserializer)?;
        Ok(WrappedAbilitySet(AbilitySet::from_u8(byte).ok_or_else(
            || serde::de::Error::custom(format!("Invalid ability set: {:X}", byte)),
        )?))
    }
}

/// VM representation of a struct type in Move.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct FatStructType {
    pub address: AccountAddress,
    pub module: Identifier,
    pub name: Identifier,
    pub abilities: WrappedAbilitySet,
    pub ty_args: Vec<FatType>,
    pub layout: Vec<FatType>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) enum FatType {
    Bool,
    U8,
    U64,
    U128,
    Address,
    Signer,
    Vector(Box<FatType>),
    Struct(Box<FatStructType>),
    Reference(Box<FatType>),
    MutableReference(Box<FatType>),
    TyParam(usize),
    // NOTE: Added in bytecode version v6, do not reorder!
    U16,
    U32,
    U256,
}

impl FatStructType {
    pub fn subst(&self, ty_args: &[FatType]) -> PartialVMResult<FatStructType> {
        Ok(Self {
            address: self.address,
            module: self.module.clone(),
            name: self.name.clone(),
            abilities: self.abilities,
            ty_args: self
                .ty_args
                .iter()
                .map(|ty| ty.subst(ty_args))
                .collect::<PartialVMResult<_>>()?,
            layout: self
                .layout
                .iter()
                .map(|ty| ty.subst(ty_args))
                .collect::<PartialVMResult<_>>()?,
        })
    }

    pub fn struct_tag(&self) -> PartialVMResult<StructTag> {
        let ty_args = self
            .ty_args
            .iter()
            .map(|ty| ty.type_tag())
            .collect::<PartialVMResult<Vec<_>>>()?;
        Ok(StructTag {
            address: self.address,
            module: self.module.clone(),
            name: self.name.clone(),
            type_params: ty_args,
        })
    }
}

impl FatType {
    pub fn subst(&self, ty_args: &[FatType]) -> PartialVMResult<FatType> {
        use FatType::*;

        let res = match self {
            TyParam(idx) => match ty_args.get(*idx) {
                Some(ty) => ty.clone(),
                None => {
                    return Err(
                        PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                            .with_message(format!(
                            "fat type substitution failed: index out of bounds -- len {} got {}",
                            ty_args.len(),
                            idx
                        )),
                    );
                },
            },

            Bool => Bool,
            U8 => U8,
            U16 => U16,
            U32 => U32,
            U64 => U64,
            U128 => U128,
            U256 => U256,
            Address => Address,
            Signer => Signer,
            Vector(ty) => Vector(Box::new(ty.subst(ty_args)?)),
            Reference(ty) => Reference(Box::new(ty.subst(ty_args)?)),
            MutableReference(ty) => MutableReference(Box::new(ty.subst(ty_args)?)),

            Struct(struct_ty) => Struct(Box::new(struct_ty.subst(ty_args)?)),
        };

        Ok(res)
    }

    pub fn type_tag(&self) -> PartialVMResult<TypeTag> {
        use FatType::*;

        let res = match self {
            Bool => TypeTag::Bool,
            U8 => TypeTag::U8,
            U16 => TypeTag::U16,
            U32 => TypeTag::U32,
            U64 => TypeTag::U64,
            U128 => TypeTag::U128,
            U256 => TypeTag::U256,
            Address => TypeTag::Address,
            Signer => TypeTag::Signer,
            Vector(ty) => TypeTag::Vector(Box::new(ty.type_tag()?)),
            Struct(struct_ty) => TypeTag::Struct(Box::new(struct_ty.struct_tag()?)),

            Reference(_) | MutableReference(_) | TyParam(_) => {
                return Err(
                    PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                        .with_message(format!("cannot derive type tag for {:?}", self)),
                )
            },
        };

        Ok(res)
    }
}

impl From<&TypeTag> for FatType {
    fn from(tag: &TypeTag) -> FatType {
        use FatType::*;
        match tag {
            TypeTag::Bool => Bool,
            TypeTag::U8 => U8,
            TypeTag::U16 => U16,
            TypeTag::U32 => U32,
            TypeTag::U64 => U64,
            TypeTag::U128 => U128,
            TypeTag::Address => Address,
            TypeTag::Signer => Signer,
            TypeTag::Vector(inner) => Vector(Box::new(inner.as_ref().into())),
            TypeTag::Struct(inner) => Struct(Box::new(inner.as_ref().into())),
            TypeTag::U256 => U256,
        }
    }
}

impl From<&StructTag> for FatStructType {
    fn from(struct_tag: &StructTag) -> FatStructType {
        FatStructType {
            address: struct_tag.address,
            module: struct_tag.module.clone(),
            name: struct_tag.name.clone(),
            abilities: WrappedAbilitySet(AbilitySet::EMPTY), // We can't get abilities from a struct tag
            ty_args: struct_tag
                .type_params
                .iter()
                .map(|inner| inner.into())
                .collect(),
            layout: vec![], // We can't get field types from struct tag
        }
    }
}

impl TryInto<MoveStructLayout> for &FatStructType {
    type Error = PartialVMError;

    fn try_into(self) -> Result<MoveStructLayout, Self::Error> {
        Ok(MoveStructLayout::new(
            self.layout
                .iter()
                .map(|ty| ty.try_into())
                .collect::<PartialVMResult<Vec<_>>>()?,
        ))
    }
}

impl TryInto<MoveTypeLayout> for &FatType {
    type Error = PartialVMError;

    fn try_into(self) -> Result<MoveTypeLayout, Self::Error> {
        Ok(match self {
            FatType::Address => MoveTypeLayout::Address,
            FatType::U8 => MoveTypeLayout::U8,
            FatType::U16 => MoveTypeLayout::U16,
            FatType::U32 => MoveTypeLayout::U32,
            FatType::U64 => MoveTypeLayout::U64,
            FatType::U128 => MoveTypeLayout::U128,
            FatType::U256 => MoveTypeLayout::U256,
            FatType::Bool => MoveTypeLayout::Bool,
            FatType::Vector(v) => MoveTypeLayout::Vector(Box::new(v.as_ref().try_into()?)),
            FatType::Struct(s) => MoveTypeLayout::Struct(MoveStructLayout::new(
                s.layout
                    .iter()
                    .map(|ty| ty.try_into())
                    .collect::<PartialVMResult<Vec<_>>>()?,
            )),
            FatType::Signer => MoveTypeLayout::Signer,
            FatType::Reference(_) | FatType::MutableReference(_) | FatType::TyParam(_) => {
                return Err(PartialVMError::new(StatusCode::ABORT_TYPE_MISMATCH_ERROR))
            },
        })
    }
}
