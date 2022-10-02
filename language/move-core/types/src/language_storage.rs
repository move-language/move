// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    account_address::AccountAddress,
    identifier::{IdentStr, Identifier},
    parser::{parse_struct_tag, parse_type_tag},
};
#[cfg(any(test, feature = "fuzzing"))]
use proptest_derive::Arbitrary;
use serde::{Deserialize, Serialize, Serializer};
use std::{
    fmt::{Display, Formatter},
    str::FromStr,
};

pub const CODE_TAG: u8 = 0;
pub const RESOURCE_TAG: u8 = 1;

const MAX_TYPE_TAG_NESTING: u8 = 8;

/// Hex address: 0x1
pub const CORE_CODE_ADDRESS: AccountAddress = AccountAddress::ONE;

#[derive(Deserialize, Debug, PartialEq, Hash, Eq, Clone, PartialOrd, Ord)]
pub enum TypeTag {
    // alias for compatibility with old json serialized data.
    #[serde(rename = "bool", alias = "Bool")]
    Bool,
    #[serde(rename = "u8", alias = "U8")]
    U8,
    #[serde(rename = "u64", alias = "U64")]
    U64,
    #[serde(rename = "u128", alias = "U128")]
    U128,
    #[serde(rename = "address", alias = "Address")]
    Address,
    #[serde(rename = "signer", alias = "Signer")]
    Signer,
    #[serde(rename = "vector", alias = "Vector")]
    Vector(Box<TypeTag>),
    #[serde(rename = "struct", alias = "Struct")]
    Struct(Box<StructTag>),
}

#[derive(Serialize)]
enum SafeTypeTag<'a> {
    #[serde(rename = "bool", alias = "Bool")]
    Bool,
    #[serde(rename = "u8", alias = "U8")]
    U8,
    #[serde(rename = "u64", alias = "U64")]
    U64,
    #[serde(rename = "u128", alias = "U128")]
    U128,
    #[serde(rename = "address", alias = "Address")]
    Address,
    #[serde(rename = "signer", alias = "Signer")]
    Signer,
    #[serde(rename = "vector", alias = "Vector")]
    Vector(Box<SafeTypeTag<'a>>),
    #[serde(rename = "struct", alias = "Struct")]
    Struct(Box<SafeStructTag<'a>>),
}

impl<'a> From<&'a TypeTag> for SafeTypeTag<'a> {
    fn from(type_tag: &'a TypeTag) -> Self {
        match &type_tag {
            TypeTag::Bool => SafeTypeTag::Bool,
            TypeTag::U8 => SafeTypeTag::U8,
            TypeTag::U64 => SafeTypeTag::U64,
            TypeTag::U128 => SafeTypeTag::U128,
            TypeTag::Address => SafeTypeTag::Address,
            TypeTag::Signer => SafeTypeTag::Signer,
            TypeTag::Vector(value) => SafeTypeTag::Vector(Box::new((&**value).into())),
            TypeTag::Struct(value) => SafeTypeTag::Struct(Box::new((&**value).into())),
        }
    }
}

impl Serialize for TypeTag {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match &self {
            TypeTag::Vector(type_tag) => {
                validate_type_tag_recursion(vec![(**type_tag).clone()].as_slice(), 1)
            }
            TypeTag::Struct(value) => validate_type_tag_recursion(&value.type_params, 1),
            _ => Ok(()),
        }
        .map_err(serde::ser::Error::custom)?;

        serializer.serialize_newtype_struct("TypeTag", &SafeTypeTag::from(self))
    }
}

fn validate_type_tag_recursion(
    type_tags: &[TypeTag],
    count: u8,
) -> std::result::Result<(), String> {
    if count >= MAX_TYPE_TAG_NESTING {
        return Err(format!(
            "Hit TypeTag nesting limit during serialization: {}",
            count
        ));
    }

    for type_tag in type_tags {
        match type_tag {
            TypeTag::Vector(type_tag) => {
                validate_type_tag_recursion(vec![(**type_tag).clone()].as_slice(), count + 1)
            }
            TypeTag::Struct(value) => validate_type_tag_recursion(&value.type_params, count + 1),
            _ => Ok(()),
        }?;
    }
    Ok(())
}

impl FromStr for TypeTag {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_type_tag(s)
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Hash, Eq, Clone, PartialOrd, Ord)]
pub struct StructTag {
    pub address: AccountAddress,
    pub module: Identifier,
    pub name: Identifier,
    // alias for compatibility with old json serialized data.
    #[serde(rename = "type_args", alias = "type_params")]
    pub type_params: Vec<TypeTag>,
}

#[derive(Serialize)]
struct SafeStructTag<'a> {
    address: AccountAddress,
    module: &'a Identifier,
    name: &'a Identifier,
    // alias for compatibility with old json serialized data.
    #[serde(rename = "type_args", alias = "type_params")]
    type_params: Vec<SafeTypeTag<'a>>,
}

impl<'a> From<&'a StructTag> for SafeStructTag<'a> {
    fn from(struct_tag: &'a StructTag) -> Self {
        Self {
            address: struct_tag.address,
            module: &struct_tag.module,
            name: &struct_tag.name,
            type_params: struct_tag
                .type_params
                .iter()
                .map(|ty_arg| ty_arg.into())
                .collect::<Vec<_>>(),
        }
    }
}

impl StructTag {
    pub fn access_vector(&self) -> Vec<u8> {
        let mut key = vec![RESOURCE_TAG];
        key.append(&mut bcs::to_bytes(self).unwrap());
        key
    }

    pub fn module_id(&self) -> ModuleId {
        ModuleId::new(self.address, self.module.to_owned())
    }
}

impl FromStr for StructTag {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_struct_tag(s)
    }
}

/// Represents the initial key into global storage where we first index by the address, and then
/// the struct tag
#[derive(Serialize, Deserialize, Debug, PartialEq, Hash, Eq, Clone, PartialOrd, Ord)]
pub struct ResourceKey {
    pub address: AccountAddress,
    pub type_: StructTag,
}

impl ResourceKey {
    pub fn address(&self) -> AccountAddress {
        self.address
    }

    pub fn type_(&self) -> &StructTag {
        &self.type_
    }
}

impl ResourceKey {
    pub fn new(address: AccountAddress, type_: StructTag) -> Self {
        ResourceKey { address, type_ }
    }
}

/// Represents the initial key into global storage where we first index by the address, and then
/// the struct tag
#[derive(Serialize, Deserialize, Debug, PartialEq, Hash, Eq, Clone, PartialOrd, Ord)]
#[cfg_attr(any(test, feature = "fuzzing"), derive(Arbitrary))]
#[cfg_attr(any(test, feature = "fuzzing"), proptest(no_params))]
pub struct ModuleId {
    address: AccountAddress,
    name: Identifier,
}

impl From<ModuleId> for (AccountAddress, Identifier) {
    fn from(module_id: ModuleId) -> Self {
        (module_id.address, module_id.name)
    }
}

impl ModuleId {
    pub fn new(address: AccountAddress, name: Identifier) -> Self {
        ModuleId { address, name }
    }

    pub fn name(&self) -> &IdentStr {
        &self.name
    }

    pub fn address(&self) -> &AccountAddress {
        &self.address
    }

    pub fn access_vector(&self) -> Vec<u8> {
        let mut key = vec![CODE_TAG];
        key.append(&mut bcs::to_bytes(self).unwrap());
        key
    }
}

impl Display for ModuleId {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}::{}", self.address, self.name)
    }
}

impl ModuleId {
    pub fn short_str_lossless(&self) -> String {
        format!("0x{}::{}", self.address.short_str_lossless(), self.name)
    }
}

impl Display for StructTag {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "0x{}::{}::{}",
            self.address.short_str_lossless(),
            self.module,
            self.name
        )?;
        if let Some(first_ty) = self.type_params.first() {
            write!(f, "<")?;
            write!(f, "{}", first_ty)?;
            for ty in self.type_params.iter().skip(1) {
                write!(f, ", {}", ty)?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl Display for TypeTag {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            TypeTag::Struct(s) => write!(f, "{}", s),
            TypeTag::Vector(ty) => write!(f, "vector<{}>", ty),
            TypeTag::U8 => write!(f, "u8"),
            TypeTag::U64 => write!(f, "u64"),
            TypeTag::U128 => write!(f, "u128"),
            TypeTag::Address => write!(f, "address"),
            TypeTag::Signer => write!(f, "signer"),
            TypeTag::Bool => write!(f, "bool"),
        }
    }
}

impl Display for ResourceKey {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "0x{}/{}", self.address.short_str_lossless(), self.type_)
    }
}

impl From<StructTag> for TypeTag {
    fn from(t: StructTag) -> TypeTag {
        TypeTag::Struct(Box::new(t))
    }
}

#[cfg(test)]
mod tests {
    use super::TypeTag;
    use crate::{
        account_address::AccountAddress, identifier::Identifier, language_storage::StructTag,
    };
    use std::mem;

    #[test]
    fn test_type_tag_serde() {
        let a = TypeTag::Struct(Box::new(StructTag {
            address: AccountAddress::ONE,
            module: Identifier::new("abc").unwrap(),
            name: Identifier::new("abc").unwrap(),
            type_params: vec![TypeTag::U8],
        }));
        let b = serde_json::to_string(&a).unwrap();
        let c: TypeTag = serde_json::from_str(&b).unwrap();
        assert!(a.eq(&c), "Typetag serde error");
        assert_eq!(mem::size_of::<TypeTag>(), 16);
    }

    #[test]
    fn test_nested_type_tag_struct_serde() {
        let mut type_tags = vec![make_type_tag_struct(TypeTag::U8)];

        let limit = super::MAX_TYPE_TAG_NESTING - 1;
        while type_tags.len() < limit.into() {
            type_tags.push(make_type_tag_struct(type_tags.last().unwrap().clone()));
        }

        bcs::to_bytes(type_tags.last().unwrap()).unwrap();

        let nest_limit = make_type_tag_struct(type_tags.last().unwrap().clone());
        bcs::to_bytes(&nest_limit).unwrap_err();
    }

    #[test]
    fn test_nested_type_tag_vector_serde() {
        let mut type_tags = vec![make_type_tag_struct(TypeTag::U8)];

        let limit = super::MAX_TYPE_TAG_NESTING - 1;
        while type_tags.len() < limit.into() {
            type_tags.push(make_type_tag_vector(type_tags.last().unwrap().clone()));
        }

        bcs::to_bytes(type_tags.last().unwrap()).unwrap();

        let nest_limit = make_type_tag_vector(type_tags.last().unwrap().clone());
        bcs::to_bytes(&nest_limit).unwrap_err();
    }

    fn make_type_tag_vector(type_param: TypeTag) -> TypeTag {
        TypeTag::Vector(Box::new(type_param))
    }

    fn make_type_tag_struct(type_param: TypeTag) -> TypeTag {
        TypeTag::Struct(Box::new(StructTag {
            address: AccountAddress::ONE,
            module: Identifier::new("a").unwrap(),
            name: Identifier::new("a").unwrap(),
            type_params: vec![type_param],
        }))
    }
}
