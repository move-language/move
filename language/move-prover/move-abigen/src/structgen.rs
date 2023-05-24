// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::{bail, Result, anyhow};
#[allow(unused_imports)]
use log::{debug, info, warn};
use move_core_types::{
    account_address::AccountAddress,
    language_storage::StructTag,
};
use move_model::{
    model::{GlobalEnv, StructEnv},
    ty::{PrimitiveType, Type},
};
use serde::{Deserialize, Serialize};
use serde_reflection::{ContainerFormat, Format, Registry, Named};
use std::{collections::BTreeMap};

/// Options passed into the ABI generator.
#[derive(Default, Debug, Clone, Serialize, Deserialize)]
#[serde(default, deny_unknown_fields)]
pub struct StructAbigenOptions {
    /// Include struct's module address/name in the generated name,
    pub use_canonical_name: bool,
}

/// The ABI generator.
pub struct StructAbigen<'env> {
    /// Options.
    options: &'env StructAbigenOptions,
    /// Input definitions.
    env: &'env GlobalEnv,
    /// Map from file name to generated script ABI (if any).
    output: BTreeMap<StructTag, ContainerFormat>,
}

impl<'env> StructAbigen<'env> {
    /// Creates a new ABI generator.
    pub fn new(env: &'env GlobalEnv, options: &'env StructAbigenOptions) -> Self {
        Self {
            options,
            env,
            output: BTreeMap::new(),
        }
    }

    /// Returns the result of ABI generation, a vector of pairs of filenames
    /// and JSON content.
    pub fn into_result(mut self) -> Registry {
        std::mem::take(&mut self.output)
            .into_iter()
            .map(|(tag, format)| {
                (
                    self.struct_tag_to_name(&tag),
                    format,
                )
            })
            .collect::<BTreeMap<String, ContainerFormat>>()
    }

    /// Generates ABIs for all script modules in the environment (excluding the dependency set).
    pub fn gen(&mut self) -> Result<()> {
        for module in self.env.get_modules() {
            for struct_ in module.get_structs() {
                if struct_.get_type_parameters().is_empty() {
                    self.register_struct_tag(&StructTag {
                        address: *module.self_address(),
                        module: module.get_identifier(),
                        name: struct_.get_identifier().ok_or_else(|| anyhow!("Fail to get identifier"))?,
                        type_params: vec![],
                    })?;
                } else {
                    // Struct has a generic type parameter. User need to create the directive to specify the monomorphized struct name.
                    unimplemented!()
                }
            }
        }
        Ok(())
    }

    fn register_struct_tag(&mut self, tag: &StructTag) -> Result<()> {
        if self.output.contains_key(tag) {
            return Ok(())
        }
        let struct_id = self.env.find_struct_by_tag(tag).ok_or_else(|| anyhow!("Fail to get tag for {:?}", tag))?;
        let type_args = tag.type_params.iter().map(|ty| Type::from_type_tag(&ty, &self.env)).collect::<Vec<_>>();
        let struct_env = self.env.get_struct(struct_id);
        let mut fields =  vec![];
        for field in struct_env.get_fields() {
            let fmt = self.type_to_format(field.get_type().instantiate(&type_args))?;
            fields.push(Named {
                name: field.get_identifier().ok_or_else(|| anyhow!("Fail to get tag for {:?}", tag))?.to_string(),
                value: fmt,
            });
        }
        self.output.insert(tag.clone(), ContainerFormat::Struct(fields));
        Ok(())
    }

    fn struct_tag_to_name(&self, tag: &StructTag) -> String {
        if self.options.use_canonical_name {
            tag.to_canonical_string()
                .replace(":", "_")
                .replace("<", "_")
                .replace(">", "_")
        } else {
            tag.name.as_str().to_string()
        }
    }

    fn type_to_format(&mut self, ty: Type) -> Result<Format> {
        Ok(match ty {
            Type::Primitive(primitive) => match primitive {
                PrimitiveType::Address => Format::TupleArray {
                    content: Box::new(Format::Bytes),
                    size: AccountAddress::LENGTH,
                },
                PrimitiveType::Bool => Format::Bool,
                PrimitiveType::U8 => Format::I8,
                PrimitiveType::U16 => Format::I16,
                PrimitiveType::U32 => Format::I32,
                PrimitiveType::U64 => Format::I64,
                PrimitiveType::U128 => Format::I128,
                PrimitiveType::U256 => Format::TupleArray {
                    content: Box::new(Format::Bytes),
                    size: move_core_types::u256::U256_NUM_BYTES,
                },
                PrimitiveType::Signer
                | PrimitiveType::EventStore
                | PrimitiveType::Num
                | PrimitiveType::Range => bail!("Unexpected type in struct field"),
            },
            Type::Vector(ty) => Format::Seq(Box::new(self.type_to_format(*ty)?)),
            Type::Struct(_, _, _) => {
                let tag = ty.into_struct_tag(&self.env).ok_or_else(|| anyhow!("Failed to convert type into struct tag"))?;
                self.register_struct_tag(&tag)?;
                Format::TypeName(self.struct_tag_to_name(&tag))
            }
            Type::Error | Type::Fun(_, _) | Type::Reference(_, _) | Type::ResourceDomain(_, _, _) | Type::TypeParameter(_) | Type::Tuple(_) | Type::Var(_) | Type::TypeDomain(_) =>  bail!("Unexpected type in struct field"),
            
        })
    }
}
