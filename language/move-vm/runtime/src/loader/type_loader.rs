// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use move_binary_format::{
    binary_views::BinaryIndexedView, errors::PartialVMResult, file_format::SignatureToken,
};
use move_vm_types::loaded_data::runtime_types::{StructIdentifier, Type};
use std::sync::Arc;

// `intern_type` converts a signature token into the in memory type representation used by the MoveVM.
pub fn intern_type(
    module: BinaryIndexedView,
    tok: &SignatureToken,
    struct_name_table: &[Arc<StructIdentifier>],
) -> PartialVMResult<Type> {
    let res = match tok {
        SignatureToken::Bool => Type::Bool,
        SignatureToken::U8 => Type::U8,
        SignatureToken::U16 => Type::U16,
        SignatureToken::U32 => Type::U32,
        SignatureToken::U64 => Type::U64,
        SignatureToken::U128 => Type::U128,
        SignatureToken::U256 => Type::U256,
        SignatureToken::Address => Type::Address,
        SignatureToken::Signer => Type::Signer,
        SignatureToken::TypeParameter(idx) => Type::TyParam(*idx),
        SignatureToken::Vector(inner_tok) => {
            let inner_type = intern_type(module, inner_tok, struct_name_table)?;
            Type::Vector(Box::new(inner_type))
        },
        SignatureToken::Reference(inner_tok) => {
            let inner_type = intern_type(module, inner_tok, struct_name_table)?;
            Type::Reference(Box::new(inner_type))
        },
        SignatureToken::MutableReference(inner_tok) => {
            let inner_type = intern_type(module, inner_tok, struct_name_table)?;
            Type::MutableReference(Box::new(inner_type))
        },
        SignatureToken::Struct(sh_idx) => {
            let struct_handle = module.struct_handle_at(*sh_idx);
            Type::Struct {
                name: struct_name_table[sh_idx.0 as usize].clone(),
                ability: struct_handle.abilities,
            }
        },
        SignatureToken::StructInstantiation(sh_idx, tys) => {
            let type_args: Vec<_> = tys
                .iter()
                .map(|tok| intern_type(module, tok, struct_name_table))
                .collect::<PartialVMResult<_>>()?;
            let struct_handle = module.struct_handle_at(*sh_idx);
            Type::StructInstantiation {
                name: struct_name_table[sh_idx.0 as usize].clone(),
                base_ability_set: struct_handle.abilities,
                ty_args: Arc::new(type_args),
                phantom_ty_args_mask: struct_handle
                    .type_parameters
                    .iter()
                    .map(|ty| ty.is_phantom)
                    .collect(),
            }
        },
    };
    Ok(res)
}
