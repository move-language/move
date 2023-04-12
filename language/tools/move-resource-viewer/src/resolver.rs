// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    fat_type::{FatStructType, FatType, WrappedAbilitySet},
    module_cache::ModuleCache,
};
use anyhow::{anyhow, Error, Result};
use move_binary_format::{
    access::ModuleAccess,
    errors::PartialVMError,
    file_format::{
        SignatureToken, StructDefinitionIndex, StructFieldInformation, StructHandleIndex,
    },
    views::FunctionHandleView,
    CompiledModule,
};
use move_bytecode_utils::module_cache::GetModule;
use move_core_types::{
    account_address::AccountAddress,
    identifier::{IdentStr, Identifier},
    language_storage::{ModuleId, StructTag, TypeTag},
    resolver::MoveResolver,
};
use std::rc::Rc;

pub(crate) struct Resolver<'a, T: ?Sized> {
    pub state: &'a T,
    cache: ModuleCache,
}

impl<'a, T: MoveResolver + ?Sized> GetModule for Resolver<'a, T> {
    type Error = Error;
    type Item = Rc<CompiledModule>;

    fn get_module_by_id(&self, module_id: &ModuleId) -> Result<Option<Self::Item>, Self::Error> {
        if let Some(module) = self.cache.get(module_id) {
            return Ok(Some(module));
        }
        let blob = self
            .state
            .get_module(module_id)
            .map_err(|e| anyhow!("Error retrieving module {:?}: {:?}", module_id, e))?
            .ok_or_else(|| anyhow!("Module {:?} can't be found", module_id))?;
        let compiled_module = CompiledModule::deserialize(&blob).map_err(|status| {
            anyhow!(
                "Module {:?} deserialize with error code {:?}",
                module_id,
                status
            )
        })?;
        Ok(Some(self.cache.insert(module_id.clone(), compiled_module)))
    }
}

impl<'a, T: MoveResolver + ?Sized> Resolver<'a, T> {
    pub fn new(state: &'a T) -> Self {
        Resolver {
            state,
            cache: ModuleCache::new(),
        }
    }

    fn get_module(&self, address: &AccountAddress, name: &IdentStr) -> Result<Rc<CompiledModule>> {
        let module_id = ModuleId::new(*address, name.to_owned());
        self.get_module_by_id_or_err(&module_id)
    }

    pub fn get_module_by_id_or_err(&self, module_id: &ModuleId) -> Result<Rc<CompiledModule>> {
        self.get_module_by_id(module_id)
            .map(|opt| opt.expect("My GetModule impl always returns Some."))
    }

    pub fn resolve_function_arguments(
        &self,
        module: &ModuleId,
        function: &IdentStr,
    ) -> Result<Vec<FatType>> {
        let m = self.get_module_by_id_or_err(module)?;
        for def in m.function_defs.iter() {
            let fhandle = m.function_handle_at(def.function);
            let fhandle_view = FunctionHandleView::new(m.as_ref(), fhandle);
            if fhandle_view.name() == function {
                return fhandle_view
                    .parameters()
                    .0
                    .iter()
                    .map(|signature| self.resolve_signature(m.clone(), signature))
                    .collect::<Result<_>>();
            }
        }
        Err(anyhow!("Function {:?} not found in {:?}", function, module))
    }

    pub fn resolve_type(&self, type_tag: &TypeTag) -> Result<FatType> {
        Ok(match type_tag {
            TypeTag::Address => FatType::Address,
            TypeTag::Signer => FatType::Signer,
            TypeTag::Bool => FatType::Bool,
            TypeTag::Struct(st) => FatType::Struct(Box::new(self.resolve_struct(st)?)),
            TypeTag::U8 => FatType::U8,
            TypeTag::U16 => FatType::U16,
            TypeTag::U32 => FatType::U32,
            TypeTag::U64 => FatType::U64,
            TypeTag::U256 => FatType::U256,
            TypeTag::U128 => FatType::U128,
            TypeTag::Vector(ty) => FatType::Vector(Box::new(self.resolve_type(ty)?)),
        })
    }

    pub fn resolve_struct(&self, struct_tag: &StructTag) -> Result<FatStructType> {
        let module = self.get_module(&struct_tag.address, &struct_tag.module)?;
        let struct_def = find_struct_def_in_module(module.clone(), struct_tag.name.as_ident_str())?;
        let ty_args = struct_tag
            .type_params
            .iter()
            .map(|ty| self.resolve_type(ty))
            .collect::<Result<Vec<_>>>()?;
        let ty_body = self.resolve_struct_definition(module, struct_def)?;
        ty_body.subst(&ty_args).map_err(|e: PartialVMError| {
            anyhow!("StructTag {:?} cannot be resolved: {:?}", struct_tag, e)
        })
    }

    pub fn get_field_names(&self, ty: &FatStructType) -> Result<Vec<Identifier>> {
        let module = self.get_module(&ty.address, ty.module.as_ident_str())?;
        let struct_def_idx = find_struct_def_in_module(module.clone(), ty.name.as_ident_str())?;
        let struct_def = module.struct_def_at(struct_def_idx);

        match &struct_def.field_information {
            StructFieldInformation::Native => Err(anyhow!("Unexpected Native Struct")),
            StructFieldInformation::Declared(defs) => Ok(defs
                .iter()
                .map(|field_def| module.identifier_at(field_def.name).to_owned())
                .collect()),
        }
    }

    fn resolve_signature(
        &self,
        module: Rc<CompiledModule>,
        sig: &SignatureToken,
    ) -> Result<FatType> {
        Ok(match sig {
            SignatureToken::Bool => FatType::Bool,
            SignatureToken::U8 => FatType::U8,
            SignatureToken::U16 => FatType::U16,
            SignatureToken::U32 => FatType::U32,
            SignatureToken::U64 => FatType::U64,
            SignatureToken::U128 => FatType::U128,
            SignatureToken::U256 => FatType::U256,
            SignatureToken::Address => FatType::Address,
            SignatureToken::Signer => FatType::Signer,
            SignatureToken::Vector(ty) => {
                FatType::Vector(Box::new(self.resolve_signature(module, ty)?))
            },
            SignatureToken::Struct(idx) => {
                FatType::Struct(Box::new(self.resolve_struct_handle(module, *idx)?))
            },
            SignatureToken::StructInstantiation(idx, toks) => {
                let struct_ty = self.resolve_struct_handle(module.clone(), *idx)?;
                let args = toks
                    .iter()
                    .map(|tok| self.resolve_signature(module.clone(), tok))
                    .collect::<Result<Vec<_>>>()?;
                FatType::Struct(Box::new(
                    struct_ty
                        .subst(&args)
                        .map_err(|status| anyhow!("Substitution failure: {:?}", status))?,
                ))
            },
            SignatureToken::TypeParameter(idx) => FatType::TyParam(*idx as usize),
            SignatureToken::MutableReference(_) => return Err(anyhow!("Unexpected Reference")),
            SignatureToken::Reference(inner) => match **inner {
                SignatureToken::Signer => FatType::Reference(Box::new(FatType::Signer)),
                _ => return Err(anyhow!("Unexpected Reference")),
            },
        })
    }

    fn resolve_struct_handle(
        &self,
        module: Rc<CompiledModule>,
        idx: StructHandleIndex,
    ) -> Result<FatStructType> {
        let struct_handle = module.struct_handle_at(idx);
        let target_module = {
            let module_handle = module.module_handle_at(struct_handle.module);
            self.get_module(
                module.address_identifier_at(module_handle.address),
                module.identifier_at(module_handle.name),
            )?
        };
        let target_idx = find_struct_def_in_module(
            target_module.clone(),
            module.identifier_at(struct_handle.name),
        )?;
        self.resolve_struct_definition(target_module, target_idx)
    }

    fn resolve_struct_definition(
        &self,
        module: Rc<CompiledModule>,
        idx: StructDefinitionIndex,
    ) -> Result<FatStructType> {
        let struct_def = module.struct_def_at(idx);
        let struct_handle = module.struct_handle_at(struct_def.struct_handle);
        let address = *module.address();
        let module_name = module.name().to_owned();
        let name = module.identifier_at(struct_handle.name).to_owned();
        let abilities = struct_handle.abilities;
        let ty_args = (0..struct_handle.type_parameters.len())
            .map(FatType::TyParam)
            .collect();
        match &struct_def.field_information {
            StructFieldInformation::Native => Err(anyhow!("Unexpected Native Struct")),
            StructFieldInformation::Declared(defs) => Ok(FatStructType {
                address,
                module: module_name,
                name,
                abilities: WrappedAbilitySet(abilities),
                ty_args,
                layout: defs
                    .iter()
                    .map(|field_def| self.resolve_signature(module.clone(), &field_def.signature.0))
                    .collect::<Result<_>>()?,
            }),
        }
    }
}

fn find_struct_def_in_module(
    module: Rc<CompiledModule>,
    name: &IdentStr,
) -> Result<StructDefinitionIndex> {
    for (i, defs) in module.struct_defs().iter().enumerate() {
        let st_handle = module.struct_handle_at(defs.struct_handle);
        if module.identifier_at(st_handle.name) == name {
            return Ok(StructDefinitionIndex::new(i as u16));
        }
    }
    Err(anyhow!(
        "Struct {:?} not found in {:?}",
        name,
        module.self_id()
    ))
}
