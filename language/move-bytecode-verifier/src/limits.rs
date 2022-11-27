// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::VerifierConfig;
use move_binary_format::{
    binary_views::BinaryIndexedView,
    errors::{Location, PartialVMError, PartialVMResult, VMResult},
    file_format::{CompiledModule, CompiledScript, SignatureToken},
    IndexKind,
};
use move_core_types::vm_status::StatusCode;

pub struct LimitsVerifier<'a> {
    resolver: BinaryIndexedView<'a>,
}

impl<'a> LimitsVerifier<'a> {
    pub fn verify_module(config: &VerifierConfig, module: &'a CompiledModule) -> VMResult<()> {
        Self::verify_module_impl(config, module)
            .map_err(|e| e.finish(Location::Module(module.self_id())))
    }

    fn verify_module_impl(
        config: &VerifierConfig,
        module: &'a CompiledModule,
    ) -> PartialVMResult<()> {
        let limit_check = Self {
            resolver: BinaryIndexedView::Module(module),
        };
        limit_check.verify_function_handles(config)?;
        limit_check.verify_struct_handles(config)?;
        limit_check.verify_type_nodes(config)
    }

    pub fn verify_script(config: &VerifierConfig, module: &'a CompiledScript) -> VMResult<()> {
        Self::verify_script_impl(config, module).map_err(|e| e.finish(Location::Script))
    }

    fn verify_script_impl(
        config: &VerifierConfig,
        script: &'a CompiledScript,
    ) -> PartialVMResult<()> {
        let limit_check = Self {
            resolver: BinaryIndexedView::Script(script),
        };
        limit_check.verify_function_handles(config)?;
        limit_check.verify_struct_handles(config)
    }

    fn verify_struct_handles(&self, config: &VerifierConfig) -> PartialVMResult<()> {
        if let Some(limit) = config.max_generic_instantiation_length {
            for (idx, struct_handle) in self.resolver.struct_handles().iter().enumerate() {
                if struct_handle.type_parameters.len() > limit {
                    return Err(PartialVMError::new(StatusCode::TOO_MANY_TYPE_PARAMETERS)
                        .at_index(IndexKind::StructHandle, idx as u16));
                }
            }
        }
        Ok(())
    }

    fn verify_function_handles(&self, config: &VerifierConfig) -> PartialVMResult<()> {
        for (idx, function_handle) in self.resolver.function_handles().iter().enumerate() {
            if let Some(limit) = config.max_generic_instantiation_length {
                if function_handle.type_parameters.len() > limit {
                    return Err(PartialVMError::new(StatusCode::TOO_MANY_TYPE_PARAMETERS)
                        .at_index(IndexKind::FunctionHandle, idx as u16));
                }
            };
            if let Some(limit) = config.max_function_parameters {
                if self
                    .resolver
                    .signature_at(function_handle.parameters)
                    .0
                    .len()
                    > limit
                {
                    return Err(PartialVMError::new(StatusCode::TOO_MANY_PARAMETERS)
                        .at_index(IndexKind::FunctionHandle, idx as u16));
                }
            };
        }
        Ok(())
    }

    fn verify_type_nodes(&self, config: &VerifierConfig) -> PartialVMResult<()> {
        for sign in self.resolver.signatures() {
            for ty in &sign.0 {
                self.verify_type_node(config, ty)?
            }
        }
        for cons in self.resolver.constant_pool() {
            self.verify_type_node(config, &cons.type_)?
        }
        Ok(())
    }

    fn verify_type_node(
        &self,
        config: &VerifierConfig,
        ty: &SignatureToken,
    ) -> PartialVMResult<()> {
        if let Some(max) = &config.max_type_nodes {
            // Structs and Parameters can expand to an unknown number of nodes, therefore
            // we give them a higher size weight here.
            const STRUCT_SIZE_WEIGHT: usize = 4;
            const PARAM_SIZE_WEIGHT: usize = 4;
            let mut size = 0;
            for t in ty.preorder_traversal() {
                // Notice that the preorder traversal will iterate all type instantiations, so we
                // why we can ignore them below.
                match t {
                    SignatureToken::Struct(..) | SignatureToken::StructInstantiation(..) => {
                        size += STRUCT_SIZE_WEIGHT
                    }
                    SignatureToken::TypeParameter(..) => size += PARAM_SIZE_WEIGHT,
                    _ => size += 1,
                }
            }
            if size > *max {
                return Err(PartialVMError::new(StatusCode::TOO_MANY_TYPE_NODES));
            }
        }
        Ok(())
    }
}
