// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! This module implements the checker for verifying correctness of function bodies.
//! The overall verification is split between stack_usage_verifier.rs and
//! abstract_interpreter.rs. CodeUnitVerifier simply orchestrates calls into these two files.
use crate::{
    acquires_list_verifier::AcquiresVerifier, control_flow, locals_safety, reference_safety,
    stack_usage_verifier::StackUsageVerifier, type_safety, verifier::VerifierConfig,
};
use move_binary_format::{
    access::ModuleAccess,
    binary_views::{BinaryIndexedView, FunctionView},
    control_flow_graph::ControlFlowGraph,
    errors::{Location, PartialVMError, PartialVMResult, VMResult},
    file_format::{
        CompiledModule, CompiledScript, FunctionDefinition, FunctionDefinitionIndex,
        IdentifierIndex, TableIndex,
    },
    IndexKind,
};
use move_core_types::vm_status::StatusCode;
use std::collections::HashMap;

pub struct CodeUnitVerifier<'a> {
    resolver: BinaryIndexedView<'a>,
    function_view: FunctionView<'a>,
    name_def_map: &'a HashMap<IdentifierIndex, FunctionDefinitionIndex>,
}

impl<'a> CodeUnitVerifier<'a> {
    pub fn verify_module(
        verifier_config: &VerifierConfig,
        module: &'a CompiledModule,
    ) -> VMResult<()> {
        Self::verify_module_impl(verifier_config, module)
            .map_err(|e| e.finish(Location::Module(module.self_id())))
    }

    fn verify_module_impl(
        verifier_config: &VerifierConfig,
        module: &CompiledModule,
    ) -> PartialVMResult<()> {
        let mut name_def_map = HashMap::new();
        for (idx, func_def) in module.function_defs().iter().enumerate() {
            let fh = module.function_handle_at(func_def.function);
            name_def_map.insert(fh.name, FunctionDefinitionIndex(idx as u16));
        }
        for (idx, function_definition) in module.function_defs().iter().enumerate() {
            let index = FunctionDefinitionIndex(idx as TableIndex);
            Self::verify_function(
                verifier_config,
                index,
                function_definition,
                module,
                &name_def_map,
            )
            .map_err(|err| err.at_index(IndexKind::FunctionDefinition, index.0))?
        }
        Ok(())
    }

    pub fn verify_script(
        verifier_config: &VerifierConfig,
        module: &'a CompiledScript,
    ) -> VMResult<()> {
        Self::verify_script_impl(verifier_config, module).map_err(|e| e.finish(Location::Script))
    }

    fn verify_script_impl(
        verifier_config: &VerifierConfig,
        script: &'a CompiledScript,
    ) -> PartialVMResult<()> {
        // create `FunctionView` and `BinaryIndexedView`
        control_flow::verify(verifier_config, None, &script.code)?;
        let function_view = FunctionView::script(script);
        let resolver = BinaryIndexedView::Script(script);
        let name_def_map = HashMap::new();
        //verify
        let code_unit_verifier = CodeUnitVerifier {
            resolver,
            function_view,
            name_def_map: &name_def_map,
        };
        code_unit_verifier.verify_common(verifier_config)
    }

    fn verify_function(
        verifier_config: &VerifierConfig,
        index: FunctionDefinitionIndex,
        function_definition: &FunctionDefinition,
        module: &CompiledModule,
        name_def_map: &HashMap<IdentifierIndex, FunctionDefinitionIndex>,
    ) -> PartialVMResult<()> {
        // nothing to verify for native function
        let code = match &function_definition.code {
            Some(code) => code,
            None => return Ok(()),
        };
        // create `FunctionView` and `BinaryIndexedView`
        let function_handle = module.function_handle_at(function_definition.function);
        control_flow::verify(verifier_config, Some(index), code)?;
        let function_view = FunctionView::function(module, index, code, function_handle);

        if let Some(limit) = verifier_config.max_basic_blocks {
            if function_view.cfg().blocks().len() > limit {
                return Err(
                    PartialVMError::new(StatusCode::TOO_MANY_BASIC_BLOCKS).at_code_offset(index, 0)
                );
            }
        }

        let resolver = BinaryIndexedView::Module(module);
        // verify
        let code_unit_verifier = CodeUnitVerifier {
            resolver,
            function_view,
            name_def_map,
        };
        code_unit_verifier.verify_common(verifier_config)?;
        AcquiresVerifier::verify(module, index, function_definition)
    }

    fn verify_common(&self, verifier_config: &VerifierConfig) -> PartialVMResult<()> {
        StackUsageVerifier::verify(verifier_config, &self.resolver, &self.function_view)?;
        type_safety::verify(&self.resolver, &self.function_view)?;
        locals_safety::verify(&self.resolver, &self.function_view)?;
        reference_safety::verify(&self.resolver, &self.function_view, self.name_def_map)
    }
}
