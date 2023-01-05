// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! This module contains the public APIs supported by the bytecode verifier.
use crate::{
    ability_field_requirements, check_duplication::DuplicationChecker,
    code_unit_verifier::CodeUnitVerifier, constants, friends,
    instantiation_loops::InstantiationLoopChecker, instruction_consistency::InstructionConsistency,
    limits::LimitsVerifier, script_signature,
    script_signature::no_additional_script_signature_checks, signature::SignatureChecker,
    struct_defs::RecursiveStructDefChecker,
};
use move_binary_format::{
    check_bounds::BoundsChecker,
    errors::{Location, VMResult},
    file_format::{CompiledModule, CompiledScript},
};

#[derive(Debug, Clone)]
pub struct VerifierConfig {
    pub max_loop_depth: Option<usize>,
    pub max_function_parameters: Option<usize>,
    pub max_generic_instantiation_length: Option<usize>,
    pub max_basic_blocks: Option<usize>,
    pub max_value_stack_size: usize,
    pub max_type_nodes: Option<usize>,
    pub max_push_size: Option<usize>,
    pub max_dependency_depth: Option<usize>,
    pub max_struct_definitions: Option<usize>,
    pub max_fields_in_struct: Option<usize>,
    pub max_function_definitions: Option<usize>,
    // Max number of nodes which are all allowed when instantiating a generic type.
    // This does not include field types of structs.
    pub max_type_instantiation_size: Option<usize>,
    // Max number of nodes which are all allowed when instantiating a generic function.
    pub max_function_instantiation_size: Option<usize>,
}

/// Helper for a "canonical" verification of a module.
///
/// Clients that rely on verification should call the proper passes
/// internally rather than using this function.
///
/// This function is intended to provide a verification path for clients
/// that do not require full control over verification. It is advised to
/// call this umbrella function instead of each individual checkers to
/// minimize the code locations that need to be updated should a new checker
/// is introduced.
pub fn verify_module(module: &CompiledModule) -> VMResult<()> {
    verify_module_with_config(&VerifierConfig::default(), module)
}

pub fn verify_module_with_config(config: &VerifierConfig, module: &CompiledModule) -> VMResult<()> {
    BoundsChecker::verify_module(module).map_err(|e| {
        // We can't point the error at the module, because if bounds-checking
        // failed, we cannot safely index into module's handle to itself.
        e.finish(Location::Undefined)
    })?;
    LimitsVerifier::verify_module(config, module)?;
    DuplicationChecker::verify_module(module)?;
    SignatureChecker::verify_module(module)?;
    InstructionConsistency::verify_module(module)?;
    constants::verify_module(module)?;
    friends::verify_module(module)?;
    ability_field_requirements::verify_module(module)?;
    RecursiveStructDefChecker::verify_module(module)?;
    InstantiationLoopChecker::verify_module(module)?;
    CodeUnitVerifier::verify_module(config, module)?;
    script_signature::verify_module(module, no_additional_script_signature_checks)
}

/// Helper for a "canonical" verification of a script.
///
/// Clients that rely on verification should call the proper passes
/// internally rather than using this function.
///
/// This function is intended to provide a verification path for clients
/// that do not require full control over verification. It is advised to
/// call this umbrella function instead of each individual checkers to
/// minimize the code locations that need to be updated should a new checker
/// is introduced.
pub fn verify_script(script: &CompiledScript) -> VMResult<()> {
    verify_script_with_config(&VerifierConfig::default(), script)
}

pub fn verify_script_with_config(config: &VerifierConfig, script: &CompiledScript) -> VMResult<()> {
    BoundsChecker::verify_script(script).map_err(|e| e.finish(Location::Script))?;
    LimitsVerifier::verify_script(config, script)?;
    DuplicationChecker::verify_script(script)?;
    SignatureChecker::verify_script(script)?;
    InstructionConsistency::verify_script(script)?;
    constants::verify_script(script)?;
    CodeUnitVerifier::verify_script(config, script)?;
    script_signature::verify_script(script, no_additional_script_signature_checks)
}

impl Default for VerifierConfig {
    fn default() -> Self {
        Self {
            max_loop_depth: None,
            max_function_parameters: None,
            max_generic_instantiation_length: None,
            max_basic_blocks: None,
            max_type_nodes: None,
            // Max size set to 1024 to match the size limit in the interpreter.
            max_value_stack_size: 1024,
            // Max number of pushes in one function
            max_push_size: None,
            // Max depth in dependency tree for both direct and friend dependencies
            max_dependency_depth: None,
            // Max count of structs in a module
            max_struct_definitions: None,
            // Max count of fields in a struct
            max_fields_in_struct: None,
            // Max count of functions in a module
            max_function_definitions: None,
            // Max size set to 10000 to restrict number of pushes in one function
            // max_push_size: Some(10000),
            // max_dependency_depth: Some(100),
            // max_struct_definitions: Some(200),
            // max_fields_in_struct: Some(30),
            // max_function_definitions: Some(1000),
            max_type_instantiation_size: None,
            max_function_instantiation_size: None,
        }
    }
}
