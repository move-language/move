// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! This module contains the public APIs supported by the bytecode verifier.
use crate::{
    ability_field_requirements, check_duplication::DuplicationChecker,
    code_unit_verifier::CodeUnitVerifier, constants, friends,
    instantiation_loops::InstantiationLoopChecker, instruction_consistency::InstructionConsistency,
    script_signature, script_signature::no_additional_script_signature_checks,
    signature::SignatureChecker, struct_defs::RecursiveStructDefChecker,
};
use move_binary_format::{
    check_bounds::BoundsChecker,
    errors::{Location, VMResult},
    file_format::{CompiledModule, CompiledScript},
};

#[derive(Debug, Clone, Default)]
pub struct VerifierConfig {
    pub max_loop_depth: Option<usize>,
    pub treat_friend_as_private: bool,
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
    DuplicationChecker::verify_script(script)?;
    SignatureChecker::verify_script(script)?;
    InstructionConsistency::verify_script(script)?;
    constants::verify_script(script)?;
    CodeUnitVerifier::verify_script(config, script)?;
    script_signature::verify_script(script, no_additional_script_signature_checks)
}
