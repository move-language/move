// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use move_binary_format::file_format_common::VERSION_MAX;
use move_bytecode_verifier::{verifier::MAX_CONSTANT_VECTOR_LEN, VerifierConfig};

/// Dynamic config options for the Move VM.
pub struct VMConfig {
    pub verifier: VerifierConfig,
    pub max_binary_format_version: u32,
    // When this flag is set to true, MoveVM will perform type check at every instruction
    // execution to ensure that type safety cannot be violated at runtime.
    pub paranoid_type_checks: bool,
    pub runtime_limits_config: VMRuntimeLimitsConfig,
}

impl Default for VMConfig {
    fn default() -> Self {
        Self {
            verifier: VerifierConfig::default(),
            max_binary_format_version: VERSION_MAX,
            paranoid_type_checks: false,
            runtime_limits_config: VMRuntimeLimitsConfig::default(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct VMRuntimeLimitsConfig {
    /// Maximum number of items that can be pushed into a vec
    pub vector_len_max: u64,
}
impl Default for VMRuntimeLimitsConfig {
    fn default() -> Self {
        Self {
            vector_len_max: MAX_CONSTANT_VECTOR_LEN,
        }
    }
}
