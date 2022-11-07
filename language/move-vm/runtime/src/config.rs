use move_bytecode_verifier::VerifierConfig;

/// Dynamic config options for the Move VM.
pub struct VMConfig {
    pub verifier: VerifierConfig,
}

impl Default for VMConfig {
    fn default() -> Self {
        Self {
            verifier: VerifierConfig::default(),
        }
    }
}
