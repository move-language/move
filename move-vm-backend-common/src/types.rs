use alloc::vec::Vec;
use anyhow::{Error, Result};
use serde::{Deserialize, Serialize};

/// Package contains a list of module bytecodes.
#[derive(Serialize, Deserialize)]
pub struct ModuleBundle {
    /// Module bytecodes.
    modules: Vec<Vec<u8>>,
}

impl ModuleBundle {
    /// Create a new ModuleBundle.
    pub fn new(modules: Vec<Vec<u8>>) -> Self {
        Self {
            modules
        }
    }

    /// Gets module bytecodes.
    pub fn into_inner(self) -> Vec<Vec<u8>> {
        self.modules
    }

    /// Serializes data.
    pub fn encode(self) -> Result<Vec<u8>> {
        bcs::to_bytes(&self).map_err(Error::msg)
    }
}

impl TryFrom<&[u8]> for ModuleBundle {
    type Error = Error;

    fn try_from(blob: &[u8]) -> Result<Self, Self::Error> {
        bcs::from_bytes(blob).map_err(Error::msg)
    }
}
