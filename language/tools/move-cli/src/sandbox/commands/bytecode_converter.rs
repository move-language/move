// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::{bail, Result};
use move_binary_format::{file_format::CompiledScript, CompiledModule};
use std::{
    fmt::Display,
    path::{Path, PathBuf},
};

#[derive(Debug)]
pub enum BytecodeFormat {
    Json,
    Mv,
}

impl Display for BytecodeFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BytecodeFormat::Json => write!(f, "json"),
            BytecodeFormat::Mv => write!(f, "mv"),
        }
    }
}

impl BytecodeFormat {
    pub fn from_path(path: &Path) -> Result<Self> {
        match path.extension().and_then(|s| s.to_str()) {
            Some("json") => Ok(Self::Json),
            Some("mv") => Ok(Self::Mv),
            _ => bail!("Invalid file extension: {:?}", path.extension()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CompiledUnit {
    Module(CompiledModule),
    Script(CompiledScript),
}

impl CompiledUnit {
    pub fn from_json(json: &str) -> Result<Self> {
        match CompiledModule::from_json(json) {
            Ok(module) => Ok(Self::Module(module)),
            Err(_) => {
                let script: CompiledScript = CompiledScript::from_json(json)?;
                Ok(Self::Script(script))
            }
        }
    }

    pub fn deserialize(bytes: &[u8]) -> Result<Self> {
        match CompiledModule::deserialize(bytes) {
            Ok(module) => Ok(Self::Module(module)),
            Err(_) => {
                let script: CompiledScript = CompiledScript::deserialize(bytes)?;
                Ok(Self::Script(script))
            }
        }
    }

    pub fn to_json(&self) -> Result<String> {
        match self {
            Self::Module(module) => module.to_json(),
            Self::Script(script) => script.to_json(),
        }
    }

    pub fn serialize(&self, binary: &mut Vec<u8>) -> Result<()> {
        match self {
            Self::Module(module) => module.serialize(binary),
            Self::Script(script) => script.serialize(binary),
        }
    }
}

#[derive(Debug)]
pub struct InputSource {
    path: PathBuf,
    format: BytecodeFormat,
}

impl InputSource {
    pub fn new(path: PathBuf) -> Result<Self> {
        let format = BytecodeFormat::from_path(&path)?;
        Ok(Self { path, format })
    }

    pub fn convert(&self, output_dir: &Path, verify: bool) -> Result<PathBuf> {
        let (compiled_unit, output_format) = match self.format {
            BytecodeFormat::Json => {
                let json = std::fs::read_to_string(&self.path)?;
                let compiled_unit = CompiledUnit::from_json(&json)?;
                (compiled_unit, BytecodeFormat::Mv)
            }
            BytecodeFormat::Mv => {
                let bytes = std::fs::read(&self.path)?;
                let compiled_unit = CompiledUnit::deserialize(&bytes)?;
                (compiled_unit, BytecodeFormat::Json)
            }
        };
        if verify {
            match &compiled_unit {
                CompiledUnit::Module(module) => move_bytecode_verifier::verify_module(module)?,
                CompiledUnit::Script(script) => move_bytecode_verifier::verify_script(script)?,
            }
        }
        let output = match output_format {
            BytecodeFormat::Json => compiled_unit.to_json()?.as_bytes().to_vec(),
            BytecodeFormat::Mv => {
                let mut bytes = vec![];
                compiled_unit.serialize(&mut bytes)?;
                bytes
            }
        };
        let mut output_path = output_dir.join(self.path.file_stem().unwrap());
        output_path.set_extension(output_format.to_string());
        std::fs::write(&output_path, output)?;
        Ok(output_path)
    }
}

pub fn bytecode_converter<P>(input_path: P, output_dir: P, verify: bool) -> Result<()>
where
    P: AsRef<Path>,
{
    let input_path = input_path.as_ref();
    let output_dir = output_dir.as_ref();
    if input_path.is_dir() {
        bail!("The input file path is a directory, please input a file path");
    }
    let input_source = InputSource::new(input_path.to_path_buf())?;
    if !output_dir.is_dir() {
        bail!("The output file path is a file, please input a directory path");
    }
    let output_file = input_source.convert(output_dir, verify)?;
    println!("{:?}", output_file);
    Ok(())
}
