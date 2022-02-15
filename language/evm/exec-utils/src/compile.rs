// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::{bail, format_err, Result};
use std::{
    collections::BTreeMap,
    ffi::OsStr,
    fs::{self},
    path::{Path, PathBuf},
    process::Command,
};

fn solc_path() -> Result<PathBuf> {
    let solc_exe = move_command_line_common::env::read_env_var("SOLC_EXE");

    if solc_exe.is_empty() {
        bail!("failed to find path to solc -- is the environment variable SOLC_EXE set?")
    }

    Ok(PathBuf::from(&solc_exe))
}

fn solc_impl(
    source_paths: impl IntoIterator<Item = impl AsRef<OsStr>>,
    output_dir: &Path,
) -> Result<BTreeMap<String, Vec<u8>>> {
    Command::new(&solc_path()?)
        .args(source_paths)
        .arg("--bin")
        .arg("-o")
        .arg(output_dir)
        .output()
        .map_err(|err| format_err!("failed to call solc: {:?}", err))?;

    let mut compiled_contracts = BTreeMap::new();

    for entry in fs::read_dir(output_dir)? {
        let entry = entry?;

        if entry.file_type()?.is_file() {
            let path = entry.path();
            if let Some(ext) = path.extension() {
                if ext == "bin" {
                    let data = fs::read(&path)?;
                    let data = hex::decode(&data)?;

                    compiled_contracts.insert(
                        path.file_stem()
                            .ok_or_else(|| format_err!("failed to extract file name"))?
                            .to_string_lossy()
                            .to_string(),
                        data,
                    );
                }
            }
        }
    }

    Ok(compiled_contracts)
}

/// Compile the solodity sources using solc.
/// Return a mapping with keys being contract names and values being compiled bytecode.
///
/// `~/bin/solc` must exist in order for this to work.
pub fn solc(
    source_paths: impl IntoIterator<Item = impl AsRef<OsStr>>,
) -> Result<BTreeMap<String, Vec<u8>>> {
    let temp = tempfile::tempdir()?;

    solc_impl(source_paths, temp.path())
}
