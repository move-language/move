// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

pub mod compilation;
mod package_lock;
pub mod resolution;
pub mod source_package;

use anyhow::{bail, Result};
use clap::*;
use move_core_types::account_address::AccountAddress;
use move_model::model::GlobalEnv;
use serde::{Deserialize, Serialize};
use source_package::layout::SourcePackageLayout;
use std::{
    collections::BTreeMap,
    fmt,
    io::Write,
    path::{Path, PathBuf},
};

use crate::{
    compilation::{
        build_plan::BuildPlan, compiled_package::CompiledPackage, model_builder::ModelBuilder,
    },
    package_lock::PackageLock,
    resolution::resolution_graph::{ResolutionGraph, ResolvedGraph},
    source_package::{layout, manifest_parser},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Architecture {
    Move,

    AsyncMove,

    #[cfg(feature = "evm-backend")]
    Ethereum,
}

impl fmt::Display for Architecture {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Move => write!(f, "move"),

            Self::AsyncMove => write!(f, "async-move"),

            #[cfg(feature = "evm-backend")]
            Self::Ethereum => write!(f, "ethereum"),
        }
    }
}

impl Architecture {
    fn all() -> impl Iterator<Item = Self> {
        IntoIterator::into_iter([
            Self::Move,
            Self::AsyncMove,
            #[cfg(feature = "evm-backend")]
            Self::Ethereum,
        ])
    }

    fn try_parse_from_str(s: &str) -> Result<Self> {
        Ok(match s {
            "move" => Self::Move,

            "async-move" => Self::AsyncMove,

            #[cfg(feature = "evm-backend")]
            "ethereum" => Self::Ethereum,

            _ => {
                let supported_architectures = Self::all()
                    .map(|arch| format!("\"{}\"", arch))
                    .collect::<Vec<_>>();
                let be = if supported_architectures.len() == 1 {
                    "is"
                } else {
                    "are"
                };
                bail!(
                    "Unrecognized architecture {} -- only {} {} supported",
                    s,
                    supported_architectures.join(", "),
                    be
                )
            }
        })
    }
}

#[derive(Debug, Parser, Clone, Serialize, Deserialize, Eq, PartialEq, PartialOrd)]
#[clap(author, version, about)]
pub struct BuildConfig {
    /// Compile in 'dev' mode. The 'dev-addresses' and 'dev-dependencies' fields will be used if
    /// this flag is set. This flag is useful for development of packages that expose named
    /// addresses that are not set to a specific value.
    #[clap(name = "dev-mode", short = 'd', long = "dev", global = true)]
    pub dev_mode: bool,

    /// Compile in 'test' mode. The 'dev-addresses' and 'dev-dependencies' fields will be used
    /// along with any code in the 'test' directory.
    #[clap(name = "test-mode", long = "test", global = true)]
    pub test_mode: bool,

    /// Generate documentation for packages
    #[clap(name = "generate-docs", long = "doc", global = true)]
    pub generate_docs: bool,

    /// Generate ABIs for packages
    #[clap(name = "generate-abis", long = "abi", global = true)]
    pub generate_abis: bool,

    /// Installation directory for compiled artifacts. Defaults to current directory.
    #[clap(long = "install-dir", parse(from_os_str), global = true)]
    pub install_dir: Option<PathBuf>,

    /// Force recompilation of all packages
    #[clap(name = "force-recompilation", long = "force", global = true)]
    pub force_recompilation: bool,

    /// Additional named address mapping. Useful for tools in rust
    #[clap(skip)]
    pub additional_named_addresses: BTreeMap<String, AccountAddress>,

    #[clap(long = "arch", global = true, parse(try_from_str = Architecture::try_parse_from_str))]
    pub architecture: Option<Architecture>,
}

impl Default for BuildConfig {
    fn default() -> Self {
        Self {
            dev_mode: false,
            test_mode: false,
            generate_docs: false,
            generate_abis: false,
            install_dir: None,
            force_recompilation: false,
            additional_named_addresses: BTreeMap::new(),
            architecture: None,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd)]
pub struct ModelConfig {
    /// If set, also files which are in dependent packages are considered as targets.
    pub all_files_as_targets: bool,
    /// If set, a string how targets are filtered. A target is included if its file name
    /// contains this string. This is similar as the `cargo test <string>` idiom.
    pub target_filter: Option<String>,
}

impl BuildConfig {
    /// Compile the package at `path` or the containing Move package.
    pub fn compile_package<W: Write>(self, path: &Path, writer: &mut W) -> Result<CompiledPackage> {
        let resolved_graph = self.resolution_graph_for_package(path)?;
        let mutx = PackageLock::lock();
        let ret = BuildPlan::create(resolved_graph)?.compile(writer);
        mutx.unlock();
        ret
    }

    #[cfg(feature = "evm-backend")]
    pub fn compile_package_evm<W: Write>(self, path: &Path, writer: &mut W) -> Result<()> {
        let resolved_graph = self.resolution_graph_for_package(path)?;
        let mutx = PackageLock::lock();
        let ret = BuildPlan::create(resolved_graph)?.compile_evm(writer);
        mutx.unlock();
        ret
    }

    // NOTE: If there are no renamings, then the root package has the global resolution of all named
    // addresses in the package graph in scope. So we can simply grab all of the source files
    // across all packages and build the Move model from that.
    // TODO: In the future we will need a better way to do this to support renaming in packages
    // where we want to support building a Move model.
    pub fn move_model_for_package(
        self,
        path: &Path,
        model_config: ModelConfig,
    ) -> Result<GlobalEnv> {
        let resolved_graph = self.resolution_graph_for_package(path)?;
        let mutx = PackageLock::lock();
        let ret = ModelBuilder::create(resolved_graph, model_config).build_model();
        mutx.unlock();
        ret
    }

    pub fn resolution_graph_for_package(mut self, path: &Path) -> Result<ResolvedGraph> {
        if self.test_mode {
            self.dev_mode = true;
        }
        let path = SourcePackageLayout::try_find_root(path)?;
        let manifest_string =
            std::fs::read_to_string(path.join(layout::SourcePackageLayout::Manifest.path()))?;
        let toml_manifest = manifest_parser::parse_move_manifest_string(manifest_string)?;
        let mutx = PackageLock::lock();
        // This should be locked as it inspects the environment for `MOVE_HOME` which could
        // possibly be set by a different process in parallel.
        let manifest = manifest_parser::parse_source_manifest(toml_manifest)?;
        let resolution_graph = ResolutionGraph::new(manifest, path, self)?;
        let ret = resolution_graph.resolve();
        mutx.unlock();
        ret
    }
}
