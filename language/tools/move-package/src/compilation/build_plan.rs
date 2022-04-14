// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    compilation::compiled_package::CompiledPackage, resolution::resolution_graph::ResolvedGraph,
    source_package::parsed_manifest::PackageName,
};
use anyhow::Result;
use move_compiler::{compiled_unit::AnnotatedCompiledUnit, diagnostics::FilesSourceText, Compiler};
use petgraph::algo::toposort;
use std::{
    collections::{BTreeMap, BTreeSet},
    io::Write,
    path::Path,
};

use super::{compiled_package::CompilationCachingStatus, package_layout::CompiledPackageLayout};

#[cfg(feature = "evm-arch")]
use {
    colored::Colorize,
    move_to_yul::{options::Options as MoveToYulOptions, run_to_yul},
    termcolor::Buffer,
};

#[derive(Debug, Clone)]
pub struct BuildPlan {
    root: PackageName,
    sorted_deps: Vec<PackageName>,
    resolution_graph: ResolvedGraph,
}

impl BuildPlan {
    pub fn create(resolution_graph: ResolvedGraph) -> Result<Self> {
        let mut sorted_deps = match toposort(&resolution_graph.graph, None) {
            Ok(nodes) => nodes,
            Err(err) => {
                // Is a DAG after resolution otherwise an error should be raised from that.
                anyhow::bail!("IPE: Cyclic dependency found after resolution {:?}", err)
            }
        };

        sorted_deps.reverse();

        Ok(Self {
            root: resolution_graph.root_package.package.name,
            sorted_deps,
            resolution_graph,
        })
    }

    pub fn compile<W: Write>(
        &self,
        writer: &mut W,
    ) -> Result<(CompiledPackage, CompilationCachingStatus)> {
        self.compile_with_driver(writer, |compiler, _| compiler.build_and_report())
    }

    pub fn compile_with_driver<W: Write>(
        &self,
        writer: &mut W,
        mut compiler_driver: impl FnMut(
            Compiler,
            bool,
        )
            -> anyhow::Result<(FilesSourceText, Vec<AnnotatedCompiledUnit>)>,
    ) -> Result<(CompiledPackage, CompilationCachingStatus)> {
        let root_package = &self.resolution_graph.package_table[&self.root];
        let project_root = match &self.resolution_graph.build_options.install_dir {
            Some(under_path) => under_path.clone(),
            None => self.resolution_graph.root_package_path.clone(),
        };
        let mut compiled: BTreeMap<PackageName, (CompiledPackage, CompilationCachingStatus)> =
            BTreeMap::new();
        for package_ident in &self.sorted_deps {
            let resolved_package = self.resolution_graph.get_package(package_ident);
            let dependencies: Vec<_> = resolved_package
                .transitive_dependencies(&self.resolution_graph)
                .into_iter()
                .map(|package_name| compiled.get(&package_name).unwrap().clone())
                .collect();
            let compiled_package = CompiledPackage::build(
                writer,
                &project_root,
                resolved_package.clone(),
                dependencies,
                &self.resolution_graph,
                package_ident == &root_package.source_package.package.name,
                &mut compiler_driver,
            )?;
            compiled.insert(*package_ident, compiled_package);
        }
        let compiled_names = compiled.keys().collect::<BTreeSet<_>>();
        Self::clean(
            &project_root.join(CompiledPackageLayout::Root.path()),
            compiled_names,
        )?;
        Ok(compiled
            .remove(&root_package.source_package.package.name)
            .unwrap())
    }

    #[cfg(feature = "evm-arch")]
    pub fn compile_evm<W: Write>(&self, writer: &mut W) -> Result<()> {
        let root_package = &self.resolution_graph.package_table[&self.root];
        let project_root = match &self.resolution_graph.build_options.install_dir {
            Some(under_path) => under_path.clone(),
            None => self.resolution_graph.root_package_path.clone(),
        };
        let build_root_path = project_root
            .join(CompiledPackageLayout::Root.path())
            .join("evm");

        // Step 1: Compile Move into Yul
        //   Step 1a: Gather command line arguments for move-to-yul
        let package_names = self
            .resolution_graph
            .package_table
            .iter()
            .map(|(name, _)| name.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        let dependencies = self
            .resolution_graph
            .package_table
            .iter()
            .filter_map(|(name, package)| {
                if name == &root_package.source_package.package.name {
                    None
                } else {
                    Some(format!(
                        "{}/sources",
                        package.package_path.to_string_lossy()
                    ))
                }
            })
            .collect();

        let sources = vec![format!(
            "{}/sources",
            root_package.package_path.to_string_lossy()
        )];

        let named_address_mapping = self
            .resolution_graph
            .extract_named_address_mapping()
            .map(|(name, addr)| format!("{}={}", name.as_str(), addr))
            .collect();

        let yul_output = format!(
            "{}/{}.yul",
            build_root_path.to_string_lossy(),
            root_package.source_package.package.name
        );
        let abi_output = format!(
            "{}/{}.abi.json",
            build_root_path.to_string_lossy(),
            root_package.source_package.package.name
        );

        //   Step 1b: Call move-to-yul
        writeln!(
            writer,
            "{} {} to Yul",
            "COMPILING".bold().green(),
            package_names
        )?;

        if let Err(err) = std::fs::create_dir_all(&build_root_path) {
            writeln!(
                writer,
                "{} Failed to create build dir {}",
                "ERROR".bold().red(),
                build_root_path.to_string_lossy(),
            )?;

            return Err(err.into());
        }

        // TODO: should inherit color settings from current shell
        let mut error_buffer = Buffer::ansi();
        if let Err(err) = run_to_yul(
            &mut error_buffer,
            MoveToYulOptions {
                dependencies,
                named_address_mapping,
                sources,
                output: yul_output.clone(),
                abi_output,

                ..MoveToYulOptions::default()
            },
        ) {
            writeln!(
                writer,
                "{} Failed to compile Move into Yul",
                "ERROR".bold().red()
            )?;

            writeln!(
                writer,
                "{}",
                std::str::from_utf8(error_buffer.as_slice()).unwrap()
            )?;

            let mut source = err.source();
            while let Some(s) = source {
                writeln!(writer, "{}", s)?;
                source = s.source();
            }

            return Err(err);
        }

        // Step 2: Compile Yul into bytecode using solc
        let yul_source = match std::fs::read_to_string(&yul_output) {
            Ok(yul_source) => yul_source,
            Err(err) => {
                writeln!(
                    writer,
                    "{} Failed to read from {}",
                    "ERROR".bold().red(),
                    yul_output,
                )?;

                return Err(err.into());
            }
        };

        let bytecode_output = format!(
            "{}/{}.bin",
            build_root_path.to_string_lossy(),
            root_package.source_package.package.name
        );

        writeln!(
            writer,
            "{} EVM bytecote from Yul",
            "GENERATING".bold().green(),
        )?;

        match evm_exec_utils::compile::solc_yul(&yul_source, false) {
            Ok((bytecode, _)) => {
                let mut bytecode_file = match std::fs::File::create(&bytecode_output) {
                    Ok(file) => file,
                    Err(err) => {
                        writeln!(
                            writer,
                            "{} Failed to create bytecode output {}",
                            "ERROR".bold().red(),
                            bytecode_output,
                        )?;

                        return Err(err.into());
                    }
                };

                if let Err(err) = bytecode_file.write_all(hex::encode(&bytecode).as_bytes()) {
                    writeln!(
                        writer,
                        "{} Failed to write bytecode to file {}",
                        "ERROR".bold().red(),
                        bytecode_output,
                    )?;

                    return Err(err.into());
                }
            }
            Err(err) => {
                writeln!(
                    writer,
                    "{} Failed to generate EVM bytecote",
                    "ERROR".bold().red()
                )?;

                let mut source = err.source();
                while let Some(s) = source {
                    writeln!(writer, "{}", s)?;
                    source = s.source();
                }

                return Err(err);
            }
        }

        Ok(())
    }

    // Clean out old packages that are no longer used, or no longer used under the current
    // compilation flags
    fn clean(build_root: &Path, keep_paths: BTreeSet<&PackageName>) -> Result<()> {
        for dir in std::fs::read_dir(build_root)? {
            let path = dir?.path();
            if !keep_paths.iter().any(|name| path.ends_with(name.as_str())) {
                std::fs::remove_dir_all(&path)?;
            }
        }
        Ok(())
    }
}
