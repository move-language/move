// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use std::{
    collections::BTreeSet,
    fs::{self, File},
    io::Write,
    path::PathBuf,
};

use move_package::{
    resolution::{dependency_graph::DependencyGraph, lock_file::LockFile},
    source_package::manifest_parser::parse_move_manifest_from_file,
};
use move_symbol_pool::Symbol;

#[test]
fn no_dep_graph() {
    let pkg = no_dep_test_package();

    let manifest = parse_move_manifest_from_file(&pkg).expect("Loading manifest");
    let graph = DependencyGraph::new(
        &manifest,
        pkg,
        /* skip_fetch_latest_git_deps */ true,
        &mut std::io::sink(),
    )
    .expect("Creating DependencyGraph");

    assert!(
        graph.package_graph.contains_node(graph.root_package),
        "A graph for a package with no dependencies should still contain the root package",
    );

    assert_eq!(graph.topological_order(), vec![graph.root_package]);
}

#[test]
fn no_dep_graph_from_lock() {
    let pkg = no_dep_test_package();

    let snapshot = pkg.join("Move.locked");
    let graph = DependencyGraph::read_from_lock(
        pkg,
        Symbol::from("Root"),
        &mut File::open(&snapshot).expect("Opening snapshot"),
    )
    .expect("Reading DependencyGraph");

    assert!(
        graph.package_graph.contains_node(graph.root_package),
        "A graph for a package with no dependencies should still contain the root package",
    );

    assert_eq!(graph.topological_order(), vec![graph.root_package]);
}

#[test]
fn lock_file_roundtrip() {
    let tmp = tempfile::tempdir().unwrap();
    let pkg = one_dep_test_package();

    let snapshot = pkg.join("Move.locked");
    let commit = tmp.path().join("Move.lock");

    let graph = DependencyGraph::read_from_lock(
        pkg,
        Symbol::from("Root"),
        &mut File::open(&snapshot).expect("Opening snapshot"),
    )
    .expect("Reading DependencyGraph");

    let lock = graph.write_to_lock().expect("Writing DependencyGraph");

    lock.commit(&commit).expect("Committing lock file");

    let expect = fs::read_to_string(&snapshot).expect("Reading snapshot");
    let actual = fs::read_to_string(commit).expect("Reading committed lock");

    assert_eq!(
        expect, actual,
        "LockFile -> DependencyGraph -> LockFile roundtrip"
    );
}

#[test]
fn lock_file_missing_dependency() {
    let tmp = tempfile::tempdir().unwrap();
    let pkg = one_dep_test_package();

    let commit = tmp.path().join("Move.lock");
    let lock = LockFile::new(&pkg).expect("Creating new lock file");

    // Write a reference to a dependency that there isn't package information for.
    writeln!(&*lock, r#"dependencies = [{{ name = "OtherDep" }}]"#).unwrap();
    lock.commit(&commit).expect("Writing partial lock file");

    let Err(err) = DependencyGraph::read_from_lock(
        pkg,
        Symbol::from("Root"),
        &mut File::open(&commit).expect("Opening empty lock file"),
    ) else {
        panic!("Expected reading dependencies to fail.");
    };

    let message = err.to_string();
    assert!(
        message.contains("No source found for package OtherDep, depended on by: Root"),
        "{message}",
    );
}

#[test]
fn always_deps() {
    let pkg = dev_dep_test_package();

    let manifest = parse_move_manifest_from_file(&pkg).expect("Loading manifest");
    let graph = DependencyGraph::new(
        &manifest,
        pkg,
        /* skip_fetch_latest_git_deps */ true,
        &mut std::io::sink(),
    )
    .expect("Creating DependencyGraph");

    assert_eq!(
        graph.always_deps,
        BTreeSet::from([
            Symbol::from("Root"),
            Symbol::from("A"),
            Symbol::from("B"),
            Symbol::from("C"),
        ]),
    );
}

#[test]
fn always_deps_from_lock() {
    let pkg = dev_dep_test_package();
    let snapshot = pkg.join("Move.locked");

    let graph = DependencyGraph::read_from_lock(
        pkg,
        Symbol::from("Root"),
        &mut File::open(&snapshot).expect("Opening snapshot"),
    )
    .expect("Creating DependencyGraph");

    assert_eq!(
        graph.always_deps,
        BTreeSet::from([
            Symbol::from("Root"),
            Symbol::from("A"),
            Symbol::from("B"),
            Symbol::from("C"),
        ]),
    );
}

fn no_dep_test_package() -> PathBuf {
    [".", "tests", "test_sources", "basic_no_deps"]
        .into_iter()
        .collect()
}

fn one_dep_test_package() -> PathBuf {
    [".", "tests", "test_sources", "one_dep"]
        .into_iter()
        .collect()
}

fn dev_dep_test_package() -> PathBuf {
    [".", "tests", "test_sources", "dep_dev_dep_diamond"]
        .into_iter()
        .collect()
}
