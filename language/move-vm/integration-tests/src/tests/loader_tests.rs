// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::compiler::{compile_modules_in_file, expect_modules};
use move_binary_format::{
    file_format::{
        empty_module, AddressIdentifierIndex, IdentifierIndex, ModuleHandle, TableIndex,
    },
    CompiledModule,
};
use move_bytecode_verifier::VerifierConfig;
use move_compiler::Compiler;
use move_core_types::{
    account_address::AccountAddress,
    effects::ChangeSet,
    ident_str,
    identifier::{IdentStr, Identifier},
    language_storage::{ModuleId, TypeTag},
    resolver::{LinkageResolver, ModuleResolver, ResourceResolver},
    value::MoveValue,
};
use move_vm_runtime::{config::VMConfig, move_vm::MoveVM, session::SerializedReturnValues};
use move_vm_test_utils::InMemoryStorage;
use move_vm_types::{gas::UnmeteredGasMeter, loaded_data::runtime_types::Type};

use std::{collections::BTreeMap, path::PathBuf, sync::Arc, thread, str::FromStr};

const DEFAULT_ACCOUNT: AccountAddress = AccountAddress::TWO;
const UPGRADE_ACCOUNT: AccountAddress = {
    let mut address = [0u8; AccountAddress::LENGTH];
    address[AccountAddress::LENGTH - 1] = 3u8;
    AccountAddress::new(address)
};

struct Adapter {
    store: RelinkingStore,
    vm: Arc<MoveVM>,
    functions: Vec<(ModuleId, Identifier)>,
}

#[derive(Clone)]
struct RelinkingStore {
    store: InMemoryStorage,
    context: AccountAddress,
    linkage: BTreeMap<ModuleId, ModuleId>,
}

impl Adapter {
    fn new(store: InMemoryStorage) -> Self {
        let functions = vec![
            (
                ModuleId::new(DEFAULT_ACCOUNT, Identifier::new("A").unwrap()),
                Identifier::new("entry_a").unwrap(),
            ),
            (
                ModuleId::new(DEFAULT_ACCOUNT, Identifier::new("D").unwrap()),
                Identifier::new("entry_d").unwrap(),
            ),
            (
                ModuleId::new(DEFAULT_ACCOUNT, Identifier::new("E").unwrap()),
                Identifier::new("entry_e").unwrap(),
            ),
            (
                ModuleId::new(DEFAULT_ACCOUNT, Identifier::new("F").unwrap()),
                Identifier::new("entry_f").unwrap(),
            ),
            (
                ModuleId::new(DEFAULT_ACCOUNT, Identifier::new("C").unwrap()),
                Identifier::new("just_c").unwrap(),
            ),
        ];
        let config = VMConfig {
            verifier: VerifierConfig {
                max_dependency_depth: Some(100),
                ..Default::default()
            },
            ..Default::default()
        };
        Self {
            store: RelinkingStore::new(store),
            vm: Arc::new(MoveVM::new_with_config(vec![], config).unwrap()),
            functions,
        }
    }

    fn fresh(self) -> Self {
        let config = VMConfig {
            verifier: VerifierConfig {
                max_dependency_depth: Some(100),
                ..Default::default()
            },
            ..Default::default()
        };
        Self {
            store: self.store,
            vm: Arc::new(MoveVM::new_with_config(vec![], config).unwrap()),
            functions: self.functions,
        }
    }

    fn relink(self, context: AccountAddress, linkage: BTreeMap<ModuleId, ModuleId>) -> Self {
        let config = VMConfig {
            verifier: VerifierConfig {
                max_dependency_depth: Some(100),
                ..Default::default()
            },
            ..Default::default()
        };
        Self {
            store: self.store.relink(context, linkage),
            vm: Arc::new(MoveVM::new_with_config(vec![], config).unwrap()),
            functions: self.functions,
        }
    }

    fn publish_modules(&mut self, modules: Vec<CompiledModule>) {
        let mut session = self.vm.new_session(&self.store);

        for module in modules {
            let mut binary = vec![];
            module.serialize(&mut binary).unwrap_or_else(|e| {
                panic!("failure in module serialization: {e:?}\n{:#?}", module)
            });
            session
                .publish_module(binary, DEFAULT_ACCOUNT, &mut UnmeteredGasMeter)
                .unwrap_or_else(|e| panic!("failure publishing module: {e:?}\n{:#?}", module));
        }
        let (changeset, _) = session.finish().expect("failure getting write set");
        self.store
            .apply(changeset)
            .expect("failure applying write set");
    }

    fn publish_modules_with_error(&mut self, modules: Vec<CompiledModule>) {
        let mut session = self.vm.new_session(&self.store);

        for module in modules {
            let mut binary = vec![];
            module.serialize(&mut binary).unwrap_or_else(|e| {
                panic!("failure in module serialization: {e:?}\n{:#?}", module)
            });
            session
                .publish_module(binary, DEFAULT_ACCOUNT, &mut UnmeteredGasMeter)
                .expect_err("publishing must fail");
        }
    }

    fn publish_module_bundle(&mut self, modules: Vec<CompiledModule>) {
        let mut session = self.vm.new_session(&self.store);
        let binaries: Vec<_> = modules
            .into_iter()
            .map(|module| {
                let mut binary = vec![];
                module.serialize(&mut binary).unwrap_or_else(|e| {
                    panic!("failure in module serialization: {e:?}\n{:#?}", module)
                });
                binary
            })
            .collect();

        session
            .publish_module_bundle(binaries, DEFAULT_ACCOUNT, &mut UnmeteredGasMeter)
            .unwrap_or_else(|e| panic!("failure publishing module bundle: {e:?}"));

        let (changeset, _) = session.finish().expect("failure getting write set");
        self.store
            .apply(changeset)
            .expect("failure applying write set");
    }

    fn publish_module_bundle_with_error(&mut self, modules: Vec<CompiledModule>) {
        let mut session = self.vm.new_session(&self.store);
        let binaries: Vec<_> = modules
            .into_iter()
            .map(|module| {
                let mut binary = vec![];
                module.serialize(&mut binary).unwrap_or_else(|e| {
                    panic!("failure in module serialization: {e:?}\n{:#?}", module)
                });
                binary
            })
            .collect();

        session
            .publish_module_bundle(binaries, DEFAULT_ACCOUNT, &mut UnmeteredGasMeter)
            .expect_err("publishing bundle must fail");
    }

    fn load_type(&self, type_tag: &TypeTag) -> Type {
        let session = self.vm.new_session(&self.store);
        session.load_type(type_tag).expect("Loading type should succeed")
    }

    fn call_functions(&self) {
        for (module_id, name) in &self.functions {
            self.call_function(module_id, name);
        }
    }

    fn call_functions_async(&self, reps: usize) {
        let mut children = vec![];
        for _ in 0..reps {
            for (module_id, name) in self.functions.clone() {
                let vm = self.vm.clone();
                let data_store = self.store.clone();
                children.push(thread::spawn(move || {
                    let mut session = vm.new_session(&data_store);
                    session
                        .execute_function_bypass_visibility(
                            &module_id,
                            &name,
                            vec![],
                            Vec::<Vec<u8>>::new(),
                            &mut UnmeteredGasMeter,
                        )
                        .unwrap_or_else(|_| {
                            panic!("Failure executing {:?}::{:?}", module_id, name)
                        });
                }));
            }
        }
        for child in children {
            let _ = child.join();
        }
    }

    fn call_function_with_return(&self, module: &ModuleId, name: &IdentStr) -> Vec<MoveValue> {
        self.call_function(module, name)
            .return_values
            .into_iter()
            .map(|(bytes, ty)| {
                MoveValue::simple_deserialize(&bytes[..], &ty)
                    .expect("Can't deserialize return value")
            })
            .collect()
    }

    fn call_function_with_error(&self, module: &ModuleId, name: &IdentStr) {
        let mut session = self.vm.new_session(&self.store);
        session
            .execute_function_bypass_visibility(
                module,
                name,
                vec![],
                Vec::<Vec<u8>>::new(),
                &mut UnmeteredGasMeter,
            )
            .expect_err("calling function must fail");
    }

    fn call_function(&self, module: &ModuleId, name: &IdentStr) -> SerializedReturnValues {
        let mut session = self.vm.new_session(&self.store);
        session
            .execute_function_bypass_visibility(
                module,
                name,
                vec![],
                Vec::<Vec<u8>>::new(),
                &mut UnmeteredGasMeter,
            )
            .unwrap_or_else(|e| panic!("Failure executing {module:?}::{name:?}: {e:#?}"))
    }
}

impl RelinkingStore {
    fn new(store: InMemoryStorage) -> Self {
        Self {
            store,
            context: AccountAddress::ZERO,
            linkage: BTreeMap::new(),
        }
    }

    fn relink(self, context: AccountAddress, linkage: BTreeMap<ModuleId, ModuleId>) -> Self {
        let Self { store, .. } = self;
        Self {
            store,
            context,
            linkage,
        }
    }

    fn apply(&mut self, changeset: ChangeSet) -> anyhow::Result<()> {
        self.store.apply(changeset)
    }
}

/// Implemented by referencing the store's built-in data structures
impl LinkageResolver for RelinkingStore {
    type Error = ();

    fn link_context(&self) -> AccountAddress {
        self.context
    }

    /// Remaps `module_id` if it exists in the current linkage table, or returns it unchanged
    /// otherwise.
    fn relocate(&self, module_id: &ModuleId) -> Result<ModuleId, Self::Error> {
        Ok(self.linkage.get(module_id).unwrap_or(module_id).clone())
    }
}

/// Implement by forwarding to the underlying in memory storage
impl ModuleResolver for RelinkingStore {
    type Error = ();

    fn get_module(&self, id: &ModuleId) -> Result<Option<Vec<u8>>, Self::Error> {
        self.store.get_module(id)
    }
}

/// Implement by forwarding to the underlying in memory storage
impl ResourceResolver for RelinkingStore {
    type Error = ();

    fn get_resource(
        &self,
        address: &AccountAddress,
        typ: &move_core_types::language_storage::StructTag,
    ) -> Result<Option<Vec<u8>>, Self::Error> {
        self.store.get_resource(address, typ)
    }
}

fn get_fixture(fixture: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.extend(["src", "tests", fixture]);
    path
}

fn get_loader_tests_modules() -> Vec<CompiledModule> {
    compile_modules_in_file(&get_fixture("loader_tests_modules.move")).unwrap()
}

fn get_relinker_tests_modules_with_deps<'s>(
    module: &'s str,
    deps: impl IntoIterator<Item = &'s str>,
) -> anyhow::Result<Vec<CompiledModule>> {
    fn fixture_string_path(module: &str) -> String {
        get_fixture(&format!("relinking_tests_{module}.move"))
            .to_str()
            .unwrap()
            .to_string()
    }

    let (_, units) = Compiler::from_files(
        vec![fixture_string_path(module)],
        deps.into_iter().map(fixture_string_path).collect(),
        BTreeMap::<String, _>::new(),
    )
    .build_and_report()?;

    expect_modules(units).collect()
}

#[test]
fn load() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);
    let modules = get_loader_tests_modules();
    adapter.publish_modules(modules);
    // calls all functions sequentially
    adapter.call_functions();
}

#[test]
fn load_concurrent() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);
    let modules = get_loader_tests_modules();
    adapter.publish_modules(modules);
    // makes 15 threads
    adapter.call_functions_async(3);
}

#[test]
fn load_concurrent_many() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);
    let modules = get_loader_tests_modules();
    adapter.publish_modules(modules);
    // makes 150 threads
    adapter.call_functions_async(30);
}

#[test]
fn relink() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);

    let a0 = ModuleId::new(DEFAULT_ACCOUNT, ident_str!("a").to_owned());
    let b0 = ModuleId::new(DEFAULT_ACCOUNT, ident_str!("b").to_owned());
    let c0 = ModuleId::new(DEFAULT_ACCOUNT, ident_str!("c").to_owned());
    let c1 = ModuleId::new(UPGRADE_ACCOUNT, ident_str!("c").to_owned());

    let c0_modules = get_relinker_tests_modules_with_deps("c_v0", []).unwrap();
    let c1_modules = get_relinker_tests_modules_with_deps("c_v1", []).unwrap();
    let b0_modules = get_relinker_tests_modules_with_deps("b_v0", ["c_v0"]).unwrap();
    let a0_modules = get_relinker_tests_modules_with_deps("a_v0", ["b_v0", "c_v1"]).unwrap();

    // Publish the first version of C, and B which is published depending on it.
    adapter.publish_modules(c0_modules);
    adapter.publish_modules(b0_modules);

    assert_eq!(
        vec![MoveValue::U64(42 + 1)],
        adapter.call_function_with_return(&b0, ident_str!("b")),
    );

    let mut adapter = adapter.relink(UPGRADE_ACCOUNT, BTreeMap::from_iter([(c0, c1)]));

    // Publish the next version of C, and then A which depends on the new version of C, but also B.
    // B will be relinked to use C when executed in the adapter relinking against A.
    adapter.publish_modules(c1_modules);
    adapter.publish_modules(a0_modules);

    assert_eq!(
        vec![MoveValue::U64(44 + 43 + 1)],
        adapter.call_function_with_return(&a0, ident_str!("a")),
    );
}

#[test]
fn relink_publish_err() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);

    let c0_modules = get_relinker_tests_modules_with_deps("c_v0", []).unwrap();
    let b1_modules = get_relinker_tests_modules_with_deps("b_v1", ["c_v1"]).unwrap();

    // B was built against the later version of C but published against the earlier version,
    // which should fail because a function is missing.
    adapter.publish_modules(c0_modules);
    adapter.publish_modules_with_error(b1_modules);
}

#[test]
fn relink_load_err() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);

    let b0 = ModuleId::new(DEFAULT_ACCOUNT, ident_str!("b").to_owned());
    let b1 = ModuleId::new(UPGRADE_ACCOUNT, ident_str!("b").to_owned());
    let c0 = ModuleId::new(DEFAULT_ACCOUNT, ident_str!("c").to_owned());
    let c1 = ModuleId::new(UPGRADE_ACCOUNT, ident_str!("c").to_owned());

    let c0_modules = get_relinker_tests_modules_with_deps("c_v0", []).unwrap();
    let c1_modules = get_relinker_tests_modules_with_deps("c_v1", []).unwrap();
    let b0_modules = get_relinker_tests_modules_with_deps("b_v0", ["c_v0"]).unwrap();
    let b1_modules = get_relinker_tests_modules_with_deps("b_v1", ["c_v1"]).unwrap();

    // B v0 works with C v0
    adapter.publish_modules(c0_modules);
    adapter.publish_modules(b0_modules);

    assert_eq!(
        vec![MoveValue::U64(42 + 1)],
        adapter.call_function_with_return(&b0, ident_str!("b")),
    );

    let mut adapter = adapter.relink(
        UPGRADE_ACCOUNT,
        BTreeMap::from_iter([(b0.clone(), b1.clone()), (c0.clone(), c1)]),
    );

    // B v1 works with C v1
    adapter.publish_modules(c1_modules);
    adapter.publish_modules(b1_modules);

    assert_eq!(
        vec![MoveValue::U64(44 * 43)],
        adapter.call_function_with_return(&b0, ident_str!("b")),
    );

    let adapter = adapter.relink(
        UPGRADE_ACCOUNT,
        BTreeMap::from_iter([(b0.clone(), b1), (c0.clone(), c0)]),
    );

    // But B v1 *does not* work with C v0
    adapter.call_function_with_error(&b0, ident_str!("b0"));
}

#[test]
fn relink_type_identity() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);

    let b0 = ModuleId::new(DEFAULT_ACCOUNT, ident_str!("b").to_owned());
    let c0 = ModuleId::new(DEFAULT_ACCOUNT, ident_str!("c").to_owned());
    let b1 = ModuleId::new(UPGRADE_ACCOUNT, ident_str!("b").to_owned());
    let c1 = ModuleId::new(UPGRADE_ACCOUNT, ident_str!("c").to_owned());
    let c0_modules = get_relinker_tests_modules_with_deps("c_v0", []).unwrap();
    let c1_modules = get_relinker_tests_modules_with_deps("c_v1", []).unwrap();
    let b1_modules = get_relinker_tests_modules_with_deps("b_v1", ["c_v1"]).unwrap();

    adapter.publish_modules(c0_modules);
    let c0_s = adapter.load_type(&TypeTag::from_str("0x2::c::S").unwrap());

    let mut adapter = adapter.relink(UPGRADE_ACCOUNT, BTreeMap::from_iter([(b0, b1), (c0, c1)]));

    adapter.publish_modules(c1_modules);
    adapter.publish_modules(b1_modules);

    let c1_s = adapter.load_type(&TypeTag::from_str("0x2::c::S").unwrap());
    let b1_s = adapter.load_type(&TypeTag::from_str("0x2::b::S").unwrap());

    assert_eq!(c0_s, c1_s);
    assert_ne!(c1_s, b1_s);
}

#[test]
fn publish_bundle_and_load() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);

    let a0 = ModuleId::new(DEFAULT_ACCOUNT, ident_str!("a").to_owned());
    let c1_modules = get_relinker_tests_modules_with_deps("c_v1", []).unwrap();
    let b0_modules = get_relinker_tests_modules_with_deps("b_v0", ["c_v0"]).unwrap();
    let a0_modules = get_relinker_tests_modules_with_deps("a_v0", ["b_v0", "c_v1"]).unwrap();

    let mut modules = vec![];
    modules.extend(c1_modules);
    modules.extend(b0_modules);
    modules.extend(a0_modules);

    // Publish all the modules together
    adapter.publish_module_bundle(modules);

    assert_eq!(
        vec![MoveValue::U64(44 + 43 + 1)],
        adapter.call_function_with_return(&a0, ident_str!("a")),
    );
}

#[test]
fn publish_bundle_with_err_retry() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);

    let a0 = ModuleId::new(DEFAULT_ACCOUNT, ident_str!("a").to_owned());
    let c0_modules = get_relinker_tests_modules_with_deps("c_v0", []).unwrap();
    let c1_modules = get_relinker_tests_modules_with_deps("c_v1", []).unwrap();
    let b0_modules = get_relinker_tests_modules_with_deps("b_v0", ["c_v0"]).unwrap();
    let a0_modules = get_relinker_tests_modules_with_deps("a_v0", ["b_v0", "c_v1"]).unwrap();

    let mut modules = vec![];
    modules.extend(c0_modules);
    modules.extend(b0_modules.clone());
    modules.extend(a0_modules.clone());

    // Publishing the bundle should fail, because `a0` should not link with `c0`.
    adapter.publish_module_bundle_with_error(modules);

    let mut modules = vec![];
    modules.extend(c1_modules);
    modules.extend(b0_modules);
    modules.extend(a0_modules);

    // Try again and everything should publish successfully (in particular, the failed publish
    // will not leave behind modules in the loader).
    adapter.publish_module_bundle(modules);

    assert_eq!(
        vec![MoveValue::U64(44 + 43 + 1)],
        adapter.call_function_with_return(&a0, ident_str!("a")),
    );
}

#[test]
fn deep_dependency_list_err_0() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);

    let mut modules = vec![];

    // create a chain of dependencies
    let max = 350u64;
    dependency_chain(1, max, &mut modules);
    adapter.publish_modules(modules);

    let mut adapter = adapter.fresh();
    let name = format!("A{}", max);
    let dep_name = format!("A{}", max - 1);
    let deps = vec![dep_name];
    let module = empty_module_with_dependencies(name, deps);
    adapter.publish_modules_with_error(vec![module]);
}

#[test]
fn deep_dependency_list_err_1() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);

    let mut modules = vec![];

    // create a chain of dependencies
    let max = 101u64;
    dependency_chain(1, max, &mut modules);
    adapter.publish_modules(modules);

    let mut adapter = adapter.fresh();
    let name = format!("A{}", max);
    let dep_name = format!("A{}", max - 1);
    let deps = vec![dep_name];
    let module = empty_module_with_dependencies(name, deps);
    adapter.publish_modules_with_error(vec![module]);
}

#[test]
fn deep_dependency_list_ok_0() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);

    let mut modules = vec![];

    // create a chain of dependencies
    let max = 100u64;
    dependency_chain(1, max, &mut modules);
    adapter.publish_modules(modules);

    let mut adapter = adapter.fresh();
    let name = format!("A{}", max);
    let dep_name = format!("A{}", max - 1);
    let deps = vec![dep_name];
    let module = empty_module_with_dependencies(name, deps);
    adapter.publish_modules(vec![module]);
}

#[test]
fn deep_dependency_list_ok_1() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);

    let mut modules = vec![];

    // create a chain of dependencies
    let max = 30u64;
    dependency_chain(1, max, &mut modules);
    adapter.publish_modules(modules);

    let mut adapter = adapter.fresh();
    let name = format!("A{}", max);
    let dep_name = format!("A{}", max - 1);
    let deps = vec![dep_name];
    let module = empty_module_with_dependencies(name, deps);
    adapter.publish_modules(vec![module]);
}

#[test]
fn deep_dependency_tree_err_0() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);

    let mut modules = vec![];

    // create a tree of dependencies
    let width = 5u64;
    let height = 101u64;
    dependency_tree(width, height, &mut modules);
    adapter.publish_modules(modules);

    // use one of the module in the tree
    let mut adapter = adapter.fresh();
    let name = "ASome".to_string();
    let dep_name = format!("A_{}_{}", height - 1, width - 1);
    let deps = vec![dep_name];
    let module = empty_module_with_dependencies(name, deps);
    adapter.publish_modules_with_error(vec![module]);
}

#[test]
fn deep_dependency_tree_err_1() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);

    let mut modules = vec![];

    // create a tree of dependencies
    let width = 3u64;
    let height = 350u64;
    dependency_tree(width, height, &mut modules);
    adapter.publish_modules(modules);

    // use one of the module in the tree
    let mut adapter = adapter.fresh();
    let name = "ASome".to_string();
    let dep_name = format!("A_{}_{}", height - 1, width - 1);
    let deps = vec![dep_name];
    let module = empty_module_with_dependencies(name, deps);
    adapter.publish_modules_with_error(vec![module]);
}

#[test]
fn deep_dependency_tree_ok_0() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);

    let mut modules = vec![];

    // create a tree of dependencies
    let width = 10u64;
    let height = 20u64;
    dependency_tree(width, height, &mut modules);
    adapter.publish_modules(modules);

    // use one of the module in the tree
    let mut adapter = adapter.fresh();
    let name = "ASome".to_string();
    let dep_name = format!("A_{}_{}", height - 1, width - 1);
    let deps = vec![dep_name];
    let module = empty_module_with_dependencies(name, deps);
    adapter.publish_modules(vec![module]);
}

#[test]
fn deep_dependency_tree_ok_1() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);

    let mut modules = vec![];

    // create a tree of dependencies
    let width = 3u64;
    let height = 100u64;
    dependency_tree(width, height, &mut modules);
    adapter.publish_modules(modules);

    // use one of the module in the tree
    let mut adapter = adapter.fresh();
    let name = "ASome".to_string();
    let dep_name = format!("A_{}_{}", height - 1, width - 1);
    let deps = vec![dep_name];
    let module = empty_module_with_dependencies(name, deps);
    adapter.publish_modules(vec![module]);
}

#[test]
fn deep_friend_list_ok_0() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);

    let mut modules = vec![];

    // create a chain of dependencies
    let max = 100u64;
    friend_chain(1, max, &mut modules);
    adapter.publish_modules(modules);

    let mut adapter = adapter.fresh();
    let name = format!("A{}", max);
    let dep_name = format!("A{}", max - 1);
    let deps = vec![dep_name];
    let module = empty_module_with_friends(name, deps);
    adapter.publish_modules(vec![module]);
}

#[test]
fn deep_friend_list_ok_1() {
    let data_store = InMemoryStorage::new();
    let mut adapter = Adapter::new(data_store);

    let mut modules = vec![];

    // create a chain of dependencies
    let max = 30u64;
    friend_chain(1, max, &mut modules);
    adapter.publish_modules(modules);

    let mut adapter = adapter.fresh();
    let name = format!("A{}", max);
    let dep_name = format!("A{}", max - 1);
    let deps = vec![dep_name];
    let module = empty_module_with_friends(name, deps);
    adapter.publish_modules(vec![module]);
}

fn leaf_module(name: &str) -> CompiledModule {
    let mut module = empty_module();
    module.identifiers[0] = Identifier::new(name).unwrap();
    module.address_identifiers[0] = DEFAULT_ACCOUNT;
    module
}

// Create a list of dependent modules
fn dependency_chain(start: u64, end: u64, modules: &mut Vec<CompiledModule>) {
    let module = leaf_module("A0");
    modules.push(module);

    for i in start..end {
        let name = format!("A{}", i);
        let dep_name = format!("A{}", i - 1);
        let deps = vec![dep_name];
        let module = empty_module_with_dependencies(name, deps);
        modules.push(module);
    }
}

// Create a tree (well a forest or DAG really) of dependent modules
fn dependency_tree(width: u64, height: u64, modules: &mut Vec<CompiledModule>) {
    let mut deps = vec![];
    for i in 0..width {
        let name = format!("A_{}_{}", 0, i);
        let module = leaf_module(name.as_str());
        deps.push(name);
        modules.push(module);
    }
    for i in 1..height {
        let mut new_deps = vec![];
        for j in 0..width {
            let name = format!("A_{}_{}", i, j);
            let module = empty_module_with_dependencies(name.clone(), deps.clone());
            new_deps.push(name);
            modules.push(module);
        }
        deps = new_deps;
    }
}

// Create a module that uses (depends on) the list of given modules
fn empty_module_with_dependencies(name: String, deps: Vec<String>) -> CompiledModule {
    let mut module = empty_module();
    module.address_identifiers[0] = DEFAULT_ACCOUNT;
    module.identifiers[0] = Identifier::new(name).unwrap();
    for dep in deps {
        module.identifiers.push(Identifier::new(dep).unwrap());
        module.module_handles.push(ModuleHandle {
            address: AddressIdentifierIndex(0),
            name: IdentifierIndex((module.identifiers.len() - 1) as TableIndex),
        });
    }
    module
}

// Create a list of friends modules
fn friend_chain(start: u64, end: u64, modules: &mut Vec<CompiledModule>) {
    let module = leaf_module("A0");
    modules.push(module);

    for i in start..end {
        let name = format!("A{}", i);
        let dep_name = format!("A{}", i - 1);
        let deps = vec![dep_name];
        let module = empty_module_with_friends(name, deps);
        modules.push(module);
    }
}

// Create a module that uses (friends on) the list of given modules
fn empty_module_with_friends(name: String, deps: Vec<String>) -> CompiledModule {
    let mut module = empty_module();
    module.address_identifiers[0] = DEFAULT_ACCOUNT;
    module.identifiers[0] = Identifier::new(name).unwrap();
    for dep in deps {
        module.identifiers.push(Identifier::new(dep).unwrap());
        module.friend_decls.push(ModuleHandle {
            address: AddressIdentifierIndex(0),
            name: IdentifierIndex((module.identifiers.len() - 1) as TableIndex),
        });
    }
    module
}
