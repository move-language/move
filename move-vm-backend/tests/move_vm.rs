use crate::mock::StorageMock;
use move_vm_backend::Mvm;

use move_core_types::account_address::AccountAddress;
use move_core_types::identifier::Identifier;
use move_core_types::language_storage::StructTag;
use move_vm_backend_common::types::ModuleBundle;

use move_core_types::language_storage::TypeTag;

use move_vm_test_utils::gas_schedule::GasStatus;

pub mod mock;

/// Reads bytes from a file for the given path.
/// Can panic if the file doesn't exist.
fn read_bytes(file_path: &str) -> Vec<u8> {
    std::fs::read(file_path).unwrap_or_else(|e| panic!("Can't read {file_path}: {e}"))
}

/// Reads a precompiled Move module from our assets directory.
fn read_module_bytes_from_project(project: &str, module_name: &str) -> Vec<u8> {
    const MOVE_PROJECTS: &str = "tests/assets/move-projects";

    let path =
        format!("{MOVE_PROJECTS}/{project}/build/{project}/bytecode_modules/{module_name}.mv");

    read_bytes(&path)
}

/// Reads a precompiled Move bundle from our assets directory.
fn read_bundle_from_project(project: &str, bundle_name: &str) -> Vec<u8> {
    const MOVE_PROJECTS: &str = "tests/assets/move-projects";

    let path = format!("{MOVE_PROJECTS}/{project}/build/{project}/bundles/{bundle_name}.mvb");

    read_bytes(&path)
}

/// Reads a precompiled Move stdlib module from our assets directory for specified project.
/// This will be replaced in the future with a proper stdlib genesis initialization.
fn read_stdlib_module_bytes_from_project(project: &str, stdlib_module_name: &str) -> Vec<u8> {
    const MOVE_PROJECTS: &str = "tests/assets/move-projects";

    let path =
        format!("{MOVE_PROJECTS}/{project}/build/{project}/bytecode_modules/dependencies/MoveStdlib/{stdlib_module_name}.mv");

    read_bytes(&path)
}

/// Reads a precompiled Move scripts from our assets directory.
fn read_script_bytes_from_project(project: &str, script_name: &str) -> Vec<u8> {
    const MOVE_PROJECTS: &str = "tests/assets/move-projects";

    let path =
        format!("{MOVE_PROJECTS}/{project}/build/{project}/bytecode_scripts/{script_name}.mv");

    read_bytes(&path)
}

#[test]
#[ignore = "we need to build the move package before with a script before running the test"]
// This test heavily depends on Move.toml files for thes used Move packages.
fn publish_module_test() {
    let store = StorageMock::new();
    let vm = Mvm::new(store).unwrap();

    let address = AccountAddress::from_hex_literal("0xCAFE").unwrap();
    let module = read_module_bytes_from_project("empty", "Empty");

    let mut gas_status = GasStatus::new_unmetered();

    let result = vm.publish_module(&module, address, &mut gas_status);

    assert!(result.is_ok(), "failed to publish the module");
}

#[test]
#[ignore = "we need to build the move package before with a script before running the test"]
// This test heavily depends on Move.toml files for thes used Move packages.
fn publish_module_package_from_multiple_module_files() {
    let store = StorageMock::new();
    let vm = Mvm::new(store).unwrap();
    let mut gas_status = GasStatus::new_unmetered();

    let module_1 = read_module_bytes_from_project("using_stdlib_natives", "Vector");
    let module_2 = read_module_bytes_from_project("using_stdlib_natives", "DependsOnVector");
    let addr = AccountAddress::from_hex_literal("0x2").unwrap();

    // Order matters - module_2 depends on module_1!
    let modules = ModuleBundle::new(vec![module_1.clone(), module_2.clone()])
        .encode()
        .unwrap();
    let result = vm.publish_module_package(&modules, addr, &mut gas_status);
    assert!(result.is_ok(), "failed to publish the package");

    // Recreate the storage and the MoveVM
    let store = StorageMock::new();
    let vm = Mvm::new(store).unwrap();
    // Order matters - we cannot publish module_2 before module_1!
    let modules = ModuleBundle::new(vec![module_2, module_1])
        .encode()
        .unwrap();
    let result = vm.publish_module_package(&modules, addr, &mut gas_status);
    assert!(
        result.is_err(),
        "publishing a package with a wrong order succeeded"
    );
}

#[test]
#[ignore = "we need to build the move package before with a script before running the test"]
// This test heavily depends on Move.toml files for thes used Move packages.
fn publish_module_package_from_bundle_file() {
    let store = StorageMock::new();
    let vm = Mvm::new(store).unwrap();
    let mut gas_status = GasStatus::new_unmetered();

    let bundle = read_bundle_from_project("using_stdlib_natives", "using_stdlib_natives");
    let addr = AccountAddress::from_hex_literal("0x2").unwrap();

    let result = vm.publish_module_package(&bundle, addr, &mut gas_status);
    assert!(result.is_ok(), "failed to publish the package");
}

#[allow(non_snake_case)]
#[test]
#[ignore = "we need to build the move package before with a script before running the test"]
// This test heavily depends on Move.toml files for thes used Move packages.
fn publish_module_dependent_on_stdlib_natives() {
    let store = StorageMock::new();
    let vm = Mvm::new(store).unwrap();
    let mut gas_status = GasStatus::new_unmetered();

    let mod_using_stdlib_natives = read_module_bytes_from_project("using_stdlib_natives", "Vector");
    let addr_StdNativesUser = AccountAddress::from_hex_literal("0x2").unwrap();

    // Natives are part of the MoveVM so no need to publish compiled stdlib bytecode modules.
    let result = vm.publish_module(
        &mod_using_stdlib_natives,
        addr_StdNativesUser,
        &mut gas_status,
    );
    assert!(result.is_ok(), "the first module cannot be published");

    let mod_depends_on_using_stdlib_natives =
        read_module_bytes_from_project("depends_on__using_stdlib_natives", "VectorUser");
    let addr_TestingNatives = AccountAddress::from_hex_literal("0x4").unwrap();

    let result = vm.publish_module(
        &mod_depends_on_using_stdlib_natives,
        addr_TestingNatives,
        &mut gas_status,
    );
    assert!(result.is_ok(), "the second module cannot be published");
}

#[allow(non_snake_case)]
#[test]
#[ignore = "we need to build the move package before with a script before running the test"]
// This test heavily depends on Move.toml files for thes used Move packages.
fn publish_module_using_stdlib_full_fails() {
    let store = StorageMock::new();
    let vm = Mvm::new(store).unwrap();
    let mut gas_status = GasStatus::new_unmetered();

    let mod_using_stdlib_natives =
        read_module_bytes_from_project("using_stdlib_full", "StringAndVector");
    let addr_StdNativesUser = AccountAddress::from_hex_literal("0x3").unwrap();

    // In order to publish a module which is using the full stdlib package, we first must publish
    // the stdlib package itself to the MoveVM.
    let result = vm.publish_module(
        &mod_using_stdlib_natives,
        addr_StdNativesUser,
        &mut gas_status,
    );
    assert!(result.is_err(), "the module shouldn't be published");
}

#[test]
#[ignore = "we need to build the move package before with a script before running the test"]
// This test heavily depends on Move.toml files for thes used Move packages.
fn get_module_and_module_abi() {
    let store = StorageMock::new();
    let vm = Mvm::new(store).unwrap();

    let module = read_module_bytes_from_project("using_stdlib_natives", "Vector");
    let address = AccountAddress::from_hex_literal("0x2").unwrap();

    let mut gas_status = GasStatus::new_unmetered();
    let result = vm.publish_module(&module, address, &mut gas_status);
    assert!(result.is_ok(), "failed to publish the module");

    let result = vm.get_module(address, "Vector");
    assert_eq!(
        result.expect("failed to get the module"),
        Some(module),
        "invalid module received"
    );

    let result = vm.get_module_abi(address, "Vector");
    assert!(result.unwrap().is_some(), "failed to get the module abi");
}

#[test]
#[ignore = "we need to build the move package before with a script before running the test"]
// This test heavily depends on Move.toml files for thes used Move packages.
fn get_resource() {
    let store = StorageMock::new();
    let vm = Mvm::new(store).unwrap();
    let mut gas_status = GasStatus::new_unmetered();

    let addr_std = AccountAddress::from_hex_literal("0x1").unwrap();
    let module = read_stdlib_module_bytes_from_project("basic_coin", "signer");
    let result = vm.publish_module(&module, addr_std, &mut gas_status);

    assert!(result.is_ok(), "Failed to publish the stdlib module");

    let address = AccountAddress::from_hex_literal("0xCAFE").unwrap();
    let module = read_module_bytes_from_project("basic_coin", "BasicCoin");
    let result = vm.publish_module(&module, address, &mut gas_status);

    assert!(result.is_ok(), "Failed to publish the module");

    let script = read_script_bytes_from_project("basic_coin", "main");
    let addr_param = bcs::to_bytes(&address).unwrap();
    let type_args: Vec<TypeTag> = vec![];
    let params: Vec<&[u8]> = vec![&addr_param];
    let result = vm.execute_script(&script, type_args, params, &mut gas_status);

    assert!(result.is_ok(), "script execution failed");

    let tag = StructTag {
        address,
        module: Identifier::new("BasicCoin").unwrap(),
        name: Identifier::new("Balance").unwrap(),
        type_params: vec![],
    };
    // Check if the resource exists and is published on our address
    let result = vm.get_resource(&address, &bcs::to_bytes(&tag).unwrap());

    // Check if the resource exists
    assert!(
        result.unwrap().is_some(),
        "resource not found in the module"
    );

    let tag = StructTag {
        address,
        module: Identifier::new("BasicCoin").unwrap(),
        name: Identifier::new("NonExisting").unwrap(),
        type_params: vec![],
    };
    let result = vm.get_resource(&address, &bcs::to_bytes(&tag).unwrap());

    // Check if the resource does not exist
    assert!(
        result.unwrap().is_none(),
        "resource found in the module (but it shouldn't)"
    );
}

#[test]
#[ignore = "we need to build the move package before with a script before running the test"]
fn execute_script_with_no_params_test() {
    let store = StorageMock::new();
    let vm = Mvm::new(store).unwrap();

    let script = read_script_bytes_from_project("simple_scripts", "empty_loop");

    let mut gas_status = GasStatus::new_unmetered();

    let type_args: Vec<TypeTag> = vec![];
    let params: Vec<&[u8]> = vec![];

    let result = vm.execute_script(&script, type_args, params, &mut gas_status);

    assert!(result.is_ok(), "failed to execute the script");
}

#[test]
#[ignore = "we need to build the move package before with a script before running the test"]
fn execute_script_params_test() {
    let store = StorageMock::new();
    let vm = Mvm::new(store).unwrap();

    let script = read_script_bytes_from_project("simple_scripts", "empty_loop_param");

    let mut gas_status = GasStatus::new_unmetered();

    let iter_count = bcs::to_bytes(&10u64).unwrap();
    let type_args: Vec<TypeTag> = vec![];
    let params: Vec<&[u8]> = vec![&iter_count];

    let result = vm.execute_script(&script, type_args, params, &mut gas_status);

    assert!(result.is_ok(), "failed to execute the script");
}

#[test]
#[ignore = "we need to build the move package before with a script before running the test"]
fn execute_script_generics_test() {
    let store = StorageMock::new();
    let vm = Mvm::new(store).unwrap();

    let script = read_script_bytes_from_project("simple_scripts", "generic_1");

    let mut gas_status = GasStatus::new_unmetered();

    let param = bcs::to_bytes(&100u64).unwrap();
    let type_args: Vec<TypeTag> = vec![TypeTag::U64];
    let params: Vec<&[u8]> = vec![&param];

    let result = vm.execute_script(&script, type_args, params, &mut gas_status);

    assert!(result.is_ok(), "failed to execute the script");

    // Execute once more but change param type
    let param = bcs::to_bytes(&true).unwrap();
    let type_args: Vec<TypeTag> = vec![TypeTag::Bool];
    let params: Vec<&[u8]> = vec![&param];

    let result = vm.execute_script(&script, type_args, params, &mut gas_status);

    assert!(result.is_ok(), "failed to execute the script");
}

#[test]
#[ignore = "we need to build the move package before with a script before running the test"]
fn execute_script_generics_incorrect_params_test() {
    let store = StorageMock::new();
    let vm = Mvm::new(store).unwrap();

    let script = read_script_bytes_from_project("simple_scripts", "generic_1");

    let mut gas_status = GasStatus::new_unmetered();

    // Execute with mismatched params
    let param = bcs::to_bytes(&true).unwrap();
    let type_args: Vec<TypeTag> = vec![TypeTag::U64];
    let params: Vec<&[u8]> = vec![&param];

    let result = vm.execute_script(&script, type_args, params, &mut gas_status);

    assert!(result.is_err(), "script execution should fail");

    // Execute with wrong params count
    let param = bcs::to_bytes(&true).unwrap();
    let type_args: Vec<TypeTag> = vec![TypeTag::U64, TypeTag::Bool];
    let params: Vec<&[u8]> = vec![&param];

    let result = vm.execute_script(&script, type_args, params, &mut gas_status);

    assert!(result.is_err(), "script execution should fail");
}

#[test]
#[ignore = "we need to build the move package before with a script before running the test"]
fn execute_function_test() {
    let store = StorageMock::new();
    let vm = Mvm::new(store).unwrap();
    let mut gas_status = GasStatus::new_unmetered();

    let addr_std = AccountAddress::from_hex_literal("0x1").unwrap();
    let module = read_stdlib_module_bytes_from_project("basic_coin", "signer");
    let result = vm.publish_module(&module, addr_std, &mut gas_status);

    assert!(result.is_ok(), "Failed to publish the stdlib module");

    let address = AccountAddress::from_hex_literal("0xCAFE").unwrap();
    let module = read_module_bytes_from_project("basic_coin", "BasicCoin");
    let result = vm.publish_module(&module, address, &mut gas_status);

    assert!(result.is_ok(), "Failed to publish the module");

    let addr_param = bcs::to_bytes(&address).unwrap();
    let mod_name = Identifier::new("BasicCoin").unwrap();
    let func_name = Identifier::new("publish_balance").unwrap();

    let type_args: Vec<TypeTag> = vec![];
    let params: Vec<&[u8]> = vec![&addr_param];
    let result = vm.execute_function(
        address,
        mod_name,
        func_name,
        type_args,
        params,
        &mut gas_status,
    );

    assert!(result.is_ok(), "script execution failed");
}
