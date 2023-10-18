use crate::mock::StorageMock;
use move_vm_backend::Mvm;

use move_core_types::account_address::AccountAddress;
use move_core_types::identifier::Identifier;
use move_core_types::language_storage::{ModuleId, StructTag};

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

/// Reads a precompiled Move stdlib module from our assets directory for specified project.
/// This will be replaced in the future with a proper stdlib genesis initialization.
fn read_stdlib_module_bytes_from_project(project: &str, stdlib_module_name: &str) -> Vec<u8> {
    const MOVE_PROJECTS: &str = "tests/assets/move-projects";

    let path =
        format!("{MOVE_PROJECTS}/{project}/build/{project}/bytecode_modules/dependencies/MoveStdlib/{stdlib_module_name}.mv");

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

    let result = vm.publish_module(module.as_slice(), address, &mut gas_status);

    assert!(result.is_ok(), "failed to publish the module");
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

    let module_id =
        bcs::to_bytes(&ModuleId::new(address, Identifier::new("Vector").unwrap())).unwrap();

    let mut gas_status = GasStatus::new_unmetered();
    let result = vm.publish_module(module.as_slice(), address, &mut gas_status);
    assert!(result.is_ok(), "failed to publish the module");

    let result = vm.get_module(&module_id);
    assert_eq!(
        result.expect("failed to get the module"),
        Some(module),
        "invalid module received"
    );

    let result = vm.get_module_abi(&module_id);
    assert!(result.unwrap().is_some(), "failed to get the module abi");
}

#[test]
#[ignore = "we need to build the move package before with a script before running the test"]
// This test heavily depends on Move.toml files for thes used Move packages.
fn get_resource() {
    let store = StorageMock::new();
    let vm = Mvm::new(store).unwrap();

    let address = AccountAddress::from_hex_literal("0x1").unwrap();
    let module = read_stdlib_module_bytes_from_project("basic_coin", "signer");
    let mut gas_status = GasStatus::new_unmetered();
    let result = vm.publish_module(module.as_slice(), address, &mut gas_status);
    assert!(result.is_ok(), "Failed to publish the stdlib module");

    let address = AccountAddress::from_hex_literal("0xCAFE").unwrap();
    let module = read_module_bytes_from_project("basic_coin", "BasicCoin");

    let mut gas_status = GasStatus::new_unmetered();
    let result = vm.publish_module(module.as_slice(), address, &mut gas_status);
    assert!(result.is_ok(), "Failed to publish the module");

    let tag = StructTag {
        address,
        module: Identifier::new("BasicCoin").unwrap(),
        name: Identifier::new("Balance").unwrap(),
        type_params: vec![],
    };

    let result = vm.get_resource(&address, &bcs::to_bytes(&tag).unwrap());

    // Check if the resource exists
    assert!(
        result.unwrap().is_some(),
        "resource not found in the module"
    );

    let tag = StructTag {
        address,
        module: Identifier::new("BasicCoin").unwrap(),
        name: Identifier::new("Non-existing").unwrap(),
        type_params: vec![],
    };

    let result = vm.get_resource(&address, &bcs::to_bytes(&tag).unwrap());

    // Check if the resource does not exist
    assert!(
        result.unwrap().is_none(),
        "resource found in the module (but it shouldn't)"
    );
}
