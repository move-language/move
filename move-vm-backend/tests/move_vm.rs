use crate::mock::StorageMock;
use move_vm_backend::Mvm;

use move_core_types::account_address::AccountAddress;
use move_core_types::identifier::Identifier;
use move_core_types::language_storage::ModuleId;
use move_vm_backend::warehouse::Warehouse;

use move_vm_test_utils::gas_schedule::GasStatus;

pub mod mock;

#[test]
fn load_module_not_found_test() {
    let store = Warehouse::new(StorageMock::new());
    let vm = Mvm::new(store).unwrap();

    let module_id = ModuleId::new(
        AccountAddress::new([0x1; AccountAddress::LENGTH]),
        Identifier::new("TestModule").unwrap(),
    );

    let result = vm.load_module(&module_id);

    assert!(result.is_err());
}

#[test]
#[ignore = "we need to build the move package before with a script before running the test"]
fn publish_and_load_module_test() {
    let store = Warehouse::new(StorageMock::new());
    let vm = Mvm::new(store).unwrap();

    let address = AccountAddress::from_hex_literal("0xCAFE").unwrap();

    let module = include_bytes!("assets/move-projects/empty/build/empty/bytecode_modules/Empty.mv").to_vec();

    let mut gas_status = GasStatus::new_unmetered();

    let result = vm.publish_module(
        module.as_slice(),
        address,
        &mut gas_status,
    );
    assert!(result.is_ok(), "Failed to publish the module");

    let module_id = ModuleId::new(address, Identifier::new("Empty").unwrap());
    assert!(vm.load_module(&module_id).is_ok(), "Failed to load the module");
}

#[test]
#[ignore = "we need to build the move package before with a script before running the test"]
fn publish_module_test() {
    let store = Warehouse::new(StorageMock::new());
    let vm = Mvm::new(store).unwrap();

    let address = AccountAddress::from_hex_literal("0xCAFE").unwrap();

    let module = include_bytes!("assets/move-projects/empty/build/empty/bytecode_modules/Empty.mv").to_vec();

    let mut gas_status = GasStatus::new_unmetered();

    let result = vm.publish_module(
        module.as_slice(),
        address,
        &mut gas_status,
    );

    assert!(result.is_ok(), "Failed to publish the module");
}
