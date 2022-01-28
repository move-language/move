// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use std::convert::TryInto;

use move_core_types::{
    account_address::AccountAddress,
    identifier::Identifier,
    language_storage::ModuleId,
    value::{MoveValue, serialize_values}, vm_status::StatusCode
};
use move_vm_runtime::{move_vm::MoveVM, session::ExecutionResult};
use move_vm_test_utils::InMemoryStorage;
use move_vm_types::gas_schedule::GasStatus;

use crate::compiler::{compile_units, as_module};


const TEST_ADDR: AccountAddress = AccountAddress::new([42; AccountAddress::LENGTH]);
const TEST_MODULE_ID: &str = "M";
const EXPECT_MUTREF_OUT_VALUE: u64 = 90;
const USE_MUTREF_LABEL: &str = "use_mutref";
const USE_REF_LABEL: &str = "use_ref";
const FUN_NAMES: [&str; 2] = [USE_MUTREF_LABEL, USE_REF_LABEL];

// ensure proper errors are returned when ref & mut ref args fail to deserialize
#[test]
fn fail_arg_deserialize() {
    let mod_code = setup_module();
    // all of these should fail to deserialize because the functions expect u64 args
    vec![MoveValue::U8(16), MoveValue::U128(512), MoveValue::Bool(true)]
    .iter()
    .for_each(|mv| {
        FUN_NAMES.iter().for_each(|name| {
            match run(&mod_code, name, mv.clone()) {
                ExecutionResult::Success { .. } => {
                    panic!("Should have failed to deserialize non-u64 type to u64");
                },
                ExecutionResult::Fail { error, .. } => {
                    assert_eq!(error.major_status(), StatusCode::FAILED_TO_DESERIALIZE_ARGUMENT);
                }
            }
        });
    });
}

// check happy path for writing to mut ref args - may be unecessary / covered by other tests
#[test]
fn mutref_output_success() {
    let mod_code = setup_module();
    match run(&mod_code, USE_MUTREF_LABEL, MoveValue::U64(1)) {
        ExecutionResult::Success { mutable_ref_values, .. } => {
            assert_eq!(1, mutable_ref_values.len());
            let parsed = parse_u64_arg(mutable_ref_values.first().unwrap());
            assert_eq!(EXPECT_MUTREF_OUT_VALUE, parsed);
        },
        ExecutionResult::Fail { error, .. } => {
            panic!("{:?}", error);
        }
    }
}

// TODO - how can we cause serialization errors in values returned from Move ?
// that would allow us to test error paths for outputs as well

fn setup_module() -> ModuleCode {
    // first function takes a mutable ref & writes to it, the other takes immutable ref, so we exercise both paths
    let code = format!(
        r#"
        module 0x{}::{} {{
            fun {}(a: &mut u64) {{ *a = {}; }}
            fun {}(_a: & u64) {{ }}
        }}
    "#,
        TEST_ADDR, TEST_MODULE_ID, USE_MUTREF_LABEL, EXPECT_MUTREF_OUT_VALUE, USE_REF_LABEL
    );

    let module_id = ModuleId::new(TEST_ADDR, Identifier::new(TEST_MODULE_ID).unwrap());
    (module_id, code)
}

fn run(module: &ModuleCode, fun_name: &str, arg_val0: MoveValue) -> ExecutionResult {
    let module_id = &module.0;
    let modules = vec![module.clone()];
    let (vm, storage) = setup_vm(&modules);
    let sess = vm.new_session(&storage);

    let fun_name = Identifier::new(fun_name).unwrap();
    let mut gas_status = GasStatus::new_unmetered();

    sess.execute_function_for_effects(
        &module_id,
        &fun_name,
        vec![],
        serialize_values(&vec![arg_val0]),
        &mut gas_status
    )
}

type ModuleCode = (ModuleId, String);

// TODO - move some utility functions to where test infra lives, see about unifying with similar code
fn setup_vm(modules: &Vec<ModuleCode>) -> (MoveVM, InMemoryStorage) {
    let mut storage = InMemoryStorage::new();
    compile_modules(&mut storage, modules);
    (MoveVM::new(vec![]).unwrap(), storage)
}

fn compile_modules(mut storage: &mut InMemoryStorage, modules: &Vec<ModuleCode>) {
    modules.iter().for_each(|(id, code)| {
        compile_module(&mut storage, &id, &code);
    });
}

fn compile_module(storage: &mut InMemoryStorage, mod_id: &ModuleId, code: &String) {
    let mut units = compile_units(&code).unwrap();
    let module = as_module(units.pop().unwrap());
    let mut blob = vec![];
    module.serialize(&mut blob).unwrap();
    storage.publish_or_overwrite_module(mod_id.clone(), blob);
}

fn parse_u64_arg(arg: &Vec<u8>) -> u64 {
    let as_arr: [u8; 8] = arg[..8].try_into().expect("wrong u64 length, must be 8 bytes");
    u64::from_le_bytes(as_arr)
}
