// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#[test]
fn reference_analysis_crash() {
    use move_binary_format::file_format::{
        empty_module, AbilitySet,
        Bytecode::{BrTrue, LdFalse, MoveLoc, MutBorrowGlobal, Pop, Ret},
        CodeUnit, Constant, FieldDefinition, FunctionDefinition, FunctionHandle,
        FunctionHandleIndex, IdentifierIndex, ModuleHandleIndex, Signature, SignatureIndex,
        SignatureToken::{Address, Bool, U128, U64},
        StructDefinition, StructDefinitionIndex, StructFieldInformation, StructHandle,
        StructHandleIndex, TypeSignature, Visibility,
    };
    use move_core_types::{
        account_address::AccountAddress, identifier::Identifier, vm_status::StatusCode,
    };
    use std::str::FromStr;

    let mut module = empty_module();
    module.version = 5;

    module.struct_handles.push(StructHandle {
        module: ModuleHandleIndex(0),
        name: IdentifierIndex(1),
        abilities: AbilitySet::ALL,
        type_parameters: vec![],
    });

    let fun_handle = FunctionHandle {
        module: ModuleHandleIndex(0),
        name: IdentifierIndex(2),
        parameters: SignatureIndex(0),
        return_: SignatureIndex(1),
        type_parameters: vec![],
    };

    module.function_handles.push(fun_handle);

    module.signatures.pop();
    module.signatures.push(Signature(vec![
        Address, U64, Address, Address, U128, Address, U64, U64, U64,
    ]));
    module.signatures.push(Signature(vec![]));
    module
        .signatures
        .push(Signature(vec![Address, Bool, Address]));

    module.identifiers.extend(
        vec![
            Identifier::from_str("zf_hello_world").unwrap(),
            Identifier::from_str("awldFnU18mlDKQfh6qNfBGx8X").unwrap(),
            Identifier::from_str("aQPwJNHyAHpvJ").unwrap(),
            Identifier::from_str("aT7ZphKTrKcYCwCebJySrmrKlckmnL5").unwrap(),
            Identifier::from_str("arYpsFa2fvrpPJ").unwrap(),
        ]
        .into_iter(),
    );
    module.address_identifiers.push(AccountAddress::random());

    module.constant_pool.push(Constant {
        type_: Address,
        data: AccountAddress::ZERO.into_bytes().to_vec(),
    });

    module.struct_defs.push(StructDefinition {
        struct_handle: StructHandleIndex(0),
        field_information: StructFieldInformation::Declared(vec![FieldDefinition {
            name: IdentifierIndex::new(3),
            signature: TypeSignature(Address),
        }]),
    });

    let code_unit = CodeUnit {
        code: vec![
            LdFalse,
            BrTrue(13),
            MoveLoc(3),
            MutBorrowGlobal(StructDefinitionIndex(0)),
            MoveLoc(6),
            Pop,
            MoveLoc(5),
            MutBorrowGlobal(StructDefinitionIndex(0)),
            MoveLoc(0),
            MutBorrowGlobal(StructDefinitionIndex(0)),
            Pop,
            Pop,
            Pop,
            Ret,
        ],
        locals: SignatureIndex::new(2),
    };
    let fun_def = FunctionDefinition {
        code: Some(code_unit),
        function: FunctionHandleIndex(0),
        visibility: Visibility::Public,
        is_entry: false,
        acquires_global_resources: vec![],
    };

    module.function_defs.push(fun_def);
    match crate::verify_module(&module) {
        Ok(_) => {}
        Err(e) => assert_eq!(e.major_status(), StatusCode::GLOBAL_REFERENCE_ERROR),
    }
}
