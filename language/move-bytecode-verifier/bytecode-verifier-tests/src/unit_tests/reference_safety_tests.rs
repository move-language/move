// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use std::time::Instant;

use move_binary_format::file_format::{
    empty_module, Bytecode, CodeUnit, FunctionDefinition, FunctionHandle, FunctionHandleIndex,
    IdentifierIndex, ModuleHandleIndex, Signature, SignatureIndex, SignatureToken,
    Visibility::Public,
};
use move_bytecode_verifier::VerifierConfig;
use move_core_types::identifier::Identifier;

const NUM_COPYLOCS: u16 = 1880;
const NUM_CHILDREN: u16 = 1020;
const NUM_FUNCTIONS: u16 = 2;

#[test]
fn test_copyloc_pop() {
    let mut m = empty_module();

    // parameters of f0, f1, ...
    m.signatures
        .push(Signature(vec![SignatureToken::Reference(Box::new(
            SignatureToken::Vector(Box::new(SignatureToken::U8)),
        ))]));
    // locals of f0, f1, ...
    m.signatures.push(Signature(vec![
        SignatureToken::Reference(Box::new(SignatureToken::Vector(Box::new(
            SignatureToken::U8,
        )))),
        SignatureToken::U8, // ignore this, it's just here because I don't want to fix indices and the TypeParameter after removing the collision
    ]));
    // for VecImmBorrow
    m.signatures.push(Signature(
        std::iter::repeat(SignatureToken::U8).take(1).collect(),
    ));
    m.signatures
        .push(Signature(vec![SignatureToken::TypeParameter(0)]));

    for i in 0..NUM_FUNCTIONS {
        m.identifiers
            .push(Identifier::new(format!("f{}", i)).unwrap());
        m.function_handles.push(FunctionHandle {
            module: ModuleHandleIndex(0),
            name: IdentifierIndex(i),
            parameters: SignatureIndex(1),
            return_: SignatureIndex(0),
            type_parameters: vec![],
        });
        m.function_defs.push(FunctionDefinition {
            function: FunctionHandleIndex(i),
            visibility: Public,
            is_entry: false,
            acquires_global_resources: vec![],
            code: Some(CodeUnit {
                locals: SignatureIndex(2),
                code: vec![],
            }),
        });
        let code = &mut m.function_defs[i as usize].code.as_mut().unwrap().code;

        // create reference id
        code.push(Bytecode::CopyLoc(0));
        code.push(Bytecode::StLoc(1));
        // create NUM_CHLIDREN children of id
        for _ in 0..NUM_CHILDREN {
            code.push(Bytecode::CopyLoc(1));
            code.push(Bytecode::LdU64(0));
            code.push(Bytecode::VecImmBorrow(SignatureIndex(3)));
        }
        // then do a whole lot of copylocs on that reference
        for _ in 0..NUM_COPYLOCS {
            code.push(Bytecode::CopyLoc(1));
            code.push(Bytecode::Pop);
        }
        for _ in 0..NUM_CHILDREN {
            code.push(Bytecode::Pop);
        }

        code.push(Bytecode::Ret);
    }

    println!("{:#?}", m);

    let start = Instant::now();
    move_bytecode_verifier::verify_module_with_config(
        // mainnet config
        &VerifierConfig {
            max_loop_depth: Some(5),
            max_generic_instantiation_length: Some(32),
            max_function_parameters: Some(128),
            max_basic_blocks: Some(1024),
            max_value_stack_size: 1024,
            max_type_nodes: Some(256),
            max_push_size: Some(5000),
            max_dependency_depth: Some(100),
            max_struct_definitions: Some(200),
            max_fields_in_struct: Some(30),
            max_function_definitions: Some(1000),
            max_back_edges_per_function: Some(20),
            max_back_edges_per_module: Some(400),
            max_basic_blocks_in_script: Some(1024),
        },
        &m,
    )
    .unwrap();
    println!("verification time: {:?}", start.elapsed());

    //let mut module = vec![];
    //m.serialize(&mut module).unwrap();
    //println!("Serialized len: {}", module.len());
    //CompiledModule::deserialize(&module).unwrap();
}
