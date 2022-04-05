// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use std::convert::TryInto;

use crate::{
    diag,
    expansion::ast::{Address, AttributeName_, ModuleIdent, ModuleIdent_},
    naming::ast as N,
    parser::ast::FunctionName,
    shared::{
        known_attributes::{KnownAttribute, NativeAttribute},
        CompilationEnv, Identifier,
    },
};
use move_ir_types::ast as IR;

pub const FAKE_NATIVE_ATTR: AttributeName_ =
    AttributeName_::Known(KnownAttribute::Native(NativeAttribute::BytecodeInstruction));

pub fn function(
    env: &mut CompilationEnv,
    module_opt: Option<ModuleIdent>,
    function_name: FunctionName,
    function: &N::Function,
) {
    let loc = match function.attributes.get_loc_(&FAKE_NATIVE_ATTR) {
        None => return,
        Some(loc) => *loc,
    };
    let module = match module_opt {
        Some(module) => module,
        None => {
            let msg = format!(
                "Invalid usage of '{}' attribute to map function to bytecode instruction.",
                NativeAttribute::BYTECODE_INSTRUCTION
            );
            let smsg = "Script functions are never mapped to bytecode instructions";
            let diag = diag!(
                Attributes::InvalidBytecodeInst,
                (loc, msg),
                (function_name.loc(), smsg),
            );
            env.add_diag(diag);
            return;
        }
    };
    if resolve_builtin(&module, &function_name).is_none() {
        let attr_msg = format!(
            "Invalid usage of '{}' attribute to map function to bytecode instruction.",
            NativeAttribute::BYTECODE_INSTRUCTION
        );
        let name_msg = format!(
            "No known mapping of '{}::{}' to bytecode instruction",
            module, function_name
        );
        let diag = diag!(
            Attributes::InvalidBytecodeInst,
            (loc, attr_msg),
            (function_name.loc(), name_msg),
        );
        env.add_diag(diag);
    }
    match &function.body.value {
        N::FunctionBody_::Native => (),
        N::FunctionBody_::Defined(_) => {
            let attr_msg = format!(
                "Invalid usage of '{}' attribute on non-native function",
                NativeAttribute::BYTECODE_INSTRUCTION
            );
            let diag = diag!(Attributes::InvalidBytecodeInst, (loc, attr_msg));
            env.add_diag(diag);
        }
    }
}

pub fn resolve_builtin(
    module: &ModuleIdent,
    function: &FunctionName,
) -> Option<fn(Vec<IR::Type>) -> IR::Bytecode_> {
    let sp!(_, ModuleIdent_ { address, module }) = module;
    let addr_name = match address {
        Address::Numerical(Some(sp!(_, n_)), _) | Address::NamedUnassigned(sp!(_, n_)) => n_,
        _ => return None,
    };
    Some(
        match (
            addr_name.as_str(),
            module.value().as_str(),
            function.value().as_str(),
        ) {
            ("Std", "Vector", "empty") => |tys| IR::Bytecode_::VecPack(expect_one_ty_arg(tys), 0),
            ("Std", "Vector", "length") => |tys| IR::Bytecode_::VecLen(expect_one_ty_arg(tys)),
            ("Std", "Vector", "borrow") => {
                |tys| IR::Bytecode_::VecImmBorrow(expect_one_ty_arg(tys))
            }
            ("Std", "Vector", "push_back") => {
                |tys| IR::Bytecode_::VecPushBack(expect_one_ty_arg(tys))
            }
            ("Std", "Vector", "borrow_mut") => {
                |tys| IR::Bytecode_::VecMutBorrow(expect_one_ty_arg(tys))
            }
            ("Std", "Vector", "pop_back") => {
                |tys| IR::Bytecode_::VecPopBack(expect_one_ty_arg(tys))
            }
            ("Std", "Vector", "destroy_empty") => {
                |tys| IR::Bytecode_::VecUnpack(expect_one_ty_arg(tys), 0)
            }
            ("Std", "Vector", "swap") => |tys| IR::Bytecode_::VecSwap(expect_one_ty_arg(tys)),
            _ => return None,
        },
    )
}

fn expect_one_ty_arg(ty_args: Vec<IR::Type>) -> IR::Type {
    let [ty]: [IR::Type; 1] = ty_args
        .try_into()
        .expect("ICE native bytecode function expected a single type argument");
    ty
}
