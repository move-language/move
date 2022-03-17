// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    attr_derivation::{
        attr_params, find_attr_slice, new_attr, new_native_fun, new_simple_type, new_var,
    },
    parser::ast::{FunctionName, ModuleDefinition, ModuleMember, Visibility},
    shared::CompilationEnv,
};
use move_ir_types::location::sp;
use move_symbol_pool::Symbol;

const CONTRACT_ATTR: &str = "contract";
const CALLABLE_ATTR: &str = "callable";
const EXTERNAL_ATTR: &str = "external";

pub(crate) fn derive_for_evm(_env: &mut CompilationEnv, mod_def: &mut ModuleDefinition) {
    if find_attr_slice(&mod_def.attributes, CONTRACT_ATTR).is_none() {
        // Not an EVM contract module
        return;
    }
    let mut new_funs = vec![];
    for mem in &mod_def.members {
        if let ModuleMember::Function(fun_def) = mem {
            if let Some(attr) = find_attr_slice(&fun_def.attributes, CALLABLE_ATTR) {
                // Generate a `call_<name>(contract: address, <args>)` native function for
                // cross contract calls.
                let loc = attr.loc;
                let call_name =
                    FunctionName(sp(loc, Symbol::from(format!("call_{}", fun_def.name))));
                let mut sign = fun_def.signature.clone();
                sign.parameters.insert(
                    0,
                    (new_var(loc, "_target"), new_simple_type(loc, "address")),
                );
                // Create new #[external(params)] attribute, taking over parameters given to
                // #[callable].
                let attrs = sp(
                    loc,
                    vec![new_attr(
                        loc,
                        EXTERNAL_ATTR,
                        attr_params(attr).into_iter().cloned().collect(),
                    )],
                );
                new_funs.push(new_native_fun(
                    loc,
                    call_name,
                    attrs,
                    Visibility::Public(loc),
                    sign,
                ));
            }
        }
    }
    for fun_def in new_funs {
        mod_def.members.push(ModuleMember::Function(fun_def))
    }
}
