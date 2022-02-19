// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

// ! Module defining attributes used by the generator.

use move_model::{
    ast::Attribute,
    model::{FunctionEnv, GlobalEnv, ModuleEnv},
};

const CONTRACT_ATTR: &str = "contract";
const CREATE_ATTR: &str = "create";
const CALLABLE_ATTR: &str = "callable";
const EVM_ARITH_ATTR: &str = "evm_arith";
const PAYABLE_ATTR: &str = "payable";
const RECEIVE_ATTR: &str = "receive";
const RECEIVE_FALLBACK: &str = "fallback";

/// Check whether a simple attribute is present in an attribute list.
pub fn has_simple_attr(env: &GlobalEnv, attrs: &[Attribute], name: &str) -> bool {
    attrs.iter().any(|a| match a {
        Attribute::Apply(_, s, args)
            if args.is_empty() && env.symbol_pool().string(*s).as_str() == name =>
        {
            true
        }
        _ => false,
    })
}

/// Check whether the module has a `#[contract]` attribute.
pub fn is_contract_module(module: &ModuleEnv<'_>) -> bool {
    has_simple_attr(module.env, module.get_attributes(), CONTRACT_ATTR)
}

/// Check whether the module has a `#[evm_arith]` attribute.
pub fn is_evm_arith_module(module: &ModuleEnv) -> bool {
    has_simple_attr(module.env, module.get_attributes(), EVM_ARITH_ATTR)
}

/// Check whether the function has a `#[callable]` attribute.
pub fn is_callable_fun(fun: &FunctionEnv<'_>) -> bool {
    has_simple_attr(fun.module_env.env, fun.get_attributes(), CALLABLE_ATTR)
}

/// Check whether the function has a `#[create]` attribute.
pub fn is_create_fun(fun: &FunctionEnv<'_>) -> bool {
    has_simple_attr(fun.module_env.env, fun.get_attributes(), CREATE_ATTR)
}

/// Check whether the function has a `#[payable]` attribute.
pub fn is_payable_fun(fun: &FunctionEnv<'_>) -> bool {
    has_simple_attr(fun.module_env.env, fun.get_attributes(), PAYABLE_ATTR)
}

/// Check whether the function has a `#[receive]` attribute.
pub fn is_receive_fun(fun: &FunctionEnv<'_>) -> bool {
    has_simple_attr(fun.module_env.env, fun.get_attributes(), RECEIVE_ATTR)
}

/// Check whether the function has a `#[fallback]]` attribute.
pub fn is_fallback_fun(fun: &FunctionEnv<'_>) -> bool {
    has_simple_attr(fun.module_env.env, fun.get_attributes(), RECEIVE_FALLBACK)
}
