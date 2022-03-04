// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

// ! Module defining attributes used by the generator.

use move_model::{
    ast::{Attribute, AttributeValue, Value},
    model::{FunctionEnv, GlobalEnv, ModuleEnv},
};

const CREATE_ATTR: &str = "create";
const CALLABLE_ATTR: &str = "callable";
const EVM_ARITH_ATTR: &str = "evm_arith";
const PAYABLE_ATTR: &str = "payable";
const RECEIVE_ATTR: &str = "receive";
const RECEIVE_FALLBACK_ATTR: &str = "fallback";
const TEST_ATTR: &str = "evm_test";
const SIGNATURE: &str = "sig";

/// Extract the value from an attribute
fn extract_attr_value_str(
    env: &GlobalEnv,
    attrs: &[Attribute],
    attr_name: &str,
    value_name: &str,
) -> Option<String> {
    for attr in attrs {
        if let Attribute::Apply(_, s, args) = attr {
            if env.symbol_pool().string(*s).as_str() == attr_name {
                for inner_attr in args {
                    if let Attribute::Assign(_, symbol, value) = inner_attr {
                        if env.symbol_pool().string(*symbol).as_str() == value_name {
                            if let AttributeValue::Value(_, Value::ByteArray(vec_str)) = value {
                                return Some(
                                    vec_str.iter().map(|c| *c as char).collect::<String>(),
                                );
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

/// Extract the solidity signature from the callable attribute
pub fn extract_callable_signature(fun: &FunctionEnv<'_>) -> Option<String> {
    extract_attr_value_str(
        fun.module_env.env,
        fun.get_attributes(),
        CALLABLE_ATTR,
        SIGNATURE,
    )
}

/// Check whether an attribute is present in an attribute list.
pub fn has_attr(env: &GlobalEnv, attrs: &[Attribute], name: &str, simple_flag: bool) -> bool {
    let is_empty = |args: &Vec<Attribute>| {
        if simple_flag {
            args.is_empty()
        } else {
            true
        }
    };
    attrs.iter().any(|a| match a {
        Attribute::Apply(_, s, args)
            if is_empty(args) && env.symbol_pool().string(*s).as_str() == name =>
        {
            true
        }
        _ => false,
    })
}

/// Check whether the module has a `#[evm_arith]` attribute.
pub fn is_evm_arith_module(module: &ModuleEnv) -> bool {
    has_attr(module.env, module.get_attributes(), EVM_ARITH_ATTR, true)
}

/// Check whether the function has a `#[callable]` attribute.
pub fn is_callable_fun(fun: &FunctionEnv<'_>) -> bool {
    has_attr(
        fun.module_env.env,
        fun.get_attributes(),
        CALLABLE_ATTR,
        false,
    )
}

/// Check whether the function has a `#[create]` attribute.
pub fn is_create_fun(fun: &FunctionEnv<'_>) -> bool {
    has_attr(fun.module_env.env, fun.get_attributes(), CREATE_ATTR, true)
}

/// Check whether the function has a `#[payable]` attribute.
pub fn is_payable_fun(fun: &FunctionEnv<'_>) -> bool {
    has_attr(fun.module_env.env, fun.get_attributes(), PAYABLE_ATTR, true)
}

/// Check whether the function has a `#[receive]` attribute.
pub fn is_receive_fun(fun: &FunctionEnv<'_>) -> bool {
    has_attr(fun.module_env.env, fun.get_attributes(), RECEIVE_ATTR, true)
}

/// Check whether the function has a `#[fallback]]` attribute.
pub fn is_fallback_fun(fun: &FunctionEnv<'_>) -> bool {
    has_attr(
        fun.module_env.env,
        fun.get_attributes(),
        RECEIVE_FALLBACK_ATTR,
        true,
    )
}

/// Checks whether this function is a contract function.
pub fn is_contract_fun(fun: &FunctionEnv) -> bool {
    is_callable_fun(fun) || is_fallback_fun(fun) || is_receive_fun(fun)
}

/// Check whether the function has a `#[evm_test] attribute.
pub fn is_test_fun(fun: &FunctionEnv<'_>) -> bool {
    has_attr(fun.module_env.env, fun.get_attributes(), TEST_ATTR, true)
}
