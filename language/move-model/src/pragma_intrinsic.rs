// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    ast::{PropertyValue, Value},
    builder::{
        model_builder::{SpecStructEntry, StructEntry},
        module_builder::SpecBlockContext,
    },
    pragmas::INTRINSIC_PRAGMA,
    Loc, ModelBuilder,
};

pub(crate) fn handle_intrinsic_declaration(
    builder: &ModelBuilder,
    loc: &Loc,
    context: &SpecBlockContext,
    prop: PropertyValue,
) -> Option<PropertyValue> {
    match context {
        SpecBlockContext::Struct(symbol) => {
            let target = builder.struct_table.get(symbol).expect("struct entry");
            handle_intrinsic_declaration_in_struct(builder, loc, target, prop)
        }
        SpecBlockContext::Function(symbol) => {
            builder.fun_table.get(symbol).expect("function entry");
            // TODO(mengxu): add checking there
            Some(prop)
        }
        SpecBlockContext::Schema(..)
        | SpecBlockContext::Module
        | SpecBlockContext::FunctionCode(..) => {
            unreachable!(
                "\"pragma {}\" is not allowed on context {}",
                INTRINSIC_PRAGMA, context
            );
        }
    }
}

fn handle_intrinsic_declaration_in_struct(
    builder: &ModelBuilder,
    loc: &Loc,
    target: &StructEntry,
    prop: PropertyValue,
) -> Option<PropertyValue> {
    match &prop {
        PropertyValue::Value(_) | PropertyValue::Symbol(_) => {
            builder.error(loc, "invalid intrinsic declaration");
            None
        }
        PropertyValue::QualifiedSymbol(symbol) => match builder.spec_struct_table.get(symbol) {
            None => {
                builder.error(
                    loc,
                    &format!(
                        "no such intrinsic type: {}",
                        symbol.display(builder.env.symbol_pool())
                    ),
                );
                None
            }
            Some(intrinsic) => {
                check_struct_entry_conformance(builder, loc, target, intrinsic);
                // TODO(mengxu): for compatibility reason, we still leave a marker on the intrinsic
                // declaration such that it can be picked-up by the `is_intrinsic()` function in
                // `GlobalEnv`.
                Some(PropertyValue::Value(Value::Bool(true)))
            }
        },
    }
}

fn check_struct_entry_conformance(
    builder: &ModelBuilder,
    loc: &Loc,
    target: &StructEntry,
    intrinsic: &SpecStructEntry,
) {
    if target.type_params.len() != intrinsic.type_params.len() {
        builder.error_with_notes(
            loc,
            "number of type parameters mismatch",
            vec![
                format!(
                    "the intrinsic type has {} type parameters",
                    intrinsic.type_params.len()
                ),
                format!(
                    "the struct declaration has {} type parameters",
                    target.type_params.len()
                ),
            ],
        );
    }

    if target.is_resource
        && !intrinsic.abilities.has_key()
        && (intrinsic.abilities.has_copy() || intrinsic.abilities.has_drop())
    {
        builder.error(loc, "ability modifiers mismatch");
    }

    // TODO(mengxu): we check ability constraints in the type params
    // TODO(mengxu): check ability compatability of the struct
}
