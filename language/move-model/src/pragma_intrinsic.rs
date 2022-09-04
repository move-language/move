// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    ast::{ModuleName, Operation, PropertyValue, QualifiedSymbol, Value},
    builder::{
        model_builder::{FunEntry, SpecFunEntry, StructEntry},
        module_builder::SpecBlockContext,
    },
    model::{QualifiedId, SpecFunId},
    pragmas::INTRINSIC_PRAGMA,
    ty::{PrimitiveType, Type},
    Loc, ModelBuilder, StructId,
};

pub(crate) fn handle_intrinsic_declaration(
    builder: &mut ModelBuilder,
    module: &ModuleName,
    loc: &Loc,
    context: &SpecBlockContext,
    prop: PropertyValue,
) -> Option<PropertyValue> {
    // obtain the fully qualified name to the intrinsic part
    let intrinsic = match prop {
        PropertyValue::Value(_) => {
            builder.error(loc, "invalid intrinsic declaration");
            return None;
        }
        PropertyValue::Symbol(name) => QualifiedSymbol {
            module_name: module.clone(),
            symbol: name,
        },
        PropertyValue::QualifiedSymbol(symbol) => symbol,
    };

    // check declaration validity
    match context {
        SpecBlockContext::Struct(symbol) => {
            let target = builder.struct_table.get(symbol).expect("struct entry");
            let intrinsic_id =
                check_intrinsic_declaration_in_struct(builder, loc, target, intrinsic)?;
            let target_id = target.module_id.qualified(target.struct_id);
            builder
                .intrinsic_types
                .entry(intrinsic_id)
                .or_default()
                .insert(target_id);
        }
        SpecBlockContext::Function(symbol) => {
            let target = builder.fun_table.get(symbol).expect("function entry");
            let intrinsic_id =
                check_intrinsic_declaration_in_function(builder, loc, target, intrinsic)?;
            let target_id = target.module_id.qualified(target.fun_id);
            builder
                .intrinsic_funs
                .entry(intrinsic_id)
                .or_default()
                .insert(target_id);
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

    // TODO(mengxu): for compatibility reason, we still leave a marker on the intrinsic declaration
    // such that it can be picked-up by the `is_intrinsic()` function in `GlobalEnv`.
    Some(PropertyValue::Value(Value::Bool(true)))
}

fn check_intrinsic_declaration_in_struct(
    builder: &ModelBuilder,
    loc: &Loc,
    target: &StructEntry,
    symbol: QualifiedSymbol,
) -> Option<QualifiedId<StructId>> {
    // obtain the intrinsic type
    let intrinsic = match builder.spec_struct_table.get(&symbol) {
        None => {
            builder.error(
                loc,
                &format!(
                    "no such intrinsic type: {}",
                    symbol.display(builder.env.symbol_pool())
                ),
            );
            return None;
        }
        Some(intrinsic) => intrinsic,
    };

    // check type parameters
    // TODO(mengxu): check ability constraints in the type params
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
        return None;
    }

    // check abilities
    // TODO(mengxu): check ability compatability of the struct
    if target.is_resource
        && !intrinsic.abilities.has_key()
        && (intrinsic.abilities.has_copy() || intrinsic.abilities.has_drop())
    {
        builder.error(loc, "ability modifiers mismatch");
        return None;
    }

    // conformance checked
    Some(intrinsic.module_id.qualified(intrinsic.struct_id))
}

fn check_intrinsic_declaration_in_function(
    builder: &ModelBuilder,
    loc: &Loc,
    target: &FunEntry,
    symbol: QualifiedSymbol,
) -> Option<QualifiedId<SpecFunId>> {
    // obtain the intrinsic function
    let candidates = match builder.spec_fun_table.get(&symbol) {
        None => {
            builder.error(
                loc,
                &format!(
                    "no such intrinsic function: {}",
                    symbol.display(builder.env.symbol_pool())
                ),
            );
            return None;
        }
        Some(intrinsics) => intrinsics,
    };

    // due to overloading, we need to find the one that matches with the declaration
    for candidate in candidates {
        let matched = check_function_entry_conformance(builder, target, candidate);
        if matched.is_some() {
            return matched;
        }
    }

    // none of the candidates matches with the declaration
    builder.error(
        loc,
        &format!(
            "no matching intrinsic function found for `{}`",
            symbol.display(builder.env.symbol_pool())
        ),
    );
    None
}

fn check_function_entry_conformance(
    builder: &ModelBuilder,
    target: &FunEntry,
    candidate: &SpecFunEntry,
) -> Option<QualifiedId<SpecFunId>> {
    let qid = match &candidate.oper {
        Operation::Function(mid, fid, _) => mid.qualified(*fid),
        _ => {
            // only defined spec functions can be intrinsic (built-ins are not)
            return None;
        }
    };

    // check type parameters
    // TODO(mengxu): check ability constraints in the type params
    if candidate.type_params.len() != target.type_params.len() {
        return None;
    }

    // check parameter types
    if candidate.arg_types.len() != target.params.len() {
        return None;
    }
    let matched = candidate
        .arg_types
        .iter()
        .zip(target.params.iter())
        .all(|(lhs_ty, (_, rhs_ty))| is_more_abstract(builder, lhs_ty, rhs_ty));
    if !matched {
        return None;
    }

    // check return type
    if !is_more_abstract(builder, &candidate.result_type, &target.result_type) {
        return None;
    }

    // found the target
    Some(qid)
}

// check that the `lhs` type is more abstract than the `rhs` type
fn is_more_abstract(builder: &ModelBuilder, lhs: &Type, rhs: &Type) -> bool {
    // equivalent types can always be refined to each other
    if lhs == rhs {
        return true;
    }

    // if the `lhs` type is an intrinsic type, check whether it can be concretized to `rhs`
    if let Type::Struct(lhs_mid, lhs_sid, lhs_ty_args) = lhs {
        if let Some(entry) = builder
            .reverse_struct_table
            .get(&(*lhs_mid, *lhs_sid))
            .and_then(|qsym| builder.spec_struct_table.get(qsym))
        {
            let lhs_qid = lhs_mid.qualified(*lhs_sid);

            // check concretization through explicit intrinsic marks
            if let Type::Struct(rhs_mid, rhs_sid, rhs_ty_args) = rhs {
                let rhs_qid = rhs_mid.qualified(*rhs_sid);
                let matched = builder
                    .intrinsic_types
                    .get(&lhs_qid)
                    .map(|e| e.contains(&rhs_qid))
                    .unwrap_or(false);
                if matched && are_more_abstract_types(builder, lhs_ty_args, rhs_ty_args) {
                    return true;
                }
            }

            // check concretization through modeled types
            for lhs_modeled_ty in &entry.modeled_types {
                let lhs_modeled_ty_inst = lhs_modeled_ty.instantiate(lhs_ty_args);
                if is_more_abstract(builder, &lhs_modeled_ty_inst, rhs) {
                    return true;
                }
            }
        }
    }

    // recursively check the refinement relationship
    match (lhs, rhs) {
        (Type::Primitive(PrimitiveType::Num), Type::Primitive(PrimitiveType::U8))
        | (Type::Primitive(PrimitiveType::Num), Type::Primitive(PrimitiveType::U64))
        | (Type::Primitive(PrimitiveType::Num), Type::Primitive(PrimitiveType::U128)) => true,
        (Type::Tuple(lhs_tys), Type::Tuple(rhs_tys)) => {
            are_more_abstract_types(builder, lhs_tys, rhs_tys)
        }
        (Type::Vector(lhs_elem), Type::Vector(rhs_elem)) => {
            is_more_abstract(builder, lhs_elem, rhs_elem)
        }
        (Type::Struct(lhs_mid, lhs_sid, lhs_tys), Type::Struct(rhs_mid, rhs_sid, rhs_tys)) => {
            if (lhs_mid != rhs_mid) || (lhs_sid != rhs_sid) {
                return false;
            }
            are_more_abstract_types(builder, lhs_tys, rhs_tys)
        }
        (Type::Reference(lhs_is_mut, lhs_ty), Type::Reference(rhs_is_mut, rhs_ty)) => {
            if lhs_is_mut != rhs_is_mut {
                return false;
            }
            is_more_abstract(builder, lhs_ty, rhs_ty)
        }
        (Type::Fun(lhs_tys, lhs_ret), Type::Fun(rhs_tys, rhs_ret)) => {
            are_more_abstract_types(builder, lhs_tys, rhs_tys)
                && is_more_abstract(builder, lhs_ret, rhs_ret)
        }
        (Type::TypeDomain(lhs_elem), Type::TypeDomain(rhs_elem)) => {
            is_more_abstract(builder, lhs_elem, rhs_elem)
        }
        (
            Type::ResourceDomain(lhs_mid, lhs_sid, lhs_tys_opt),
            Type::ResourceDomain(rhs_mid, rhs_sid, rhs_tys_opt),
        ) => {
            if (lhs_mid != rhs_mid) || (lhs_sid != rhs_sid) {
                return false;
            }
            match (lhs_tys_opt, rhs_tys_opt) {
                (None, None) => true,
                (Some(from_tys), Some(into_tys)) => {
                    are_more_abstract_types(builder, from_tys, into_tys)
                }
                _ => false,
            }
        }
        _ => false,
    }
}

fn are_more_abstract_types(builder: &ModelBuilder, lhs_tys: &[Type], rhs_tys: &[Type]) -> bool {
    if lhs_tys.len() != rhs_tys.len() {
        return false;
    }
    lhs_tys
        .iter()
        .zip(rhs_tys.iter())
        .all(|(lhs_ty, rhs_ty)| is_more_abstract(builder, lhs_ty, rhs_ty))
}
