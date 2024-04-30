// Copyright © Aptos Foundation
// SPDX-License-Identifier: Apache-2.0

//! Checks for ability violations.
//! prerequisite:
//! - Copies, moves, and drops have been made explicit in assignment instructions
//! - Abort analysis has been performed so that ExitStateAnnotation are available

use crate::pipeline::exit_state_analysis::{ExitStateAnnotation, ExitStateAtCodeOffset};
use move_binary_format::file_format::{Ability, AbilitySet, CodeOffset};
use move_model::{
    ast::TempIndex,
    model::{FunId, FunctionEnv, Loc, ModuleId, QualifiedId, StructId, TypeParameterKind},
    ty::{self, gen_get_ty_param_kinds, Type},
};
use move_stackless_bytecode::{
    function_target::{FunctionData, FunctionTarget},
    function_target_pipeline::{FunctionTargetProcessor, FunctionTargetsHolder},
    stackless_bytecode::{AssignKind, Bytecode, Operation},
};

/// Returns the abilities of the given type.
fn type_abilities(func_target: &FunctionTarget, ty: &Type) -> AbilitySet {
    let ty_params = func_target.get_type_parameters();
    let global_env = func_target.global_env();
    global_env.type_abilities(ty, &ty_params)
}

/// Determines if the given type has the given ability
pub fn has_ability(target: &FunctionTarget, ty: &Type, ability: Ability) -> bool {
    type_abilities(target, ty).has_ability(ability)
}

/// Checks if the given type has the given ability, and add diagnostics if not.
/// Note that drop ability must only be enforced if there is an execution path from this code offset
/// that returns. It's legit in Move to do not drop before an abort or infinite loop.
fn check_ability(target: &FunctionTarget, ty: &Type, ability: Ability, loc: &Loc, err_msg: &str) {
    if !has_ability(target, ty, ability) {
        target.global_env().error(loc, err_msg)
    }
}

/// Checks if the given type has constraint copy, and add diagnostics if not.
pub fn check_copy(func_target: &FunctionTarget, ty: &Type, loc: &Loc, err_msg: &str) {
    check_ability(func_target, ty, Ability::Copy, loc, err_msg)
}

/// Checks if the given temporary variable has constraint copy, and add diagnostics if not.
fn check_copy_for_temp_with_msg(
    func_target: &FunctionTarget,
    t: TempIndex,
    loc: &Loc,
    err_msg: &str,
) {
    let ty = func_target.get_local_type(t);
    check_copy(func_target, ty, loc, err_msg)
}

/// `t` is the local containing the reference read
fn check_read_ref(target: &FunctionTarget, t: TempIndex, loc: &Loc) {
    if let Type::Reference(_, ty) = target.get_local_type(t) {
        check_copy(target, ty, loc, "cannot copy")
    } else {
        panic!("ICE ability checker: read_ref has non-reference argument")
    }
}

/// Checks drop ability for the given type;
/// generates an error, unless the state after `code_offset` won't return.
/// Drop ability must only be enforced if there is an execution path from this code offset
/// that returns. It's legit in Move to not drop in code definitely not leading to a return.
pub fn cond_check_drop(
    func_target: &FunctionTarget,
    code_offset: CodeOffset,
    ty: &Type,
    loc: &Loc,
    err_msg: &str,
) {
    let abort_state = get_exit_state_at(func_target, code_offset);
    if abort_state.after.may_return() {
        check_ability(func_target, ty, Ability::Drop, loc, err_msg)
    }
}

/// If temporary variable `t` does not have ability `drop`, generates an error,
/// unless the state after `code_offset` won't return.
/// Drop ability must only be enforced if there is an execution path from this code offset
/// that returns. It's legit in Move to not drop in code definitely not leading to a return.
fn cond_check_drop_for_temp_with_msg(
    func_target: &FunctionTarget,
    code_offset: CodeOffset,
    t: TempIndex,
    loc: &Loc,
    err_msg: &str,
) {
    let ty = func_target.get_local_type(t);
    cond_check_drop(func_target, code_offset, ty, loc, err_msg)
}

/// `t` is the local containing the reference being written to
/// `code_offset` is the code offset of the WriteRef instruction.
/// Drop ability must only be enforced if there is an execution path from this code offset
/// that returns. It's legit in Move to not drop in code definitely not leading to a return.
fn check_write_ref(target: &FunctionTarget, code_offset: CodeOffset, t: TempIndex, loc: &Loc) {
    if let Type::Reference(_, ty) = target.get_local_type(t) {
        cond_check_drop(target, code_offset, ty, loc, "write_ref: cannot drop")
    } else {
        panic!("ICE ability checker: write_ref has non-reference destination")
    }
}

/// Checks the given struct type (`Type::Struct(mod_id, struct_id, insts)`) has key ability
fn check_key_for_struct(
    target: &FunctionTarget,
    mod_id: ModuleId,
    struct_id: StructId,
    insts: &[Type],
    loc: &Loc,
    err_msg: &str,
) {
    if !check_struct_inst(target, mod_id, struct_id, insts, loc).has_ability(Ability::Key) {
        target.global_env().error(loc, err_msg)
    }
}

/// Generates a function that given module id, struct id,
/// returns the struct signature
fn gen_get_struct_sig<'a>(
    target: &'a FunctionTarget,
) -> impl Fn(ModuleId, StructId) -> (Vec<TypeParameterKind>, AbilitySet) + Copy + 'a {
    target.global_env().gen_get_struct_sig()
}

/// Checks that the type arguments to the struct type identified by `mid::sid` is properly instantiated,
/// and returns the abilities of the resulting instantiated struct type.
fn check_struct_inst(
    target: &FunctionTarget,
    mid: ModuleId,
    sid: StructId,
    ty_args: &[Type],
    loc: &Loc,
) -> AbilitySet {
    let ty_params = target.get_type_parameters();
    ty::check_struct_inst(
        mid,
        sid,
        ty_args,
        gen_get_ty_param_kinds(&ty_params),
        gen_get_struct_sig(target),
        Some((loc, |loc: &Loc, msg: &str| {
            target.global_env().error(loc, msg)
        })),
    )
}

/// Checks if the given function is properly instantiated
fn check_fun_inst(target: &FunctionTarget, mid: ModuleId, fid: FunId, inst: &[Type], loc: &Loc) {
    let qid = QualifiedId {
        module_id: mid,
        id: fid,
    };
    let fun_env = target.global_env().get_function(qid);
    for (param, ty) in fun_env.get_type_parameters().iter().zip(inst.iter()) {
        let required_abilities = param.1.abilities;
        let given_abilities = check_instantiation(target, ty, loc);
        if !required_abilities.is_subset(given_abilities) {
            // TODO(#11376): specify which field, why
            target.global_env().error(loc, "invalid instantiation")
        }
    }
}

/// `ty::infer_and_check_abilities` in the context of a `FunctionTarget`
/// where the abilities of type parameters comes from the function generics
pub fn check_instantiation(target: &FunctionTarget, ty: &Type, loc: &Loc) -> AbilitySet {
    let ty_params = target.get_type_parameters();
    ty::infer_and_check_abilities(
        ty,
        gen_get_ty_param_kinds(&ty_params),
        gen_get_struct_sig(target),
        loc,
        |loc, msg| target.global_env().error(loc, msg),
    )
}

pub struct AbilityChecker();

impl FunctionTargetProcessor for AbilityChecker {
    fn process(
        &self,
        _targets: &mut FunctionTargetsHolder,
        fun_env: &FunctionEnv,
        data: FunctionData,
        _scc_opt: Option<&[FunctionEnv]>,
    ) -> FunctionData {
        if fun_env.is_native() {
            return data;
        }
        let target = FunctionTarget::new(fun_env, &data);
        check_fun_signature(&target);
        for (code_offset, bytecode) in target.get_bytecode().iter().enumerate() {
            check_bytecode(&target, code_offset as CodeOffset, bytecode)
        }
        data
    }

    fn name(&self) -> String {
        "AbilityChecker".to_owned()
    }
}

fn check_fun_signature(target: &FunctionTarget) {
    for param in target.get_parameters() {
        let param_ty = target.get_local_type(param);
        // TODO: provide more accurate location
        check_instantiation(target, param_ty, &target.get_loc());
    }
    // return type is checked in function body
}

fn check_bytecode(target: &FunctionTarget, code_offset: CodeOffset, bytecode: &Bytecode) {
    let loc = target.get_bytecode_loc(bytecode.get_attr_id());
    match bytecode {
        Bytecode::Assign(_, dst, src, kind) => {
            // drop of dst during the assignment has been made explicit
            // so we don't check it here, plus this could be an initialization
            match kind {
                AssignKind::Copy | AssignKind::Store => {
                    check_copy_for_temp_with_msg(target, *src, &loc, "cannot copy");
                    if *dst == *src {
                        // dst is not dropped in advance in this case, since it's read by src
                        cond_check_drop_for_temp_with_msg(
                            target,
                            code_offset,
                            *dst,
                            &loc,
                            "invalid implicit drop",
                        )
                    }
                },
                AssignKind::Move => (),
                AssignKind::Inferred => {
                    panic!("ICE ability checker given inferred assignment")
                },
            }
        },
        Bytecode::Call(attr_id, _, op, srcs, _) => {
            use Operation::*;
            let loc = target.get_bytecode_loc(*attr_id);
            match op {
                Function(mod_id, fun_id, insts) => {
                    check_fun_inst(target, *mod_id, *fun_id, insts, &loc);
                },
                Pack(mod_id, struct_id, insts) => {
                    check_struct_inst(target, *mod_id, *struct_id, insts, &loc);
                },
                Unpack(mod_id, struct_id, insts) => {
                    check_struct_inst(target, *mod_id, *struct_id, insts, &loc);
                },
                MoveTo(mod_id, struct_id, insts) => {
                    check_key_for_struct(target, *mod_id, *struct_id, insts, &loc, "no key ability")
                },
                MoveFrom(mod_id, struct_id, insts) => {
                    check_key_for_struct(target, *mod_id, *struct_id, insts, &loc, "no key ability")
                },
                Exists(mod_id, struct_id, insts) => {
                    check_key_for_struct(target, *mod_id, *struct_id, insts, &loc, "no key ability")
                },
                BorrowGlobal(mod_id, struct_id, insts) => {
                    check_key_for_struct(target, *mod_id, *struct_id, insts, &loc, "no key ability")
                },
                BorrowField(mod_id, struct_id, insts, _) => {
                    check_struct_inst(target, *mod_id, *struct_id, insts, &loc);
                },
                Drop => cond_check_drop_for_temp_with_msg(
                    target,
                    code_offset,
                    srcs[0],
                    &loc,
                    "cannot drop",
                ),
                ReadRef => check_read_ref(target, srcs[0], &loc),
                WriteRef => check_write_ref(target, code_offset, srcs[0], &loc),
                _ => (),
            }
        },
        _ => (),
    }
}

fn get_exit_state<'a>(target: &'a FunctionTarget<'a>) -> &'a ExitStateAnnotation {
    target
        .get_annotations()
        .get::<ExitStateAnnotation>()
        .expect("abort state annotation")
}

fn get_exit_state_at<'a>(
    target: &'a FunctionTarget<'a>,
    code_offset: CodeOffset,
) -> &'a ExitStateAtCodeOffset {
    get_exit_state(target)
        .get_annotation_at(code_offset)
        .expect("abort state")
}
