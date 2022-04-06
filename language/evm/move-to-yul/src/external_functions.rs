// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    context::Context,
    dispatcher_generator::TARGET_CONTRACT_DOES_NOT_CONTAIN_CODE,
    functions::FunctionGenerator,
    solidity_ty::{abi_head_sizes_sum, SoliditySignature, SolidityType},
    yul_functions::{substitute_placeholders, YulFunction},
};
use itertools::Itertools;
use move_model::{
    emit, emitln,
    model::{FunId, FunctionEnv, QualifiedInstId},
};
use move_stackless_bytecode::function_target_pipeline::FunctionVariant;
use sha3::{Digest, Keccak256};

/// Generate external functions
pub(crate) fn define_external_fun(
    gen: &mut FunctionGenerator,
    ctx: &Context,
    fun_id: &QualifiedInstId<FunId>,
    solidity_sig_str: &str,
) {
    let fun = ctx.env.get_function(fun_id.to_qualified_id());
    let mut sig = SoliditySignature::create_default_solidity_signature(ctx, &fun);
    let parsed_sig_opt = SoliditySignature::parse_into_solidity_signature(solidity_sig_str, &fun);
    // Check compatibility
    if let Ok(parsed_sig) = parsed_sig_opt {
        if !parsed_sig.check_sig_compatibility_for_external_fun(ctx, &fun) {
            ctx.env.error(
                &fun.get_loc(),
                "solidity signature is not compatible with the move signature",
            );
            return;
        } else {
            sig = parsed_sig;
        }
    } else if let Err(msg) = parsed_sig_opt {
        ctx.env.error(&fun.get_loc(), &format!("{}", msg));
        return;
    }

    let target = &ctx.targets.get_target(&fun, &FunctionVariant::Baseline);

    // Emit function header
    let params = (0..target.get_parameter_count())
        .map(|idx| ctx.make_local_name(target, idx))
        .join(", ");
    let results = if target.get_return_count() == 0 {
        "".to_string()
    } else {
        (0..target.get_return_count())
            .map(|i| ctx.make_result_name(target, i))
            .join(", ")
    };
    let ret_results = if results.is_empty() {
        "".to_string()
    } else {
        format!(" -> {} ", results)
    };
    emit!(
        ctx.writer,
        "function {}({}){} ",
        ctx.make_function_name(fun_id),
        params,
        ret_results
    );
    let failure_call = gen.parent.call_builtin_str(
        ctx,
        YulFunction::Abort,
        std::iter::once(TARGET_CONTRACT_DOES_NOT_CONTAIN_CODE.to_string()),
    );
    let revert_forward =
        gen.parent
            .call_builtin_str(ctx, YulFunction::RevertForward, vec![].into_iter());
    // Prepare variables used in the function
    let contract_addr_var = ctx.make_local_name(target, 0); // the first parameter is the address of the target contract
    let mut local_name_idx = target.get_parameter_count(); // local variable
    let pos_var = ctx.make_local_name(target, local_name_idx);
    local_name_idx += 1;
    let end_var = ctx.make_local_name(target, local_name_idx);
    local_name_idx += 1;
    let success_var = ctx.make_local_name(target, local_name_idx);
    // Generate the function body
    ctx.emit_block(|| {
        if target.get_return_count() == 0 {
            // Check extcodesize if no return data is expected
            emitln!(
                ctx.writer,
                "if iszero(extcodesize({})) {{ {} }}",
                contract_addr_var,
                failure_call
            );
        }
        emitln!(ctx.writer, "// storage for arguments and returned data");
        emitln!(
            ctx.writer,
            "let {} := mload({})",
            pos_var,
            substitute_placeholders("${MEM_SIZE_LOC}").unwrap()
        );

        let fun_sig = format!("{}", sig);
        let function_selector =
            format!("0x{:x}", Keccak256::digest(fun_sig.as_bytes()))[..10].to_string();
        let para_vec = vec![function_selector, "224".to_string()];
        let shl224 = gen
            .parent
            .call_builtin_str(ctx, YulFunction::Shl, para_vec.iter().cloned());

        emitln!(ctx.writer, "mstore({}, {})", pos_var, shl224);
        let mut encode_params = "".to_string();
        if target.get_parameter_count() > 1 {
            encode_params = format!(
                ", {}",
                (1..target.get_parameter_count())
                    .map(|idx| ctx.make_local_name(target, idx))
                    .join(", ")
            );
        }

        let mut para_types = fun.get_parameter_types();
        if para_types.len() > 1 {
            para_types = para_types[1..].to_vec();
        } else {
            para_types = vec![];
        }
        let sig_para_vec = sig
            .para_types
            .clone()
            .into_iter()
            .map(|(ty, _, _)| ty)
            .collect::<Vec<_>>();
        let sig_para_locs = sig
            .para_types
            .clone()
            .into_iter()
            .map(|(_, _, loc)| loc)
            .collect_vec();
        let encode =
            gen.parent
                .generate_abi_tuple_encoding(ctx, sig_para_vec, sig_para_locs, para_types);
        emitln!(
            ctx.writer,
            "let {} := {}(add({}, 4){})",
            end_var,
            encode,
            pos_var,
            encode_params
        );

        // Make the call
        let mut call = "call".to_string();
        let is_delegatecall = is_delegate(ctx, fun_id);
        let is_staticcall = is_static(ctx, fun_id);
        if is_delegatecall {
            call = "delegatecall".to_string();
        } else if is_staticcall {
            call = "staticcall".to_string();
        }
        let gas = "gas()".to_string(); // TODO: set gas?
        let mut value = "".to_string();
        if !is_delegatecall && !is_staticcall {
            value = "0, ".to_string(); // TODO: allow sending eth along with making the external call?
        }
        let sig_ret_vec = sig
            .ret_types
            .clone()
            .into_iter()
            .map(|(ty, _)| ty)
            .collect::<Vec<_>>();
        let dynamic_return = check_dynamic(&sig_ret_vec);
        let estimated_size = if dynamic_return {
            0
        } else {
            abi_head_sizes_sum(&sig_ret_vec, true)
        };
        emitln!(
            ctx.writer,
            "let {} := {}({}, {}, {} {}, sub({}, {}), {}, {})",
            success_var,
            call,
            gas,
            contract_addr_var,
            value,
            pos_var,
            end_var,
            pos_var,
            pos_var,
            estimated_size
        );
        emitln!(
            ctx.writer,
            "if iszero({}) {{ {} }}",
            success_var,
            revert_forward
        ); // TODO: try call
        emit!(ctx.writer, "if {} ", success_var);
        ctx.emit_block(|| {
            if dynamic_return {
                emitln!(ctx.writer, "// copy dynamic return data out");
                emitln!(
                    ctx.writer,
                    "returndatacopy({}, 0, returndatasize())",
                    pos_var
                );
            }
            emitln!(
                ctx.writer,
                "// update freeMemoryPointer according to dynamic return size"
            );
            let return_size = "returndatasize()".to_string();
            gen.parent.call_builtin(
                ctx,
                YulFunction::MallocAt,
                vec![pos_var.clone(), return_size.clone()].into_iter(),
            );
            emitln!(
                ctx.writer,
                "// decode return parameters from external try-call into retVars"
            );
            if !results.is_empty() {
                emit!(ctx.writer, "{} := ", results);
            }
            let abi_decode_from_memory =
                gen.parent
                    .generate_abi_tuple_decoding_ret(ctx, &sig, fun.get_return_types(), true);
            emitln!(
                ctx.writer,
                "{}({}, add({}, {}))",
                abi_decode_from_memory,
                pos_var,
                pos_var,
                return_size
            );
        });
    });
}

fn check_dynamic(types: &[SolidityType]) -> bool {
    types.iter().any(|a| !a.is_static())
}

/// Placeholder for checking whether to make a delegate call
fn is_delegate(_ctx: &Context, _fun_id: &QualifiedInstId<FunId>) -> bool {
    false
}

/// Placeholder for checking whether to make a static call
fn is_static(_ctx: &Context, _fun_id: &QualifiedInstId<FunId>) -> bool {
    false
}

impl SoliditySignature {
    /// Check whether the user defined solidity signature is compatible with the Move signature
    pub(crate) fn check_sig_compatibility_for_external_fun(
        &self,
        ctx: &Context,
        fun: &FunctionEnv<'_>,
    ) -> bool {
        let para_types = fun.get_parameter_types();
        let sig_para_vec = self
            .para_types
            .iter()
            .map(|(ty, _, _)| ty)
            .collect::<Vec<_>>();
        if para_types.len() != sig_para_vec.len() + 1 {
            // the extra (first) parameter of the move function is the address of the target contract
            return false;
        }
        if !para_types[0].is_address() {
            return false;
        }
        // Check parameter type list
        for type_pair in para_types[1..].iter().zip(sig_para_vec.iter()) {
            let (m_ty, s_ty) = type_pair;
            if !s_ty.check_type_compatibility(ctx, m_ty) {
                return false;
            }
        }
        // Check return type list
        // TODO: try_call
        let sig_ret_vec = self.ret_types.iter().map(|(ty, _)| ty).collect::<Vec<_>>();
        let ret_types = fun.get_return_types();
        if ret_types.len() != sig_ret_vec.len() {
            return false;
        }
        for type_pair in ret_types.iter().zip(sig_ret_vec.iter()) {
            let (m_ty, s_ty) = type_pair;
            if !s_ty.check_type_compatibility(ctx, m_ty) {
                return false;
            }
        }
        true
    }
}
