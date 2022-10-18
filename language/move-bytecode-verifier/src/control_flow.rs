// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! This module implements a checker for verifies control flow. The following properties are
//! ensured:
//! - All forward jumps do not enter into the middle of a loop
//! - All "breaks" (forward, loop-exiting jumps) go to the "end" of the loop
//! - All "continues" (back jumps in a loop) are only to the current loop
use crate::loop_summary::{LoopPartition, LoopSummary};
use crate::verifier::VerifierConfig;
use move_binary_format::{
    binary_views::FunctionView,
    errors::{PartialVMError, PartialVMResult},
    file_format::{CodeOffset, CodeUnit, FunctionDefinitionIndex},
};
use move_core_types::vm_status::StatusCode;
use std::collections::BTreeSet;

pub fn verify_fallthrough(
    current_function_opt: Option<FunctionDefinitionIndex>,
    code: &CodeUnit,
) -> PartialVMResult<()> {
    let current_function = current_function_opt.unwrap_or(FunctionDefinitionIndex(0));
    // Check to make sure that the bytecode vector ends with a branching instruction.
    match code.code.last() {
        None => Err(PartialVMError::new(StatusCode::EMPTY_CODE_UNIT)),
        Some(last) if !last.is_unconditional_branch() => {
            Err(PartialVMError::new(StatusCode::INVALID_FALL_THROUGH)
                .at_code_offset(current_function, (code.code.len() - 1) as CodeOffset))
        }
        Some(_) => Ok(()),
    }
}

pub fn verify_reducibility<'a>(
    verifier_config: &VerifierConfig,
    function_view: &'a FunctionView<'a>,
) -> PartialVMResult<()> {
    let current_function = function_view.index().unwrap_or(FunctionDefinitionIndex(0));
    let err = move |code: StatusCode, offset: CodeOffset| {
        Err(PartialVMError::new(code).at_code_offset(current_function, offset))
    };

    let summary = LoopSummary::new(function_view.cfg());
    let mut partition = LoopPartition::new(&summary);

    for head in summary.preorder().rev() {
        let back = summary.back_edges(head);
        if back.is_empty() {
            continue;
        }

        let mut body = BTreeSet::new();
        for node in back {
            let node = partition.containing_loop(*node);

            if node != head {
                body.insert(node);
            }
        }

        let mut frontier: Vec<_> = body.iter().cloned().collect();
        while let Some(node) = frontier.pop() {
            for pred in summary.pred_edges(node) {
                let pred = partition.containing_loop(*pred);
                if !summary.is_descendant(head, pred) {
                    return err(StatusCode::INVALID_LOOP_SPLIT, summary.block(pred));
                }

                if pred != head && body.insert(pred) {
                    frontier.push(pred);
                }
            }
        }

        let depth = partition.collapse_loop(head, &body);
        if let Some(max_depth) = verifier_config.max_loop_depth {
            if depth as usize > max_depth {
                return err(StatusCode::LOOP_MAX_DEPTH_REACHED, summary.block(head));
            }
        }
    }

    Ok(())
}
