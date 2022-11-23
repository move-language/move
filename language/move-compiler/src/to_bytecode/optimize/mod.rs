// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::parser::ast::FunctionName;
use move_ir_types::ast::{self as IR};
use std::collections::{BTreeSet, HashMap};

mod remove_fallthrough_jumps;
mod remove_unnecessary_store;
mod remove_unused_locals;

pub(crate) fn code(
    _f: &FunctionName,
    loop_heads: &BTreeSet<IR::BlockLabel_>,
    locals: &mut Vec<(IR::Var, IR::Type)>,
    blocks: &mut IR::BytecodeBlocks,
) {
    let mut changed = true;
    while changed {
        let fallthrough_or_block_removed = remove_fallthrough_jumps::code(loop_heads, blocks);
        let store_removed = remove_unnecessary_store::code(blocks);
        let local_removed = remove_unused_locals::code(locals, blocks);
        changed = fallthrough_or_block_removed || store_removed || local_removed;
    }
}

fn remap_labels(blocks: &mut IR::BytecodeBlocks, map: &HashMap<IR::BlockLabel_, IR::BlockLabel_>) {
    use IR::Bytecode_ as B;
    for (_, block) in blocks {
        for instr in block {
            match &mut instr.value {
                B::Branch(lbl) | B::BrTrue(lbl) | B::BrFalse(lbl) => {
                    *lbl = map[lbl].clone();
                }
                _ => (),
            }
        }
    }
}
