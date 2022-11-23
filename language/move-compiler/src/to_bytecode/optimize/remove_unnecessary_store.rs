// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use move_ir_types::ast as IR;

// Removes any unnecessary store/move combo. This could mean moving/copying a value out of a local
// just to put it back in. Or storing a value in a local just to remove it.

pub fn code(blocks: &mut IR::BytecodeBlocks) -> bool {
    let mut changed = false;
    loop {
        let removed = remove_unnecessary_store(blocks);
        if !removed {
            break;
        }
        changed = true
    }
    changed
}

fn remove_unnecessary_store(blocks: &mut IR::BytecodeBlocks) -> bool {
    let mut changed = false;
    for (_lbl, block) in blocks {
        let mut new_block = vec![];
        let mut i = 0;
        while i < block.len() {
            match (&block[i], block.get(i + 1)) {
                (sp!(_, IR::Bytecode_::CopyLoc(v1)), Some(sp!(_, IR::Bytecode_::StLoc(v2))))
                | (sp!(_, IR::Bytecode_::MoveLoc(v1)), Some(sp!(_, IR::Bytecode_::StLoc(v2))))
                | (sp!(_, IR::Bytecode_::StLoc(v1)), Some(sp!(_, IR::Bytecode_::MoveLoc(v2))))
                    if v1 == v2 =>
                {
                    changed = true;
                    i += 2
                }
                (instr, _) => {
                    new_block.push(instr.clone());
                    i += 1
                }
            }
        }
        *block = new_block;
    }

    changed
}
