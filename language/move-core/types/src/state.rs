// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VMState {
    DESERIALIZER,
    VERIFIER,
    RUNTIME,
    OTHER,
}

// Small TODO: We should be safe here in a single-threaded wasm environment - but we might
// need to use RefCell for move-compiler though
static mut STATE: VMState = VMState::OTHER;

pub fn set_state(state: VMState) -> VMState {
    unsafe {
        STATE = state;
    }
    state
}

pub fn get_state() -> VMState {
    unsafe { STATE }
}
