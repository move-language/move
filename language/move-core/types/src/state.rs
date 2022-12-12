use std::cell::RefCell;

#[derive(Clone, Copy, Debug)]
pub enum VMState {
    DESERIALIZER,
    VERIFIER,
    RUNTIME,
    OTHER,
}

thread_local! {
    static STATE: RefCell<VMState> = RefCell::new(VMState::OTHER);
}

pub fn set_state(state: VMState) -> VMState {
    STATE.with(|s| s.replace(state))
}

pub fn get_state() -> VMState {
    STATE.with(|s| *s.borrow())
}
