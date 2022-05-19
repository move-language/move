address 0x2 {
module Events {
    use std::event;
    use std::signer;

    struct AnEvent has copy, drop, store { i: u64 }
    struct Handle has key{ h: event::EventHandle<AnEvent> }

    public fun emit(account: &signer, i: u64) acquires Handle {
        let addr = signer::address_of(account);
        if (!exists<Handle>(addr)) {
            move_to(account, Handle { h: event::new_event_handle(account) })
        };

        let handle = borrow_global_mut<Handle>(addr);

        event::emit_event(&mut handle.h, AnEvent { i })
    }
}
}
