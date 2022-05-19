module 0x2::A {
    use std::event;

    struct MyEvent<phantom T> has copy, drop, store { b: bool }

    public fun do_emit<T: copy + drop + store>(account: &signer) {
        let handle = event::new_event_handle<MyEvent<T>>(account);
        event::emit_event(&mut handle, MyEvent{ b: true });
        event::destroy_handle(handle);
    }

    #[test(a=@0x2)]
    public fun emit(a: &signer) {
        do_emit<u64>(a);
    }
}
