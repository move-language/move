module 0x1::FortyTwo {
    use Evm::Evm::{emit};

    #[event]
    struct SimpleEvent {
        x: u64,
    }

    #[callable(sig=b"emitNothing(uint64)")]
    public fun emitNothing(_x: u64) {
    }

    #[callable(sig=b"emitEvent(uint64)")]
    public fun emitSimpleEvent(x: u64) {
        emit(SimpleEvent{x});
    }

    #[callable(sig=b"emitEventTwice(uint64)")]
    public fun emitSimpleEventTwice(x: u64) {
        emit(SimpleEvent{x});
        emit(SimpleEvent{x: x+x});
    }

    // TODO: move-to-yul does not support events with string args.
    // #[event]
    // struct MyEvent {
    //     x: u64,
    //     message: vector<u8>,
    // }

    // #[callable(sig=b"emitMyEvent(uint64)")]
    // public fun emitMyEvent(x: u64) {
    //     emit(MyEvent{x, message: b"hello_event"});
    // }

    // #[callable(sig=b"emitMyEventTwice(uint64)")]
    // public fun emitMyEventTwice(x: u64) {
    //     emit(MyEvent{x, message: b"hello_event_#1"});
    //     emit(MyEvent{x+x, message: b"hello_event_#2"});
    // }
}
