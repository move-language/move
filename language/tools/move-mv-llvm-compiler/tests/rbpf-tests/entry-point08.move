// input entry-point08.json
// log 17

module 0x10::debug {
  native public fun print<T>(x: &T);
}

module 0xa000::A {
    use 0x10::debug;

    public entry fun bar(): u64 {
        let rv = 123;
        debug::print(&rv);
        rv
    }
}

module 0xb000::B {
    use 0x10::debug;

    public entry fun foo(): u64 {
        let rv = 17;
        debug::print(&rv);
        rv
    }
}
