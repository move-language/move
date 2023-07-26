// input entry-point02.json
// log 123

module 0x10::debug {
  native public fun print<T>(x: &T);
}

module 0xa000::entry_point {
    use 0x10::debug;

    public entry fun bar(): u64 {
        let rv = 123;
        debug::print(&rv);
        rv
    }

    public entry fun foo(): u64 {
        let rv = 17;
        debug::print(&rv);
        rv
    }
}
