module 0x10::debug {
  native public fun print<T>(x: &T);
}

module multi_module::A {
    use 0x10::debug;
    use std::signer;

    public entry fun bar(): u64 {
        let rv = 19;
        debug::print(&rv);
        rv
    }

    public entry fun foo(account: &signer): u64 {
        let rv = 17;
        debug::print(&rv);
        debug::print(&signer::address_of(account));
        rv
    }
}

module multi_module::B {
    use 0x10::debug;

    public entry fun foo_bar() {
        let val = 17;
        debug::print(&val);
    }
}
