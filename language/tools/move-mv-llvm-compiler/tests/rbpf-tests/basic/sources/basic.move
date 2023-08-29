// input ../input.json
// log 19

module 0x10::debug {
  native public fun print<T>(x: &T);
}

module basic::basic {
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
