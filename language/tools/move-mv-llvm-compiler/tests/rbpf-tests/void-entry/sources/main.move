// input ../input.json
// log 19

module 0x10::debug {
  native public fun print<T>(x: &T);
}

module void_entry::main {
    use 0x10::debug;

    public entry fun bar() {
        let rv = 19;
        debug::print(&rv);
    }
}
