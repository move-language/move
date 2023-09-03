module 0x10::debug {
  native public fun print<T>(x: &T);
}

module UnitTest::UnitTest {
    use 0x10::debug;

    public entry fun bar(rv: u64): u64 {
        debug::print(&rv);
        rv
    }

    #[test]
    fun test_bar() {
        let ret = bar(17);
        assert!(ret == 17, 0);
    }

    #[test]
    fun test_foo() {
        let ret = bar(19);
        assert!(ret == 19, 1);
    }
}
