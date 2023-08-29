module 0x10::debug {
  native public fun print<T>(x: &T);
}

module UnitTest::UnitTest {
    use 0x10::debug;

    public entry fun bar(): u64 {
        let rv = 19;
        debug::print(&rv);
        rv
    }

    #[test]
    fun test_bar() {
        let ret = bar();
        assert!(ret == 19, 0);
    }
}
