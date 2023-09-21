module 0x10::debug {
  native public fun print<T>(x: &T);
}

module UnitTest::UnitTest {
    use 0x10::debug;

    const ONE: u64 = 0x20001;
    const TWO: u64 = 0x20002;

    public entry fun bar(rv: u64): u64 {
        debug::print(&rv);
        rv
    }

    #[test]
    fun test_bar() {
        let ret = bar(17);
        assert!(ret == 17, UnitTest::UnitTest::ONE);
    }

    #[test, expected_failure(abort_code = UnitTest::UnitTest::TWO)]
    fun test_foo() {
        let ret = bar(19);
        assert!(ret == 17, UnitTest::UnitTest::TWO);
    }
}
