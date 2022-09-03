module 0x2::M {
    struct S has drop {
      a: u64,
      b: bool,
      c: S2
    }

    struct S2 has drop {
        x: u128
    }

    // =============================================

    fun pack_S2(x: u128): S2 {
        S2{x}
    }
    #[evm_test]
    fun test_pack_S2() {
        let s = pack_S2(42);
        assert!(s.x == 42, 100)
    }
    #[evm_test]
    fun test_pack_S2_fail() {
        let s = pack_S2(42);
        assert!(s.x == 41, 100)
    }

    // =============================================

    fun pack_S(a: u64, b: bool): S {
        S{a, b, c: pack_S2((a as u128))}
    }
    #[evm_test]
    fun test_pack_S() {
        let s = pack_S(42, true);
        assert!(s.a == 42, 100);
        assert!(s.b == true, 101);
        assert!(s.c.x == 42, 102);
    }

    // =============================================

    fun read_S(s: &S): u64 {
        s.a + (s.c.x as u64)
    }
    #[evm_test]
    fun test_read_S() {
        let s = pack_S(42, true);
        assert!(read_S(&s) == 84, 100);
    }

    // =============================================

    fun write_S(s: &mut S, v: u64) {
        s.a = v;
        s.c.x = (s.a as u128);
    }
    #[evm_test]
    fun test_write_S() {
        let s = pack_S(42, true);
        write_S(&mut s, 43);
        assert!(s.a == 43, 100);
        assert!(s.c.x == 43, 101);
    }

    // =============================================

    fun read_and_write_S(): S {
        let s = pack_S(1, false);
        let x = read_S(&s);
        write_S(&mut s, x);
        s
    }
    #[evm_test]
    fun test_read_and_write_S() {
        let s = read_and_write_S();
        assert!(s.a == 2, 100);
        assert!(s.c.x == 2, 101);
    }


    // =============================================

    fun unpack(s: S): S2 {
        let S{a: _a, b: _b, c} = s;
        c
    }
    #[evm_test]
    fun test_unpack() {
        let s = pack_S(33, false);
        let s1 = unpack(s);
        assert!(s1.x == 33, 101);
    }
}
