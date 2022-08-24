module std::base16 {
    use std::vector;

    const ELAYERZERO_INVALID_LENGTH: u64 = 0;
    const ELAYERZERO_INVALID_CHARACTER: u64 = 1;

    const BASE16_CHARS: vector<u8> = b"0123456789abcdef";
    const DECODE_LUT: vector<u8> = vector<u8>[
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 1, 2,  3, 4, 5,
    6, 7,  8, 9, 255, 255, 255, 255, 255, 255, 255, 10, 11, 12, 13, 14, 15, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 10, 11, 12, 13, 14, 15, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255
    ];

    public fun encode(bytes: vector<u8>): vector<u8> {
        let retval = vector::empty<u8>();
        let n = vector::length(&bytes);
        let i = 0u64;
        while (i < n) {
            let v = vector::borrow(&bytes, i);
            let c1 = vector::borrow(&BASE16_CHARS, ((*v >> 4) as u64));
            let c2 = vector::borrow(&BASE16_CHARS, ((*v & 0x0f) as u64));
            vector::push_back(&mut retval, *c1);
            vector::push_back(&mut retval, *c2);
            i = i + 1;
        };
        return retval
    }

    public fun decode(bytes: vector<u8>): vector<u8> {
        let retval = vector::empty<u8>();
        let n = vector::length(&bytes);
        assert!(n & 1 == 0, ELAYERZERO_INVALID_LENGTH);
        let i = 0u64;
        while (i < n) {
            let s0 = vector::borrow(&bytes, i);
            let s1 = vector::borrow(&bytes, i + 1);
            let r0 = vector::borrow(&DECODE_LUT, ((*s0) as u64));
            let r1 = vector::borrow(&DECODE_LUT, ((*s1) as u64));
            assert!((*r0 | *r1) < 128u8, ELAYERZERO_INVALID_CHARACTER);
            let v = (((*r0 << 4) | *r1) as u8);
            vector::push_back(&mut retval, v);
            i = i + 2;
        };
        return retval
    }

    #[test_only]
    const ELAZERZERO_INVALID_LENGTH: u64 = 90;
    const ELAYERZERO_NOT_MATCH: u64 = 91;

    #[test_only]
    fun equal(v1: &vector<u8>, v2: &vector<u8>): bool {
        let n = vector::length(v1);
        let m = vector::length(v2);
        if (n != m) return false;

        let i = 0u64;
        while (i < n) {
            let v1 = vector::borrow(v1, i);
            let v2 = vector::borrow(v2, i);
            if (*v1 != *v2)  return false;
            i = i + 1;
        };
        return true
    }

    #[test]
    fun test_encode() {
        let bytes = x"0123456789abcdef";
        assert!(vector::length(&bytes) == 8, ELAZERZERO_INVALID_LENGTH);

        let encoded = encode(bytes);
        assert!(vector::length(&encoded) == 2 * vector::length(&bytes), ELAZERZERO_INVALID_LENGTH);

        let decoded = decode(encoded);
        assert!(equal(&bytes, &decoded) == true, ELAYERZERO_NOT_MATCH);
    }
}
