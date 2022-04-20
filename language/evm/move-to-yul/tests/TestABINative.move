#[contract]
module 0x2::M {
     use Evm::Evm::concat;
    use Std::Vector;

    #[decode]
    public native fun decode_two_u8(input: vector<u8>): (u8, u8);

    #[encode]
    public native fun encode_two_u8(v1: u8, v2: u8): vector<u8>;

    #[decode(sig=b"decode_two_bytes1(bytes) returns (bytes1, bytes1)")]
    public native fun decode_two_bytes1(input: vector<u8>) :(vector<u8>, vector<u8>);

    #[encode(sig=b"encode_two_bytes1(bytes1, bytes1) returns (bytes)")]
    public native fun encode_two_bytes1(input_1: vector<u8>, input_2: vector<u8>) : vector<u8>;

    #[encode_packed(sig=b"encode_packed(uint16, uint16) returns (bytes)")]
    public native fun encode_packed(input_1: u64, input_2: u64) : vector<u8>;

    #[encode_packed(sig=b"encode_packed_string(string, string) returns (bytes)")]
    public native fun encode_packed_string(input_1: vector<u8>, input_2: vector<u8>) : vector<u8>;

    #[evm_test]
    fun test_encode_packed_uint16() {
        let v1 = 41u64;
        let v2 = 42u64;
        let v = encode_packed(v1, v2);
        assert!(Vector::length(&v) == 4, 101);
    }

    #[evm_test]
    fun test_encode_packed_string() {
        assert!(encode_packed_string(b"", b"") == b"", 100);
        assert!(encode_packed_string(b"1", b"2") == b"12", 101);
        assert!(encode_packed_string(b"", b"abc") == b"abc", 102);
        assert!(encode_packed_string(encode_packed_string(b"a", b"bc"), b"de") == b"abcde", 103);
        assert!(encode_packed_string(b"test", b"") == b"test", 104);
    }

    #[evm_test]
    fun test_decode_two_bytes1() {
        let v = Vector::empty<u8>();
        Vector::push_back(&mut v, 42u8);
        let i = 1;
        while (i <= 31) {
            Vector::push_back(&mut v, 0);
            i = i + 1
        };
        Vector::push_back(&mut v, 43u8);
        let i = 1;
        while (i <= 31) {
            Vector::push_back(&mut v, 0);
            i = i + 1
        };
        let (v1, v2) = decode_two_bytes1(v);
        assert!(Vector::length(&v1) == 1, 101);
        assert!(Vector::length(&v2) == 1, 102);
        assert!(*Vector::borrow(&v1, 0) == 42, 103);
        assert!(*Vector::borrow(&v2, 0) == 43, 104);
    }

    #[evm_test]
    fun test_marshalling_two_bytes1() {
        let v = Vector::empty<u8>();
        Vector::push_back(&mut v, 42u8);
        let i = 1;
        while (i <= 31) {
            Vector::push_back(&mut v, 0);
            i = i + 1
        };
        Vector::push_back(&mut v, 43u8);
        let i = 1;
        while (i <= 31) {
            Vector::push_back(&mut v, 0);
            i = i + 1
        };
        let (v1, v2) = decode_two_bytes1(v);
        let v_ = encode_two_bytes1(v1, v2);
        assert!(v == v_, 101);
    }

    #[evm_test]
    fun test_decode_two_u8() {
        let v = Vector::empty<u8>();
        let i = 1;
        while (i <= 31) {
            Vector::push_back(&mut v, 0);
            i = i + 1
        };
        Vector::push_back(&mut v, 42u8);
        let i = 1;
        while (i <= 31) {
            Vector::push_back(&mut v, 0);
            i = i + 1
        };
        Vector::push_back(&mut v, 43u8);
        let (v1, v2) = decode_two_u8(v);
        assert!(v1 == 42, 101);
        assert!(v2 == 43, 102);
    }

    #[evm_test]
    fun test_marshalling_two_u8() {
        let v = Vector::empty<u8>();
        let i = 1;
        while (i <= 31) {
            Vector::push_back(&mut v, 0);
            i = i + 1
        };
        Vector::push_back(&mut v, 42u8);
        let i = 1;
        while (i <= 31) {
            Vector::push_back(&mut v, 0);
            i = i + 1
        };
        Vector::push_back(&mut v, 43u8);
        let (v1, v2) = decode_two_u8(v);
        assert!(v1 == 42, 101);
        assert!(v2 == 43, 102);
        let v_ = encode_two_u8(v1, v2);
        assert!(v == v_, 103)

    }

}
