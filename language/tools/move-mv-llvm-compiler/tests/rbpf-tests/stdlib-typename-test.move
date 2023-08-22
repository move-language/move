// use-stdlib

module 0xA::type_name_tests {
    use 0x1::type_name::{get, into_string};
    use 0x1::ascii::string;

    struct TestStruct {}

    struct TestGenerics<phantom T> { }

    struct TestMultiGenerics<phantom T1, phantom T2, phantom T3> { }

    public fun test_ground_types() {
        assert!(into_string(get<u8>()) == string(b"u8"), 0);
        assert!(into_string(get<u64>()) == string(b"u64"), 0);
        assert!(into_string(get<u128>()) == string(b"u128"), 0);
        assert!(into_string(get<address>()) == string(b"address"), 0);
        assert!(into_string(get<signer>()) == string(b"signer"), 0);
        assert!(into_string(get<vector<u8>>()) == string(b"vector<u8>"), 0)
    }

    // Original Note: these tests assume a 16 byte address length, and will fail on platforms where addresses are 20 or 32 bytes
    // Note: Updated to 32 bytes for Solana.
    public fun test_structs() {
        assert!(into_string(get<TestStruct>()) == string(b"000000000000000000000000000000000000000000000000000000000000000a::type_name_tests::TestStruct"), 0);
        assert!(into_string(get<0x1::ascii::String>()) == string(b"0000000000000000000000000000000000000000000000000000000000000001::ascii::String"), 0);
        assert!(into_string(get<0x1::option::Option<u64>>()) == string(b"0000000000000000000000000000000000000000000000000000000000000001::option::Option<u64>"), 0);
        assert!(into_string(get<0x1::string::String>()) == string(b"0000000000000000000000000000000000000000000000000000000000000001::string::String"), 0);
    }

    // Original Note: these tests assume a 16 byte address length, and will fail on platforms where addresses are 20 or 32 bytes
    // Note: Updated to 32 bytes for Solana.
    public fun test_generics() {
        assert!(into_string(get<TestGenerics<0x1::string::String>>()) == string(b"000000000000000000000000000000000000000000000000000000000000000a::type_name_tests::TestGenerics<0000000000000000000000000000000000000000000000000000000000000001::string::String>"), 0);
        assert!(into_string(get<vector<TestGenerics<u64>>>()) == string(b"vector<000000000000000000000000000000000000000000000000000000000000000a::type_name_tests::TestGenerics<u64>>"), 0);
        assert!(into_string(get<0x1::option::Option<TestGenerics<u8>>>()) == string(b"0000000000000000000000000000000000000000000000000000000000000001::option::Option<000000000000000000000000000000000000000000000000000000000000000a::type_name_tests::TestGenerics<u8>>"), 0);
    }

    // Original Note: these tests assume a 16 byte address length, and will fail on platforms where addresses are 20 or 32 bytes
    // Note: Updated to 32 bytes for Solana.
    public fun test_multi_generics() {
        assert!(into_string(get<TestMultiGenerics<bool, u64, u128>>()) == string(b"000000000000000000000000000000000000000000000000000000000000000a::type_name_tests::TestMultiGenerics<bool,u64,u128>"), 0);
        assert!(into_string(get<TestMultiGenerics<bool, vector<u64>, TestGenerics<u128>>>()) == string(b"000000000000000000000000000000000000000000000000000000000000000a::type_name_tests::TestMultiGenerics<bool,vector<u64>,000000000000000000000000000000000000000000000000000000000000000a::type_name_tests::TestGenerics<u128>>"), 0);
    }
}

script {
    use 0xA::type_name_tests as TT;

    fun main() {
        TT::test_ground_types();
        TT::test_structs();
        TT::test_generics();
        TT::test_multi_generics();
    }
}
