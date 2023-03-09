#[test_only]
module std::struct_tag_tests {
    use std::ascii::{Self, String};
    use std::option::{Option};
    use std::struct_tag;

    struct TestStruct has drop {}
    struct TestStructGeneric1<phantom T> has drop {}
    struct TestStructGeneric2<phantom X, phantom Y> has drop {}
    struct TestStructGeneric3<phantom X, phantom Y, phantom Z> has drop {}

    #[test]
    fun test_plain_struct() {        
        assert!(struct_tag::get<TestStruct>() == struct_tag::new_for_testing(@0x1, ascii::string(b"struct_tag_tests"), ascii::string(b"TestStruct"), vector[]), 0);
        assert!(struct_tag::get<String>() == struct_tag::new_for_testing(@0x1, ascii::string(b"ascii"), ascii::string(b"String"), vector[]), 0);
    }

    #[test]
    fun test_generic_struct() {
        // testing withbasic single generic
        let new_test_struct_1 = struct_tag::new_for_testing(
            @0x1, 
            ascii::string(b"struct_tag_tests"), 
            ascii::string(b"TestStructGeneric1"),
            vector[ascii::string(b"00000000000000000000000000000001::ascii::String")]
        );

        // testing with two generics
        let new_test_struct_2_a = struct_tag::new_for_testing(
            @0x1, 
            ascii::string(b"struct_tag_tests"), 
            ascii::string(b"TestStructGeneric2"),
            vector[
                ascii::string(b"00000000000000000000000000000001::ascii::String"),
                ascii::string(b"address")
            ]
        );

        // testing with two generics with nested generic
        let new_test_struct_2_b = struct_tag::new_for_testing(
            @0x1, 
            ascii::string(b"struct_tag_tests"), 
            ascii::string(b"TestStructGeneric2"),
            vector[
                ascii::string(b"00000000000000000000000000000001::ascii::String"),
                ascii::string(b"00000000000000000000000000000001::option::Option<u64>")
            ]
        );

        // testing with multiple(two or more) generics with nested generic
        let new_test_struct_3_a = struct_tag::new_for_testing(
            @0x1, 
            ascii::string(b"struct_tag_tests"), 
            ascii::string(b"TestStructGeneric3"),
            vector[
                ascii::string(b"00000000000000000000000000000001::ascii::String"),
                ascii::string(b"address"),
                ascii::string(b"00000000000000000000000000000001::option::Option<u64>")
            ]
        );

        // testing with multiple generics with nested two or more generics
        let new_test_struct_3_b = struct_tag::new_for_testing(
            @0x1, 
            ascii::string(b"struct_tag_tests"), 
            ascii::string(b"TestStructGeneric3"),
            vector[
                ascii::string(b"00000000000000000000000000000001::ascii::String"),
                ascii::string(b"address"),
                ascii::string(b"00000000000000000000000000000001::struct_tag_tests::TestStructGeneric2<00000000000000000000000000000001::ascii::String,00000000000000000000000000000001::option::Option<u8>>")
            ]
        );

        assert!(struct_tag::get<TestStructGeneric1<String>>() == new_test_struct_1, 0);
        assert!(struct_tag::get<TestStructGeneric2<String, address>>() == new_test_struct_2_a, 0);
        assert!(struct_tag::get<TestStructGeneric2<String, Option<u64>>>() == new_test_struct_2_b, 0);
        assert!(struct_tag::get<TestStructGeneric3<String, address, Option<u64>>>() == new_test_struct_3_a, 0);
        assert!(struct_tag::get<TestStructGeneric3<String, address, TestStructGeneric2<String, Option<u8>>>>() == new_test_struct_3_b, 0);
    }

    #[test]
    #[expected_failure(abort_code = 0, location = std::struct_tag_tests)]
    fun test_invalid_properties_failure() {
        // supplying invalid address
        let new_test_struct_1_a = struct_tag::new_for_testing(
            @0x2, 
            ascii::string(b"struct_tag_tests"), 
            ascii::string(b"TestStructGeneric1"),
            vector[ascii::string(b"00000000000000000000000000000001::ascii::String")]
        );

        // supplying invalid module name
        let new_test_struct_1_b = struct_tag::new_for_testing(
            @0x1, 
            ascii::string(b"fake_module_name"), 
            ascii::string(b"TestStructGeneric1"),
            vector[ascii::string(b"00000000000000000000000000000001::ascii::String")]
        );

        // supplying invalid struct name
        let new_test_struct_1_c = struct_tag::new_for_testing(
            @0x1, 
            ascii::string(b"struct_tag_tests"), 
            ascii::string(b"TestStructGeneric"),
            vector[ascii::string(b"00000000000000000000000000000001::ascii::String")]
        );

        // supplying invalid generic
        let new_test_struct_1_d = struct_tag::new_for_testing(
            @0x1, 
            ascii::string(b"fake_module_name"), 
            ascii::string(b"TestStructGeneric1"),
            vector[ascii::string(b"00000000000000000000000000000001::string::String")]
        );

        // supplying incorrectly positioned generics
        let new_test_struct_3_a = struct_tag::new_for_testing(
            @0x1, 
            ascii::string(b"struct_tag_tests"), 
            ascii::string(b"TestStructGeneric3"),
            vector[
                ascii::string(b"address"),
                ascii::string(b"00000000000000000000000000000001::option::Option<u64>"),
                ascii::string(b"00000000000000000000000000000001::ascii::String"),
            ]
        );

        // supplying incomplete generics
        let new_test_struct_3_b = struct_tag::new_for_testing(
            @0x1, 
            ascii::string(b"struct_tag_tests"), 
            ascii::string(b"TestStructGeneric3"),
            vector[
                ascii::string(b"00000000000000000000000000000001::ascii::String"),
                ascii::string(b"00000000000000000000000000000001::struct_tag_tests::TestStructGeneric2<00000000000000000000000000000001::ascii::String,00000000000000000000000000000001::option::Option<u8>>")
            ]
        );

        assert!(struct_tag::get<TestStructGeneric1<String>>() == new_test_struct_1_a, 0);
        assert!(struct_tag::get<TestStructGeneric1<String>>() == new_test_struct_1_b, 0);
        assert!(struct_tag::get<TestStructGeneric1<String>>() == new_test_struct_1_c, 0);
        assert!(struct_tag::get<TestStructGeneric1<String>>() == new_test_struct_1_d, 0);
        assert!(struct_tag::get<TestStructGeneric3<String, address, Option<u64>>>() == new_test_struct_3_a, 0);
        assert!(struct_tag::get<TestStructGeneric3<String, address, TestStructGeneric2<String, Option<u8>>>>() == new_test_struct_3_b, 0);
    }

    #[test]
    #[expected_failure(abort_code = 0, location = std::struct_tag)]
    fun test_invalid_struct_type_failure() {
        // supplying type that is not a struct
        struct_tag::get<u64>();
        struct_tag::get<address>();
    }
}
