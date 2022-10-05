module extensions::type_info {
    use std::string;

    struct TypeInfo has copy, drop, store {
        account_address: address,
        module_name: vector<u8>,
        struct_name: vector<u8>,
    }

    // these are mocks of the type reflection scheme
    public native fun type_of<T>(): TypeInfo;
    public native fun type_name<T>(): string::String;

    public fun account_address(type_info: &TypeInfo): address {
        type_info.account_address
    }

    public fun module_name(type_info: &TypeInfo): vector<u8> {
        type_info.module_name
    }

    public fun struct_name(type_info: &TypeInfo): vector<u8> {
        type_info.struct_name
    }
}

module 0x42::test {
    use extensions::type_info;
    use std::string;

    struct MyTable<phantom K, phantom V> {}

    fun test_type_name_concrete(): string::String {
        spec {
            assert type_info::type_name<bool>().bytes == b"bool";
        };
        type_info::type_name<MyTable<vector<bool>, address>>()
    }
    spec test_type_name_concrete {
        ensures result.bytes == b"0x42::test::MyTable<vector<bool>, address>";
    }

    fun test_type_name_symbolic<T>(): string::String {
        spec {
            assert type_info::type_name<T>().bytes == type_info::type_name<T>().bytes;
        };
        type_info::type_name<MyTable<T, T>>()
    }
    spec test_type_name_symbolic {
        ensures result.bytes != b"vector<bool>";
        // TODO(mengxu): however, this ensures fails to verify.
        // Further investigation needed, could be issues with ConcatVec.
        // ensures result != type_info::type_name<vector<T>>();
    }

    fun test_type_info_concrete(): type_info::TypeInfo {
        spec {
            assert type_info::type_of<MyTable<address, u128>>().account_address == @0x42;
            assert type_info::type_of<MyTable<address, u128>>().module_name == b"test";
            assert type_info::type_of<MyTable<address, u128>>().struct_name == b"MyTable";
        };
        type_info::type_of<MyTable<vector<bool>, address>>()
    }
    spec test_type_info_concrete {
        ensures result.account_address == @0x42;
        ensures result.module_name == b"test";
        ensures result.struct_name == b"MyTable";
    }

    fun test_type_info_symbolic<T>(): type_info::TypeInfo {
        spec {
            assert type_info::type_of<T>().account_address == type_info::type_of<T>().account_address;
            assert type_info::type_of<T>().module_name == type_info::type_of<T>().module_name;
            assert type_info::type_of<T>().struct_name == type_info::type_of<T>().struct_name;
        };
        let info = type_info::type_of<T>();
        assert!(type_info::account_address(&info) == @0x42, 1);
        assert!(type_info::module_name(&info) == b"test", 2);
        assert!(type_info::struct_name(&info) == b"MyTable", 2);
        info
    }
    spec test_type_info_symbolic {
        ensures result.account_address == @0x42;
        ensures result.module_name == b"test";
        ensures result.struct_name == b"MyTable";
    }

    fun test_type_info_ignores_type_param<T>(): type_info::TypeInfo {
        type_info::type_of<MyTable<T, address>>()
    }
    spec test_type_info_ignores_type_param {
        ensures result == type_info::type_of<MyTable<address, T>>();
    }

    fun test_type_info_can_abort<T>(): type_info::TypeInfo {
        type_info::type_of<T>()
    }
    spec test_type_info_can_abort {
        // this should not pass
        aborts_if false;
    }
}
