module extensions::type_info {
    use std::string;

    // this is a mock of the type reflection scheme
    public native fun type_name<T>(): string::String;
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
}
