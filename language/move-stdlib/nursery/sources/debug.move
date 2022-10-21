/// Module providing debug functionality.
module std::debug {
    native public fun print<T>(x: &T);

    native public fun print_stack_trace();

    #[test_only]
    use std::string;

    #[test]
    fun test_print_string() {
        let str_bytes = b"Hello, sane Move debugging!";

        print(&str_bytes);

        let str = string::utf8(str_bytes);
        print<string::String>(&str);
    }
}
