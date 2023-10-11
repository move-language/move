module StdUser::StringAndVector {
    use std::vector;
    use std::string;

    public fun test_strings_and_vectors() {
        let v = vector::empty<u32>();
        vector::push_back(&mut v, 5);
        vector::push_back(&mut v, 6);

        // Create a random string.
        let s = string::utf8(b"abcd");

        // Do some string operations
        let sub = string::sub_string(&s, 4, 4);
        assert!(string::is_empty(&sub), 22);

        assert!(*vector::borrow(&v, 0) == 5, 42);
        assert!(*vector::borrow(&v, 1) == 6, 42);
        assert!(vector::pop_back(&mut v) == 6, 42);
        assert!(vector::pop_back(&mut v) == 5, 42);
    }

    public fun sum_after_vector_popping(a: u64, b: u64): u64 {
        let v = vector::empty<u64>();
        vector::push_back(&mut v, a);
        vector::push_back(&mut v, b);

        vector::pop_back(&mut v) + vector::pop_back(&mut v)
    }
}
