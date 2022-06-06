module 0x1::A {
    use std::vector;

    #[test]
    fun native_abort_unexpected_abort() {
        vector::borrow(&vector::empty<u64>(), 1);
    }

    #[test]
    #[expected_failure(abort_code = 0)]
    fun native_abort_good_wrong_code() {
        vector::borrow(&vector::empty<u64>(), 1);
    }

    #[test]
    #[expected_failure(abort_code = 1)]
    fun native_abort_good_right_code() {
        vector::borrow(&vector::empty<u64>(), 1);
    }
}
