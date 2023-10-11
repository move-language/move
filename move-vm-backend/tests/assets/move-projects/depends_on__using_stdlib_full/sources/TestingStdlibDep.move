module TestingStdlibDep::StringAndVectorUser {
    fun call_test_vectors() {
        StdUser::StringAndVector::test_strings_and_vectors();
    }

    public fun sum(a: u64, b: u64): u64 {
        call_test_vectors();

        StdUser::StringAndVector::sum_after_vector_popping(a, b)
    }

    #[test]
    fun make_sure_non_zero_coin_passes() {
        assert!(sum(1, 1) == 2, 0);
    }
}
