module StdNativesUser::DependsOnVector {
    fun call_test_vectors() {
        StdNativesUser::Vector::test_vectors();
    }

    public fun sum(a: u64, b: u64): u64 {
        call_test_vectors();

        StdNativesUser::Vector::sum_after_vector_popping(a, b)
    }

    #[test]
    fun make_sure_sum_works() {
        assert!(sum(1, 1) == 2, 0);
    }
}
