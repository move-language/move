module 0xff::M {

    #[no_borrow(i)]
    fun test1(_v: &vector<u64>, _i: &u64): &u64 {
        abort(22)
    }

    #[no_borrow]
    fun test2(_v: &vector<u64>, _i: &u64): &u64 {
        abort(22)
    }

    #[no_borrow(p=k)]
    fun test3(_v: &vector<u64>, _i: &u64): &u64 {
        abort(22)
    }
}
