module 0xff::M {
    // Without no_borrow attribute

    struct Collection {}

    fun borrow_mut(_s: &Collection, _i: &u64): &u64 {
        abort (22)
    }

    fun use_borrow_mut(_s: &Collection): &u64 {
        let key = 22;
        // borrow_mut creates a borrow dep from _both_ parameters because of the way
        // Move approxiomates this
        borrow_mut(_s, &key) // Invalid return because of still being borrowed
    }
}

module 0xff::N {
    // With no_borrow attribute

    struct Collection {}

    #[no_borrow(_i)]
    fun borrow_mut(_s: &Collection, _i: &u64): &u64 {
        abort(22)
    }

    fun use_borrow_mut(_s: &Collection): &u64 {
        let key = 22;
        // borrow_mut creates a borrow dep from _both_ parameters because of the way
        // Move approxiomates this
        borrow_mut(_s, &key)
    }
}
