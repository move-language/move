module 0xff::M {
    #[open_struct]
    struct R has key { f: u64 }

    fun direct_static_borrow(addr: address): &R acquires R {
        let r = borrow_global<R>(addr);
        r
    }

    fun call_direct_static_borrow_conflict(): &R acquires R {
        let r = direct_static_borrow(@0x42);
        let _r2 = borrow_global_mut<R>(@0x42); // can't borrow as it is still borrowed
        r
    }

}
