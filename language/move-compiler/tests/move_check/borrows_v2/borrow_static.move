module 0x8675309::M {
    struct R has key { f: u64 }

    fun direct_static_borrow(addr: address): &R acquires R {
        let r = borrow_global<R>(addr);
        r
    }

    fun indirect_static_borrow(addr: address): &u64 acquires R {
        let r = borrow_global<R>(addr);
        &r.f
    }

    fun call_direct_static_borrow_missing_acquires(): &R acquires R {
        let r = direct_static_borrow(@0x42);
        r
    }
}
