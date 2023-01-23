module 0xff::M {
    #[open_struct]
    struct R has key { f: u64 }

    public fun direct_static_borrow_mut(addr: address): &mut R acquires R {
        let r = borrow_global_mut<R>(addr);
        r
    }
}

module 0xff::L {
    use 0xff::M;

    fun call_direct_static_borrow_conflict(): &M::R acquires M::R {
        let r = M::direct_static_borrow_mut(@0x42);
        // Below should not be allowed
        let _r2 = M::direct_static_borrow_mut(@0x42);
        r
    }
}
