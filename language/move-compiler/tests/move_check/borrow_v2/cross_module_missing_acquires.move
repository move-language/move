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

    public fun use_static_borrow_mut(addr: address): &mut M::R {
        M::direct_static_borrow_mut(addr)
    }
}
