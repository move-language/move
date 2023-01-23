module 0xff::M {
    #[open_struct]
    struct R has key, drop { f: u64 }

    public fun direct_static_borrow(addr: address): &R acquires R {
        let r = borrow_global<R>(addr);
        r
    }

    public fun move_away(addr: address) acquires R {
        move_from<R>(addr);
    }

    public fun get(r: &R): u64 { r.f }
}

module 0xff::L {
    use 0xff::M;

    fun test(): u64 acquires M::R {
        let r = M::direct_static_borrow(@0x1);
        M::move_away(@0x1); // Expected to error because we borrow a R
        M::get(r)
    }
}
