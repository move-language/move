module 0xff::M {
    struct R has key { f: u64 }

    fun borrow(addr: address): &R acquires R {
        borrow_global<R>(addr)
    }

    fun call_borrow(): &R acquires R {
        borrow(@0x42) // expect to be complained no open struct
    }
}
