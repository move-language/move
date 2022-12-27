/// Open structs cause pain because acquires logic is propagated outside of modules.
///
/// The below is a canonical example, which also shows why open structs need to be distinguished from opaque ones.
/// The resource owned by `M::R` is exclusively maintained by `M::set/get`. However since it the struct is open,
/// someone can also expose a reference at a later point as seen in fictious `M::borrow`. It becomes obvious we need to
/// take care of acquires as part of the type contract, that is why `#[open_struct]` is need as a way to declare
/// our intention.
module 0xff::M {
    #[open_struct]
    struct R has key { f: u64 }

    public fun set(x: u64) acquires R {
        borrow_global_mut<R>(@1).f = x
    }

    public fun get(): u64 acquires R {
        borrow_global<R>(@1).f
    }

    // In the future we may want to add:
    // public fun borrow(addr: address): &R acquires R {
    //     borrow_global<R>(addr)
    // }
}

module 0xff::L {
    use 0xff::M;

    fun test() {
        M::set(1);
        assert!(M::get() == 1, 1)
    }
}
