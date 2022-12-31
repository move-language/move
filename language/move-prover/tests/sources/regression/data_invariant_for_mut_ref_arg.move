module 0x42::struct_invariant_mut_ref_param {
    use std::vector;

    struct S {
        v: vector<u64>,
    }
    spec S {
        invariant len(v) == 0;
    }

    public fun empty(): S {
        S { v: vector::empty<u64>() }
    }

    public fun push_1(s: &mut S) {
        // NOTE: this inline spec block should not be
        // proved becauuse `s: &mut S` might not be in
        // a consistent state with the data invariant.
        // This is because data invariants are only
        // asserted when the mutable borrow ends.
        spec {
            assert len(s.v) == 0;
        };
        vector::push_back(&mut s.v, 1);
    }
}
