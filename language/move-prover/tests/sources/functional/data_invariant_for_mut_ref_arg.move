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
        spec {
            assert len(s.v) == 0;
        };
        vector::push_back(&mut s.v, 1);
    }

    public fun push_2(s: &mut S) {
        spec {
            assert len(s.v) == 0;
        };
        vector::push_back(&mut s.v, 2);
        let t = freeze(s);
        let _ = vector::length(&t.v);
    }
}
