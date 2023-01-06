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

    public fun push_3(s: &mut S): &mut S {
        spec {
            assert len(s.v) == 0;
        };
        vector::push_back(&mut s.v, 3);
        s
    }

    public fun push_and_pop_violation(s: &mut S) {
        spec {
            assert len(s.v) == 0;
        };
        vector::push_back(&mut s.v, 0);

        // NOTE: data invariant violation will be fired here even the overall
        // function body (i.e., with the subsequent pop) does not violate the
        // data invariant. This is currently a feature, not a bug, i.e.,
        // this is an intentional design choice as this is more conservative.
        //
        // 1) Should you prefer to allow temporary violation of data invariant
        // within a function, follow the example `push_and_pop_correct` below.
        //
        // 2) Should you prefer to have data invariant enforced when a `&mut`
        // param gets destroyed instead of when a `&mut` param is written-back
        // to, add an extra check in the `post_writeback_check_opt` derivation
        // section in `move-prover/bytecode/src/memory_instrumentation.rs`.

        vector::pop_back(&mut s.v);
    }

    public fun push_and_pop_correct(s: &mut S) {
        spec {
            assert len(s.v) == 0;
        };
        let v = &mut s.v;
        vector::push_back(v, 0);
        vector::pop_back(v);

        // NOTE: data invariant violation will not be fired in this case.
    }
}
