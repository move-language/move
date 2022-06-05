// This file contain various test cases for existentially quantifying vectors.
module 0x42::VectorExists {

    use std::vector;
    spec module {
        fun e_in_v_vec(e: u64, v: vector<u64>): bool {
            exists x in v: x == e
        }
        fun e_in_v_range(e: u64, v: vector<u64>): bool {
            exists i in 0..len(v): v[i] == e
        }
        fun e_in_v_u64(e: u64, v: vector<u64>): bool {
            exists i: u64 where 0 <= i && i < len(v): v[i] == e
        }
    }

    public fun do_nothing_ref(_v: &vector<u64>) {
    }
    spec do_nothing_ref {
        aborts_if false;

        ensures _v == _v;
        ensures exists l: u64: l == len(_v);
        ensures exists l: u64 where l == len(_v): l == len(_v);

        ensures e_in_v_vec(0, _v) ==> e_in_v_vec(0, _v);
        ensures e_in_v_range(0, _v) ==> e_in_v_range(0, _v);
        ensures e_in_v_u64(0, _v) ==> e_in_v_u64(0, _v);

        ensures forall e: u64: (e_in_v_vec(e, _v) ==> e_in_v_vec(e, _v));
        ensures forall e: u64: (e_in_v_range(e, _v) ==> e_in_v_range(e, _v));
        ensures forall e: u64: (e_in_v_u64(e, _v) ==> e_in_v_u64(e, _v));
    }

    public fun do_nothing_ref_mut(_v: &mut vector<u64>) {
    }
    spec do_nothing_ref_mut {
        aborts_if false;

        ensures old(_v) == _v;
        ensures exists l: u64: l == len(old(_v));
        ensures exists l: u64: l == len(_v);
        ensures exists l: u64 where l == len(old(_v)): l == len(_v);

        ensures old(e_in_v_vec(0, _v)) ==> e_in_v_vec(0, _v);
        ensures old(e_in_v_range(0, _v)) ==> e_in_v_range(0, _v);
        ensures old(e_in_v_u64(0, _v)) ==> e_in_v_u64(0, _v);

        ensures forall e: u64: (old(e_in_v_vec(e, _v)) ==> e_in_v_vec(e, _v));
        ensures forall e: u64: (old(e_in_v_range(e, _v)) ==> e_in_v_range(e, _v));
        ensures forall e: u64: (old(e_in_v_u64(e, _v)) ==> e_in_v_u64(e, _v));
    }

    public fun push_one(v: &mut vector<u64>) {
        vector::push_back(v, 1);
    }
    spec push_one {
        pragma verify=false;
        aborts_if false;
        ensures v[len(v)-1] == 1;

        ensures e_in_v_vec(1, v);
        ensures e_in_v_range(1, v);
        ensures exists i in 0..len(v): v[i] == 1;

        ensures exists i: u64 where (i == len(v)-1): v[i] == 1;

        ensures exists i: u64 : v[i] == 1; // FIXME: not verified, but should be.
        ensures e_in_v_u64(1, v); // FIXME: not verified, but should be.

        ensures old(e_in_v_vec(0, v)) ==> e_in_v_vec(0, v);
        ensures old(e_in_v_range(0, v)) ==> e_in_v_range(0, v);
        ensures old(e_in_v_u64(0, v)) ==> e_in_v_u64(0, v);
    }
}
