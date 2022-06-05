// This file consists of a series of test cases which are client functions
// using the standard vector module.
module 0x42::TestVector {
    use std::vector;

    spec module {
        pragma verify = true;
    }

    // -----------------------------
    // Testing with concrete vectors
    // -----------------------------

    fun test_vector_equal(_v: vector<u64>, _w: &mut vector<u64>) {
    }
    spec test_vector_equal {
        aborts_if false;
        ensures _v == _v;
        ensures _v == _v[0..len(_v)];
        ensures _w == _w;
        ensures old(_w) == old(_w);
        ensures _w == _w[0..len(_w)];
        ensures old(_w) == old(_w[0..len(_w)]);
    }

    // succeeds. [] == [].
    fun test_empty() : (vector<u64>, vector<u64>)
    {
        let ev1 = vector::empty<u64>();
        let ev2 = vector::empty<u64>();
        (ev1, ev2)
    }
    spec test_empty {
        ensures result_1 == result_2;
        ensures len(result_1) == 0;
        ensures len(result_2) == 0;
    }

    // succeeds. [1] == [1]
    fun test_push() : (vector<u64>, vector<u64>)
    {
        let ev1 = vector::empty<u64>();
        let ev2 = vector::empty<u64>();
        vector::push_back(&mut ev1, 1);
        vector::push_back(&mut ev2, 1);
        (ev1, ev2)
    }
    spec test_push {
        ensures result_1 == result_2;
        ensures len(result_1) == 1;
        ensures len(result_2) == 1;
    }

    //succeeds. [] == [].
    fun test_push_pop() : (vector<u64>, vector<u64>)
    {
        let ev1 = vector::empty<u64>();
        let ev2 = vector::empty<u64>();
        vector::push_back(&mut ev1, 1);
        vector::pop_back(&mut ev1);
        (ev1, ev2)
    }
    spec test_push_pop {
        ensures result_1 == result_2;
    }

    //succeeds. [1,2] != [1].
    fun test_neq1() : (vector<u64>, vector<u64>)
    {
        let ev1 = vector::empty<u64>();
        let ev2 = vector::empty<u64>();
        vector::push_back(&mut ev1, 1);
        vector::push_back(&mut ev1, 2);
        vector::push_back(&mut ev2, 1);
        (ev1, ev2)
    }
    spec test_neq1 {
        ensures result_1 != result_2;
    }

    // succeeds. [1] == [0]
    fun test_neq2() : (vector<u64>, vector<u64>)
    {
        let ev1 = vector::empty<u64>();
        let ev2 = vector::empty<u64>();
        vector::push_back(&mut ev1, 1);
        vector::push_back(&mut ev2, 0);
        (ev1, ev2)
    }
    spec test_neq2 {
        ensures result_1 != result_2;
    }

    // succeeds. reverse([]) == [].
    fun test_reverse1() : (vector<u64>, vector<u64>)
    {
        let ev1 = vector::empty<u64>();
        let ev2 = vector::empty<u64>();
        vector::reverse(&mut ev1);
        (ev1, ev2)
    }
    spec test_reverse1 {
        ensures result_1 == result_2;
    }

    // succeeds. reverse([1,2]) == [2,1].
    fun test_reverse2() : (vector<u64>, vector<u64>)
    {
        let ev1 = vector::empty<u64>();
        let ev2 = vector::empty<u64>();
        vector::push_back(&mut ev1, 1);
        vector::push_back(&mut ev1, 2);
        vector::push_back(&mut ev2, 2);
        vector::push_back(&mut ev2, 1);
        vector::reverse(&mut ev1);
        (ev1, ev2)
    }
    spec test_reverse2 {
        ensures result_1 == result_2;
    }

    // succeeds. reverse([1,2]) != [1,2].
    fun test_reverse3() : (vector<u64>, vector<u64>)
    {
        let ev1 = vector::empty<u64>();
        let ev2 = vector::empty<u64>();
        vector::push_back(&mut ev1, 1);
        vector::push_back(&mut ev1, 2);
        vector::push_back(&mut ev2, 1);
        vector::push_back(&mut ev2, 2);
        vector::reverse(&mut ev1);
        (ev1, ev2)
    }
    spec test_reverse3 {
        ensures result_1 != result_2;
    }

    // succeeds. swap([1,2],0,1) == [2,1].
    fun test_swap() : (vector<u64>, vector<u64>)
    {
        let ev1 = vector::empty<u64>();
        let ev2 = vector::empty<u64>();
        vector::push_back(&mut ev1, 1);
        vector::push_back(&mut ev1, 2);
        vector::push_back(&mut ev2, 2);
        vector::push_back(&mut ev2, 1);
        vector::swap(&mut ev1, 0, 0);
        vector::swap(&mut ev1, 0, 1);
        (ev1, ev2)
    }
    spec test_swap {
        ensures result_1 == result_2;
    }

    // succeeds. Always aborts because the first index argument of `swap` is out-of-bounds.
    fun test_swap_abort1()
    {
        let ev1 = vector::empty<u64>();
        vector::push_back(&mut ev1, 1);
        vector::swap(&mut ev1, 1, 0);
    }
    spec test_swap_abort1 {
        aborts_if true;
    }

    // succeeds. Always aborts because the second index argument of `swap` is out-of-bounds.
    fun test_swap_abort2()
    {
        let ev1 = vector::empty<u64>();
        vector::push_back(&mut ev1, 1);
        vector::swap(&mut ev1, 0, 1);
    }
    spec test_swap_abort2 {
        aborts_if true;
    }

    // succeeds. len([1]) = len([]) + 1.
    fun test_length1() : (u64, u64)
    {
        let ev1 = vector::empty<u64>();
        let ev2 = vector::empty<u64>();
        vector::push_back(&mut ev1, 1);
        (vector::length(&ev1), vector::length(&ev2))
    }
    spec test_length1 {
        ensures result_1 == result_2 + 1;
    }

    fun vector_of_proper_positives(): vector<u64> {
        let v = vector::empty();
        vector::push_back(&mut v, 1);
        vector::push_back(&mut v, 2);
        vector::push_back(&mut v, 3);
        v
    }
    spec vector_of_proper_positives {
      ensures forall n in result: n > 0;
      ensures forall i in 0..len(result), j in 0..len(result) where result[i] == result[j] : i == j;
    }

    // succeeds. 7 == 7.
    fun test_borrow1() : u64
    {
        let v = vector::empty<u64>();
        vector::push_back(&mut v, 7);
        *vector::borrow(&v, 0)
    }
    spec test_borrow1 {
        ensures result == 7;
    }

    // succeeds. 7 != 7.
    fun test_borrow2() : u64
    {
        let v = vector::empty<u64>();
        vector::push_back(&mut v, 0);
        *vector::borrow(&v, 0)
    }
    spec test_borrow2 {
        ensures result != 7;
    }

    // succeeds. Always aborts due to the out-of-bounds index used.
    fun test_borrow3() : u64
    {
        let v = vector::empty<u64>();
        vector::push_back(&mut v, 7);
        *vector::borrow(&v, 1)
    }
    spec test_borrow3 {
        aborts_if true;
    }

    fun test_slice() : (vector<u64>,vector<u64>)
    {
        let ev1 = vector::empty<u64>();
        let ev2 = vector::empty<u64>();
        vector::push_back(&mut ev1, 1);
        vector::push_back(&mut ev1, 2);
        vector::push_back(&mut ev2, 0);
        vector::push_back(&mut ev2, 1);
        vector::push_back(&mut ev2, 2);
        vector::push_back(&mut ev2, 3);
        vector::push_back(&mut ev2, 1);
        vector::push_back(&mut ev2, 2);
        (ev1, ev2)
    }
    spec test_slice {
        ensures result_1 == result_2[1..3];
        ensures result_1 != result_2[0..2];
        ensures result_1 == result_2[4..6];
        ensures result_1[0..2] == result_2[4..6];
        ensures result_1[1..2] == result_2[2..3];
        ensures result_2[1..3] == result_2[4..6];
    }

    fun test_contains() : (vector<u64>, bool, bool)
    {
        let b1: bool;
        let b2: bool;
        let ev1 = vector::empty<u64>();
        vector::push_back(&mut ev1, 1);
        vector::push_back(&mut ev1, 2);
        vector::push_back(&mut ev1, 3);
        vector::push_back(&mut ev1, 5);
        b1 = vector::contains(&ev1, &3);
        b2 = vector::contains(&ev1, &4);
        (ev1, b1, b2)
    }
    spec test_contains {
        aborts_if false;
        ensures result_2 == true;
        ensures result_3 == false;
        ensures len(result_1) == 4;
        ensures result_1[0] == 1; // FIXME: Comment this line out to reveal the bug. This function is not verified without this line, but should be.
        ensures result_1[1] == 2;
        ensures result_1[2] == 3;
        ensures result_1[3] == 5;
        ensures exists x in result_1: x == 1;
        ensures exists x in result_1: x == 2;
        ensures exists x in result_1: x == 3;
        ensures !(exists x in result_1: x == 4);
        ensures exists x in result_1: x == 5;
    }

    fun test_index_of(): (vector<u64>, bool, u64, bool, u64) {
        let b1: bool;
        let b2: bool;
        let i1: u64;
        let i2: u64;
        let ev1 = vector::empty<u64>();
        vector::push_back(&mut ev1, 1);
        vector::push_back(&mut ev1, 2);
        vector::push_back(&mut ev1, 3);
        vector::push_back(&mut ev1, 7);
        vector::push_back(&mut ev1, 7);
        vector::push_back(&mut ev1, 1);
        vector::reverse(&mut ev1);
        (b1, i1) = vector::index_of<u64>(&ev1, &4);
        (b2, i2) = vector::index_of<u64>(&ev1, &7);
        (ev1, b1, i1, b2, i2)
    }
    spec test_index_of {
        aborts_if false;
        ensures result_2 == false;
        ensures result_3 == 0;
        ensures result_4 == true;
        ensures result_1[1] == 7; // FIXME: Comment this line out to reveal the bug. This function is not verified without this line, but should be.
        ensures result_1[2] == 7;
        ensures result_5 == 1;
    }


    // ---------------------------
    // Testing functions with args
    // ---------------------------

    fun test_length2(v: vector<u64>) : (u64, u64)
    {
        let x: u64;
        let y: u64;
        x = vector::length(&v);
        vector::push_back(&mut v, 1);
        vector::push_back(&mut v, 2);
        vector::push_back(&mut v, 3);
        y = vector::length(&v);
        (x, y)
    }
    spec test_length2 {
        ensures result_1 + 3 == result_2;
    }

    fun test_length3(v: vector<u64>) : (u64, u64)
    {
        let l = vector::length(&v);
        vector::push_back(&mut v, 1);
        (l, vector::length(&v))
    }
    spec test_length3 {
        ensures len(v) == result_1;
        ensures result_1 + 1 == result_2;
    }

    fun test_length4(v: &mut vector<u64>) : (u64, u64)
    {
        let l = vector::length(v);
        vector::push_back(v, 1);
        (l, vector::length(v))
    }
    spec test_length4 {
        ensures len(old(v)) == result_1;
        ensures result_1 + 1 == result_2;
        ensures v != old(v);
        ensures len(v) == result_2;
    }

    // succeeds. v == v.
    fun test_id1(v: vector<u64>) : vector<u64>
    {
        v
    }
    spec test_id1 {
        ensures result == v;
    }

    // succeeds. reverse(reverse(v)) == v.
    fun test_id2(v: vector<u64>) : vector<u64>
    {
        vector::reverse(&mut v);
        vector::reverse(&mut v);
        v
    }
    spec test_id2 {
        ensures result == v;
    }

    // succeeds. reverse(some_obscure_reverse_routine(v)) == v.
    fun test_id3(v: vector<u64>) : vector<u64>
    {
        let l: u64 = vector::length(&v);
        if(l <= 1) {
        }
        else {
            if(l <= 3) {
                vector::swap(&mut v, 0, l-1);
            }
            else {
                vector::reverse(&mut v);
            }
        };
        vector::reverse(&mut v);
        v
    }
    spec test_id3 {
        ensures result == v;
    }

    // succeeds. If the input vector is empty, destroy it, and return a new empty vector.
    fun test_destroy_empty1(v: vector<u64>) : vector<u64>
    {
        if(vector::is_empty(&v)) {
            vector::destroy_empty(v);
            vector::empty<u64>()
        }
        else {
            v
        }
    }
    spec test_destroy_empty1 {
        ensures result == v;
    }

    // succeeds. If the input vector is empty, destroy it, and return a new empty vector.
    fun test_destroy_empty2(v: vector<u64>)
    {
        if(vector::is_empty(&v)) {
            vector::swap(&mut v, 0, 0);
        }
        else {
            vector::destroy_empty(v);
        }
    }
    spec test_destroy_empty2 {
        aborts_if true;
    }

    fun test_borrow_mut(v: &mut vector<u64>) : u64
    {
        let x = *vector::borrow(v, 0);
        *vector::borrow_mut(v, 0) = 7;
        x
    }
    spec test_borrow_mut {
        aborts_if len(v) == 0;
    }


    // --------------------------------------------
    // Custom Option type using vector as container
    // --------------------------------------------

    struct T<E> has copy, drop {
        v: vector<E>
    }

    fun none<E>(): T<E> {
        T<E> {v: vector::empty<E>()}
    }

    fun some<E>(e: E): T<E> {
        let v = vector::empty<E>();
        vector::push_back(&mut v, e);
        T<E> {v: v}
    }

    fun unwrap_or<E: copy + drop>(x: T<E>, e: E): E {
        let T<E> {v : v} = x;
        if (vector::is_empty<E>(&v))
            e
        else
            vector::pop_back<E>(&mut v)
    }

    fun option_type(): (u64, u64) {
        let n = none<u64>();
        let s = some<u64>(42);
        (unwrap_or<u64>(n, 0), unwrap_or<u64>(s, 0))
    }
    spec option_type {
        ensures result_1 == 0;
        ensures result_2 == 42;
    }
}
