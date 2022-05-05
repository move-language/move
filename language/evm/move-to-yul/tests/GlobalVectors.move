#[evm_contract]
module 0x2::GlobalVectors {
    use Std::Vector;
    use Evm::Evm::sign;

    struct S has copy, drop, store { x: u128, y: u64 }
    struct R has copy, drop { s: S, v: vector<u64> }
    struct T<Elem: drop> has drop, key { v: vector<Elem> }

    #[evm_test]
    fun test_move_to() acquires  T {
        let v = Vector::empty();
        Vector::push_back(&mut v, 10);
        Vector::push_back(&mut v, 11);
        Vector::push_back(&mut v, 12);
        move_to(&sign(@0x42), T { v });
        assert!(Vector::length(&borrow_global<T<u64>>(@0x42).v) == 3, 101);
        assert!(*Vector::borrow(&borrow_global<T<u64>>(@0x42).v, 0) == 10, 102);
        assert!(*Vector::borrow(&borrow_global<T<u64>>(@0x42).v, 1) == 11, 103);
        assert!(*Vector::borrow(&borrow_global<T<u64>>(@0x42).v, 2) == 12, 104);
    }

    #[evm_test]
    fun test_move_to_vector_of_struct() acquires  T {
        let v = Vector::empty();
        Vector::push_back(&mut v, S { x: 10, y: 40});
        Vector::push_back(&mut v, S { x: 11, y: 41});
        Vector::push_back(&mut v, S { x: 12, y: 42});
        move_to(&sign(@0x42), T { v });
        assert!(Vector::length(&borrow_global<T<S>>(@0x42).v) == 3, 101);
        assert!(*&Vector::borrow(&borrow_global<T<S>>(@0x42).v, 0).x == 10, 102);
        assert!(*&Vector::borrow(&borrow_global<T<S>>(@0x42).v, 1).x == 11, 103);
        assert!(*&Vector::borrow(&borrow_global<T<S>>(@0x42).v, 2).x == 12, 104);
    }

    #[evm_test]
    fun test_move_to_vector_of_vector() acquires  T {
        let v = Vector::empty();
        Vector::push_back(&mut v, Vector::singleton(10));
        Vector::push_back(&mut v, Vector::singleton(11));
        Vector::push_back(&mut v, Vector::singleton(12));
        move_to(&sign(@0x42), T { v });
        assert!(Vector::length(&borrow_global<T<vector<u64>>>(@0x42).v) == 3, 101);
        assert!(*Vector::borrow(Vector::borrow(&borrow_global<T<vector<u64>>>(@0x42).v, 0), 0) == 10, 102);
        assert!(*Vector::borrow(Vector::borrow(&borrow_global<T<vector<u64>>>(@0x42).v, 1), 0) == 11, 102);
        assert!(*Vector::borrow(Vector::borrow(&borrow_global<T<vector<u64>>>(@0x42).v, 2), 0) == 12, 102);
    }

    #[evm_test]
    fun test_push_back_global() acquires  T {
        let v = Vector::empty();
        Vector::push_back(&mut v, 10);
        Vector::push_back(&mut v, 11);
        Vector::push_back(&mut v, 12);

        move_to(&sign(@0x42), T { v: copy v });

        Vector::push_back(&mut borrow_global_mut<T<u64>>(@0x42).v, 13);
        Vector::push_back(&mut borrow_global_mut<T<u64>>(@0x42).v, 14);
        assert!(Vector::length(&borrow_global<T<u64>>(@0x42).v) == 5, 101);

        assert!(*Vector::borrow(&borrow_global<T<u64>>(@0x42).v, 0) == 10, 102);
        assert!(*Vector::borrow(&borrow_global<T<u64>>(@0x42).v, 1) == 11, 103);
        assert!(*Vector::borrow(&borrow_global<T<u64>>(@0x42).v, 2) == 12, 104);
        assert!(*Vector::borrow(&borrow_global<T<u64>>(@0x42).v, 3) == 13, 105);
        assert!(*Vector::borrow(&borrow_global<T<u64>>(@0x42).v, 4) == 14, 106);
    }

    #[evm_test]
    fun test_push_back_struct_global() acquires  T {
        let v = Vector::empty();
        Vector::push_back(&mut v, S { x: 10, y: 40});
        Vector::push_back(&mut v, S { x: 11, y: 41});
        Vector::push_back(&mut v, S { x: 12, y: 42});
        move_to(&sign(@0x42), T { v });
        Vector::push_back(&mut borrow_global_mut<T<S>>(@0x42).v, S { x: 13, y: 43});
        Vector::push_back(&mut borrow_global_mut<T<S>>(@0x42).v, S { x: 14, y: 44});
        assert!(Vector::length(&borrow_global<T<S>>(@0x42).v) == 5, 101);
        assert!(*&Vector::borrow(&borrow_global<T<S>>(@0x42).v, 0).x == 10, 102);
        assert!(*&Vector::borrow(&borrow_global<T<S>>(@0x42).v, 1).x == 11, 103);
        assert!(*&Vector::borrow(&borrow_global<T<S>>(@0x42).v, 2).x == 12, 104);
    }

    #[evm_test]
    fun test_move_from() acquires  T {
        let v = Vector::empty();
        Vector::push_back(&mut v, 10);
        Vector::push_back(&mut v, 11);
        Vector::push_back(&mut v, 12);

        move_to(&sign(@0x42), T { v });
        let local_t = move_from<T<u64>>(@0x42);
        let T { v } = local_t;
        assert!(Vector::length(&v) == 3, 101);
        assert!(*Vector::borrow(&v, 0) == 10, 102);
        assert!(*Vector::borrow(&v, 1) == 11, 103);
        assert!(*Vector::borrow(&v, 2) == 12, 104);
        Vector::push_back(&mut v, 13);
        Vector::push_back(&mut v, 14);
        assert!(*Vector::borrow(&v, 3) == 13, 105);
        assert!(*Vector::borrow(&v, 4) == 14, 106);
    }

    #[evm_test]
    fun test_move_from_vector_of_struct() acquires  T {
        let v = Vector::empty();
        Vector::push_back(&mut v, S { x: 10, y: 40});
        Vector::push_back(&mut v, S { x: 11, y: 41});
        Vector::push_back(&mut v, S { x: 12, y: 42});
        move_to(&sign(@0x42), T { v });
        let local_t = move_from<T<S>>(@0x42);
        assert!(Vector::length(&local_t.v) == 3, 101);
        assert!(*&Vector::borrow(&local_t.v, 0).x == 10, 102);
        assert!(*&Vector::borrow(&local_t.v, 1).x == 11, 103);
        assert!(*&Vector::borrow(&local_t.v, 2).x == 12, 104);
    }

    #[evm_test]
    fun test_move_from_vector_of_vector() acquires  T {
        let v = Vector::empty();
        Vector::push_back(&mut v, Vector::singleton(10));
        Vector::push_back(&mut v, Vector::singleton(11));
        Vector::push_back(&mut v, Vector::singleton(12));
        move_to(&sign(@0x42), T { v });
        let local_t = move_from<T<vector<u64>>>(@0x42);
        assert!(Vector::length(&local_t.v) == 3, 101);
        assert!(*Vector::borrow(Vector::borrow(&local_t.v, 0), 0) == 10, 102);
        assert!(*Vector::borrow(Vector::borrow(&local_t.v, 1), 0) == 11, 102);
        assert!(*Vector::borrow(Vector::borrow(&local_t.v, 2), 0) == 12, 102);
    }


    #[evm_test]
    fun test_pop_back_global() acquires  T {
        let v = Vector::empty();
        Vector::push_back(&mut v, 10);
        Vector::push_back(&mut v, 11);
        Vector::push_back(&mut v, 12);

        move_to(&sign(@0x42), T { v });
        let e = Vector::pop_back(&mut borrow_global_mut<T<u64>>(@0x42).v);
        assert!(e == 12, 101);
        assert!(Vector::length(&borrow_global<T<u64>>(@0x42).v) == 2, 102);
        e = Vector::pop_back(&mut borrow_global_mut<T<u64>>(@0x42).v);
        assert!(e == 11, 103);
        assert!(Vector::length(&borrow_global<T<u64>>(@0x42).v) == 1, 104);
    }

    #[evm_test]
    fun test_pop_back_struct_global() acquires  T {
        let v = Vector::empty();
        Vector::push_back(&mut v, S { x: 10, y: 40});
        Vector::push_back(&mut v, S { x: 11, y: 41});
        Vector::push_back(&mut v, S { x: 12, y: 42});
        move_to(&sign(@0x42), T { v });
        let e = Vector::pop_back(&mut borrow_global_mut<T<S>>(@0x42).v);
        assert!(e.x == 12 && e.y == 42, 101);
        assert!(Vector::length(&borrow_global<T<S>>(@0x42).v) == 2, 102);
        e = Vector::pop_back(&mut borrow_global_mut<T<S>>(@0x42).v);
        assert!(e.x == 11 && e.y == 41, 103);
        assert!(Vector::length(&borrow_global<T<S>>(@0x42).v) == 1, 104);
    }

    #[evm_test]
    fun test_swap_global() acquires T {
        let v = Vector::empty();
        Vector::push_back(&mut v, 42);
        Vector::push_back(&mut v, 43);
        Vector::push_back(&mut v, 44);

        move_to(&sign(@0x42), T { v });
        Vector::swap(&mut borrow_global_mut<T<u64>>(@0x42).v, 0, 2);

        assert!(Vector::length(&borrow_global<T<u64>>(@0x42).v) == 3, 101);

        assert!(*Vector::borrow(&borrow_global<T<u64>>(@0x42).v, 0) == 44, 102);
        assert!(*Vector::borrow(&borrow_global<T<u64>>(@0x42).v, 1) == 43, 103);
        assert!(*Vector::borrow(&borrow_global<T<u64>>(@0x42).v, 2) == 42, 104);
    }

    #[evm_test]
    fun test_borrow_mut_global() acquires T {
        let v = Vector::empty();
        Vector::push_back(&mut v, 42);
        Vector::push_back(&mut v, 43);
        Vector::push_back(&mut v, 44);

        move_to(&sign(@0x42), T { v });

        let e = Vector::borrow_mut(&mut borrow_global_mut<T<u64>>(@0x42).v, 0);
        *e = 12;
        assert!(*Vector::borrow(&borrow_global<T<u64>>(@0x42).v, 0) == 12, 102);
    }

    #[evm_test]
    fun test_read_ref_copy() acquires T {
        let v = Vector::empty();
        Vector::push_back(&mut v, 65u8);
        move_to(&sign(@0x42), T { v });
        let v1 = *&borrow_global<T<u8>>(@0x42).v;
        assert!(Vector::length(&v1) == 1, 101);
        assert!(*Vector::borrow(&v1, 0) == 65u8, 102);
    }
}
