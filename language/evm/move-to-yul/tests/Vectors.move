module 0x2::Vectors {
    use Std::Vector;

    struct S has copy, drop { x: u128, y: bool, z: u64 }
    struct R has copy, drop { s: S, v: vector<u64> }

    fun empty_vector() : vector<u64> {
        Vector::empty<u64>()
    }

    #[evm_test]
    fun test_empty() {
        let v = empty_vector();
        assert!(Vector::length(&v) == 0, 101);
    }

    #[evm_test]
    fun test_borrow_fail() {
        let v = empty_vector();
        assert!(*Vector::borrow(&v, 0) == 0, 101);
    }

    fun one_elem_u64() : vector<u64>{
        let v = Vector::empty<u64>();
        Vector::push_back(&mut v, 42);
        v
    }

    #[evm_test]
    #[callable]
    fun test_one_elem_u64() {
        let v = one_elem_u64();
        assert!(Vector::length(&v) == 1, 101);
        assert!(*Vector::borrow(&v, 0) == 42, 102);
    }


    fun one_elem_struct() : vector<S>{
        let v = Vector::empty<S>();
        Vector::push_back(&mut v, S { x: 42, y: true, z: 789 });
        v
    }

    #[evm_test]
    fun test_one_elem_struct() {
        let v = one_elem_struct();
        assert!(Vector::length(&v) == 1, 101);
        assert!(*&Vector::borrow(&v, 0).x == 42, 102);
        assert!(*&Vector::borrow(&v, 0).y == true, 103);
        assert!(*&Vector::borrow(&v, 0).z == 789, 104);
    }

    #[evm_test]
    fun test_push_back() {
        let v = one_elem_u64();
        assert!(Vector::length(&v) == 1, 101);
        assert!(*Vector::borrow(&v, 0) == 42, 102);

        Vector::push_back(&mut v, 43);
        assert!(Vector::length(&v) == 2, 103);
        assert!(*Vector::borrow(&v, 0) == 42, 104);
        assert!(*Vector::borrow(&v, 1) == 43, 105);

        Vector::push_back(&mut v, 44);
        assert!(Vector::length(&v) == 3, 106);
        assert!(*Vector::borrow(&v, 0) == 42, 107);
        assert!(*Vector::borrow(&v, 1) == 43, 108);
        assert!(*Vector::borrow(&v, 2) == 44, 109);

        Vector::push_back(&mut v, 45);
        assert!(Vector::length(&v) == 4, 110);
        assert!(*Vector::borrow(&v, 0) == 42, 111);
        assert!(*Vector::borrow(&v, 1) == 43, 112);
        assert!(*Vector::borrow(&v, 2) == 44, 113);
        assert!(*Vector::borrow(&v, 3) == 45, 114);
    }

    #[evm_test]
    fun test_swap() {
        let v = one_elem_u64();
        Vector::push_back(&mut v, 43);
        Vector::push_back(&mut v, 44);
        assert!(*Vector::borrow(&v, 0) == 42, 101);
        assert!(*Vector::borrow(&v, 1) == 43, 102);
        assert!(*Vector::borrow(&v, 2) == 44, 103);

        Vector::swap(&mut v, 0, 2);
        assert!(*Vector::borrow(&v, 0) == 44, 104);
        assert!(*Vector::borrow(&v, 1) == 43, 105);
        assert!(*Vector::borrow(&v, 2) == 42, 106);
    }

    #[evm_test]
    fun test_swap_fail() {
        let v = one_elem_u64();
        Vector::push_back(&mut v, 34);
        Vector::swap(&mut v, 1, 2);
    }

    #[evm_test]
    fun test_pop_back() {
        let v = one_elem_u64();
        Vector::push_back(&mut v, 43);
        assert!(Vector::length(&v) == 2, 101);
        assert!(*Vector::borrow(&v, 0) == 42, 102);
        assert!(*Vector::borrow(&v, 1) == 43, 103);

        let e = Vector::pop_back(&mut v);
        assert!(Vector::length(&v) == 1, 104);
        assert!(e == 43, 105);

        let e = Vector::pop_back(&mut v);
        assert!(Vector::length(&v) == 0, 106);
        assert!(e == 42, 107);

        Vector::destroy_empty(v);
    }

    #[evm_test]
    fun test_pop_back_empty_fail() {
        let v = Vector::empty<address>();
        Vector::pop_back(&mut v); // should abort here
    }

    #[evm_test]
    fun test_destroy_empty() {
        let v = Vector::empty<address>();
        Vector::destroy_empty(v);
    }

    #[evm_test]
    fun test_destroy_non_empty_fail() {
        let v = one_elem_struct();
        Vector::destroy_empty(v); // should abort here
    }

    #[evm_test]
    fun test_borrow_mut() {
        let v = one_elem_struct();
        Vector::push_back(&mut v, S { x: 45, y: false, z: 123 });
        let s1_ref = Vector::borrow_mut(&mut v, 0);
        *&mut s1_ref.x = 90;
        *&mut s1_ref.y = false;
        *&mut s1_ref.z = 1028;
        // the first element is properly changed
        assert!(*&Vector::borrow(&v, 0).x == 90, 101);
        assert!(*&Vector::borrow(&v, 0).y == false, 102);
        assert!(*&Vector::borrow(&v, 0).z == 1028, 103);
        // the second element is not changed
        assert!(*&Vector::borrow(&v, 1).x == 45, 104);
        assert!(*&Vector::borrow(&v, 1).y == false, 105);
        assert!(*&Vector::borrow(&v, 1).z == 123, 106);
        // TODO: uncomment this after we've implemented equality for struct
        // assert!(*Vector::borrow(&v, 0) == S { x: 90, y: false, z: 1028 }, 104);

        let s2_ref = Vector::borrow_mut(&mut v, 1);
        *&mut s2_ref.x = 10;
        *&mut s2_ref.y = true;
        *&mut s2_ref.z = 456;
        assert!(*&Vector::borrow(&v, 1).x == 10, 107);
        assert!(*&Vector::borrow(&v, 1).y == true, 108);
        assert!(*&Vector::borrow(&v, 1).z == 456, 109);
    }

    #[evm_test]
    fun test_nested_vectors() {
        // construct three vectors
        let v0 = Vector::empty<u64>();
        Vector::push_back(&mut v0, 10);
        Vector::push_back(&mut v0, 11);

        let v1 = Vector::empty<u64>();
        Vector::push_back(&mut v1, 12);
        Vector::push_back(&mut v1, 13);
        Vector::push_back(&mut v1, 14);

        let v2 = Vector::empty<u64>();
        Vector::push_back(&mut v2, 15);
        Vector::push_back(&mut v2, 16);
        Vector::push_back(&mut v2, 17);
        Vector::push_back(&mut v2, 18);

        // push all three vectors into another vector
        let v = Vector::empty<vector<u64>>();
        Vector::push_back(&mut v, v0);
        Vector::push_back(&mut v, v1);
        Vector::push_back(&mut v, v2);


        assert!(Vector::length(&v) == 3, 101);
        assert!(Vector::length(Vector::borrow(&v, 0)) == 2, 102);
        assert!(Vector::length(Vector::borrow(&v, 1)) == 3, 103);
        assert!(Vector::length(Vector::borrow(&v, 2)) == 4, 104);

        assert!(*Vector::borrow(Vector::borrow(&v, 0), 0) == 10, 105);
        assert!(*Vector::borrow(Vector::borrow(&v, 1), 1) == 13, 105);
        assert!(*Vector::borrow(Vector::borrow(&v, 2), 2) == 17, 105);
        assert!(*Vector::borrow(Vector::borrow(&v, 2), 3) == 18, 105);
    }

    #[evm_test]
    fun test_vectors_in_structs() {
        let v = Vector::empty<u64>();
        Vector::push_back(&mut v, 10);
        Vector::push_back(&mut v, 11);
        Vector::push_back(&mut v, 12);

        let r = R{ s: S{ x: 42, y: true, z: 9 }, v };
        assert!(Vector::length(&r.v) == 3, 101);
        assert!(*Vector::borrow(&r.v, 0) == 10, 102);
        assert!(*Vector::borrow(&r.v, 1) == 11, 103);
        assert!(*Vector::borrow(&r.v, 2) == 12, 104);

        *Vector::borrow_mut(&mut r.v, 1) = 41;
        assert!(*Vector::borrow(&r.v, 1) == 41, 105);

        *&mut r.v = one_elem_u64();
        assert!(Vector::length(&r.v) == 1, 106);
        assert!(*Vector::borrow(&r.v, 0) == 42, 107);
    }

    #[evm_test]
    fun test_vector_equality() {
        let v = Vector::empty<u8>();
        Vector::push_back(&mut v, 10);
        Vector::push_back(&mut v, 11);
        Vector::push_back(&mut v, 12);
        Vector::push_back(&mut v, 13);

        assert!(v == x"0a0b0c0d", 101);
        assert!(!(v != x"0a0b0c0d"), 102);
        assert!(v != x"0a0b0c", 103);
        assert!(!(v == x"0a0b0c"), 104);

        Vector::push_back(&mut v, 14);
        assert!(v == x"0a0b0c0d0e", 105);
    }
}
