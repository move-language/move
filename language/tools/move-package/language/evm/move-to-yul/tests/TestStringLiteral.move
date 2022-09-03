module 0x2::M {
    use Evm::Evm::sign;
    use Std::Vector;

    struct T has key, drop {
        s: vector<u8>
    }

    #[evm_test]
    fun h1() acquires T {
        let x = b"abc";
        let t = T { s: x } ;
        move_to<T>(&sign(@3), t);
        let v = borrow_global<T>(@3).s;
        assert!(Vector::length(&v) == 3, 96);
        assert!(*Vector::borrow(&v, 0) == 97, 97);
        assert!(*Vector::borrow(&v, 1) == 98u8, 98);
        assert!(*Vector::borrow(&v, 2) == 99u8, 99);
        borrow_global_mut<T>(@3).s = b"efgh";
        let v = borrow_global<T>(@3).s;
        assert!(Vector::length(&v) == 4, 100);
        assert!(*Vector::borrow(&v, 0) == 101u8, 101);
        assert!(*Vector::borrow(&v, 1) == 102u8, 102);
        assert!(*Vector::borrow(&v, 2) == 103u8, 103);
        assert!(*Vector::borrow(&v, 3) == 104u8, 104);

    }
}
