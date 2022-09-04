module 0x1::intrinsics {
    struct S {}

    spec module {
        struct Vec<E> :: [vector<E>];
        struct Map<K, V>;
        struct Event has copy, drop;

        native fun vec_transfer<E>(v: &mut Vec<E>, n: Num): Vec<E>;
    }
}

module 0x42::M {
    struct MyR {}
    spec MyR {
        // rejected as the RHS of intrinsic must be a qualified name
        pragma intrinsic = true;
    }

    struct MyS {}
    spec MyS {
        // rejected due an invalid intrinsic type (even S is a valid type)
        pragma intrinsic = 0x1::intrinsics::S;
    }

    struct MyVec<phantom E, phantom F> {}
    spec MyVec {
        use 0x1::intrinsics::Vec;
        // rejected due to mismatched type parameter number
        pragma intrinsic = Vec;
    }

    struct MyMap<phantom X> {}
    spec MyMap  {
        use 0x1::intrinsics;
        // rejected due to mismatched type parameter number
        pragma intrinsic = intrinsics::Map;
    }

    struct MyEvent has key {}
    spec MyEvent {
        // rejected due to mismatched ability modifiers
        pragma intrinsic = 0x1::intrinsics::Event;
    }

    struct MyVecOk<phantom E> {}
    spec MyVecOk {
        use 0x1::intrinsics::Vec;
        pragma intrinsic = Vec;
    }

    native fun my_vec_transfer1<E, F>(v: &mut MyVecOk<E>, n: u64): vector<E>;
    spec my_vec_transfer1 {
        // rejected due to mismatched type parameter number
        pragma intrinsic = 0x1::intrinsics::vec_transfer;
    }

    native fun my_vec_transfer2<E>(v: &mut MyVecOk<E>, n: u64, b: bool): vector<E>;
    spec my_vec_transfer2 {
        // rejected due to mismatched parameter number
        pragma intrinsic = 0x1::intrinsics::vec_transfer;
    }

    native fun my_vec_transfer3<E>(v: &mut MyVecOk<E>): vector<E>;
    spec my_vec_transfer3 {
        // rejected due to mismatched parameter number
        pragma intrinsic = 0x1::intrinsics::vec_transfer;
    }

    native fun my_vec_transfer4<E>(v: &MyVecOk<E>): vector<E>;
    spec my_vec_transfer4 {
        // rejected due to mismatched parameter type
        pragma intrinsic = 0x1::intrinsics::vec_transfer;
    }

    native fun my_vec_transfer5<E>(v: &MyVecOk<E>): &mut vector<E>;
    spec my_vec_transfer5 {
        // rejected due to mismatched return type
        pragma intrinsic = 0x1::intrinsics::vec_transfer;
    }
}
