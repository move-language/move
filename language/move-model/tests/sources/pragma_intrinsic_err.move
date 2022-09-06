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
    spec MyMap {
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

    native fun my_vec_transfer6<E>(v: &MyVecOk<E>): &mut vector<E>;
    spec module {
        // uninterpreted spec fun
        fun vec_transfer_uninterp<E>(v: &MyVecOk<E>): &mut vector<E>;
    }
    spec my_vec_transfer6 {
        // rejected as vec_transfer_uninterp is not an intrinsic function
        pragma intrinsic = vec_transfer_uninterp;
    }

    native fun my_vec_transfer7<E>(v: &MyVecOk<E>): u64;
    spec module {
        // defined spec fun
        fun vec_transfer_defined<E>(v: &MyVecOk<E>): u64 {
            0
        }
    }
    spec my_vec_transfer7 {
        // rejected as vec_transfer_defined is not an intrinsic function
        pragma intrinsic = vec_transfer_defined;
    }

    // error as this function needs to have an intrinsic counterpart
    native public fun no_intrinsic_public();

    // error as this function can be called by a public non-intrinsic function
    native fun no_intrinsic_internal();
    public fun call_no_intrinsic_internal() {
        no_intrinsic_internal();
    }

    // error as this function can be called by a public non-intrinsic function
    native fun no_intrinsic_internal_recursive();
    fun call_no_intrinsic_internal_recursive(v: u64) {
        if (v == 0) {
            no_intrinsic_internal_recursive();
        } else {
            call_no_intrinsic_internal_recursive(v - 1);
        }
    }
    public fun external_caller() {
        call_no_intrinsic_internal_recursive(100);
    }
}
