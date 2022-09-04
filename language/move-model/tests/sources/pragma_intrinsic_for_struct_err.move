module 0x1::intrinsics {
    struct S {}

    spec module {
        struct Vector<E> :: [vector<E>];
        struct Map<K, V>;
        struct Event has copy, drop;
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
        use 0x1::intrinsics::Vector;
        // rejected due to mismatched type parameter number
        pragma intrinsic = Vector;
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
}
