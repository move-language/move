module 0x1::intrinsics {
    spec module {
        struct Vector<E> :: [vector<E>];
        struct Map<K, V>;
        struct Event has copy, drop;
    }
}

module 0x42::M {
    struct MyVec<phantom E> {}
    spec MyVec {
        use 0x1::intrinsics::Vector;
        pragma intrinsic = Vector;
    }

    struct MyMap<phantom X, phantom Y> {}
    spec MyMap  {
        use 0x1::intrinsics;
        pragma intrinsic = intrinsics::Map;
    }

    struct MyEvent has copy, drop {}
    spec MyEvent {
        pragma intrinsic = 0x1::intrinsics::Event;
    }
}
