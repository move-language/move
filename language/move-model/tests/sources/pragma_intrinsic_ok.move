module 0x1::intrinsics {
    spec module {
        struct Vec<E> :: [vector<E>];
        struct Map<K, V>;
        struct Event has copy, drop;

        native fun vec_len<E>(v: &Vec<E>): num;
        native fun vec_append<E>(lhs: &mut Vec<E>, rhs: &mut Vec<E>): num;
        native fun map_into_vec_pair<K, V>(m: Map<K, V>): (Vec<K>, Vec<V>);
        native fun map_from_vec_pair<K, V>(v1: Vec<K>, v2: Vec<V>): Map<K, V>;
    }
}

module 0x42::M {
    struct MyVec<phantom E> {}
    spec MyVec {
        use 0x1::intrinsics::Vec;
        pragma intrinsic = Vec;
    }

    native fun len<E>(v: &MyVec<E>): u64;
    spec len {
        use 0x1::intrinsics;
        pragma intrinsic = intrinsics::vec_len;
    }

    fun append<E>(lhs: &mut MyVec<E>, rhs: &mut MyVec<E>): u64 {
        len(lhs) + len(rhs)
    }
    spec append {
        use 0x1::intrinsics;
        pragma intrinsic = intrinsics::vec_append;
    }

    struct MyMap<phantom X, phantom Y> {}
    spec MyMap  {
        use 0x1::intrinsics;
        pragma intrinsic = intrinsics::Map;
    }

    native fun into_vec_pair<X, Y>(m: MyMap<X, Y>): (MyVec<X>, vector<Y>);
    spec into_vec_pair {
        pragma intrinsic = 0x1::intrinsics::map_into_vec_pair;
    }

    native fun from_vec_pair<X, Y>(v1: vector<X>, v2: MyVec<Y>): MyMap<X, Y>;
    spec from_vec_pair {
        pragma intrinsic = 0x1::intrinsics::map_from_vec_pair;
    }

    struct MyEvent has copy, drop {}
    spec MyEvent {
        pragma intrinsic = 0x1::intrinsics::Event;
    }
}
