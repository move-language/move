module 0x42::M {
    struct Foo {}
    spec module {
        struct V1<E> :: [vector<NoSuchTypeParam>];
        struct V2<E> :: [&E];
        struct V3 :: [Foo];
        struct V4 :: [|u64| address];

        native fun f1<T1, T2>(a: &T0): T1;
        native fun f2<T1, T2>(a: &T1): T0;

        struct Vec<E> :: [vector<E>];
        native fun f3(a: num): Vec<E>;
        native fun f4<E>(a: num): Vec<E, E>;
    }
}
