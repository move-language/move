module 0x42::M {
    struct Foo {}
    spec module {
        struct V1<E> :: [vector<NoSuchTypeParam>];
        struct V2<E> :: [&E];
        struct V3 :: [Foo];
        struct V4 :: [|u64| address];
    }
}
