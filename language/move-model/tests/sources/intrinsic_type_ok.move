module 0x42::M {
    spec module {
        struct Vector<E> :: [vector<E>];
        struct Map<K, V>;
        struct Num :: [u8, u64, u128, num];
        struct PairOfVec<E1, E2> :: [(vector<E1>, vector<E2>)];
        struct VecOfBool :: [vector<bool>];
    }
}
