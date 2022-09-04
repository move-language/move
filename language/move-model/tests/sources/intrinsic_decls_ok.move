module 0x42::M {
    spec module {
        struct Vec<E> :: [vector<E>];
        struct Map<K, V>;
        struct Num :: [u8, u64, u128, num];
        struct PairOfVec<E1, E2> :: [(vector<E1>, vector<E2>)];
        struct VecOfBool :: [vector<bool>];

        native fun vec_new<E>(): Vec<E>;
        native fun vec_dup<E>(v: &Vec<E>): Vec<E>;
        native fun vec_borrow_mut<E>(v: &mut Vec<E>, i: Num): &mut E;

        native fun map_get_all_keys<K, V>(m: &Map<K, V>): Vec<K>;
        native fun map_get_all_key_val_pairs<K, V>(m: &Map<K, V>): Vec<(K, V)>;
        native fun map_get_some_key_val<K, V>(m: &Map<K, V>): (&K, &V);
        native fun map_zip_with_vec<K, V, E>(m: &Map<K, V>, v: &Vec<E>): PairOfVec<(K, V), E>;
    }
}
