/// Specifications of the `Table` module.
spec extensions::table {
    spec struct IntrinsicTable<K: copy + drop, V> has store;

    spec native fun intrinsic_new<K: copy + drop, V>(): IntrinsicTable<K, V>;
    spec native fun intrinsic_destroy_empty<K: copy + drop, V>(m: IntrinsicTable<K, V>);
    spec native fun intrinsic_add<K: copy + drop, V>(m: &mut IntrinsicTable<K, V>, key: K, val: V);
    spec native fun intrinsic_borrow<K: copy + drop, V>(m: &IntrinsicTable<K, V>, key: K): &V;
    spec native fun intrinsic_borrow_mut<K: copy + drop, V>(m: &mut IntrinsicTable<K, V>, key: K): &mut V;
    spec native fun intrinsic_length<K: copy + drop, V>(m: &IntrinsicTable<K, V>): num;
    spec native fun intrinsic_empty<K: copy + drop, V>(m: &IntrinsicTable<K, V>): bool;
    spec native fun intrinsic_remove<K: copy + drop, V>(m: &mut IntrinsicTable<K, V>, key: K): V;
    spec native fun intrinsic_contains<K: copy + drop, V>(m: &IntrinsicTable<K, V>, key: K): bool;

    // Make most of the public API intrinsic. Those functions have custom specifications in the prover.

    spec Table {
        pragma intrinsic = IntrinsicTable;
    }

    spec new {
        pragma intrinsic = intrinsic_new;
    }

    spec destroy_empty {
        pragma intrinsic = intrinsic_destroy_empty;
    }

    spec add {
        pragma intrinsic = intrinsic_add;
    }

    spec borrow {
        pragma intrinsic = intrinsic_borrow;
    }

    spec borrow_mut {
        pragma intrinsic = intrinsic_borrow_mut;
    }

    spec length {
        pragma intrinsic = intrinsic_length;
    }

    spec empty {
        pragma intrinsic = intrinsic_empty;
    }

    spec remove {
        pragma intrinsic = intrinsic_remove;
    }

    spec contains {
        pragma intrinsic = intrinsic_contains;
    }

    // Specification functions for tables

    spec native fun spec_table<K, V>(): Table<K, V>;
    spec native fun spec_len<K, V>(t: Table<K, V>): num;
    spec native fun spec_contains<K, V>(t: Table<K, V>, k: K): bool;
    spec native fun spec_add<K, V>(t: Table<K, V>, k: K, v: V): Table<K, V>;
    spec native fun spec_remove<K, V>(t: Table<K, V>, k: K): Table<K, V>;
    spec native fun spec_get<K, V>(t: Table<K, V>, k: K): V;
}
