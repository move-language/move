module 0x1::call_ok {

    struct S {
        f: bool
    }

    native fun mutate_vector(f: &mut vector<u64>);

    native fun mutate_field(s: &mut S);

    fun call_mutate_vector(f: &mut vector<u64>) {
        mutate_vector(f)
    }

    fun call_mutate_field(s: &mut S) {
        mutate_field(s)
    }
}
