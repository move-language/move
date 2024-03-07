module 0x1::call_warn {

    struct S {
        f: bool
    }

    native fun read_vector(f: &vector<u64>);

    native fun read_field(s: &S);

    fun call_read_vector(f: &mut vector<u64>) {
        read_vector(f)
    }

    fun call_read_field(s: &mut S) {
        read_field(s)
    }
}
