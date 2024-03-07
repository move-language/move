module 0x1::mutate_ok {
    use std::vector;

    struct S has drop {
        f: bool
    }

    fun mutate_vector(f: &mut vector<u64>) {
        vector::push_back(f, 7)
    }

    fun mutate_field(s: &mut S) {
        s.f = false
    }

    fun mutate_direct(s: &mut S) {
        *s = S { f: true }
    }
}
