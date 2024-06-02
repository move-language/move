module 0x1::borrow_warn {

    struct S {
        f: bool
    }

    fun borrow_param(f: &mut vector<u64>): &u64 {
        std::vector::borrow(f, 0)
    }

    fun borrow_field(s: &mut S): &bool {
        &s.f
    }
}
