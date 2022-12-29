module 0x1::borrow_ok {
    use std::vector;

    struct S {
        f: bool
    }

    fun ret_param(f: &mut vector<u64>): &mut u64 {
        vector::borrow_mut(f, 0)
    }

    fun ret_field(s: &mut S): &mut bool {
        &mut s.f
    }

    fun borrow_param(f: &mut vector<u64>) {
        let _ = vector::borrow_mut(f, 0);
    }

    fun borrow_field(s: &mut S) {
        let _ = &mut s.f;
    }

}
