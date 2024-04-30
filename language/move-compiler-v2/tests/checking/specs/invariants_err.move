module 0x42::M {

    struct S has key {
        x: u64,
    }

    spec S {
        // Direct dependency from global state
        invariant exists<S>(@0x0);
        invariant global<S>(@0x0).x == x;
        invariant spec_var > 0;
        // Indirect dependency from global state via function call.
        invariant rec_fun(true);
    }

    spec module {
        global spec_var: num;

        fun rec_fun(c: bool): bool {
            if (c) {
                rec_fun2(c)
            } else {
                spec_var > 0
            }
        }
        fun rec_fun2(c: bool): bool {
            rec_fun(!c)
        }
    }
}
