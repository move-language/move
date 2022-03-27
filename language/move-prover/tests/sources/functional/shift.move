// exclude_for: cvc5

// cvc5 seems to go into an infinite loop because of the recursive $pow function.
// For some reason, it does not time out in a reasonable time.
module 0x42::TestShift {

    fun shiftl_1_correct(x: u64): u64 {
        x << 1
    }

    spec shiftl_1_correct {
        ensures result == 2*x % (1 << 64);
        ensures x < (1 << 63) ==> result == 2*x;
    }

    fun shiftr_1_correct(x: u64): u64 {
        x >> 1
    }

    spec shiftr_1_correct {
        ensures result == x / 2;
    }


    fun shiftl_64_incorrect(x: u64): u64 {
        x << 64u8
    }

    spec shiftl_64_incorrect {
        ensures result == x * 64 + 1;
    }

    fun shiftl_7_incorrect(x: u64): u64 {
        x << 7u8
    }

    spec shiftl_7_incorrect {
        ensures result == x * 128 + 1;
    }

    fun shiftr_64_incorrect(x: u64): u64 {
        x >> 64u8
    }

    spec shiftr_64_incorrect {
        ensures result == x / 64 + 1;
    }

    fun shiftr_7_incorrect(x: u64): u64 {
        x >> 7u8
    }

    spec shiftr_7_incorrect {
        ensures result == x / 128 + 1;
    }

    fun shift_l_11_correct(x: u64): u64 {
        x << 11u8
    }

    spec shift_l_11_correct {
        ensures result == (x << 11u8) % (1 << 64);
    }

    fun shift_l_11_incorrect(x: u64): u64 {
        x << 11u8
    }

    fun shift_r_11_correct(x: u64): u64 {
        x >> 11u8
    }

    spec shift_r_11_correct {
        ensures result == x / (1 << 11);
    }


    spec shift_l_11_incorrect {
        ensures result == x << 10u8;
    }

    fun var_shift_l_incorrect(x: u64, a: u8): u64 {
        x << a
    }

    spec var_shift_l_incorrect {
        // The spec allows bits to be shifted beyond destination.
        // E.g., x = 2, a = 99
        ensures result == x << a;
    }

    fun var_shift_l_correct(x: u64, a: u8): u64 {
        x << a
    }

    spec var_shift_l_correct {
        // The spec allows bits to be shifted beyond destination.
        // E.g., x = 2, a = 99
        ensures result == (x << a) % (1 << 64);
    }

    fun var_shift_r_correct(x: u64, a: u8): u64 {
        x >> a
    }

    spec var_shift_r_correct {
        // The spec allows bits to be shifted beyond destination.
        // E.g., x = 2, a = 99
        ensures result == (x >> a);
    }

}
