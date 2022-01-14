// Tests basic arithmetic.
// TODO: this file is taken from the prover tests. Convert the specs here into test assertions
//   once unit testing works. Or remove this and use existing Move tests.
#[contract]
module 0x2::M {
    #[callable]
	fun add_two_number(x: u64, y: u64): (u64, u64) {
		let res: u64 = x + y;
		let z: u64 = 3;
		(z, res)
	}
	spec add_two_number {
	    aborts_if x + y > max_u64();
	    ensures result_1 == 3;
	    ensures result_2 == x + y;
	}

    #[callable]
    fun div(x: u64, y: u64): (u64, u64) {
        (x / y, x % y)
    }
    spec div {
        aborts_if y == 0;
        ensures result_1 == x / y;
        ensures result_2 == x % y;
    }

    // succeeds.
    #[callable]
	fun multiple_ops(x: u64, y: u64, z: u64): u64 {
		x + y * z
	}
	spec multiple_ops {
        ensures result == x + y * z;
    }

    // succeeds.
    #[callable]
	fun bool_ops(a: u64, b: u64): (bool, bool) {
        let c: bool;
        let d: bool;
        c = a > b && a >= b;
        d = a < b || a <= b;
        if (!(c != d)) abort 42;
        (c, d)
    }
    spec bool_ops {
        ensures result_1 <==> (a > b && a >= b);
        ensures result_2 <==> (a < b || a <= b);
    }

    // succeeds.
    #[callable]
	fun arithmetic_ops(a: u64): (u64, u64) {
        let c: u64;
        c = (6 + 4 - 1) * 2 / 3 % 4;
        if (c != 2) abort 42;
        (c, a)
    }
    spec arithmetic_ops {
        ensures result_1 == (6 + 4 - 1) * 2 / 3 % 4;
        ensures result_2 == a;
    }

    #[callable]
    fun f(x: u64) : u64 {
        x+1
    }
    spec f {
        aborts_if x+1 > max_u64();
        ensures result == x+1;
    }

    #[callable]
    fun g(x: u64) : u64 {
        x+2
    }
    spec g {
        aborts_if x+2 > max_u64();
        ensures result == x+2;
    }

    #[callable]
    fun h(b: bool): u64 {
        let x: u64 = 3;
        let y: u64;
        if (b) y=f(x) else y=g(x);
        if (b && y != 4) abort 4;
        if (!b && y != 5) abort 5;
        y
    }
    spec h {
        aborts_if false;
    }


    // ---------
    // Underflow
    // ---------

    // succeeds.
    #[callable]
    fun underflow(): u64 {
        let x = 0;
        x - 1
    }
	spec underflow {
	    aborts_if true;
	}


    // ----------------
    // Division by zero
    // ----------------

    // succeeds.
    #[callable]
    fun div_by_zero(): u64 {
        let x = 0;
        1 / x
    }
	spec div_by_zero {
	    aborts_if true;
	}

    #[callable]
    fun div_by_zero_u64_incorrect(x: u64, y: u64): u64 {
        x / y
    }
    spec div_by_zero_u64_incorrect {
        aborts_if false;
    }

    #[callable]
    fun div_by_zero_u64(x: u64, y: u64): u64 {
        x / y
    }
    spec div_by_zero_u64 {
        aborts_if y == 0;
    }


    // --------------------
    // Overflow by addition
    // --------------------

    // fails.
    #[callable]
    fun overflow_u8_add_incorrect(x: u8, y: u8): u8 {
        x + y
    }
    spec overflow_u8_add_incorrect {
        aborts_if false;
    }

    // succeeds.
    #[callable]
    fun overflow_u8_add(x: u8, y: u8): u8 {
        x + y
    }
    spec overflow_u8_add {
        aborts_if x + y > max_u8();
    }

    // fails.
    #[callable]
    fun overflow_u64_add_incorrect(x: u64, y: u64): u64 {
        x + y
    }
    spec overflow_u64_add_incorrect {
        aborts_if false;
    }

    // succeeds.
    #[callable]
    fun overflow_u64_add(x: u64, y: u64): u64 {
        x + y
    }
    spec overflow_u64_add {
        aborts_if x + y > max_u64();
    }

    // fails.
    #[callable]
    fun overflow_u128_add_incorrect(x: u128, y: u128): u128 {
        x + y
    }
    spec overflow_u128_add_incorrect {
        aborts_if false;
    }

    // succeeds.
    #[callable]
    fun overflow_u128_add(x: u128, y: u128): u128 {
        x + y
    }
    spec overflow_u128_add {
        aborts_if x + y > max_u128();
    }


    // --------------------------
    // Overflow by multiplication
    // --------------------------

    // fails.
    #[callable]
    fun overflow_u8_mul_incorrect(x: u8, y: u8): u8 {
        x * y
    }
    spec overflow_u8_mul_incorrect {
        aborts_if false;
    }

    // succeeds.
    #[callable]
    fun overflow_u8_mul(x: u8, y: u8): u8 {
        x * y
    }
    spec overflow_u8_mul {
        aborts_if x * y > max_u8();
    }

    // fails.
    #[callable]
    fun overflow_u64_mul_incorrect(x: u64, y: u64): u64 {
        x * y
    }
    spec overflow_u64_mul_incorrect {
        aborts_if false;
    }

    #[callable]
    fun overflow_u64_mul(x: u64, y: u64): u64 {
        x * y
    }
    spec overflow_u64_mul {
        aborts_if x * y > max_u64();
    }

    // fails.
    #[callable]
    fun overflow_u128_mul_incorrect(x: u128, y: u128): u128 {
        x * y
    }
    spec overflow_u128_mul_incorrect {
        aborts_if false;
    }

    #[callable]
    fun overflow_u128_mul(x: u128, y: u128): u128 {
        x * y
    }
    spec overflow_u128_mul {
        aborts_if x * y > max_u128(); // U128_MAX
    }
}
