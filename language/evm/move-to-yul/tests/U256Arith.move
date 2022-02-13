// Tests basic arithmetics with u256s.
// dep: ../stdlib/sources/U256.move
#[contract]
module 0x2::U256Arith {
    use 0x1::U256::{Self, U256, u256_from_words};

    #[callable]
    fun cast(x: u128, y: u128): U256 {
        u256_from_words(x, y)
    }

    #[callable]
    fun add(): U256 {
        let x = u256_from_words(6, 2);
        let y = u256_from_words(5, 1);
        U256::add(x, y)
    }

    #[callable]
    fun sub(): U256 {
        let x = u256_from_words(6, 2);
        let y = u256_from_words(5, 1);
        U256::sub(x, y)
    }

    #[callable]
    fun mul(): U256 {
        let x = u256_from_words(6, 2);
        let y = u256_from_words(5, 1);
        U256::mul(x, y)
    }

    #[callable]
    fun div(): U256 {
        let x = u256_from_words(6, 2);
        let y = u256_from_words(5, 1);
        U256::div(x, y)
    }

    #[callable]
    fun mod(): U256 {
        let x = u256_from_words(6, 2);
        let y = u256_from_words(5, 1);
        U256::mod(x, y)
    }

    #[callable]
    fun gt(): U256 {
        let x = u256_from_words(6, 2);
        let y = u256_from_words(5, 1);
        U256::gt(x, y)
    }

    #[callable]
    fun lt(): U256 {
        let x = u256_from_words(6, 2);
        let y = u256_from_words(5, 1);
        U256::lt(x, y)
    }

    #[callable]
    fun ge(): U256 {
        let x = u256_from_words(6, 2);
        let y = u256_from_words(5, 1);
        U256::ge(x, y)
    }

    #[callable]
    fun le(): U256 {
        let x = u256_from_words(6, 2);
        let y = u256_from_words(5, 1);
        U256::le(x, y)
    }

    #[callable]
    fun eq(): U256 {
        let x = u256_from_words(6, 2);
        let y = u256_from_words(5, 1);
        U256::eq(x, y)
    }

    #[callable]
    fun ne(): U256 {
        let x = u256_from_words(6, 2);
        let y = u256_from_words(5, 1);
        U256::ne(x, y)
    }

    #[callable]
    fun shl(): U256 {
        let x = u256_from_words(6, 2);
        U256::shl(x, 127)
    }

    #[callable]
    fun shr(): U256 {
        let x = u256_from_words(6, 2);
        U256::shr(x, 127)
    }
}
