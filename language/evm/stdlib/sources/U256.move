#[evm_arith]
module 0x1::U256 {
    native struct U256 has copy, drop, store;
    native public fun u256_from_words(lo: u128, hi: u128): U256;
    native public fun add(x: U256, y: U256): U256;
    native public fun sub(x: U256, y: U256): U256;
    native public fun mul(x: U256, y: U256): U256;
    native public fun div(x: U256, y: U256): U256;
    native public fun mod(x: U256, y: U256): U256;
    native public fun eq(x: U256, y: U256): U256;
    native public fun ne(x: U256, y: U256): U256;
    native public fun gt(x: U256, y: U256): U256;
    native public fun lt(x: U256, y: U256): U256;
    native public fun ge(x: U256, y: U256): U256;
    native public fun le(x: U256, y: U256): U256;
    native public fun shl(x: U256, y: u8): U256;
    native public fun shr(x: U256, y: u8): U256;
}
