module 0x100::Test {
  fun test_or(a: u8, b: u8): u8 {
    let c = a | b;
    c
  }
  fun test_and(a: u8, b: u8): u8 {
    let c = a & b;
    c
  }
  fun test_xor(a: u8, b: u8): u8 {
    let c = a ^ b;
    c
  }
  fun test_shl(a: u8, b: u8): u8 {
    let c = a << b;
    c
  }
  fun test_shr(a: u8, b: u8): u8 {
    let c = a >> b;
    c
  }
}
