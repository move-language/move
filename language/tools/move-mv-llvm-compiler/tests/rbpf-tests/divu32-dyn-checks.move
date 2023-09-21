// abort 4017

module 0x101::Test1 {
  public fun test_divu32(a: u32, b: u32): u32 {
    let c = a / b;
    c
  }
}

script {
  fun main() {
    let a: u32 = 32;
    assert!(0x101::Test1::test_divu32(a, 8) == 4, 10);  // Ok: no div by zero.

    0x101::Test1::test_divu32(a, 0);  // Abort: division by zero.
  }
}
