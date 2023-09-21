// abort 4017

module 0x101::Test1 {
  public fun test_castu16(a: u128): u16 {
    (a as u16)
  }
}

script {
  fun main() {
    assert!(0x101::Test1::test_castu16(65535u128) == 65535, 10);  // Ok: source fits in dest.

    0x101::Test1::test_castu16(65536u128);  // Abort: source too big.
  }
}
