module 0x42::M {
  fun mul_div(a: u64, b: u64, c: u64): u64 {
    (((a as u128) * (b as u128) / (c as u128)) as u64)
  }

  fun mul_div_test(a: u64, b: u64, c: u64): u64 {
    mul_div(a, b, c) + 1
  }
  spec mul_div_test {
    ensures result == mul_div(a, b, c) + 1;
  }
}
