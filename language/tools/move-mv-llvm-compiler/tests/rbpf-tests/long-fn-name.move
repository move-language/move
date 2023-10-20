// Testing that symbols are truncated to a size that passes rbpf's validation.

module 0x1::foo {
  public fun a_0123456789_0123456789_0123456789_0123456789_0123456789_0123456789_0123456789_(): u32 {
    2
  }
}

script {
  fun main() {
    let v = 0x1::foo::a_0123456789_0123456789_0123456789_0123456789_0123456789_0123456789_0123456789_();
    assert!(v == 2, 10);
  }
}
