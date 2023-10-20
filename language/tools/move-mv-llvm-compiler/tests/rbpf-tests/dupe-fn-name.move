// Testing that symbols are mangled to avoid basic collisions.

module 0x1::foo {
  public fun a(): u32 {
    2
  }
}

module 0x2::foo {
  public fun a(): u32 {
    2
  }
}

script {
  fun main() {
    let v1 = 0x1::foo::a();
    let v2 = 0x1::foo::a();
    assert!(v1 == v2, 10);
  }
}
