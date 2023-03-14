// regression-test for incorrectly double-declaring function `a` here.

module 0x101::foo {
  public fun a(): u8 {
    1
  }

  public fun b(): u8 {
    a()
  }
}

script {
  use 0x101::foo;

  fun main() {
    let v = foo::a();
    assert!(v == 1, 11);
  }
}
