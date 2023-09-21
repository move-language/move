// use-stdlib

// Testing that the runtime can destroy vectors
// with non-zero number of elements.
//
// At time of writing the compiler does not always
// generate vector destructor calls. The particular
// pattern here, with make returning a vector,
// does generate destructor calls.

module 0x2::tests {
  use 0x1::vector;

  fun make<T>(elt: T) : vector<T> {
    let v: vector<T> = vector::empty();
    vector::push_back(&mut v, elt);
    v
  }

  fun make2<T>(elt1: T, elt2: T) : vector<T> {
    let v: vector<T> = vector::empty();
    vector::push_back(&mut v, elt1);
    vector::push_back(&mut v, elt2);
    v
  }

  public fun test_bool() {
    make(true);
  }

  public fun test_u8() {
    make(1_u8);
  }

  public fun test_u16() {
    make(1_u16);
  }

  public fun test_u32() {
    make(1_u32);
  }

  public fun test_u64() {
    make(1_u64);
  }

  public fun test_u128() {
    make(1_u128);
  }

  public fun test_u256() {
    make(1_u256);
  }

  public fun test_vec_u8() {
    make(make2(1_u256, 2));
  }

  public fun test_vec_vec_u8() {
    make(make(make2(1_u256, 2)));
  }

  struct S1 has drop {
  }

  public fun test_struct1() {
    make(make2(S1 { }, S1 { }));
  }

  struct S2 has drop {
    f1: vector<u8>,
    f2: bool,
    f3: vector<S3>,
  }

  struct S3 has drop {
    f1: vector<S1>,
  }

  public fun test_struct2() {
    make(make2(
      S2 {
        f1: make(1),
        f2: true,
        f3: make(S3 {
          f1: make(S1 { }),
        }),
      },
      S2 {
        f1: make(2),
        f2: true,
        f3: make(S3 {
          f1: make(S1 { }),
        }),
      }
    ));
  }
}

script {
  use 0x2::tests;

  fun main() {
    tests::test_bool();
    tests::test_u8();
    tests::test_u16();
    tests::test_u32();
    tests::test_u64();
    tests::test_u128();
    tests::test_u256();
    tests::test_vec_u8();
    tests::test_vec_vec_u8();
    tests::test_struct1();
    tests::test_struct2();
  }
}
