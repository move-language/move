// use-stdlib
// signers 0xcafe,0x1,0x2

// test_from_bytes is not a stdlib function,
// but for testing is defined in move-native.
module 0x10::bcs {
  native public fun test_from_bytes<MoveValue>(v: &vector<u8>): MoveValue;
}

module 0x10::tests {
  use 0x10::bcs as bcs_test;
  use 0x1::bcs;
  use 0x1::vector;

  public fun test_bool() {
    let v: bool = true;
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: bool = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  public fun test_u8() {
    let v: u8 = 50;
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: u8 = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  public fun test_u16() {
    let v: u16 = 50;
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: u16 = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  public fun test_u32() {
    let v: u32 = 50;
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: u32 = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  public fun test_u64() {
    let v: u64 = 50;
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: u64 = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  public fun test_u128() {
    let v: u128 = 50;
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: u128 = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  public fun test_u256() {
    let v: u256 = 50;
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: u256 = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  public fun test_address() {
    let v: address = @50;
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: address = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  public fun test_signer(v: signer) {
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: signer = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  struct TestStruct has drop {
    a: u8,
    b: u8,
  }

  public fun test_struct() {
    let v: TestStruct = TestStruct { a: 1, b: 2 };
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: TestStruct = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  public fun test_vec_bool() {
    let v: vector<bool> = vector::empty();
    vector::push_back(&mut v, true);
    vector::push_back(&mut v, false);
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: vector<bool> = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  public fun test_vec_u8() {
    let v: vector<u8> = vector::empty();
    vector::push_back(&mut v, 1);
    vector::push_back(&mut v, 2);
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: vector<u8> = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  public fun test_vec_u16() {
    let v: vector<u16> = vector::empty();
    vector::push_back(&mut v, 1);
    vector::push_back(&mut v, 2);
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: vector<u16> = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  public fun test_vec_u32() {
    let v: vector<u32> = vector::empty();
    vector::push_back(&mut v, 1);
    vector::push_back(&mut v, 2);
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: vector<u32> = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  public fun test_vec_u64() {
    let v: vector<u64> = vector::empty();
    vector::push_back(&mut v, 1);
    vector::push_back(&mut v, 2);
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: vector<u64> = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  public fun test_vec_u128() {
    let v: vector<u128> = vector::empty();
    vector::push_back(&mut v, 1);
    vector::push_back(&mut v, 2);
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: vector<u128> = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  public fun test_vec_u256() {
    let v: vector<u256> = vector::empty();
    vector::push_back(&mut v, 1);
    vector::push_back(&mut v, 2);
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: vector<u256> = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  public fun test_vec_address() {
    let v: vector<address> = vector::empty();
    vector::push_back(&mut v, @1);
    vector::push_back(&mut v, @2);
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: vector<address> = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  public fun test_vec_signer(s1: signer, s2: signer) {
    let v: vector<signer> = vector::empty();
    vector::push_back(&mut v, s1);
    vector::push_back(&mut v, s2);
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: vector<signer> = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  public fun test_vec_vec_bool() {
    let v: vector<vector<bool>> = vector::empty();
    {
      let velt: vector<bool> = vector::empty();
      vector::push_back(&mut velt, true);
      vector::push_back(&mut velt, false);
      vector::push_back(&mut v, velt);
      let velt: vector<bool> = vector::empty();
      vector::push_back(&mut velt, false);
      vector::push_back(&mut velt, true);
      vector::push_back(&mut v, velt);
    };
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: vector<vector<bool>> = bcs_test::test_from_bytes(&vs);
    assert!(v == vv, 11);
  }

  struct TestVecStruct has drop {
    a: u8,
    b: u8,
  }

  public fun test_vec_struct() {
    let v: vector<TestVecStruct> = vector::empty();
    vector::push_back(&mut v, TestVecStruct { a: 1, b: 2 });
    vector::push_back(&mut v, TestVecStruct { a: 3, b: 4 });
    let vs: vector<u8> = bcs::to_bytes(&v);
    let vv: vector<TestVecStruct> = bcs_test::test_from_bytes(&vs);
    assert!(&v == &vv, 11);
    assert!(v == vv, 12);
  }
}

script {
  use 0x10::tests;

  fun main(s: signer, s1: signer, s2: signer) {
    tests::test_bool();
    tests::test_u8();
    tests::test_u16();
    tests::test_u32();
    tests::test_u64();
    tests::test_u128();
    tests::test_u256();
    tests::test_address();
    tests::test_struct();
    tests::test_signer(s);
    tests::test_vec_bool();
    tests::test_vec_u8();
    tests::test_vec_u16();
    tests::test_vec_u32();
    tests::test_vec_u64();
    tests::test_vec_u128();
    tests::test_vec_u256();
    tests::test_vec_address();
    tests::test_vec_signer(s1, s2);
    tests::test_vec_vec_bool();
    tests::test_vec_struct();
  }
}
