module 0x10::vector {
  native public fun empty<Element>(): vector<Element>;
  native public fun length<Element>(v: &vector<Element>): u64;
  native public fun push_back<Element>(v: &mut vector<Element>, e: Element);
  native public fun pop_back<Element>(v: &mut vector<Element>): Element;
  native public fun destroy_empty<Element>(v: vector<Element>);
  native public fun swap<Element>(v: &mut vector<Element>, i: u64, j: u64);
  native public fun borrow<Element>(v: &vector<Element>, i: u64): &Element;
  native public fun borrow_mut<Element>(v: &mut vector<Element>, i: u64): &mut Element;
}

module 0x10::tests {
  use 0x10::vector;


  public fun test_bool() {
    let v: vector<bool> = vector::empty();

    let len = vector::length(&v);
    assert!(len == 0, 10);

    vector::push_back(&mut v, true);
    vector::push_back(&mut v, false);

    let len = vector::length(&v);
    assert!(len == 2, 10);

    vector::swap(&mut v, 0, 1);

    let elt = vector::borrow(&v, 0);
    assert!(*elt == false, 10);
    let elt = vector::borrow_mut(&mut v, 0);
    assert!(*elt == false, 10);

    let elt = vector::pop_back(&mut v);
    assert!(elt == true, 10);
    let elt = vector::pop_back(&mut v);
    assert!(elt == false, 10);

    vector::destroy_empty(v);
  }

  public fun test_u8() {
    let v: vector<u8> = vector::empty();

    let len = vector::length(&v);
    assert!(len == 0, 10);

    vector::push_back(&mut v, 2);
    vector::push_back(&mut v, 3);

    let len = vector::length(&v);
    assert!(len == 2, 10);

    vector::swap(&mut v, 0, 1);

    let elt = vector::borrow(&v, 0);
    assert!(*elt == 3, 10);
    let elt = vector::borrow_mut(&mut v, 0);
    assert!(*elt == 3, 10);

    let elt = vector::pop_back(&mut v);
    assert!(elt == 2, 10);
    let elt = vector::pop_back(&mut v);
    assert!(elt == 3, 10);

    vector::destroy_empty(v);
  }

  public fun test_u16() {
    let v: vector<u16> = vector::empty();

    let len = vector::length(&v);
    assert!(len == 0, 10);

    vector::push_back(&mut v, 2);
    vector::push_back(&mut v, 3);

    let len = vector::length(&v);
    assert!(len == 2, 10);

    vector::swap(&mut v, 0, 1);

    let elt = vector::borrow(&v, 0);
    assert!(*elt == 3, 10);
    let elt = vector::borrow_mut(&mut v, 0);
    assert!(*elt == 3, 10);

    let elt = vector::pop_back(&mut v);
    assert!(elt == 2, 10);
    let elt = vector::pop_back(&mut v);
    assert!(elt == 3, 10);

    vector::destroy_empty(v);
  }

  public fun test_u64() {
    let v: vector<u64> = vector::empty();

    let len = vector::length(&v);
    assert!(len == 0, 10);

    vector::push_back(&mut v, 2);
    vector::push_back(&mut v, 3);

    let len = vector::length(&v);
    assert!(len == 2, 10);

    vector::swap(&mut v, 0, 1);

    let elt = vector::borrow(&v, 0);
    assert!(*elt == 3, 10);
    let elt = vector::borrow_mut(&mut v, 0);
    assert!(*elt == 3, 10);

    let elt = vector::pop_back(&mut v);
    assert!(elt == 2, 10);
    let elt = vector::pop_back(&mut v);
    assert!(elt == 3, 10);

    vector::destroy_empty(v);
  }

  public fun test_u128() {
    let v: vector<u128> = vector::empty();

    let len = vector::length(&v);
    assert!(len == 0, 10);

    vector::push_back(&mut v, 2);
    vector::push_back(&mut v, 3);

    let len = vector::length(&v);
    assert!(len == 2, 10);

    vector::swap(&mut v, 0, 1);

    let elt = vector::borrow(&v, 0);
    assert!(*elt == 3, 10);
    let elt = vector::borrow_mut(&mut v, 0);
    assert!(*elt == 3, 10);

    let elt = vector::pop_back(&mut v);
    assert!(elt == 2, 10);
    let elt = vector::pop_back(&mut v);
    assert!(elt == 3, 10);

    vector::destroy_empty(v);
  }

  public fun test_u256() {
    let v: vector<u256> = vector::empty();

    let len = vector::length(&v);
    assert!(len == 0, 10);

    vector::push_back(&mut v, 2);
    vector::push_back(&mut v, 3);

    let len = vector::length(&v);
    assert!(len == 2, 10);

    vector::swap(&mut v, 0, 1);

    let elt = vector::borrow(&v, 0);
    assert!(*elt == 3, 10);
    let elt = vector::borrow_mut(&mut v, 0);
    assert!(*elt == 3, 10);

    let elt = vector::pop_back(&mut v);
    assert!(elt == 2, 10);
    let elt = vector::pop_back(&mut v);
    assert!(elt == 3, 10);

    vector::destroy_empty(v);
  }

  public fun test_address() {
    let v: vector<address> = vector::empty();

    let len = vector::length(&v);
    assert!(len == 0, 10);

    vector::push_back(&mut v, @2);
    vector::push_back(&mut v, @3);

    let len = vector::length(&v);
    assert!(len == 2, 10);

    vector::swap(&mut v, 0, 1);

    let elt = vector::borrow(&v, 0);
    assert!(*elt == @3, 10);
    let elt = vector::borrow_mut(&mut v, 0);
    assert!(*elt == @3, 10);

    let elt = vector::pop_back(&mut v);
    assert!(elt == @2, 10);
    let elt = vector::pop_back(&mut v);
    assert!(elt == @3, 10);

    vector::destroy_empty(v);
  }

  // Test vector<vector<u32>>.
  public fun test_vec_vec_u32() {
    // Inner vector<u32> a.
    let vai1: vector<u32> = vector::empty();
    vector::push_back(&mut vai1, 8086);
    vector::push_back(&mut vai1, 8087);
    vector::push_back(&mut vai1, 8088);
    let vai2: vector<u32> = vector::empty();
    vector::push_back(&mut vai2, 1);
    vector::push_back(&mut vai2, 2);
    vector::push_back(&mut vai2, 3);

    // Inner vector<u32> b.
    let vbi1: vector<u32> = vector::empty();
    vector::push_back(&mut vbi1, 8086);
    vector::push_back(&mut vbi1, 8087);
    vector::push_back(&mut vbi1, 8088);
    let vbi2: vector<u32> = vector::empty();
    vector::push_back(&mut vbi2, 1);
    vector::push_back(&mut vbi2, 2);
    vector::push_back(&mut vbi2, 3);

    // Outer vector<vector<u32>> a.
    let vao: vector<vector<u32>> = vector::empty();
    vector::push_back(&mut vao, vai1);
    vector::push_back(&mut vao, vai2);

    // Outer vector<vector<u32>> b.
    let vbo: vector<vector<u32>> = vector::empty();
    vector::push_back(&mut vbo, vbi1);
    vector::push_back(&mut vbo, vbi2);

    assert!(vao == vbo, 20);

    // Inner vector<u32> c.
    let vci1: vector<u32> = vector::empty();
    vector::push_back(&mut vci1, 500);

    // Inner vector<u32> d.
    let vdi1: vector<u32> = vector::empty();
    vector::push_back(&mut vdi1, 600);

    // Outer vector<vector<u32>> a2.
    let va2o: vector<vector<u32>> = vector::empty();
    vector::push_back(&mut va2o, vci1);

    // Outer vector<vector<u32>> b2.
    let vb2o: vector<vector<u32>> = vector::empty();
    vector::push_back(&mut vb2o, vdi1);

    assert!(va2o != vb2o, 21);
  }

  // Test vector<vector<bool>>.
  public fun test_vec_vec_bool() {
    // Inner vector<bool> a.
    let vai1: vector<bool> = vector::empty();
    vector::push_back(&mut vai1, true);
    vector::push_back(&mut vai1, false);
    vector::push_back(&mut vai1, true);
    let vai2: vector<bool> = vector::empty();
    vector::push_back(&mut vai2, true);
    vector::push_back(&mut vai2, true);
    vector::push_back(&mut vai2, false);

    // Inner vector<bool> b.
    let vbi1: vector<bool> = vector::empty();
    vector::push_back(&mut vbi1, true);
    vector::push_back(&mut vbi1, false);
    vector::push_back(&mut vbi1, true);
    let vbi2: vector<bool> = vector::empty();
    vector::push_back(&mut vbi2, true);
    vector::push_back(&mut vbi2, true);
    vector::push_back(&mut vbi2, false);

    // Outer vector<vector<bool>> a.
    let vao: vector<vector<bool>> = vector::empty();
    vector::push_back(&mut vao, vai1);
    vector::push_back(&mut vao, vai2);

    // Outer vector<vector<bool>> b.
    let vbo: vector<vector<bool>> = vector::empty();
    vector::push_back(&mut vbo, vbi1);
    vector::push_back(&mut vbo, vbi2);

    assert!(vao == vbo, 30);

    // Inner vector<bool> c.
    let vci1: vector<bool> = vector::empty();
    vector::push_back(&mut vci1, false);

    // Inner vector<bool> d.
    let vdi1: vector<bool> = vector::empty();
    vector::push_back(&mut vdi1, true);

    // Outer vector<vector<bool>> a2.
    let va2o: vector<vector<bool>> = vector::empty();
    vector::push_back(&mut va2o, vci1);

    // Outer vector<vector<bool>> b2.
    let vb2o: vector<vector<bool>> = vector::empty();
    vector::push_back(&mut vb2o, vdi1);

    assert!(va2o != vb2o, 31);
  }
}

script {
  use 0x10::tests;

  fun main() {
    tests::test_bool();
    tests::test_u8();
    tests::test_u16();
    tests::test_u64();
    tests::test_u128();
    tests::test_u256();
    tests::test_address();
    tests::test_vec_vec_u32();
    tests::test_vec_vec_bool();
  }
}
