// false negatives for unsupported operators

address 0x123 {

  module M {

    public fun foo(i: u64): u64 { i & 0 }

    spec foo { ensures false; } // :(

  }

}
