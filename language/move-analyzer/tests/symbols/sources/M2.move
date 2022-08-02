module Symbols::M2 {

    /// A struct with the drop property
    /// The drop property is great.
    struct SomeOtherStruct has drop {
        /// A doc string for a field in some other struct
        some_field: u64,
    }

    /// Test docstring for some other struct
    public fun some_other_struct(v: u64): SomeOtherStruct {
        SomeOtherStruct { some_field: v }
    }

    public fun multi_arg(p1: u64, p2: u64): u64 {
        p1 + p2
    }

}
