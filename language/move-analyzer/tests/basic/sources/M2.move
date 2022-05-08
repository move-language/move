module Basic::M2 {

    struct SomeOtherStruct {
        some_field: u64,
    }

    public fun some_other_struct(v: u64): SomeOtherStruct {
        SomeOtherStruct { some_field: v }
    }

}