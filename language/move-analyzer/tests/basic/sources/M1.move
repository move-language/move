module Basic::M1 {

    struct SomeStruct {
        some_field: u64,
    }

    const SOME_CONST: u64 = 42;


    fun unpack(s: SomeStruct): u64 {
        let SomeStruct { some_field: value } = s;
        value
    }

    fun cp(value: u64): u64 {
        let ret = value;
        ret
    }

    fun pack(): SomeStruct {
        let ret = SomeStruct { some_field: 7 };
        ret
    }

    fun other_mod_struct(): Basic::M2::SomeOtherStruct {
        Basic::M2::some_other_struct(7)
    }

    use Basic::M2::{Self, SomeOtherStruct};

    fun other_mod_struct_import(): SomeOtherStruct {
        M2::some_other_struct(7)
    }


}
