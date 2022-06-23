module Symbols::M1 {

    struct SomeStruct has key, drop, store {
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
        let ret = SomeStruct { some_field: SOME_CONST };
        ret
    }

    fun other_mod_struct(): Symbols::M2::SomeOtherStruct {
        Symbols::M2::some_other_struct(SOME_CONST)
    }

    use Symbols::M2::{Self, SomeOtherStruct};

    fun other_mod_struct_import(): SomeOtherStruct {
        M2::some_other_struct(7)
    }

    fun acq(addr: address): u64 acquires SomeStruct {
        let val = borrow_global<SomeStruct>(addr);
        val.some_field
    }

    #[test]
    #[expected_failure]
    fun this_is_a_test() {
        1/0;
    }
}
