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

}
