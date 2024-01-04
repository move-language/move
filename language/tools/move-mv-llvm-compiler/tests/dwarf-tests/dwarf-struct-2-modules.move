module 0x201::M {

    struct MyStruct_2 has copy, drop {
        field1_u32:  u32,
        other_my_struct_from_101: 0x101::M::MyStruct
    }

    public fun fun_2(): 0x201::M::MyStruct_2 {
        let my_struct_2 = MyStruct_2 {
            field1_u32: 15,
            other_my_struct_from_101: 0x101::M::fun_1()
        };
        let my_struct_copy_2 = my_struct_2;
        my_struct_copy_2
    }

}
module 0x101::M {

    struct MyStruct has copy, drop {
        field1_u32:  u32,
        field2_bool: bool,
        field3_empty: EmptyStruct,
        field4_u8:   u8,
        field6_combined: Combined
    }

    struct EmptyStruct has copy, drop {}

    struct U64Struct has copy, drop { field_u64: u64 }

    struct Combined has copy, drop { field_combined_bool: bool, field_combined_u64_struct: U64Struct}

    public fun fun_1(): 0x101::M::MyStruct {
        let my_struct = MyStruct {
            field1_u32: 15,
            field2_bool: true,
            field3_empty: EmptyStruct {},
            field4_u8: 7,
            field6_combined: Combined {
                field_combined_bool: false,
                field_combined_u64_struct: U64Struct { field_u64: 1}
            }
        };
        let my_struct_copy = my_struct;
        my_struct_copy
    }
}
