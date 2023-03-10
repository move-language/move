/// Module to decompose a move struct into it's components.
module std::struct_tag {
    use std::ascii::{Self, String};

    struct StructTag has copy, store, drop {
        /// Address of the entity that the struct belongs to.
        /// taking `00000000000000000000000000000001::option::Option<u64>` for example, 
        /// the address will be `00000000000000000000000000000001`
        address_: address,
        /// the name of the module where the struct is defined. 
        /// using the example struct above the module name should be `option`
        module_name: String,
        /// the name of the struct itself.
        /// using the example struct above the module name should be `Option`
        struct_name: String,
        /// the generics or tyepe params of the struct.
        /// using the example struct above the module name should be `vector["u64"]`
        generics: vector<String>
    }

    /// Returns the tag of the struct of type `T`
    public native fun get<T>(): StructTag;

    // Converts `self` into a tuple of it's inner values
    public fun into(self: &StructTag): (address, String, String, vector<String>) {
        (self.address_, self.module_name, self.struct_name, self.generics)
    }

    /// Returns the module authority for the struct of type `T`
    public fun module_authority<T>(): StructTag {
        let StructTag { 
            address_, 
            module_name, 
            struct_name: _, 
            generics: _ 
        } = get<T>();

        StructTag {
            address_,
            module_name,
            struct_name: ascii::string(b"Witness"),
            generics: vector[]
        }
    }

    #[test_only]
    public fun new_for_testing(address_: address, module_name: String, struct_name: String, generics: vector<String>): StructTag {
        StructTag {
            address_,
            module_name,
            struct_name,
            generics
        }
    }
}