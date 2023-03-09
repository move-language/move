/// Module to decompose a move struct into it's components.
module std::struct_tag {
    use std::ascii::String;

    struct StructTag has copy, store, drop {
        /// Address of the package that the struct belongs to.
        /// taking `00000000000000000000000000000001::option::Option<u64>` for example, 
        /// it's package address will be `00000000000000000000000000000001`
        package_address: address,
        /// the name of the module where the struct is defined. 
        /// using the example struct above the module name should be `option`
        module_name: String,
        /// the name of the struct itself.
        /// using the example struct above the module name should be `Option`
        struct_name: String,
        /// the generics or tyepe params of the struct.
        /// using the example struct above the module name should be `vector[u64]`
        generics: vector<String>
    }

    /// Return the tag of the struct of type `T`
    public native fun get<T>(): StructTag;

    /// Returns the package address of `self`
    public fun package_address(self: StructTag): address {
        self.package_address
    }

    /// Returns the module name of `self`
    public fun module_name(self: StructTag): String {
        self.module_name
    }

    /// Returns the struct name of `self`
    public fun struct_name(self: StructTag): String {
        self.struct_name
    }

    /// Returns the generics of `self`
    public fun generics(self: StructTag): vector<String> {
        self.generics
    }

    #[test_only]
    public fun new_for_testing(package_address: address, module_name: String, struct_name: String, generics: vector<String>): StructTag {
        StructTag {
            package_address,
            module_name,
            struct_name,
            generics
        }
    }
}