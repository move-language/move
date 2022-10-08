
use crate::file_format::*;

#[cfg(kani)]
pub mod verification {
    use super::*;

    macro_rules! define_index_kani {
        {
            name: $name: ident,
        } => {

            impl kani::Arbitrary for $name {
                fn any()  -> Self {
                    let idx = kani::any::<TableIndex>();
                    Self(idx)
                }
            }

        };
    }

    define_index_kani! {
        name: ModuleHandleIndex,
    }
    define_index_kani! {
        name: StructHandleIndex,
    }
    define_index_kani! {
        name: FunctionHandleIndex,
    }
    define_index_kani! {
        name: FieldHandleIndex,
    }
    define_index_kani! {
        name: StructDefInstantiationIndex,
    }
    define_index_kani! {
        name: FunctionInstantiationIndex,
    }
    define_index_kani! {
        name: FieldInstantiationIndex,
    }
    define_index_kani! {
        name: IdentifierIndex,
    }
    define_index_kani! {
        name: AddressIdentifierIndex,
    }
    define_index_kani! {
        name: ConstantPoolIndex,
    }
    define_index_kani! {
        name: StructDefinitionIndex,
    }
    define_index_kani! {
        name: SignatureIndex,
    }
    define_index_kani! {
        name: FunctionDefinitionIndex,
    }

    impl kani::Arbitrary for ModuleHandle {
        fn any() -> Self {
            ModuleHandle {
                address: kani::any::<AddressIdentifierIndex>(),
                name: kani::any::<IdentifierIndex>(),
            }
        }
    }

    impl kani::Arbitrary for Bytecode {
        fn any() -> Self {
            use super::Bytecode::*;
            match kani::any::<u8>() {
                0 => Pop,
                1 => Ret,
                2 => BrTrue(kani::any()),
                3 => BrFalse(kani::any()),
                4 => Branch(kani::any()),
                5 => LdU8(kani::any()),
                6 => LdU64(kani::any()),
                7 => LdU128(kani::any()),
                8 => CastU8,
                9 => CastU64,
                10 => CastU128,
                11 => LdConst(kani::any()),
                12 => LdTrue,
                13 => LdFalse,
                14 => CopyLoc(kani::any()),
                15 => MoveLoc(kani::any()),
                16 => StLoc(kani::any()),
                17 => Call(kani::any()),
                18 => CallGeneric(kani::any()),
                19 => Pack(kani::any()),
                20 => PackGeneric(kani::any()),
                21 => Unpack(kani::any()),
                22 => UnpackGeneric(kani::any()),
                23 => ReadRef,
                24 => WriteRef,
                25 => FreezeRef,
                26 => MutBorrowLoc(kani::any()),
                27 => ImmBorrowLoc(kani::any()),
                28 => MutBorrowField(kani::any()),
                29 => MutBorrowFieldGeneric(kani::any()),
                30 => ImmBorrowField(kani::any()),
                31 => ImmBorrowFieldGeneric(kani::any()),
                32 => MutBorrowGlobal(kani::any()),
                33 => MutBorrowGlobalGeneric(kani::any()),
                34 => ImmBorrowGlobal(kani::any()),
                35 => ImmBorrowGlobalGeneric(kani::any()),
                36 => Add,
                37 => Sub,
                38 => Mul,
                39 => Mod,
                40 => Div,
                41 => BitOr,
                42 => BitAnd,
                43 => Xor,
                44 => Or,
                45 => And,
                46 => Not,
                47 => Eq,
                48 => Neq,
                49 => Lt,
                50 => Gt,
                51 => Le,
                52 => Ge,
                53 => Abort,
                54 => Nop,
                55 => Exists(kani::any()),
                56 => ExistsGeneric(kani::any()),
                57 => MoveFrom(kani::any()),
                58 => MoveFromGeneric(kani::any()),
                59 => MoveTo(kani::any()),
                60 => MoveToGeneric(kani::any()),
                61 => Shl,
                62 => Shr,
                63 => VecPack(kani::any(), kani::any()),
                64 => VecLen(kani::any()),
                65 => VecImmBorrow(kani::any()),
                66 => VecMutBorrow(kani::any()),
                67 => VecPushBack(kani::any()),
                69 => VecPopBack(kani::any()),
                70 => VecUnpack(kani::any(), kani::any()),
                _ => VecSwap(kani::any()),
            }
        }
    }

    impl kani::Arbitrary for Constant {
        fn any() -> Self {
            Constant {
                type_: kani::any(),
                data: {
                    let mut data = vec![];
                    fill(&mut data);
                    data
                },
            }
        }
    }

    impl kani::Arbitrary for SignatureToken {
        fn any() -> Self {
            match kani::any::<u8>() {
                0 => SignatureToken::Bool,
                1 => SignatureToken::U8,
                2 => SignatureToken::U64,
                3 => SignatureToken::U128,
                4 => SignatureToken::Address,
                5 => SignatureToken::Signer,
                6 => SignatureToken::Vector(Box::new(kani::any::<SignatureToken>())),
                7 => SignatureToken::Struct(kani::any()),
                8 => {
                    let mut sig_tokens = vec![];
                    fill(&mut sig_tokens);
                    SignatureToken::StructInstantiation(kani::any(), sig_tokens)
                },
                9 => SignatureToken::Reference(Box::new(kani::any::<SignatureToken>())),
                10 => SignatureToken::MutableReference(Box::new(kani::any::<SignatureToken>())),
                _ => SignatureToken::TypeParameter(kani::any())
            }
        }
    }

    impl kani::Arbitrary for Ability {
        fn any() -> Self {
            match kani::any::<u8>() {
                0 => Ability::Copy,
                1 => Ability::Drop,
                2 => Ability::Store,
                _ => Ability::Key,
            }
        }
    }

    impl kani::Arbitrary for AbilitySet {
        fn any() -> Self {
            match kani::any::<u8>() {
                0 => AbilitySet::EMPTY,
                1 => AbilitySet::PRIMITIVES,
                2 => AbilitySet::REFERENCES,
                3 => AbilitySet::SIGNER,
                4 => AbilitySet::VECTOR,
                _ => AbilitySet::ALL
            }
        }
    }

    impl kani::Arbitrary for StructTypeParameter {
        fn any() -> Self {
            StructTypeParameter {
                constraints: kani::any(),
                is_phantom: kani::any::<bool>(),
            }
        }
    }

    impl kani::Arbitrary for StructHandle {
        fn any() -> Self {
            let mut type_paras = vec![];
            let num_of_type_params = kani::any();
            for _i in 0..num_of_type_params {
                type_paras.push(kani::any())
            }
            StructHandle {
                module: kani::any(),
                name: kani::any(),
                abilities: kani::any(),
                type_parameters: type_paras
            }
        }
    }

    impl kani::Arbitrary for FieldHandle {
        fn any() -> Self {
            FieldHandle {
                owner: kani::any(),
                field: kani::any(),
            }
        }
    }

    /*
    pub struct TypeSignature(pub SignatureToken);
     */

    impl kani::Arbitrary for TypeSignature {
        fn any() -> Self {
            TypeSignature(kani::any())
        }
    }

    impl kani::Arbitrary for FunctionSignature {
        fn any() -> Self {
            let mut return_ = vec![];
            let mut parameters = vec![];
            let mut type_parameters = vec![];
            fill(&mut return_);
            fill(&mut parameters);
            fill(&mut type_parameters);
            FunctionSignature {
                return_, parameters, type_parameters
            }
        }
    }

    impl kani::Arbitrary for Signature {
        fn any() -> Self {
            let mut sigs = vec![];
            fill(&mut sigs);
            Signature(sigs)
        }
    }

    impl kani::Arbitrary for CodeUnit {
        fn any() -> Self {
            let mut code = vec![];
            fill(&mut code);
            CodeUnit {
                locals: kani::any(),
                code
            }
        }
    }

    impl kani::Arbitrary for Visibility {
        fn any() -> Self {
           match kani::any::<u8>() {
               0 => Visibility::Private,
               1 => Visibility::Public,
               _ => Visibility::Friend
           }
        }
    }

    impl kani::Arbitrary for FieldDefinition {
        fn any() -> Self {
            FieldDefinition {
                name: kani::any(),
                signature: kani::any(),
            }
        }
    }

    impl kani::Arbitrary for StructFieldInformation {
        fn any() -> Self {
            match kani::any::<u8>() {
                0 => StructFieldInformation::Native,
                _ => {
                    let mut defs = vec![];
                    fill(&mut defs);
                    StructFieldInformation::Declared(defs)
                }
            }
        }
    }

    impl kani::Arbitrary for StructDefInstantiation {
        fn any() -> Self {
            StructDefInstantiation {
                def: kani::any(),
                type_parameters: kani::any()
            }
        }
    }

    impl kani::Arbitrary for FunctionInstantiation {
        fn any() -> Self {
            FunctionInstantiation {
                handle: kani::any(),
                type_parameters: kani::any()
            }
        }
    }

    impl kani::Arbitrary for FieldInstantiation {
        fn any() -> Self {
            FieldInstantiation {
                handle: kani::any(),
                type_parameters: kani::any()
            }
        }
    }

    impl kani::Arbitrary for StructDefinition {
        fn any() -> Self {
            StructDefinition {
                struct_handle: kani::any(),
                field_information: kani::any(),
            }
        }
    }

    impl kani::Arbitrary for FunctionDefinition {
        fn any() -> Self {
            let mut acquires_global_resources = vec![];
            let idx = kani::any();
            for _i in 0..idx {
                acquires_global_resources.push(kani::any())
            }
            FunctionDefinition {
                function: kani::any(),
                visibility: kani::any(),
                is_entry: kani::any(),
                acquires_global_resources,
                code: {
                    match kani::any::<bool>() {
                        false => None,
                        true => Some(kani::any())
                    }
                }
            }
        }
    }

    impl kani::Arbitrary for FunctionHandle {
        fn any() -> Self {
            FunctionHandle {
                module: kani::any(),
                name: kani::any(),
                parameters: kani::any(),
                return_: kani::any(),
                type_parameters: {
                    let mut type_parameters = vec![];
                    fill(&mut type_parameters);
                    type_parameters
                }
            }
        }
    }

    impl kani::Arbitrary for CompiledScript {
       fn any() -> Self {
           CompiledScript {
               version: kani::any(),
               module_handles: {
                   let mut handles = vec![];
                   fill(&mut handles);
                   handles
               },
               struct_handles: {
                   let mut handles = vec![];
                   fill(&mut handles);
                   handles
               },
               function_handles: {
                   let mut handles = vec![];
                   fill(&mut handles);
                   handles
               },
               function_instantiations: {
                   let mut handles = vec![];
                   fill(&mut handles);
                   handles
               },
               signatures: {
                   let mut signatures = vec![];
                   fill(&mut signatures);
                   signatures
               },
               identifiers: {
                   let mut identifiers = vec![];
                   fill(&mut identifiers);
                   identifiers
               },
               address_identifiers: {
                   let mut identifiers = vec![];
                   fill(&mut identifiers);
                   identifiers
               },
               constant_pool: {
                   let mut constants = vec![];
                   fill(&mut constants);
                   constants
               },
               metadata: {
                   vec![]
               },
               code: kani::any(),
               type_parameters: {
                   let mut ability_set = vec![];
                   fill(&mut ability_set);
                   ability_set
               },
               parameters: kani::any()
           }
       }
    }

    impl kani::Arbitrary for CompiledModule {
        fn any() -> Self {
            CompiledModule {
                version: kani::any(),
                self_module_handle_idx: kani::any(),
                module_handles: {
                    let mut handles = vec![];
                    fill(&mut handles);
                    handles
                },
                struct_handles: {
                    let mut handles = vec![];
                    fill(&mut handles);
                    handles
                },
                function_handles: {
                    let mut handles = vec![];
                    fill(&mut handles);
                    handles
                },
                field_handles: {
                    let mut handles = vec![];
                    fill(&mut handles);
                    handles
                },
                friend_decls: {
                    let mut handles = vec![];
                    fill(&mut handles);
                    handles
                },
                struct_def_instantiations:{
                    let mut instantiations = vec![];
                    fill(&mut instantiations);
                    instantiations
                },
                function_instantiations: {
                    let mut instantiations = vec![];
                    fill(&mut instantiations);
                    instantiations
                },
                field_instantiations: {
                    let mut instantiations = vec![];
                    fill(&mut instantiations);
                    instantiations
                },
                signatures: {
                    let mut signatures = vec![];
                    fill(&mut signatures);
                    signatures
                },
                identifiers: {
                    let mut identifiers = vec![];
                    fill(&mut identifiers);
                    identifiers
                },
                address_identifiers: {
                    let mut identifiers = vec![];
                    fill(&mut identifiers);
                    identifiers
                },
                constant_pool: {
                    let mut constants = vec![];
                    fill(&mut constants);
                    constants
                },
                metadata: {
                    vec![]
                },
                struct_defs: {
                    let mut defs = vec![];
                    fill(&mut defs);
                    defs
                },
                function_defs: {
                    let mut defs = vec![];
                    fill(&mut defs);
                    defs
                }
            }
        }
    }

    fn fill<T:kani::Arbitrary>(handles: &mut Vec<T>) {
        let idx = kani::any();
        for _i in 0..idx {
            handles.push(kani::any());
        }
    }

}
