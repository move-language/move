// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! Extension traits for foreign types.

use crate::stackless::llvm;
use extension_trait::extension_trait;
use move_binary_format::file_format::SignatureToken;
use move_core_types::account_address;
use move_model::{model as mm, ty as mty};
use move_native::shared::MOVE_UNTYPED_VEC_DESC_SIZE;

#[extension_trait]
pub impl<'a> ModuleEnvExt for mm::ModuleEnv<'a> {
    fn llvm_module_name(&self) -> String {
        self.get_full_name_str().replace(':', "_")
    }
}

#[extension_trait]
pub impl<'a> FunctionEnvExt for mm::FunctionEnv<'a> {
    fn llvm_symbol_name(&self, tyvec: &[mty::Type]) -> String {
        let name = self.get_full_name_str();
        if name == "<SELF>::<SELF>" {
            // fixme move-model names script fns "<SELF>".
            // we might want to preserve the actual names
            "main".to_string()
        } else {
            self.llvm_symbol_name_full(tyvec)
        }
    }

    /// Generate a symbol name that is less than 64 bytes long.
    ///
    /// The rbpf VM supports symbol names up to 64 bytes in length
    /// (defined by SYMBOL_NAME_LENGTH_MAXIMUM within rbpf),
    /// the last byte of which is 0, so we have 63 bytes to work with.
    /// We also need to ensure that names are unique with no collisions
    /// across modules.
    ///
    /// The scheme is:
    ///
    /// - 16 bytes - The low 8 bytes of the module address, hex encoded.
    /// -  1 byte  - "_"
    /// - 15 bytes - The first 15 bytes (or fewer) of the module name.
    /// -  1 byte  - "_"
    /// - 15 bytes - The first 15 bytes (or fewer) of the function name.
    /// -  1 byte  - "_"
    /// - 14 bytes - The type hash (below).
    ///
    /// It sacrifices three bytes to separator characters for readability.
    ///
    /// ## The type hash
    ///
    /// The type hash attempts to ensure uniqueness. It includes the full
    /// module address, module name, function name, and type parameters.
    /// Because there are only N bytes available for the encoded hash, it
    /// is not particularly strong, but we probably don't need to worry about
    /// adversarial scenarios with symbol naming. We use base58 encoding to get
    /// the most bits out of the hash while using only alphanumerics.
    fn llvm_symbol_name_full(&self, tyvec: &[mty::Type]) -> String {
        let module_env = &self.module_env;
        let module_address = module_env.self_address().to_canonical_string();
        let module_name = module_env
            .get_name()
            .display(module_env.symbol_pool())
            .to_string();
        let function_name = self.get_name_str();
        let type_names = tyvec
            .iter()
            .map(|ty| ty.display(&self.get_type_display_ctx()).to_string());

        const MODULE_ADDRESS_LEN: usize = 16;
        const MODULE_NAME_LEN: usize = 15;
        const FUNCTION_NAME_LEN: usize = 15;
        const HASH_LEN: usize = 14;

        let formatted_module_address =
            &module_address[(module_address.len() - MODULE_ADDRESS_LEN)..];
        let formatted_module_name = &module_name[..MODULE_NAME_LEN.min(module_name.len())];
        let formatted_function_name = &function_name[..FUNCTION_NAME_LEN.min(function_name.len())];

        let hash: String = {
            let mut hasher = blake3::Hasher::new();
            hasher.update(module_address.as_bytes());
            hasher.update(module_name.as_bytes());
            hasher.update(b"."); // This avoids the ambiguity of sequentially hashing two user-controlled names.
            hasher.update(function_name.as_bytes());

            // Hash the types. This is just hashing the display names, which will be less
            // efficient and precise than hashing some kind of type id (perhaps the `TypeTag`).
            // To guarantee uniqueness these type names probably should have their module address etc.
            // hashed as well, which wouldn't be necessary if hashing a globally-unique type id.
            for ty in type_names {
                hasher.update(b".");
                hasher.update(ty.as_bytes());
            }

            let hash = hasher.finalize();
            let mut hash_base58 = bs58::encode(hash.as_bytes()).into_string();
            hash_base58.truncate(HASH_LEN);
            hash_base58
        };

        let symbol = format!(
            "{formatted_module_address}_{formatted_module_name}_{formatted_function_name}_{hash}"
        );
        assert!(symbol.len() < 64);

        symbol
    }

    /// Entry points follow their own naming convention
    fn llvm_symbol_name_entrypoint(&self) -> String {
        self.get_full_name_str().replace(':', "_")
    }

    /// Native functions follow their own naming convention
    fn llvm_native_fn_symbol_name(&self) -> String {
        let name = self.get_full_name_str();
        let name = name.replace("::", "_");
        format!("move_native_{name}")
    }

    fn llvm_linkage(&self) -> llvm::LLVMLinkage {
        if self.is_exposed() || self.is_native() {
            llvm::LLVMLinkage::LLVMExternalLinkage
        } else {
            llvm::LLVMLinkage::LLVMPrivateLinkage
        }
    }

    /// Gets the qualified inst id of this function (not in the model yet).
    fn get_qualified_inst_id(&self, inst: Vec<mty::Type>) -> mm::QualifiedInstId<mm::FunId> {
        self.module_env.get_id().qualified_inst(self.get_id(), inst)
    }
}

#[extension_trait]
pub impl FunIdExt for mm::FunId {
    fn qualified(&self, m: mm::ModuleId) -> mm::QualifiedId<mm::FunId> {
        mm::QualifiedId {
            module_id: m,
            id: *self,
        }
    }
}

#[extension_trait]
pub impl TypeExt for mty::Type {
    /// Used by rttydesc to name type descriptors.
    fn sanitized_display_name(&self, type_display_ctx: &mty::TypeDisplayContext) -> String {
        let name = format!("{}", self.display(type_display_ctx));
        name.replace(['<', '>', ':'], "_")
    }

    fn is_number_u8(&self) -> bool {
        if let mty::Type::Primitive(mty::PrimitiveType::U8) = self {
            return true;
        }
        false
    }

    fn vector_element_type(&self) -> mty::Type {
        if let mty::Type::Vector(et) = self {
            (**et).clone()
        } else {
            unreachable!()
        }
    }

    // Primitive type :: number width.
    fn get_bitwidth(&self) -> u64 {
        use mty::{PrimitiveType, Type};

        match self {
            Type::Primitive(PrimitiveType::Bool) => 1,
            Type::Primitive(PrimitiveType::U8) => 8,
            Type::Primitive(PrimitiveType::U16) => 16,
            Type::Primitive(PrimitiveType::U32) => 32,
            Type::Primitive(PrimitiveType::U64) => 64,
            Type::Primitive(PrimitiveType::U128) => 128,
            Type::Primitive(PrimitiveType::U256) => 256,
            Type::Primitive(PrimitiveType::Address) => {
                8 * account_address::AccountAddress::LENGTH as u64
            }
            Type::Primitive(PrimitiveType::Signer) => {
                8 * account_address::AccountAddress::LENGTH as u64
            }
            Type::Reference(_, _) => 64,
            Type::Vector(_) => 8 * MOVE_UNTYPED_VEC_DESC_SIZE,
            Type::Struct(_m, _s, ref tys) => tys.iter().fold(0, |acc, ty| acc + ty.get_bitwidth()),
            _ => {
                todo!("{self:?}")
            }
        }
    }
}

#[extension_trait]
pub impl<'a> StructEnvExt for mm::StructEnv<'a> {
    fn ll_struct_name_from_raw_name(&self, tys: &[mty::Type]) -> String {
        let raw_name = self.struct_raw_type_name(tys);
        let xs = raw_name.replace([':', '<', '>'], "_").replace(", ", ".");
        format!("struct.{}", xs)
    }

    fn struct_raw_type_name(&self, tys: &[mty::Type]) -> String {
        let qid = self.get_qualified_id();
        let s = mty::Type::Struct(qid.module_id, qid.id, tys.to_vec());
        format!("{}", s.display(&self.module_env.env.get_type_display_ctx()))
    }
}

#[extension_trait]
pub impl SignatureTokenExt for SignatureToken {
    fn find_struct_instantiation_signatures(
        sig: &SignatureToken,
        inst_signatures: &mut Vec<SignatureToken>,
    ) {
        match sig {
            SignatureToken::Reference(t) | SignatureToken::MutableReference(t) => {
                Self::find_struct_instantiation_signatures(t, inst_signatures);
            }
            SignatureToken::Vector(bt) => {
                Self::find_struct_instantiation_signatures(bt, inst_signatures);
            }
            SignatureToken::StructInstantiation(_, args) => {
                // Instantiations may contain nested instantiations.
                for arg in args {
                    Self::find_struct_instantiation_signatures(arg, inst_signatures);
                }
                inst_signatures.push(sig.clone());
            }
            _ => {}
        };
    }
}

#[test]
fn test_symbol_name() {
    use move_compiler::shared::PackagePaths;
    use move_core_types::{
        account_address::AccountAddress,
        identifier::{IdentStr, Identifier},
        language_storage::ModuleId,
    };
    use move_model::run_model_builder;
    use std::{collections::BTreeMap, path::PathBuf};

    let model = {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let manifest_dir = PathBuf::from(manifest_dir);
        let move_path = "../../tools/move-mv-llvm-compiler/tests/rbpf-tests/call-local.move";
        let move_path = manifest_dir.join(move_path);

        let named_address_map: BTreeMap<String, _> = BTreeMap::new();

        let sources = vec![PackagePaths {
            name: None,
            paths: vec![move_path.to_string_lossy().to_string()],
            named_address_map,
        }];

        run_model_builder(sources, vec![]).unwrap()
    };

    let fun = model
        .find_function_by_language_storage_id_name(
            &ModuleId::new(
                AccountAddress::from_hex_literal("0x101").unwrap(),
                Identifier::new("foo").unwrap(),
            ),
            IdentStr::new("a").unwrap(),
        )
        .unwrap();

    let symbol = fun.llvm_symbol_name_full(&[mty::Type::Primitive(mty::PrimitiveType::Bool)]);

    assert_eq!(symbol, "0000000000000101_foo_a_7JBHPr3AYTvPmP");
}
