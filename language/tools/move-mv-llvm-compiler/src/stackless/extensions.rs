// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! Extension traits for foreign types.

use crate::stackless::llvm;
use extension_trait::extension_trait;
use move_binary_format::file_format::SignatureToken;
use move_model::{model as mm, ty as mty};

#[extension_trait]
pub impl<'a> ModuleEnvExt for mm::ModuleEnv<'a> {
    fn llvm_module_name(&self) -> String {
        self.get_full_name_str().replace(':', "_")
    }
}

#[extension_trait]
pub impl<'a> FunctionEnvExt for mm::FunctionEnv<'a> {
    fn llvm_symbol_name(&self, tyvec: &[mty::Type]) -> String {
        let mut name = self.get_full_name_str();
        if name == "<SELF>::<SELF>" {
            // fixme move-model names script fns "<SELF>".
            // we might want to preserve the actual names
            "main".to_string()
        } else {
            for ty in tyvec {
                name += &format!("_{}", ty.display(&self.get_type_display_ctx()))
            }
            name.replace([':', '<', '>'], "_").replace(", ", "_")
        }
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
            Type::Primitive(PrimitiveType::Address) => 8 * 32,
            Type::Primitive(PrimitiveType::Signer) => 8 * 32,
            Type::Reference(_, _) => 64,
            Type::Vector(_) => 3 * 64,
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
