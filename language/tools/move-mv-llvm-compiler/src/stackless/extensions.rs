// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! Extension traits for foreign types.

use extension_trait::extension_trait;
use move_model::model as mm;

#[extension_trait]
pub impl<'a> ModuleEnvExt for mm::ModuleEnv<'a> {
    fn llvm_module_name(&self) -> String {
        self.get_full_name_str().replace(':', "_")
    }
}

#[extension_trait]
pub impl<'a> FunctionEnvExt for mm::FunctionEnv<'a> {
    fn llvm_symbol_name(&self) -> String {
        let name = self.get_full_name_str();
        if name == "<SELF>::<SELF>" {
            // fixme move-model names script fns "<SELF>".
            // we might want to preserve the actual names
            "main".to_string()
        } else {
            name.replace(':', "_")
        }
    }

    /// Native functions follow their own naming convention
    fn llvm_native_fn_symbol_name(&self) -> String {
        let name = self.get_full_name_str();
        let name = name.replace("::", "_");
        format!("move_native_{name}")
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
