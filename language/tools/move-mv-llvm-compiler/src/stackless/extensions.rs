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
pub impl<'a> FunctionEnxExt for mm::FunctionEnv<'a> {
    fn llvm_symbol_name(&self) -> String {
        // fixme use get_full_name_str
        let name = self.get_name_str();
        if name == "<SELF>" {
            // fixme move-model names script fns "<SELF>".
            // we might want to preserve the actual names
            "main".to_string()
        } else {
            name
        }
    }
}
