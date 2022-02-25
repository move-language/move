// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{context::Context, Generator};
use move_model::{
    ast::ModuleName,
    emit, emitln,
    model::{FunId, ModuleEnv, QualifiedId, QualifiedInstId},
};
use std::collections::BTreeMap;

/// A holder for native function generators.
#[derive(Default)]
pub(crate) struct NativeFunctions {
    generators: BTreeMap<QualifiedId<FunId>, Box<NativeFunctionGenerator>>,
}

type NativeFunctionGenerator = dyn Fn(&mut Generator, &Context, &QualifiedInstId<FunId>);

impl NativeFunctions {
    /// Create a NativeFunctions holder and register all function definitions.
    pub(crate) fn create(ctx: &Context) -> Self {
        let mut funs = NativeFunctions::default();
        funs.define_evm_functions(ctx);
        funs
    }

    /// Generate code for a native function.
    pub(crate) fn gen_native_function(
        &self,
        gen: &mut Generator,
        ctx: &Context,
        fun_id: &QualifiedInstId<FunId>,
    ) {
        if let Some(ngen) = self.generators.get(&fun_id.to_qualified_id()) {
            // generate the function header
            let fun_name = ctx.make_function_name(fun_id);
            emit!(ctx.writer, "function {}", fun_name);
            ngen(gen, ctx, fun_id)
        } else {
            ctx.env.error(
                &gen.contract_loc,
                &format!(
                    "native function `{}` not implemented",
                    ctx.env
                        .get_function(fun_id.to_qualified_id())
                        .get_full_name_str()
                ),
            )
        }
    }

    /// Defines a native function generator.
    pub(crate) fn define<F>(
        &mut self,
        ctx: &Context,
        module: &Option<ModuleEnv>,
        name: &str,
        gen: F,
    ) where
        F: Fn(&mut Generator, &Context, &QualifiedInstId<FunId>) + 'static,
    {
        if let Some(fun_id) = self.find_fun(ctx, module, name) {
            self.generators.insert(fun_id, Box::new(gen));
        }
    }

    /// Helper to find a module by name. The module may not exists as it is not used in the
    /// current compiler's run.
    fn find_module<'a>(&self, ctx: &Context<'a>, addr: &str, name: &str) -> Option<ModuleEnv<'a>> {
        let name = ModuleName::from_str(addr, ctx.env.symbol_pool().make(name));
        ctx.env.find_module(&name)
    }

    /// Helper to find a function by name.
    fn find_fun(
        &self,
        ctx: &Context,
        module: &Option<ModuleEnv>,
        name: &str,
    ) -> Option<QualifiedId<FunId>> {
        module.as_ref().and_then(|m| {
            m.find_function(ctx.env.symbol_pool().make(name))
                .map(|f| f.get_qualified_id())
        })
    }
}

// ========================================================================================
// Functions in the Evm module.

impl NativeFunctions {
    fn define_evm_functions(&mut self, ctx: &Context) {
        // TODO: may want to have symbolic representation of addr (which is 'Eth')
        let evm = &self.find_module(ctx, "0x2", "Evm");

        self.define(ctx, evm, "sign", |_, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
(addr) -> signer {
  signer := addr
}"
            );
        })
    }
}
