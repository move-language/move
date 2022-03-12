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
        funs.define_move_functions(ctx);
        funs.define_vector_functions(ctx);
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
                    "native function `{}` not implemented for type `{:?}`",
                    ctx.env
                        .get_function(fun_id.to_qualified_id())
                        .get_full_name_str(),
                    fun_id.inst,
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
        });

        self.define(ctx, evm, "blockhash", |_, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
(block_number) -> hash {
  hash := blockhash(block_number)
}"
            );
        });

        self.define(ctx, evm, "block_basefee", |_, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
() -> result {
  result := basefee()
}"
            );
        });

        self.define(ctx, evm, "block_chainid", |_, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
() -> result {
  result := chainid()
}"
            );
        });

        self.define(ctx, evm, "block_coinbase", |_, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
() -> result {
  result := coinbase()
}"
            );
        });

        self.define(ctx, evm, "block_difficulty", |_, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
() -> result {
  result := difficulty()
}"
            );
        });

        self.define(ctx, evm, "block_gaslimit", |_, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
() -> result {
  result := gaslimit()
}"
            );
        });

        self.define(ctx, evm, "block_number", |_, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
() -> result {
  result := number()
}"
            );
        });

        self.define(ctx, evm, "block_timestamp", |_, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
() -> result {
  result := timestamp()
}"
            );
        });

        self.define(ctx, evm, "gasleft", |_, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
() -> result {
  result := gas()
}"
            );
        });

        self.define(ctx, evm, "msg_data", |_, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
() -> result {
  result := calldataload(0)
}"
            );
        });

        self.define(ctx, evm, "msg_sender", |_, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
() -> result {
  result := caller()
}"
            );
        });

        self.define(ctx, evm, "msg_sig", |_, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
() -> result {
  result := timestamp()
}"
            );
        });

        self.define(ctx, evm, "msg_value", |_, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
() -> result {
  result := callvalue()
}"
            );
        });

        self.define(ctx, evm, "tx_gasprice", |_, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
() -> result {
  result := gasprice()
}"
            );
        });

        self.define(ctx, evm, "tx_origin", |_, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
() -> result {
  result := origin()
}"
            );
        });
    }

    fn define_move_functions(&mut self, ctx: &Context) {
        let signer = &self.find_module(ctx, "0x1", "Signer");

        self.define(ctx, signer, "borrow_address", |_, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
(signer_ref) -> addr_ref {
  addr_ref := signer_ref
}"
            );
        });
    }

    /// Define vector functions for a specific instantiation.
    fn define_vector_functions(&mut self, ctx: &Context) {
        let vector = &self.find_module(ctx, "0x1", "Vector");

        self.define(ctx, vector, "empty", crate::vectors::define_empty_fun);
        self.define(ctx, vector, "length", crate::vectors::define_length_fun);
        self.define(
            ctx,
            vector,
            "push_back",
            crate::vectors::define_push_back_fun,
        );
        self.define(ctx, vector, "pop_back", crate::vectors::define_pop_back_fun);
        self.define(ctx, vector, "borrow", crate::vectors::define_borrow_fun);
        self.define(ctx, vector, "borrow_mut", crate::vectors::define_borrow_fun);
        self.define(ctx, vector, "swap", crate::vectors::define_swap_fun);
        self.define(
            ctx,
            vector,
            "destroy_empty",
            crate::vectors::define_destroy_empty_fun,
        );
    }
}
