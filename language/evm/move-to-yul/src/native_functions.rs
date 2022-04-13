// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    attributes, context::Context, events, functions::FunctionGenerator, yul_functions::YulFunction,
};
use move_model::{
    ast::ModuleName,
    emit, emitln,
    model::{FunId, FunctionEnv, ModuleEnv, QualifiedId, QualifiedInstId},
    ty::Type,
};
use std::collections::BTreeMap;

/// A holder for native function generators.
#[derive(Default)]
pub(crate) struct NativeFunctions {
    generators: BTreeMap<QualifiedId<FunId>, Box<NativeFunctionGenerator>>,
}

type NativeFunctionGenerator = dyn Fn(&mut FunctionGenerator, &Context, &QualifiedInstId<FunId>);

impl NativeFunctions {
    /// Create a NativeFunctions holder and register all function definitions.
    pub(crate) fn create(ctx: &Context) -> Self {
        let mut funs = NativeFunctions::default();
        funs.define_evm_functions(ctx);
        funs.define_move_functions(ctx);
        funs.define_vector_functions(ctx);
        funs.define_table_functions(ctx);
        funs
    }

    /// Check whether fun is an emit event function
    fn is_emit_fun(&self, fun: &FunctionEnv<'_>) -> bool {
        fun.get_full_name_str() == "Evm::emit"
    }

    /// Generate code for a native function.
    pub(crate) fn gen_native_function(
        &self,
        gen: &mut FunctionGenerator,
        ctx: &Context,
        fun_id: &QualifiedInstId<FunId>,
    ) {
        if let Some(ngen) = self.generators.get(&fun_id.to_qualified_id()) {
            // generate the function header
            let fun_name = ctx.make_function_name(fun_id);
            emit!(ctx.writer, "function {}", fun_name);
            ngen(gen, ctx, fun_id)
        } else {
            let fun = &ctx.env.get_function(fun_id.to_qualified_id());
            if attributes::is_external_fun(fun) {
                if let Some(extracted_sig_str) = attributes::extract_external_signature(fun) {
                    self.define_external_fun(gen, ctx, fun_id, &extracted_sig_str)
                } else {
                    ctx.env.error(
                        &gen.parent.contract_loc,
                        &format!(
                            "external function {} must have a valid solidity signature",
                            ctx.env
                                .get_function(fun_id.to_qualified_id())
                                .get_full_name_str()
                        ),
                    )
                }
            } else if self.is_emit_fun(fun) {
                let elem_type = fun_id.inst.get(0).unwrap(); // obtain the event type
                if let Type::Struct(mid, sid, inst) = elem_type {
                    let st_id = QualifiedInstId {
                        module_id: *mid,
                        id: *sid,
                        inst: inst.to_owned(),
                    };
                    let ev_signature_map = ctx.event_signature_map.borrow();
                    let sig_opt = ev_signature_map.get(&st_id);
                    if let Some(sig) = sig_opt {
                        events::define_emit_fun(gen, ctx, sig, &st_id, fun_id);
                    } else {
                        ctx.env.error(
                            &gen.parent.contract_loc,
                            &format!(
                                "native function {} can only emit event structs",
                                ctx.env
                                    .get_function(fun_id.to_qualified_id())
                                    .get_full_name_str()
                            ),
                        )
                    }
                }
            } else {
                ctx.env.error(
                    &gen.parent.contract_loc,
                    &format!(
                        "native function {} not implemented (w/ signature `{:?}`)",
                        ctx.env
                            .get_function(fun_id.to_qualified_id())
                            .get_full_name_str(),
                        fun_id.inst,
                    ),
                )
            }
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
        F: Fn(&mut FunctionGenerator, &Context, &QualifiedInstId<FunId>) + 'static,
    {
        if let Some(fun_id) = self.find_fun(ctx, module, name) {
            self.generators.insert(fun_id, Box::new(gen));
        }
    }

    /// Helper to find a module by name. The module may not exists as it is not used in the
    /// current compiler's run.
    pub(crate) fn find_module<'a>(
        &self,
        ctx: &Context<'a>,
        addr: &str,
        name: &str,
    ) -> Option<ModuleEnv<'a>> {
        let name = ModuleName::from_str(addr, ctx.env.symbol_pool().make(name));
        ctx.env.find_module(&name)
    }

    /// Helper to find a function by name.
    pub(crate) fn find_fun(
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

        self.define(ctx, evm, "self", |_, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
() -> addr {
  addr := address()
}"
            );
        });

        self.define(ctx, evm, "abort_with", |gen, ctx: &Context, _| {
            emitln!(
                ctx.writer,
                "\
(message) {{
  let head := $Malloc(32)
  // store the function selector for Error(string)
  mstore(head, 3963877391197344453575983046348115674221700746820753546331534351508065746944)
  let pos := add(head, 4)
  mstore(pos, 32)
  pos := add(pos, 32)
  let size := {}
  mstore(pos, size)
  pos := add(pos, 32)
  {}
  size := {}
  let end := add(pos, size)
  revert(head, sub(end, head))
}}",
                gen.parent.call_builtin_str(
                    ctx,
                    YulFunction::MemoryLoadU64,
                    std::iter::once("message".to_string())
                ),
                gen.parent.call_builtin_str(
                    ctx,
                    YulFunction::CopyMemoryU8,
                    vec![
                        "add(message, 32)".to_string(),
                        "pos".to_string(),
                        "size".to_string()
                    ]
                    .into_iter()
                ),
                gen.parent.call_builtin_str(
                    ctx,
                    YulFunction::RoundUp,
                    std::iter::once("size".to_string())
                )
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
}
