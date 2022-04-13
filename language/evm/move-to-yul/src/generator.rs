// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use codespan_reporting::{diagnostic::Severity, term::termcolor::Buffer};
use itertools::Itertools;
use move_core_types::{identifier::IdentStr, language_storage::ModuleId, value::MoveValue};
use std::collections::{BTreeMap, BTreeSet};

use move_model::{
    emit, emitln,
    model::{FunId, FunctionEnv, GlobalEnv, Loc, QualifiedId, QualifiedInstId},
    ty::Type,
};

use crate::{
    abi_signature::ABIJsonSignature, attributes, context::Context, functions::FunctionGenerator,
    solidity_ty::SoliditySignature, yul_functions::YulFunction, Options,
};

use sha3::{Digest, Keccak256};

/// Mutable state of the generator.
#[derive(Default)]
pub struct Generator {
    // Location of the currently compiled contract, for general error messages.
    pub(crate) contract_loc: Loc,
    /// Move functions, including type instantiation, needed in the currently generated code block.
    needed_move_functions: Vec<QualifiedInstId<FunId>>,
    /// Move functions for which code has been emitted.
    done_move_functions: BTreeSet<QualifiedInstId<FunId>>,
    /// Yule functions needed in the currently generated code block.
    needed_yul_functions: BTreeSet<YulFunction>,
    /// Auxiliary functions needed in the current block.
    needed_auxiliary_functions: Vec<(String, Box<AuxilaryFunctionGenerator>)>,
    /// Auxiliary functions for which code has been emitted.
    done_auxiliary_functions: BTreeSet<String>,
    /// Mapping of type signature hash to type, to identify collisions.
    pub(crate) type_sig_map: BTreeMap<u32, Type>,
    /// Solidity signature for callable functions for generating JSON-ABI
    pub(crate) solidity_sigs: Vec<(SoliditySignature, attributes::FunctionAttribute)>,
    /// Solidity signature for the optional constructor for generating JSON-ABI
    pub(crate) constructor_sig: Option<SoliditySignature>,
}

type AuxilaryFunctionGenerator = dyn FnOnce(&mut Generator, &Context);

// ================================================================================================
// Entry point

impl Generator {
    /// Run the generator and produce a pair of contract name and Yul contract object.
    pub fn run(options: &Options, env: &GlobalEnv) -> (String, String, String) {
        let ctx = Context::new(options, env, false);
        let mut gen = Generator::default();
        let contract_funs = ctx.get_target_functions(attributes::is_contract_fun);
        let (contract_name, contract_loc) = if contract_funs.is_empty() {
            ("Empty".to_string(), env.unknown_loc())
        } else {
            // Use the module of the first function to determine contract name and location.
            // TODO: we want to make the contract name configurable by options
            let first_module = &contract_funs[0].module_env;
            (ctx.make_contract_name(first_module), env.unknown_loc())
        };
        gen.contract_object(&ctx, contract_loc, &contract_name, &contract_funs);
        (
            contract_name,
            ctx.writer.extract_result(),
            ctx.abi_writer.extract_result(),
        )
    }

    // Run the generator for evm unit tests and produce a mapping from function id to Yul test object.
    pub fn run_for_evm_tests(
        options: &Options,
        env: &GlobalEnv,
    ) -> BTreeMap<QualifiedId<FunId>, String> {
        let mut res = BTreeMap::new();
        let ctx = Context::new(options, env, /*for_test*/ true);

        // Go over all evm_test functions which are in modules which are target of compilation,
        // and generate a test object for them.
        for module in env.get_modules() {
            if !module.is_target() {
                continue;
            }
            for fun in module.get_functions() {
                if attributes::is_evm_test_fun(&fun) {
                    let mut gen = Generator::default();
                    gen.test_object(&ctx, &fun, &[]);
                    res.insert(fun.get_qualified_id(), ctx.writer.extract_result());
                }
            }
        }

        res
    }

    /// Run the generator for a specific unit test and generate a Yul test object for it.
    /// Return diagnostics if errors are raised.
    pub fn run_for_unit_test(
        options: &Options,
        env: &GlobalEnv,
        module_id: &ModuleId,
        fun_name: &IdentStr,
        args: &[MoveValue],
    ) -> Result<String, String> {
        let fun = env
            .find_function_by_language_storage_id_name(module_id, fun_name)
            .unwrap_or_else(|| {
                panic!(
                    "Failed to find test function {}::{}. This should not have happened.",
                    module_id, fun_name
                )
            });

        let ctx = Context::new(options, env, /*for_test*/ true);
        let mut gen = Generator::default();
        gen.test_object(&ctx, &fun, args);
        if ctx.env.has_errors() {
            let mut buffer = Buffer::no_color();
            ctx.env.report_diag(&mut buffer, Severity::Error);
            Err(String::from_utf8_lossy(buffer.as_slice()).to_string())
        } else {
            Ok(ctx.writer.extract_result())
        }
    }
}

// ================================================================================================
// Object generation

impl Generator {
    /// Generate contract object for given contract functions.
    fn contract_object(
        &mut self,
        ctx: &Context,
        contract_loc: Loc,
        contract_name: &str,
        contract_funs: &[FunctionEnv<'_>],
    ) {
        self.header(ctx);
        // Initialize contract specific state
        self.contract_loc = contract_loc;
        emit!(ctx.writer, "object \"{}\" ", contract_name);
        ctx.emit_block(|| {
            // Generate the deployment code block
            self.begin_code_block(ctx);
            self.optional_creator(ctx, contract_name);
            let contract_deployed_name = format!("{}_deployed", contract_name);
            emitln!(
                ctx.writer,
                "codecopy(0, dataoffset(\"{}\"), datasize(\"{}\"))",
                contract_deployed_name,
                contract_deployed_name
            );
            emitln!(
                ctx.writer,
                "return(0, datasize(\"{}\"))",
                contract_deployed_name,
            );
            self.end_code_block(ctx);

            // Generate the runtime object
            emit!(ctx.writer, "object \"{}\" ", contract_deployed_name);
            ctx.emit_block(|| {
                self.begin_code_block(ctx);
                emitln!(
                    ctx.writer,
                    "mstore(${MEM_SIZE_LOC}, memoryguard(${USED_MEM}))"
                );
                self.callable_functions(ctx, contract_funs);
                self.end_code_block(ctx);
            })
        });
        // Generate JSON-ABI
        self.generate_abi_string(ctx);
    }

    /// Generate JSON-ABI
    fn generate_abi_string(&self, ctx: &Context) {
        let mut res = vec![];
        let event_sigs = ctx
            .event_signature_map
            .borrow()
            .values()
            .cloned()
            .collect_vec();
        for sig in &event_sigs {
            res.push(serde_json::to_string_pretty(&ABIJsonSignature::from_event_sig(sig)).unwrap());
        }
        for (sig, attr) in &self.solidity_sigs {
            res.push(
                serde_json::to_string_pretty(&ABIJsonSignature::from_solidity_sig(
                    sig,
                    Some(*attr),
                    "function",
                ))
                .unwrap(),
            );
        }
        if let Some(constructor) = &self.constructor_sig {
            res.push(
                serde_json::to_string_pretty(&ABIJsonSignature::from_solidity_sig(
                    constructor,
                    None,
                    "constructor",
                ))
                .unwrap(),
            );
        }
        emitln!(ctx.abi_writer, "[");
        emitln!(
            ctx.abi_writer,
            "{}",
            res.iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(",\n")
        );
        emitln!(ctx.abi_writer, "]");
    }

    /// Generate test object for given function.
    ///
    /// A test object contains no nested objects and is intended to execute at transaction time,
    /// without actually deploying any contract code.
    fn test_object(&mut self, ctx: &Context, test: &FunctionEnv, args: &[MoveValue]) {
        self.header(ctx);
        ctx.check_no_generics(test);
        if test.get_return_count() > 0 {
            ctx.env
                .error(&test.get_loc(), "test functions cannot have return values");
            return;
        }
        if test.get_parameter_count() != args.len() {
            ctx.env.error(
                &test.get_loc(),
                &format!(
                    "test function has {} parameters but {} were provided",
                    test.get_parameter_count(),
                    args.len()
                ),
            );
            return;
        }
        for ty in test.get_parameter_types() {
            if !ty.is_signer_or_address() {
                ctx.env.error(
                    &test.get_loc(),
                    "only signer or address parameters are allowed currently",
                );
                return;
            }
        }

        let fun_id = test.get_qualified_id().instantiate(vec![]);
        let test_contract_name = format!("test_{}", ctx.make_function_name(&fun_id));
        emit!(ctx.writer, "object \"{}\" ", test_contract_name);
        ctx.emit_block(|| {
            self.begin_code_block(ctx);
            emitln!(
                ctx.writer,
                "mstore(${MEM_SIZE_LOC}, memoryguard(${USED_MEM}))"
            );
            self.need_move_function(&fun_id);

            for (idx, arg) in args.iter().enumerate() {
                emit!(ctx.writer, "let $arg{} := ", idx);
                match arg {
                    MoveValue::Address(addr) => {
                        emitln!(ctx.writer, "{}", addr.to_hex_literal());
                    }
                    _ => unreachable!(
                        "only address literals are allowed as test arguments currently"
                    ),
                }
            }

            let fun_name = ctx.make_function_name(&fun_id);
            emit!(ctx.writer, "{}(", fun_name);
            for idx in 0..args.len() {
                if idx > 0 {
                    emit!(ctx.writer, ", ");
                }
                emit!(ctx.writer, "$arg{}", idx);
            }
            emitln!(ctx.writer, ")");

            emitln!(ctx.writer, "return (0, 0)");
            self.end_code_block(ctx);
        });
    }

    /// Generate header for output Yul.
    fn header(&mut self, ctx: &Context) {
        emitln!(
            ctx.writer,
            "\
/* =======================================
 * Generated by Move-To-Yul compiler v{}
 * ======================================= */",
            ctx.options.version(),
        );
        emitln!(ctx.writer);
        if ctx.options.generate_source_info() {
            let mut use_src_emitted = false;
            for (file_no, file_path) in ctx
                .file_id_map
                .values()
                .sorted_by(|(n1, _), (n2, _)| n1.cmp(n2))
            {
                let use_str = format!("{}:\"{}\"", file_no, file_path);
                if !use_src_emitted {
                    emitln!(ctx.writer, "/// @use-src {}", use_str);
                    use_src_emitted = true;
                } else {
                    emitln!(ctx.writer, "///        , {}", use_str)
                }
            }
            emitln!(ctx.writer);
        }
        emitln!(ctx.writer);
    }

    /// Generate optional creator (contract constructor).
    fn optional_creator(&mut self, ctx: &Context, contract_name: &str) {
        let mut creators = ctx.get_target_functions(attributes::is_create_fun);
        if creators.len() > 1 {
            ctx.env
                .error(&creators[1].get_loc(), "multiple #[create] functions")
        }
        if let Some(creator) = creators.pop() {
            ctx.check_no_generics(&creator);
            if creator.get_return_count() > 0 {
                ctx.env.error(
                    &creator.get_loc(),
                    "return values not allowed for creator functions",
                )
            }
            if !self.is_suitable_for_dispatch(ctx, &creator) {
                ctx.env.error(
                    &creator.get_loc(),
                    "creator function has unsupported parameter types",
                );
            }

            emitln!(
                ctx.writer,
                "mstore(${MEM_SIZE_LOC}, memoryguard(${USED_MEM}))"
            );

            // Translate call to the constructor function
            let fun_id = creator.get_qualified_id().instantiate(vec![]);
            let function_name = ctx.make_function_name(&fun_id);
            let solidity_sig = self.get_solidity_signature(ctx, &creator, false);
            self.constructor_sig = Some(solidity_sig.clone());
            let param_count = solidity_sig.para_types.len();
            let mut params = "".to_string();
            if param_count > 0 {
                let program_size_str = "program_size".to_string();
                let arg_size_str = "arg_size".to_string();
                let memory_data_offset_str = "memory_data_offset".to_string();
                emitln!(
                    ctx.writer,
                    "let {} := datasize(\"{}\")",
                    program_size_str,
                    contract_name
                );
                emitln!(
                    ctx.writer,
                    "let {} := sub(codesize(), {})",
                    arg_size_str,
                    program_size_str
                );
                let malloc_call = self.call_builtin_str(
                    ctx,
                    YulFunction::Malloc,
                    std::iter::once(arg_size_str.clone()),
                );
                emitln!(
                    ctx.writer,
                    "let {} := {}",
                    memory_data_offset_str,
                    malloc_call
                );
                emitln!(
                    ctx.writer,
                    "codecopy({}, {}, {})",
                    memory_data_offset_str,
                    program_size_str,
                    arg_size_str
                );
                let decoding_fun_name = self.generate_abi_tuple_decoding_para(
                    ctx,
                    &solidity_sig,
                    creator.get_parameter_types(),
                    true,
                );
                params = (0..param_count).map(|i| format!("param_{}", i)).join(", ");
                let let_params = format!("let {} := ", params);
                emitln!(
                    ctx.writer,
                    "{}{}({}, add({}, {}))",
                    let_params,
                    decoding_fun_name,
                    memory_data_offset_str,
                    memory_data_offset_str,
                    arg_size_str
                );
            }

            // Call the function
            emitln!(ctx.writer, "{}({})", function_name, params);

            self.need_move_function(&fun_id);
        }
    }

    /// Generate Yul definitions for all callable functions.
    fn callable_functions(&mut self, ctx: &Context, contract_funs: &[FunctionEnv<'_>]) {
        self.generate_dispatcher_routine(ctx, contract_funs);
        for fun in contract_funs {
            ctx.check_no_generics(fun);
            self.function(ctx, &fun.get_qualified_id().instantiate(vec![]))
        }
    }

    /// Generate code for a function. This delegates to the function generator.
    fn function(&mut self, ctx: &Context, fun_id: &QualifiedInstId<FunId>) {
        self.done_move_functions.insert(fun_id.clone());
        FunctionGenerator::run(self, ctx, fun_id)
    }

    /// Begin a new code block.
    fn begin_code_block(&mut self, ctx: &Context) {
        assert!(self.needed_move_functions.is_empty());
        assert!(self.needed_yul_functions.is_empty());
        emitln!(ctx.writer, "code {");
        ctx.writer.indent();
    }

    /// End a code block, generating all functions needed by top-level callable functions.
    fn end_code_block(&mut self, ctx: &Context) {
        // Before the end of the code block, we need to emit definitions of all
        // functions reached by callable entry points. While we traversing this list,
        // more functions might be added due to transitive calls.
        while let Some(fun_id) = self.needed_move_functions.pop() {
            if !self.done_move_functions.contains(&fun_id) {
                self.function(ctx, &fun_id)
            }
        }

        // We also need to emit code for all needed auxiliary functions.
        while let Some((function_name, generator)) = self.needed_auxiliary_functions.pop() {
            if !self.done_auxiliary_functions.contains(&function_name) {
                emit!(ctx.writer, "function {}", function_name);
                self.done_auxiliary_functions.insert(function_name);
                generator(self, ctx)
            }
        }

        // We finally emit code for all Yul functions which have been needed by the Move
        // or auxiliary functions.
        for fun in &self.needed_yul_functions {
            emitln!(ctx.writer, &fun.yule_def());
        }
        // Empty the set of functions for next block.
        self.done_move_functions.clear();
        self.needed_yul_functions.clear();
        self.done_auxiliary_functions.clear();
        ctx.writer.unindent();
        emitln!(ctx.writer, "}")
    }
}

// ================================================================================================
// Helpers shared with other modules

impl Generator {
    /// Generate call to a builtin function.
    pub(crate) fn call_builtin(
        &mut self,
        ctx: &Context,
        fun: YulFunction,
        args: impl Iterator<Item = String>,
    ) {
        emitln!(ctx.writer, "{}", self.call_builtin_str(ctx, fun, args))
    }

    /// Generate call to a builtin function which delivers results.
    pub(crate) fn call_builtin_with_result(
        &mut self,
        ctx: &Context,
        prefix: &str,
        mut results: impl Iterator<Item = String>,
        fun: YulFunction,
        args: impl Iterator<Item = String>,
    ) {
        emitln!(
            ctx.writer,
            "{}{} := {}",
            prefix,
            results.join(", "),
            self.call_builtin_str(ctx, fun, args)
        )
    }

    /// Create the string representing call to builtin function.
    pub(crate) fn call_builtin_str(
        &mut self,
        _ctx: &Context,
        fun: YulFunction,
        mut args: impl Iterator<Item = String>,
    ) -> String {
        self.need_yul_function(fun);
        for dep in fun.yule_deps() {
            self.needed_yul_functions.insert(dep);
        }
        format!("{}({})", fun.yule_name(), args.join(", "))
    }

    /// Indicate that a Yul function is needed.
    pub(crate) fn need_yul_function(&mut self, yul_fun: YulFunction) {
        if !self.needed_yul_functions.contains(&yul_fun) {
            self.needed_yul_functions.insert(yul_fun);
            for dep in yul_fun.yule_deps() {
                self.need_yul_function(dep);
            }
        }
    }

    /// Indicate that an auxiliary function of name is needed. Return the name.
    pub(crate) fn need_auxiliary_function(
        &mut self,
        function_name: String,
        generator: Box<AuxilaryFunctionGenerator>,
    ) -> String {
        if !self.done_auxiliary_functions.contains(&function_name) {
            self.needed_auxiliary_functions
                .push((function_name.clone(), generator));
        }
        function_name
    }

    /// Indicate that a move function is needed.
    pub(crate) fn need_move_function(&mut self, fun_id: &QualifiedInstId<FunId>) {
        if !self.done_move_functions.contains(fun_id) {
            self.needed_move_functions.push(fun_id.clone())
        }
    }

    /// Copy literal string to memory
    pub(crate) fn copy_literal_to_memory(&mut self, value: Vec<u8>) -> String {
        let name_prefix = "copy_literal_string_to_memory";
        let function_name = format!("{}_{}", name_prefix, self.vector_u8_hash(&value));
        let value = value.clone();
        let generate_fun = move |gen: &mut Generator, ctx: &Context| {
            emit!(ctx.writer, "(value) ");
            ctx.emit_block(|| {
                for c in value {
                    let store_u8_str = gen.call_builtin_str(
                        ctx,
                        YulFunction::MemoryStoreU8,
                        vec!["value".to_string(), c.to_string()].into_iter(),
                    );
                    emitln!(ctx.writer, "{}", store_u8_str);
                    emitln!(ctx.writer, "value := add(value, 1)");
                }
            });
        };
        self.need_auxiliary_function(function_name, Box::new(generate_fun))
    }

    fn vector_u8_hash(&mut self, vec: &[u8]) -> u32 {
        let mut keccak = Keccak256::new();
        keccak.update(vec);
        let digest = keccak.finalize();
        u32::from_le_bytes([digest[0], digest[1], digest[2], digest[3]])
    }
}
