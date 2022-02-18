// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
};

use itertools::Itertools;
use move_core_types::language_storage::CORE_CODE_ADDRESS;
use sha3::{Digest, Keccak256};

use crate::evm_transformation::EvmTransformationProcessor;
use move_model::{
    ast::{Attribute, TempIndex},
    code_writer::CodeWriter,
    emit, emitln,
    model::{FunId, FunctionEnv, GlobalEnv, Loc, ModuleEnv, ModuleId, QualifiedInstId, StructId},
    ty::{PrimitiveType, Type, TypeDisplayContext},
};
use move_stackless_bytecode::{
    function_target::FunctionTarget,
    function_target_pipeline::{FunctionTargetPipeline, FunctionTargetsHolder, FunctionVariant},
    livevar_analysis::LiveVarAnalysisProcessor,
    reaching_def_analysis::ReachingDefProcessor,
    stackless_bytecode::{Bytecode, Constant, Label, Operation},
    stackless_control_flow_graph::{BlockContent, BlockId, StacklessControlFlowGraph},
};

use crate::{
    yul_functions::{substitute_placeholders, YulFunction, WORD_SIZE},
    Options,
};

const CONTRACT_ATTR: &str = "contract";
const CREATE_ATTR: &str = "create";
const CALLABLE_ATTR: &str = "callable";
const EVM_ARITH_ATTR: &str = "evm_arith";
const PAYABLE_ATTR: &str = "payable";
const RECEIVE_ATTR: &str = "receive";
const RECEIVE_FALLBACK: &str = "fallback";

// revert reason
const REVERT_ERR_NON_PAYABLE_FUN: usize = 99;
const UNKNOWN_SIGNATURE_AND_NO_FALLBACK_DEFINED: usize = 98;
const NO_RECIVE_OR_FALLBACK_FUN: usize = 97;
const ABI_DECODING_DATA_TOO_SHORT: usize = 96;
const ABI_DECODING_PARAM_VALIDATION: usize = 95;

/// Immutable context passed through the compilation.
struct Context<'a> {
    /// The program options.
    options: &'a Options,
    /// The global environment, containing the Move model.
    env: &'a GlobalEnv,
    /// The function target data containing the stackless bytecode.
    targets: FunctionTargetsHolder,
    /// A code writer where we emit Yul code to.
    writer: CodeWriter,
}

/// Mutable state of the generator.
#[derive(Default)]
pub struct Generator {
    // Location of the currently compiled contract, for general error messages.
    contract_loc: Loc,
    /// Move functions, including type instantiation, needed in the currently generated code block.
    needed_move_functions: Vec<QualifiedInstId<FunId>>,
    /// Move functions for which code has been emitted.
    done_move_functions: BTreeSet<QualifiedInstId<FunId>>,
    /// Yule functions needed in the currently generated code block.
    needed_yul_functions: BTreeSet<YulFunction>,
    /// A mapping from locals of the currently compiled functions from which a reference is
    /// borrowed, to the position in a memory region where these locals have evaded to.
    /// All borrowed_locals have a consecutive position in this mapping, starting at zero.
    borrowed_locals: BTreeMap<TempIndex, usize>,
    /// Memory layout info.
    struct_layout: BTreeMap<QualifiedInstId<StructId>, StructLayout>,
    /// Mapping of type signature hash to type, to identify collisions.
    type_sig_map: BTreeMap<u32, Type>,
}

/// Information about the layout of a struct in linear memory.
#[derive(Default, Clone)]
struct StructLayout {
    /// The size, in bytes, of this struct.
    size: usize,
    /// Offsets in linear memory and type for each field, indexed by logical offsets, i.e.
    /// position in the struct definition.
    offsets: BTreeMap<usize, (usize, Type)>,
    /// Field order (in terms of logical offset), optimized for best memory representation.
    field_order: Vec<usize>,
    /// The number of leading fields which are pointers to linked data. Those fields always
    /// appear first in the field_order.
    pointer_count: usize,
}

// ================================================================================================
// Entry point

impl Generator {
    /// Run the generator and produce output written to a file.
    pub fn run(options: &Options, env: &GlobalEnv) -> anyhow::Result<()> {
        let (ctx, mut gen) = Self::new(options, env);
        gen.objects(&ctx);
        ctx.writer
            .process_result(|contents| fs::write(&options.output, contents))?;
        Ok(())
    }

    /// Run the generator and produce output to a string.
    pub fn run_to_string(options: &Options, env: &GlobalEnv) -> String {
        let (ctx, mut gen) = Self::new(options, env);
        gen.objects(&ctx);
        ctx.writer.extract_result()
    }

    /// Creates immutable context and mutable state of the Generator.
    fn new<'a>(options: &'a Options, env: &'a GlobalEnv) -> (Context<'a>, Self) {
        let writer = CodeWriter::new(env.unknown_loc());
        writer.set_emit_hook(substitute_placeholders);
        (
            Context {
                options,
                env,
                targets: Self::create_bytecode(options, env),
                writer,
            },
            Generator::default(),
        )
    }

    /// Helper to create the stackless bytecode.
    fn create_bytecode(options: &Options, env: &GlobalEnv) -> FunctionTargetsHolder {
        // Populate the targets holder with all needed functions.
        let mut targets = FunctionTargetsHolder::default();
        for module in env.get_modules() {
            if !module.is_target() || !is_contract_module(&module) {
                continue;
            }
            for fun in module.get_functions() {
                if is_callable_fun(&fun)
                    || is_create_fun(&fun)
                    || is_receive_fun(&fun)
                    || is_fallback_fun(&fun)
                {
                    Self::add_fun(&mut targets, &fun)
                }
            }
        }
        // Run a minimal transformation pipeline. For now, we do reaching-def and live-var
        // to clean up some churn created by the conversion from stack to stackless bytecode.
        let mut pipeline = FunctionTargetPipeline::default();
        pipeline.add_processor(EvmTransformationProcessor::new());
        pipeline.add_processor(ReachingDefProcessor::new());
        pipeline.add_processor(LiveVarAnalysisProcessor::new());
        if options.dump_bytecode {
            pipeline.run_with_dump(env, &mut targets, &options.output, false)
        } else {
            pipeline.run(env, &mut targets);
        }

        targets
    }

    /// Adds function and all its called functions to the targets.
    fn add_fun(targets: &mut FunctionTargetsHolder, fun: &FunctionEnv<'_>) {
        targets.add_target(fun);
        for qid in fun.get_called_functions() {
            let called_fun = fun.module_env.env.get_function(qid);
            if !targets.has_target(&called_fun, &FunctionVariant::Baseline) {
                Self::add_fun(targets, &called_fun)
            }
        }
    }
}

// ================================================================================================
// Object generation

impl Generator {
    /// Generates Yul objects. For each contract module, one Yul object is created.
    /// TODO: alternatively, we may collect all #[callable] functions independent of module
    /// context, and put them into one contract object.
    fn objects(&mut self, ctx: &Context) {
        emitln!(
            ctx.writer,
            "\
/* =======================================
 * Generated by Move-To-Yul compiler v{}
 * ======================================= */",
            ctx.options.version(),
        );
        emitln!(ctx.writer);
        for file_id in ctx.env.get_source_file_ids() {
            emitln!(
                ctx.writer,
                "/// @use-src {}:\"{}\"",
                ctx.env.file_id_to_idx(file_id),
                ctx.env.get_file(file_id).to_string_lossy()
            );
        }
        emitln!(ctx.writer);

        let contract_modules = ctx
            .env
            .get_modules()
            .filter(|m| m.is_target() && is_contract_module(m))
            .collect_vec();
        for module in contract_modules {
            self.contract(ctx, &module)
        }
    }

    /// Generate contract object for given module.
    fn contract(&mut self, ctx: &Context, module: &ModuleEnv<'_>) {
        // Initialize contract specific state
        self.contract_loc = module.get_loc();
        self.type_sig_map.clear();

        // Generate the contract object.
        let contract_name = ctx.make_contract_name(module);
        emit!(ctx.writer, "object \"{}\" ", contract_name);
        ctx.emit_block(|| {
            // Generate the deployment code block
            self.begin_code_block(ctx);
            let contract_deployed_name = format!("{}_deployed", contract_name);
            emitln!(
                ctx.writer,
                "codecopy(0, dataoffset(\"{}\"), datasize(\"{}\"))",
                contract_deployed_name,
                contract_deployed_name
            );
            self.optional_creator(ctx, module);
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
                self.callable_functions(ctx, module);
                self.end_code_block(ctx);
            })
        })
    }

    /// Generate optional creator (contract constructor).
    fn optional_creator(&mut self, ctx: &Context, module: &ModuleEnv<'_>) {
        let mut creators = module
            .get_functions()
            .filter(|f| is_create_fun(f))
            .collect_vec();
        if creators.len() > 1 {
            ctx.env
                .error(&creators[1].get_loc(), "multiple #[create] functions")
        }
        if let Some(creator) = creators.pop() {
            ctx.check_no_generics(&creator);
            self.function(ctx, &creator.get_qualified_id().instantiate(vec![]));
            // TODO: implement creator invocation
            emitln!(
                ctx.writer,
                "// TODO: invocation of {}",
                creator.get_full_name_str()
            );
        }
    }

    /// Generate optional receive function.
    fn optional_receive(&mut self, ctx: &Context, module: &ModuleEnv<'_>) -> bool {
        let mut receives = module
            .get_functions()
            .filter(|f| is_receive_fun(f))
            .collect_vec();
        if receives.len() > 1 {
            ctx.env
                .error(&receives[1].get_loc(), "multiple #[receive] functions")
        }
        if let Some(receive) = receives.pop() {
            ctx.check_no_generics(&receive);
            if !is_payable_fun(&receive) {
                ctx.env
                    .error(&receive.get_loc(), "receive function must be payable")
            }
            if is_fallback_fun(&receive) || is_callable_fun(&receive) {
                ctx.env.error(
                    &receive.get_loc(),
                    "receive function must not be a fallback or callable function",
                )
            }
            if receive.get_parameter_count() > 0 {
                ctx.env.error(
                    &receive.get_loc(),
                    "receive function must not have parameters",
                )
            }
            let fun_id = &receive
                .module_env
                .get_id()
                .qualified(receive.get_id())
                .instantiate(vec![]);
            emitln!(
                ctx.writer,
                "if iszero(calldatasize()) {{ {}() stop() }}",
                ctx.make_function_name(fun_id)
            );
            true
        } else {
            false
        }
    }

    /// Generate fallback function.
    fn generate_fallback(&mut self, ctx: &Context, module: &ModuleEnv<'_>, receive_ether: bool) {
        let mut fallbacks = module
            .get_functions()
            .filter(|f| is_fallback_fun(f))
            .collect_vec();
        if fallbacks.len() > 1 {
            ctx.env
                .error(&fallbacks[1].get_loc(), "multiple #[fallback] functions")
        }
        if let Some(fallback) = fallbacks.pop() {
            ctx.check_no_generics(&fallback);
            if is_callable_fun(&fallback) {
                ctx.env.error(
                    &fallback.get_loc(),
                    "fallback function must not be a callable function",
                )
            }
            if !is_payable_fun(&fallback) {
                self.generate_call_value_check(ctx, REVERT_ERR_NON_PAYABLE_FUN);
            }
            let fun_id = &fallback
                .module_env
                .get_id()
                .qualified(fallback.get_id())
                .instantiate(vec![]);
            let fun_name = ctx.make_function_name(fun_id);
            let params_size = fallback.get_parameter_count();
            if params_size == 0 {
                emitln!(ctx.writer, "{}() stop()", fun_name);
            } else if params_size != 1 || fallback.get_return_count() != 1 {
                ctx.env.error(
                    &fallback.get_loc(),
                    "fallback function must have at most 1 parameter and 1 return value",
                );
            } else {
                emitln!(
                    ctx.writer,
                    "let retval := {}(0, calldatasize()) stop()",
                    fun_name
                );
                emitln!(ctx.writer, "return(add(retval, 0x20), mload(retval))");
            }
        } else {
            let mut err_msg = NO_RECIVE_OR_FALLBACK_FUN;
            if receive_ether {
                err_msg = UNKNOWN_SIGNATURE_AND_NO_FALLBACK_DEFINED;
            }
            self.call_builtin(
                ctx,
                YulFunction::Abort,
                std::iter::once(err_msg.to_string()),
            );
        }
    }

    /// Generate the code to check value
    fn generate_call_value_check(&mut self, ctx: &Context, err_code: TempIndex) {
        emitln!(ctx.writer, "if callvalue()");
        ctx.emit_block(|| {
            self.call_builtin(
                ctx,
                YulFunction::Abort,
                std::iter::once(err_code.to_string()),
            );
        });
    }

    /// Generate type string for encoding the function signature
    fn get_evm_type_string(&self, ctx: &Context, ty: &Type) -> String {
        use PrimitiveType::*;
        use Type::*;
        let generate_tuple = |tys: &Vec<Type>| {
            format!(
                "({})",
                tys.iter()
                    .map(|t| self.get_evm_type_string(ctx, t))
                    .collect::<Vec<_>>()
                    .join(",")
            )
        };
        match ty {
            Primitive(p) => match p {
                Bool => "bool".to_string(),
                U8 => "uint8".to_string(),
                U64 => "uint64".to_string(),
                U128 => "uint128".to_string(),
                Address => "address".to_string(),
                Signer => "address".to_string(),
                Num | Range | EventStore => {
                    panic!("unexpected field type")
                }
            },
            Vector(ety) => format!("{}[]", self.get_evm_type_string(ctx, ety)),
            Tuple(tys) => generate_tuple(tys),
            Struct(mid, sid, _) => {
                if is_u256(ctx.env, *mid, *sid) {
                    "uint256".to_string()
                } else {
                    let tys = ctx.get_or_compute_struct_types(*mid, *sid);
                    generate_tuple(&tys)
                }
            }
            TypeParameter(_)
            | Reference(_, _)
            | Fun(_, _)
            | TypeDomain(_)
            | ResourceDomain(_, _, _)
            | Error
            | Var(_) => {
                panic!("unexpected field type")
            }
        }
    }

    /// Generate parameter list for computing the function selector
    fn compute_param_types(&mut self, ctx: &Context, fun: &FunctionEnv<'_>) -> String {
        let param_types = fun.get_parameter_types();
        let display_type_slice = |tys: &[Type]| -> String {
            tys.iter()
                .map(|t| self.get_evm_type_string(ctx, t))
                .collect::<Vec<_>>()
                .join(",")
        };
        display_type_slice(&param_types)
    }

    /// Generate the start position of memory for returning from the external function
    /// Note: currently, we directly return the free memory pointer, may need to use the memory model later
    fn generate_allocate_unbounded(&mut self, ctx: &Context) {
        emitln!(
            ctx.writer,
            "let memPos := mload({})",
            substitute_placeholders("${MEM_SIZE_LOC}").unwrap()
        );
    }

    /// Generate the cleanup function used in the validator and the encoding function
    fn generate_cleanup(
        &mut self,
        ctx: &Context,
        ty: &Type,
        funs: &mut BTreeMap<String, Vec<String>>,
    ) -> String {
        let name_prefix = "cleanup";
        let function_name = format!("{}_{}", name_prefix, ctx.mangle_type(ty));

        let generate_fun = |_: &mut BTreeMap<String, Vec<String>>,
                            generated_code: &mut Vec<String>| {
            generated_code.push(format!("function {}(value) -> cleaned {{", function_name));
            use PrimitiveType::*;
            use Type::*;
            let mask_value = match ty {
                Primitive(p) => match p {
                    U8 => format!(
                        "and(value, {})",
                        substitute_placeholders("${MAX_U8}").unwrap()
                    ),
                    U64 => format!(
                        "and(value, {})",
                        substitute_placeholders("${MAX_U64}").unwrap()
                    ),
                    U128 => format!(
                        "and(value, {})",
                        substitute_placeholders("${MAX_U128}").unwrap()
                    ),
                    Address | Signer => format!(
                        "add(value, {})",
                        substitute_placeholders("${ADDRESS_U160}").unwrap()
                    ),
                    Bool => "iszero(iszero(value))".to_string(),
                    _ => {
                        panic!("unexpected field type")
                    }
                },
                Struct(mid, sid, _) => {
                    if is_u256(ctx.env, *mid, *sid) {
                        "value".to_string()
                    } else {
                        panic!("unexpected field type")
                    }
                }
                _ => panic!("unexpected field type"),
            };
            generated_code.push(format!("    cleaned := {}", mask_value));
        };

        ctx.generate_marshalling_function(function_name.clone(), funs, generate_fun)
    }

    /// Generate the validator function, which is used in the decode function
    fn generate_validator(
        &mut self,
        ctx: &Context,
        ty: &Type,
        funs: &mut BTreeMap<String, Vec<String>>,
    ) -> String {
        let name_prefix = "validator";
        let function_name = format!("{}_{}", name_prefix, ctx.mangle_type(ty));

        let generate_fun = |funs: &mut BTreeMap<String, Vec<String>>,
                            generated_code: &mut Vec<String>| {
            generated_code.push(format!("function {}(value) {{", function_name));
            let condition = format!("eq(value, {}(value))", self.generate_cleanup(ctx, ty, funs));
            let failure_call = self.call_builtin_str(
                ctx,
                YulFunction::Abort,
                std::iter::once(ABI_DECODING_PARAM_VALIDATION.to_string()),
            );
            generated_code.push(format!(
                "    if iszero({}) {{ {} }}",
                condition, failure_call
            ));
        };

        ctx.generate_marshalling_function(function_name.clone(), funs, generate_fun)
    }

    /// Generate decoding functions for primitive types
    fn generate_abi_decoding_primitive_type(
        &mut self,
        ctx: &Context,
        ty: &Type,
        funs: &mut BTreeMap<String, Vec<String>>,
    ) -> String {
        let name_prefix = "abi_decode";
        let function_name = format!("{}_{}", name_prefix, ctx.mangle_type(ty));

        let generate_fun = |funs: &mut BTreeMap<String, Vec<String>>,
                            generated_code: &mut Vec<String>| {
            generated_code.push(format!(
                "function {}(offset, end) -> value {{",
                function_name
            ));
            let load = "calldataload";
            generated_code.push(format!("    value := {}(offset)", load));
            let validator = self.generate_validator(ctx, ty, funs);
            generated_code.push(format!("    {}(value)", validator));
        };

        ctx.generate_marshalling_function(function_name.clone(), funs, generate_fun)
    }

    /// Generate decoding functions for ty
    fn generate_abi_decoding_type(
        &mut self,
        ctx: &Context,
        ty: &Type,
        funs: &mut BTreeMap<String, Vec<String>>,
    ) -> String {
        use Type::*;
        // TODO: struct and dynamic types
        match ty {
            Primitive(_) => self.generate_abi_decoding_primitive_type(ctx, ty, funs),
            Struct(mid, sid, _) => {
                if is_u256(ctx.env, *mid, *sid) {
                    self.generate_abi_decoding_primitive_type(ctx, ty, funs)
                } else {
                    "".to_string() // TODO
                }
            }
            Tuple(tys) => self.generate_abi_tuple_decoding(ctx, tys, funs),
            _ => "".to_string(), // TODO: Vector
        }
    }

    /// Generate decoding functions for tuple
    fn generate_abi_tuple_decoding(
        &mut self,
        ctx: &Context,
        param_types: &[Type],
        funs: &mut BTreeMap<String, Vec<String>>,
    ) -> String {
        let name_prefix = "abi_decode_tuple";
        let function_name = format!("{}_{}", name_prefix, ctx.mangle_types(param_types));

        let generate_fun = |funs: &mut BTreeMap<String, Vec<String>>,
                            generated_code: &mut Vec<String>| {
            let mut indent_num = 1;
            let overall_type_head_vec = ctx.type_head_sizes_vec(param_types, true);
            let overall_type_head_size = ctx.type_head_sizes_sum(param_types, true);
            let mut temp_code = vec![];
            temp_code.push(format!(
                "{}if slt(sub(dataEnd, headStart), {}) {{ {} }}",
                ident_str(indent_num),
                overall_type_head_size,
                self.call_builtin_str(
                    ctx,
                    YulFunction::Abort,
                    std::iter::once(ABI_DECODING_DATA_TOO_SHORT.to_string())
                ),
            ));
            let mut head_pos = 0;
            let mut ret_var = vec![];
            for (stack_pos, (ty, ty_size)) in overall_type_head_vec.iter().enumerate() {
                let is_static = ctx.is_static_type(ty);
                let mut local_typ_var = vec![];
                // TODO: consider the case size_on_stack is not 1
                local_typ_var.push(format!("value_{}", stack_pos));
                ret_var.push(format!("value_{}", stack_pos));
                // stack_pos += 1;

                let abi_decode_type = self.generate_abi_decoding_type(ctx, ty, funs);
                temp_code.push(format!("{}{{", ident_str(indent_num)));
                indent_num += 1;
                if is_static {
                    temp_code.push(format!(
                        "{}let offset := {}",
                        ident_str(indent_num),
                        head_pos
                    ));
                } else {
                    // TODO: dynamic types need to be revisited
                    let load = "calldataload";
                    temp_code.push(format!(
                        "{}let offset := {}(add(headStart, {}))",
                        ident_str(indent_num),
                        load,
                        head_pos
                    ));
                    temp_code.push(format!(
                        "{}if gt(offset, 0xffffffffffffffff) {{ {} }}",
                        ident_str(indent_num),
                        self.call_builtin_str(
                            ctx,
                            YulFunction::Abort,
                            std::iter::once(ABI_DECODING_DATA_TOO_SHORT.to_string())
                        )
                    ));
                }
                temp_code.push(format!(
                    "{}{} := {}(add(headStart, offset), dataEnd)",
                    ident_str(indent_num),
                    local_typ_var.iter().join(", "),
                    abi_decode_type
                ));
                indent_num -= 1;
                temp_code.push(format!("{}}}", ident_str(indent_num)));
                head_pos += ty_size;
            }
            indent_num -= 1;
            generated_code.push(format!(
                "{}function {}(headStart, dataEnd) -> {} {{",
                ident_str(indent_num),
                function_name,
                ret_var.iter().join(", ")
            ));
            generated_code.append(&mut temp_code);
        };

        ctx.generate_marshalling_function(function_name.clone(), funs, generate_fun)
    }

    /// Generate encoding functions for primitive types
    fn generate_abi_encoding_primitive_type(
        &mut self,
        ctx: &Context,
        ty: &Type,
        funs: &mut BTreeMap<String, Vec<String>>,
    ) -> String {
        let name_prefix = "abi_encode";
        let function_name = format!("{}_{}", name_prefix, ctx.mangle_type(ty));

        let generate_fun = |funs: &mut BTreeMap<String, Vec<String>>,
                            generate_encoding_funs: &mut Vec<String>| {
            generate_encoding_funs.push(format!("function {}(value, pos) {{", function_name));
            generate_encoding_funs.push(format!(
                "    mstore(pos, {}(value))",
                self.generate_cleanup(ctx, ty, funs)
            ));
        };
        ctx.generate_marshalling_function(function_name.clone(), funs, generate_fun)
    }

    /// Generate encoding functions for ty
    fn generate_abi_encoding_type(
        &mut self,
        ctx: &Context,
        ty: &Type,
        funs: &mut BTreeMap<String, Vec<String>>,
    ) -> String {
        use Type::*;
        // TODO: dynamic types
        match ty {
            Primitive(_) => self.generate_abi_encoding_primitive_type(ctx, ty, funs),
            Struct(mid, sid, _) => {
                if is_u256(ctx.env, *mid, *sid) {
                    self.generate_abi_encoding_primitive_type(ctx, ty, funs)
                } else {
                    "".to_string()
                }
            }
            _ => "".to_string(),
        }
    }

    /// Generate encoding functions for tuple
    fn generate_abi_tuple_encoding(
        &mut self,
        ctx: &Context,
        param_types: &[Type],
        funs: &mut BTreeMap<String, Vec<String>>,
    ) -> String {
        let name_prefix = "abi_encode_tuple";
        let function_name = format!("{}_{}", name_prefix, ctx.mangle_types(param_types));

        let generate_fun = |funs: &mut BTreeMap<String, Vec<String>>,
                            generate_encoding_funs: &mut Vec<String>| {
            let mut indent_num = 0;
            let mut value_params = (0..param_types.len())
                .map(|i| format!("value_{}", i))
                .join(", ");
            if value_params != *"" {
                value_params = format!(",{}", value_params);
            }
            generate_encoding_funs.push(format!(
                "{}function {}(headStart {}) -> tail {{",
                ident_str(indent_num),
                function_name,
                value_params
            ));
            indent_num += 1;
            let overall_type_head_vec = ctx.type_head_sizes_vec(param_types, true);
            let overall_type_head_size = ctx.type_head_sizes_sum(param_types, true);
            generate_encoding_funs.push(format!(
                "{}tail := add(headStart, {})",
                ident_str(indent_num),
                overall_type_head_size
            ));
            let mut head_pos = 0;
            for (stack_pos, (ty, ty_size)) in overall_type_head_vec.iter().enumerate() {
                let is_static = ctx.is_static_type(ty);
                let mut local_typ_var = vec![];
                // TODO: consider the case size_on_stack is not 1
                local_typ_var.push(format!("value_{}", stack_pos));
                let values = local_typ_var.iter().join(", ");
                let abi_encode_type = self.generate_abi_encoding_type(ctx, ty, funs);
                if is_static {
                    generate_encoding_funs.push(format!(
                        "{}{}({}, add(headStart, {}))",
                        ident_str(indent_num),
                        abi_encode_type,
                        values,
                        head_pos
                    ));
                } else {
                    // TODO: dynamic types need to be revisited
                    generate_encoding_funs.push(format!(
                        "{}mstore(add(headStart, {}), sub(tail, headStart))",
                        ident_str(indent_num),
                        head_pos
                    ));
                    generate_encoding_funs.push(format!(
                        "{}tail := {}({} tail)",
                        ident_str(indent_num),
                        abi_encode_type,
                        values
                    ));
                }
                head_pos += ty_size;
            }
        };
        ctx.generate_marshalling_function(function_name.clone(), funs, generate_fun)
    }

    /// Generate the dispatch branch for a function
    fn generate_dispatch_item(
        &mut self,
        ctx: &Context,
        fun: &FunctionEnv<'_>,
        selectors: &mut Vec<String>,
        funs: &mut BTreeMap<String, Vec<String>>,
    ) {
        let fun_id = &fun
            .module_env
            .get_id()
            .qualified(fun.get_id())
            .instantiate(vec![]);
        let function_name = ctx.make_function_name(fun_id);
        let original_fun_name = fun.symbol_pool().string(fun.get_name());
        let fun_sig = format!(
            "{}({})",
            original_fun_name,
            self.compute_param_types(ctx, fun)
        );
        let function_selector =
            format!("0x{:x}", Keccak256::digest(fun_sig.as_bytes()))[..10].to_string();
        // Check selector collision
        if selectors.contains(&function_selector) {
            ctx.env.error(
                &ctx.env.unknown_loc(),
                "Hash collision at Function Definition Hash calculation",
            );
        } else {
            selectors.push(function_selector.clone());
        }
        emitln!(ctx.writer, "case {}", function_selector);
        ctx.emit_block(|| {
            emitln!(ctx.writer, "// {}", fun_sig);
            // TODO: check delegate call
            if !is_payable_fun(fun) {
                self.generate_call_value_check(ctx, REVERT_ERR_NON_PAYABLE_FUN);
            }
            // Decoding
            let param_count = fun.get_parameter_count();
            let mut params = "".to_string();
            if param_count > 0 {
                let decoding_fun_name =
                    self.generate_abi_tuple_decoding(ctx, &fun.get_parameter_types(), funs);
                params = (0..param_count).map(|i| format!("param_{}", i)).join(", ");
                let let_params = format!("let {} := ", params);
                emitln!(
                    ctx.writer,
                    "{}{}(4, calldatasize())",
                    let_params,
                    decoding_fun_name
                );
            }
            let ret_count = fun.get_return_count();
            let mut rets = "".to_string();
            let mut let_rets = "".to_string();
            if ret_count > 0 {
                rets = (0..ret_count).map(|i| format!("ret_{}", i)).join(", ");
                let_rets = format!("let {} := ", rets);
            }
            // Call the function
            emitln!(ctx.writer, "{}{}({})", let_rets, function_name, params);
            // Encoding the return values
            let encoding_fun_name =
                self.generate_abi_tuple_encoding(ctx, &fun.get_return_types(), funs);
            if ret_count > 0 {
                rets = format!(", {}", rets);
            }
            // Prepare the return values
            self.generate_allocate_unbounded(ctx);
            emitln!(
                ctx.writer,
                "let memEnd := {}(memPos{})",
                encoding_fun_name,
                rets
            );
            emitln!(ctx.writer, "return(memPos, sub(memEnd, memPos))");
        });
    }

    /// Generate dispatcher routine
    fn generate_dispatcher_routine(&mut self, ctx: &Context, module: &ModuleEnv<'_>) {
        let callables = module
            .get_functions()
            .filter(|f| is_callable_fun(f))
            .collect_vec();
        emitln!(ctx.writer, "if iszero(lt(calldatasize(), 4))");
        let mut decode_funs_map = BTreeMap::new();
        let mut selectors = vec![];
        let para_vec = vec!["224".to_string(), "calldataload(0)".to_string()];
        let shr224 = self.call_builtin_str(ctx, YulFunction::ShiftRight, para_vec.iter().cloned());
        ctx.emit_block(|| {
            emitln!(ctx.writer, "let selector := {}", shr224);
            emitln!(ctx.writer, "switch selector");
            for callable in callables {
                self.generate_dispatch_item(ctx, &callable, &mut selectors, &mut decode_funs_map);
            }
            emitln!(ctx.writer, "default {}");
        });
        let receive_exists = self.optional_receive(ctx, module);
        self.generate_fallback(ctx, module, receive_exists);
        // Print aux functions for marshalling
        ctx.print_out_generated_func(&mut decode_funs_map);
    }

    /// Generate Yul definitions for all callable functions.
    fn callable_functions(&mut self, ctx: &Context, module: &ModuleEnv<'_>) {
        let callables = module
            .get_functions()
            .filter(|f| is_callable_fun(f) || is_fallback_fun(f) || is_receive_fun(f))
            .collect_vec();
        self.generate_dispatcher_routine(ctx, module);
        //   For now, we emit dummy calls so functions are referenced and the Yul optimizer does
        //   not remove them.
        emitln!(
            ctx.writer,
            "\n// Dummy calls to reference callables for Yul optimizer"
        );
        let mut counter = 0;
        for callable in &callables {
            let n = callable.get_return_count();
            if n > 0 {
                let dummy_locals = (0..n).map(|i| format!("$dummy{}", counter + i)).join(", ");
                emit!(ctx.writer, "let {} := ", dummy_locals);
            }
            emitln!(
                ctx.writer,
                "{}({})",
                ctx.make_function_name(&callable.get_qualified_id().instantiate(vec![])),
                (0..callable.get_parameter_count())
                    .map(|i| format!("sload({})", 100 + counter + i))
                    .join(", ")
            );
            for i in 0..n {
                emitln!(
                    ctx.writer,
                    "sstore({}, $dummy{})",
                    200 + counter + i,
                    counter + i
                )
            }
            counter += n;
        }
        emitln!(ctx.writer);
        for callable in callables {
            ctx.check_no_generics(&callable);
            self.function(ctx, &callable.get_qualified_id().instantiate(vec![]))
        }
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

        // We finally emit code for all Yul functions which have been needed by the Move
        // functions.
        for fun in &self.needed_yul_functions {
            emitln!(ctx.writer, &fun.yule_def());
        }
        ctx.writer.unindent();
        emitln!(ctx.writer, "}")
    }
}

// ================================================================================================
// Function generation

impl Generator {
    /// Generate Yul function for Move function.
    fn function(&mut self, ctx: &Context, fun_id: &QualifiedInstId<FunId>) {
        self.done_move_functions.insert(fun_id.clone());
        let fun = &ctx.env.get_function(fun_id.to_qualified_id());
        let target = &ctx.targets.get_target(fun, &FunctionVariant::Baseline);

        // Emit function header
        let params = (0..target.get_parameter_count())
            .map(|idx| ctx.make_local_name(target, idx))
            .join(", ");
        let results = if target.get_return_count() == 0 {
            "".to_string()
        } else {
            format!(
                " -> {}",
                (0..target.get_return_count())
                    .map(|i| ctx.make_result_name(target, i))
                    .join(", ")
            )
        };
        emit!(
            ctx.writer,
            "function {}({}){} ",
            ctx.make_function_name(fun_id),
            params,
            results,
        );
        ctx.emit_block(|| {
            // Emit function locals
            self.collect_borrowed_locals(target);
            let locals = (target.get_parameter_count()..target.get_local_count())
                // filter locals which are not evaded to memory
                .filter(|idx| !self.borrowed_locals.contains_key(idx))
                .map(|idx| ctx.make_local_name(target, idx))
                .join(", ");
            if !locals.is_empty() {
                emitln!(ctx.writer, "let {}", locals);
            }
            if !self.borrowed_locals.is_empty() {
                // These locals are evaded to memory, as references to them are borrowed.
                // Allocate a chunk of memory for them.
                self.call_builtin_with_result(
                    ctx,
                    "let ",
                    std::iter::once("$locals".to_string()),
                    YulFunction::Malloc,
                    std::iter::once((self.borrowed_locals.len() * WORD_SIZE).to_string()),
                );
                // For all evaded locals which are parameters, we need to initialize them from
                // the Yul parameter.
                for idx in self.borrowed_locals.keys() {
                    if *idx < target.get_parameter_count() {
                        emitln!(
                            ctx.writer,
                            "mstore({}, {})",
                            Self::local_ptr(&self.borrowed_locals, *idx).unwrap(),
                            ctx.make_local_name(target, *idx)
                        )
                    }
                }
            }

            // Compute control flow graph, entry block, and label map
            let code = target.data.code.as_slice();
            let cfg = StacklessControlFlowGraph::new_forward(code);
            let entry_bb = Self::get_actual_entry_block(&cfg);
            let label_map = Self::compute_label_map(&cfg, code);

            // Emit state machine to represent control flow.
            // TODO: Eliminate the need for this, see also
            //    https://medium.com/leaningtech/solving-the-structured-control-flow-problem-once-and-for-all-5123117b1ee2
            if cfg.successors(entry_bb).iter().all(|b| cfg.is_dummmy(*b)) {
                // In this trivial case, we have only one block and can omit the state machine
                if let BlockContent::Basic { lower, upper } = cfg.content(entry_bb) {
                    for offs in *lower..*upper + 1 {
                        self.bytecode(ctx, fun_id, target, &label_map, &code[offs as usize], false);
                    }
                } else {
                    panic!("effective entry block is not basic")
                }
            } else {
                emitln!(ctx.writer, "let $block := {}", entry_bb);
                emit!(ctx.writer, "for {} true {} ");
                ctx.emit_block(|| {
                    emitln!(ctx.writer, "switch $block");
                    for blk_id in &cfg.blocks() {
                        if let BlockContent::Basic { lower, upper } = cfg.content(*blk_id) {
                            // Emit code for this basic block.
                            emit!(ctx.writer, "case {} ", blk_id);
                            ctx.emit_block(|| {
                                for offs in *lower..*upper + 1 {
                                    self.bytecode(
                                        ctx,
                                        fun_id,
                                        target,
                                        &label_map,
                                        &code[offs as usize],
                                        true,
                                    );
                                }
                            })
                        }
                    }
                })
            }
        });
        emitln!(ctx.writer)
    }

    /// Compute the locals in the given function which are borrowed from. Such locals need
    /// to be evaded to memory and cannot be kept on the stack.
    fn collect_borrowed_locals(&mut self, target: &FunctionTarget) {
        self.borrowed_locals.clear();
        let mut mem_pos = 0;
        for bc in &target.data.code {
            if let Bytecode::Call(_, _, Operation::BorrowLoc, srcs, _) = bc {
                self.borrowed_locals.insert(srcs[0], mem_pos);
                mem_pos += 1
            }
        }
    }

    /// Get the actual entry block, skipping trailing dummy blocks.
    fn get_actual_entry_block(cfg: &StacklessControlFlowGraph) -> BlockId {
        let mut entry_bb = cfg.entry_block();
        while cfg.is_dummmy(entry_bb) {
            assert_eq!(cfg.successors(entry_bb).len(), 1);
            entry_bb = *cfg.successors(entry_bb).iter().last().unwrap();
        }
        entry_bb
    }

    /// Compute a map from labels to block ids which those labels start.
    fn compute_label_map(
        cfg: &StacklessControlFlowGraph,
        code: &[Bytecode],
    ) -> BTreeMap<Label, BlockId> {
        let mut map = BTreeMap::new();
        for id in cfg.blocks() {
            if let BlockContent::Basic { lower, .. } = cfg.content(id) {
                if let Bytecode::Label(_, l) = &code[*lower as usize] {
                    map.insert(*l, id);
                }
            }
        }
        map
    }

    /// Generate Yul statement for a bytecode.
    fn bytecode(
        &mut self,
        ctx: &Context,
        fun_id: &QualifiedInstId<FunId>,
        target: &FunctionTarget,
        label_map: &BTreeMap<Label, BlockId>,
        bc: &Bytecode,
        has_flow: bool,
    ) {
        use Bytecode::*;
        emitln!(
            ctx.writer,
            "// {}",
            bc.display(target, &BTreeMap::default())
        );
        let print_loc = || {
            let loc = target.get_bytecode_loc(bc.get_attr_id());
            emitln!(
                ctx.writer,
                "/// @src {}:{}:{}",
                ctx.env.file_id_to_idx(loc.file_id()),
                loc.span().start(),
                loc.span().end()
            );
        };
        let get_block = |l| label_map.get(l).expect("label has corresponding block");
        // Need to make a clone below to avoid cascading borrow problems. We don't want the
        // subsequent lambdas to access self.
        let borrowed_locals = self.borrowed_locals.clone();
        let local = |l: &TempIndex| {
            if let Some(ptr) = Self::local_ptr(&borrowed_locals, *l) {
                format!("mload({})", ptr)
            } else {
                ctx.make_local_name(target, *l)
            }
        };
        let make_struct_id = |m: &ModuleId, s: &StructId, inst: &[Type]| {
            m.qualified(*s)
                .instantiate(Type::instantiate_slice(inst, &fun_id.inst))
        };
        let get_local_type = |idx: TempIndex| target.get_local_type(idx).instantiate(&fun_id.inst);
        let mut builtin = |yul_fun: YulFunction, dest: &[TempIndex], srcs: &[TempIndex]| {
            print_loc();
            emitln!(
                ctx.writer,
                "{} := {}",
                local(&dest[0]),
                self.call_builtin_str(ctx, yul_fun, srcs.iter().map(local))
            )
        };
        let mut builtin_typed = |yul_fun_u8: YulFunction,
                                 yul_fun_u64: YulFunction,
                                 yul_fun_u128: YulFunction,
                                 yul_fun_u256: YulFunction,
                                 dest: &[TempIndex],
                                 srcs: &[TempIndex]| {
            use PrimitiveType::*;
            use Type::*;
            match get_local_type(srcs[0]) {
                Primitive(U8) => builtin(yul_fun_u8, dest, srcs),
                Primitive(U64) => builtin(yul_fun_u64, dest, srcs),
                Primitive(U128) => builtin(yul_fun_u128, dest, srcs),
                Struct(mid, sid, _) => {
                    if is_u256(ctx.env, mid, sid) {
                        builtin(yul_fun_u256, dest, srcs)
                    } else {
                        panic!("unexpected operand type")
                    }
                }
                _ => panic!("unexpected operand type"),
            }
        };
        match bc {
            Jump(_, l) => {
                print_loc();
                emitln!(ctx.writer, "$block := {}", get_block(l))
            }
            Branch(_, if_t, if_f, cond) => {
                print_loc();
                emitln!(
                    ctx.writer,
                    "switch {}\n\
                     case 0  {{ $block := {} }}\n\
                     default {{ $block := {} }}",
                    local(cond),
                    get_block(if_f),
                    get_block(if_t),
                )
            }
            Assign(_, dest, src, _) => {
                print_loc();
                self.assign(ctx, target, *dest, local(src))
            }
            Load(_, dest, cons) => {
                print_loc();
                emitln!(
                    ctx.writer,
                    "{} := {}",
                    local(dest),
                    self.constant(ctx, cons)
                )
            }
            Ret(_, results) => {
                print_loc();
                for (idx, result) in results.iter().enumerate() {
                    emitln!(
                        ctx.writer,
                        "{} := {}",
                        ctx.make_result_name(target, idx),
                        local(result)
                    );
                }
                if !self.borrowed_locals.is_empty() {
                    // Free memory allocated for evaded locals
                    self.call_builtin(
                        ctx,
                        YulFunction::Free,
                        vec![
                            "$locals".to_string(),
                            (self.borrowed_locals.len() * WORD_SIZE).to_string(),
                        ]
                        .into_iter(),
                    );
                }
                if has_flow {
                    emitln!(ctx.writer, "leave")
                }
            }
            Abort(_, code) => {
                print_loc();
                self.call_builtin(ctx, YulFunction::Abort, std::iter::once(local(code)))
            }
            Call(_, dest, op, srcs, _) => {
                use Operation::*;
                match op {
                    // Move function call
                    Function(m, f, inst) => {
                        print_loc();
                        self.move_call(
                            ctx,
                            target,
                            dest,
                            m.qualified(*f)
                                .instantiate(Type::instantiate_slice(inst, &fun_id.inst)),
                            srcs.iter().map(local),
                        )
                    }

                    // Packing and unpacking of structs
                    Pack(m, s, inst) => {
                        print_loc();
                        self.pack(
                            ctx,
                            target,
                            make_struct_id(m, s, inst),
                            dest[0],
                            srcs.iter().map(local),
                        )
                    }
                    Unpack(m, s, inst) => {
                        print_loc();
                        self.unpack(
                            ctx,
                            target,
                            make_struct_id(m, s, inst),
                            dest,
                            local(&srcs[0]),
                        )
                    }
                    Destroy => {
                        print_loc();
                        self.destroy(ctx, &get_local_type(srcs[0]), local(&srcs[0]))
                    }

                    // Resource management
                    MoveTo(m, s, inst) => {
                        print_loc();
                        self.move_to(
                            ctx,
                            target,
                            make_struct_id(m, s, inst),
                            local(&srcs[1]),
                            local(&srcs[0]),
                        )
                    }
                    MoveFrom(m, s, inst) => {
                        print_loc();
                        self.move_from(
                            ctx,
                            target,
                            make_struct_id(m, s, inst),
                            dest[0],
                            local(&srcs[0]),
                        )
                    }
                    Exists(m, s, inst) => {
                        print_loc();
                        self.exists(
                            ctx,
                            target,
                            make_struct_id(m, s, inst),
                            dest[0],
                            local(&srcs[0]),
                        )
                    }
                    BorrowGlobal(m, s, inst) => {
                        print_loc();
                        self.borrow_global(
                            ctx,
                            target,
                            make_struct_id(m, s, inst),
                            dest[0],
                            local(&srcs[0]),
                        )
                    }

                    // References
                    BorrowLoc => {
                        print_loc();
                        self.borrow_loc(ctx, target, dest, srcs)
                    }
                    BorrowField(m, s, inst, f) => {
                        print_loc();
                        self.borrow_field(
                            ctx,
                            get_local_type(dest[0]).skip_reference(),
                            make_struct_id(m, s, inst),
                            *f,
                            local(&dest[0]),
                            local(&srcs[0]),
                        )
                    }
                    ReadRef => {
                        print_loc();
                        self.read_ref(
                            ctx,
                            &get_local_type(dest[0]),
                            local(&dest[0]),
                            local(&srcs[0]),
                        )
                    }
                    WriteRef => {
                        print_loc();
                        self.write_ref(
                            ctx,
                            &get_local_type(srcs[1]),
                            local(&srcs[0]),
                            local(&srcs[1]),
                        )
                    }

                    // Arithmetics
                    CastU8 => builtin(YulFunction::CastU8, dest, srcs),
                    CastU64 => builtin(YulFunction::CastU64, dest, srcs),
                    CastU128 => builtin(YulFunction::CastU128, dest, srcs),
                    CastU256 => builtin(YulFunction::CastU256, dest, srcs),
                    Not => builtin(YulFunction::LogicalNot, dest, srcs),
                    Add => builtin_typed(
                        YulFunction::AddU8,
                        YulFunction::AddU64,
                        YulFunction::AddU128,
                        YulFunction::AddU256,
                        dest,
                        srcs,
                    ),
                    Sub => builtin(YulFunction::Sub, dest, srcs),
                    Mul => builtin_typed(
                        YulFunction::MulU8,
                        YulFunction::MulU64,
                        YulFunction::MulU128,
                        YulFunction::MulU256,
                        dest,
                        srcs,
                    ),
                    Div => builtin(YulFunction::Div, dest, srcs),
                    Mod => builtin(YulFunction::Mod, dest, srcs),
                    BitOr => builtin(YulFunction::BitOr, dest, srcs),
                    BitAnd => builtin(YulFunction::BitAnd, dest, srcs),
                    Xor => builtin(YulFunction::BitXor, dest, srcs),
                    Shl => builtin_typed(
                        YulFunction::ShlU8,
                        YulFunction::ShlU64,
                        YulFunction::ShlU128,
                        YulFunction::ShlU256,
                        dest,
                        srcs,
                    ),
                    Shr => builtin(YulFunction::Shr, dest, srcs),
                    Lt => builtin(YulFunction::Lt, dest, srcs),
                    Gt => builtin(YulFunction::Gt, dest, srcs),
                    Le => builtin(YulFunction::LtEq, dest, srcs),
                    Ge => builtin(YulFunction::GtEq, dest, srcs),
                    Or => builtin(YulFunction::LogicalOr, dest, srcs),
                    And => builtin(YulFunction::LogicalAnd, dest, srcs),
                    Eq => builtin(YulFunction::Eq, dest, srcs),
                    Neq => builtin(YulFunction::Neq, dest, srcs),

                    // Specification or other operations which can be ignored here
                    GetField(_, _, _, _)
                    | FreezeRef
                    | GetGlobal(_, _, _)
                    | IsParent(_, _)
                    | WriteBack(_, _)
                    | UnpackRef
                    | PackRef
                    | UnpackRefDeep
                    | PackRefDeep
                    | TraceLocal(_)
                    | TraceReturn(_)
                    | TraceAbort
                    | TraceExp(_, _)
                    | EmitEvent
                    | EventStoreDiverge
                    | OpaqueCallBegin(_, _, _)
                    | OpaqueCallEnd(_, _, _)
                    | Havoc(_)
                    | Stop
                    | TraceGlobalMem(_) => {}
                }
            }

            Label(_, _) | Nop(_) | SaveMem(_, _, _) | SaveSpecVar(_, _, _) | Prop(_, _, _) => {
                // These opcodes are not needed, ignore them
            }
        }
    }

    fn assign(&self, ctx: &Context, target: &FunctionTarget, dest: TempIndex, src: String) {
        if let Some(ptr) = Self::local_ptr(&self.borrowed_locals, dest) {
            emitln!(ctx.writer, "mstore({}, {})", ptr, src)
        } else {
            emitln!(
                ctx.writer,
                "{} := {}",
                ctx.make_local_name(target, dest),
                src
            );
        }
    }

    /// Generate a string representing a constant.
    fn constant(&self, _ctx: &Context, cons: &Constant) -> String {
        match cons {
            Constant::Bool(v) => {
                if *v {
                    "true".to_string()
                } else {
                    "false".to_string()
                }
            }
            Constant::U8(v) => {
                format!("{}", v)
            }
            Constant::U64(v) => {
                format!("{}", v)
            }
            Constant::U128(v) => {
                format!("{}", v)
            }
            Constant::U256(v) => {
                format!("{}", v)
            }
            Constant::Address(a) => {
                format!("0x{}", a.to_str_radix(16))
            }
            Constant::ByteArray(_) => "(ByteArray NYI)".to_string(),
        }
    }

    /// Generate call to a Move function.
    fn move_call(
        &mut self,
        ctx: &Context,
        target: &FunctionTarget,
        dest: &[TempIndex],
        fun: QualifiedInstId<FunId>,
        mut args: impl Iterator<Item = String>,
    ) {
        let fun_name = ctx.make_function_name(&fun);
        if !self.done_move_functions.contains(&fun) {
            self.needed_move_functions.push(fun)
        }
        let call_str = format!("{}({})", fun_name, args.join(", "));
        match dest.len() {
            0 => emitln!(ctx.writer, &call_str),
            1 => self.assign(ctx, target, dest[0], call_str),
            _ => {
                if dest
                    .iter()
                    .any(|idx| self.borrowed_locals.contains_key(idx))
                {
                    // Multiple outputs, some of them evaded into memory. Need to introduce
                    // temporaries to store the result
                    ctx.emit_block(|| {
                        let temp_names = (0..dest.len())
                            .map(|i| {
                                if self.borrowed_locals.contains_key(&dest[i]) {
                                    format!("$d{}", i)
                                } else {
                                    ctx.make_local_name(target, dest[i])
                                }
                            })
                            .collect_vec();
                        emitln!(
                            ctx.writer,
                            "let {} := {}",
                            temp_names.iter().join(", "),
                            call_str
                        );
                        for (i, temp_name) in temp_names.into_iter().enumerate() {
                            if self.borrowed_locals.contains_key(&dest[i]) {
                                self.assign(ctx, target, dest[i], temp_name)
                            }
                        }
                    })
                } else {
                    let dest_str = dest
                        .iter()
                        .map(|idx| ctx.make_local_name(target, *idx))
                        .join(", ");
                    emitln!(ctx.writer, "{} := {}", dest_str, call_str)
                }
            }
        }
    }

    /// Generate call to a builtin function.
    fn call_builtin(
        &mut self,
        ctx: &Context,
        fun: YulFunction,
        args: impl Iterator<Item = String>,
    ) {
        emitln!(ctx.writer, "{}", self.call_builtin_str(ctx, fun, args))
    }

    /// Generate call to a builtin function which delivers results.
    fn call_builtin_with_result(
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
    fn call_builtin_str(
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

    fn need_yul_function(&mut self, yul_fun: YulFunction) {
        if !self.needed_yul_functions.contains(&yul_fun) {
            self.needed_yul_functions.insert(yul_fun);
            for dep in yul_fun.yule_deps() {
                self.need_yul_function(dep);
            }
        }
    }
}

// ================================================================================================
// Memory

impl Generator {
    /// If this is a local which is borrowed and evaded to memory, return pointer to its memory.
    fn local_ptr(borrowed_locals: &BTreeMap<TempIndex, usize>, idx: TempIndex) -> Option<String> {
        borrowed_locals.get(&idx).map(|pos| {
            if *pos == 0 {
                "$locals".to_string()
            } else {
                format!("add($locals, {})", pos * WORD_SIZE)
            }
        })
    }

    /// Borrow a local.
    fn borrow_loc(
        &mut self,
        ctx: &Context,
        target: &FunctionTarget,
        dest: &[TempIndex],
        srcs: &[TempIndex],
    ) {
        let local_ptr = self.call_builtin_str(
            ctx,
            YulFunction::MakePtr,
            vec![
                "false".to_string(),
                Self::local_ptr(&self.borrowed_locals, srcs[0]).expect("local evaded to memory"),
            ]
            .into_iter(),
        );
        emitln!(
            ctx.writer,
            "{} := {}",
            ctx.make_local_name(target, dest[0]),
            local_ptr
        )
    }

    /// Read the value of reference.
    fn read_ref(&mut self, ctx: &Context, ty: &Type, dest: String, src: String) {
        let yul_fun = self.load_builtin_fun(ctx, ty.skip_reference());
        self.call_builtin_with_result(
            ctx,
            "",
            std::iter::once(dest),
            yul_fun,
            std::iter::once(src),
        )
    }

    /// Write the value of reference.
    fn write_ref(&mut self, ctx: &Context, ty: &Type, dest: String, src: String) {
        let yul_fun = self.store_builtin_fun(ctx, ty.skip_reference());
        self.call_builtin(ctx, yul_fun, vec![dest, src].into_iter())
    }

    /// Pack a structure.
    fn pack(
        &mut self,
        ctx: &Context,
        target: &FunctionTarget,
        struct_id: QualifiedInstId<StructId>,
        dest: TempIndex,
        srcs: impl Iterator<Item = String>,
    ) {
        // We unfortunately need to clone the struct layout because of borrowing issues.
        // However, it should not be too large.
        let layout = self.get_or_compute_struct_layout(ctx, &struct_id).clone();

        ctx.emit_block(|| {
            // Allocate memory
            let mem = "$mem".to_string();
            emitln!(
                ctx.writer,
                "let $mem := {}",
                self.call_builtin_str(
                    ctx,
                    YulFunction::Malloc,
                    std::iter::once(layout.size.to_string()),
                )
            );

            // Initialize fields
            let struct_env = &ctx.env.get_struct(struct_id.to_qualified_id());
            for (logical_offs, field_exp) in srcs.enumerate() {
                let yul_fun = self.memory_store_builtin_fun(
                    ctx,
                    &struct_env
                        .get_field_by_offset(logical_offs)
                        .get_type()
                        .instantiate(&struct_id.inst),
                );
                let (byte_offset, _) = *layout.offsets.get(&logical_offs).unwrap();
                let field_ptr = format!("add({}, {})", mem, byte_offset);
                self.call_builtin(ctx, yul_fun, vec![field_ptr, field_exp].into_iter());
            }
            // Store result
            self.assign(ctx, target, dest, mem);
        })
    }

    /// Unpack a structure.
    fn unpack(
        &mut self,
        ctx: &Context,
        target: &FunctionTarget,
        struct_id: QualifiedInstId<StructId>,
        dest: &[TempIndex],
        src: String,
    ) {
        // We unfortunately need to clone the struct layout because of borrowing issues.
        // However, it should not be too large.
        let layout = self.get_or_compute_struct_layout(ctx, &struct_id).clone();

        // Copy fields
        let struct_env = &ctx.env.get_struct(struct_id.to_qualified_id());
        for (logical_offs, dest_idx) in dest.iter().enumerate() {
            let yul_fun = self.memory_load_builtin_fun(
                ctx,
                &struct_env
                    .get_field_by_offset(logical_offs)
                    .get_type()
                    .instantiate(&struct_id.inst),
            );
            let (byte_offset, _) = *layout.offsets.get(&logical_offs).unwrap();
            let field_ptr = format!("add({}, {})", src, byte_offset);
            let call_str = self.call_builtin_str(ctx, yul_fun, std::iter::once(field_ptr));
            self.assign(ctx, target, *dest_idx, call_str);
        }

        // Free memory
        self.call_builtin(
            ctx,
            YulFunction::Free,
            vec![src, layout.size.to_string()].into_iter(),
        )
    }

    /// Destroy (free) a value of type.
    /// TODO: the Destroy instruction is currently not reflecting lifetime of values correctly,
    ///   but is only inserted for the original pop bytecode. We should run lifetime analysis
    ///   and insert Destroy where needed. However, this does not matter much yet, as
    ///   YulFunction::Free is a nop in the current runtime which uses arena style allocation.
    fn destroy(&mut self, ctx: &Context, ty: &Type, src: String) {
        use Type::*;
        match ty {
            Struct(m, s, inst) => {
                // Destroy all fields
                let struct_id = m.qualified(*s).instantiate(inst.clone());
                let layout = self.get_or_compute_struct_layout(ctx, &struct_id).clone();
                let struct_env = &ctx.env.get_struct(struct_id.to_qualified_id());
                for field in struct_env.get_fields() {
                    let field_type = field.get_type().instantiate(inst);
                    if !self.type_allocates_memory(&field_type) {
                        continue;
                    }
                    ctx.emit_block(|| {
                        let (byte_offset, _) = layout.offsets.get(&field.get_offset()).unwrap();
                        let field_ptr = self.call_builtin_str(
                            ctx,
                            YulFunction::LoadU256,
                            std::iter::once(format!("add({}, {})", src, byte_offset)),
                        );
                        emitln!(ctx.writer, "let $field_ptr := {}", field_ptr);
                        self.destroy(ctx, &field_type, "$field_ptr".to_string());
                    })
                }

                // Free the memory allocated by this struct.
                self.call_builtin(
                    ctx,
                    YulFunction::Free,
                    vec![src, layout.size.to_string()].into_iter(),
                )
            }
            Vector(ty) => {
                if self.type_allocates_memory(ty.as_ref()) {
                    // TODO: implement vectors
                }
            }
            _ => {}
        }
    }

    /// Borrow a field, creating a reference to it.
    fn borrow_field(
        &mut self,
        ctx: &Context,
        _ty: &Type,
        struct_id: QualifiedInstId<StructId>,
        field_offs: usize,
        dest: String,
        src: String,
    ) {
        let layout = self.get_or_compute_struct_layout(ctx, &struct_id);
        let (byte_offs, _) = *layout
            .offsets
            .get(&field_offs)
            .expect("field offset defined");
        // A reference to a struct is a pointer to a U256 which in turn points to the struct's
        // memory. We need to load this pointer first.
        let load_struct_ptr =
            self.call_builtin_str(ctx, YulFunction::LoadU256, std::iter::once(src));
        let add_offset = self.call_builtin_str(
            ctx,
            YulFunction::IndexPtr,
            vec![load_struct_ptr, byte_offs.to_string()].into_iter(),
        );
        emitln!(ctx.writer, "{} := {}", dest, add_offset)
    }

    /// Test whether resource exists.
    fn exists(
        &mut self,
        ctx: &Context,
        target: &FunctionTarget,
        struct_id: QualifiedInstId<StructId>,
        dst: TempIndex,
        addr: String,
    ) {
        // Obtain the storage base offset for this resource.
        let base_offset = self.type_storage_base(
            ctx,
            &struct_id.to_type(),
            "${RESOURCE_STORAGE_CATEGORY}",
            addr,
        );
        // Load the exists flag and store it into destination.
        let load_flag = self.call_builtin_str(
            ctx,
            YulFunction::StorageLoadU8,
            std::iter::once(base_offset),
        );
        self.assign(ctx, target, dst, load_flag);
    }

    /// Move resource from memory to storage.
    fn move_to(
        &mut self,
        ctx: &Context,
        _target: &FunctionTarget,
        struct_id: QualifiedInstId<StructId>,
        addr: String,
        value: String,
    ) {
        ctx.emit_block(|| {
            // Obtain the storage base offset for this resource.
            emitln!(
                ctx.writer,
                "let $base_offset := {}",
                self.type_storage_base(
                    ctx,
                    &struct_id.to_type(),
                    "${RESOURCE_STORAGE_CATEGORY}",
                    addr,
                )
            );
            let base_offset = "$base_offset";

            // At the base offset we store a boolean indicating whether the resource exists. Check this
            // and if it is set, abort. Otherwise set this bit.
            let exists_call = self.call_builtin_str(
                ctx,
                YulFunction::StorageLoadU8,
                std::iter::once(base_offset.to_string()),
            );
            let abort_call =
                self.call_builtin_str(ctx, YulFunction::AbortBuiltin, std::iter::empty());
            emitln!(ctx.writer, "if {} {{\n  {}\n}}", exists_call, abort_call);
            self.call_builtin(
                ctx,
                YulFunction::AlignedStorageStore,
                vec![base_offset.to_string(), "true".to_string()].into_iter(),
            );

            // Move the struct to storage.
            ctx.emit_block(|| {
                // The actual resource data starts at base_offset + 32. Set the destination address
                // to this.
                emitln!(
                    ctx.writer,
                    "let $dst := add({}, ${{RESOURCE_EXISTS_FLAG_SIZE}})",
                    base_offset
                );
                emitln!(ctx.writer, "let $src := {}", value);
                // Perform the move.
                self.move_struct_to_storage(
                    ctx,
                    &struct_id,
                    "$src".to_string(),
                    "$dst".to_string(),
                );
            });
        })
    }

    /// Moves a struct from memory to storage. This recursively moves linked data like
    /// nested structs and vectors.
    fn move_struct_to_storage(
        &mut self,
        ctx: &Context,
        struct_id: &QualifiedInstId<StructId>,
        src: String,
        dst: String,
    ) {
        let layout = self.get_or_compute_struct_layout(ctx, struct_id).clone();

        // By invariant we know that the leading fields are pointer fields. Copy them first.
        for field_offs in layout.field_order.iter().take(layout.pointer_count) {
            let (byte_offs, ty) = layout.offsets.get(field_offs).unwrap();
            assert_eq!(byte_offs % 32, 0, "pointer fields are on word boundary");
            if ty.is_vector() {
                ctx.env
                    .error(&self.contract_loc, "vectors not yet implemented");
                continue;
            }
            ctx.emit_block(|| {
                let hash = self.type_hash(ctx, ty);
                // Allocate a new storage pointer.
                emitln!(
                    ctx.writer,
                    "let $linked_dst := {}",
                    self.call_builtin_str(
                        ctx,
                        YulFunction::NewLinkedStorageBase,
                        std::iter::once(format!("0x{:x}", hash))
                    )
                );
                // Load the pointer to the linked memory.
                emitln!(
                    ctx.writer,
                    "let $linked_src := mload(add({}, {}))",
                    src,
                    byte_offs
                );
                // Recursively move.
                let field_struct_id = ty.get_struct_id(ctx.env).expect("struct");
                self.move_struct_to_storage(
                    ctx,
                    &field_struct_id,
                    "$linked_src".to_string(),
                    "$linked_dst".to_string(),
                );
                // Store the result at the destination
                self.call_builtin(
                    ctx,
                    YulFunction::AlignedStorageStore,
                    vec![
                        format!("add({}, {})", dst, byte_offs),
                        "$linked_dst".to_string(),
                    ]
                    .into_iter(),
                )
            })
        }

        // The remaining fields are all primitive. We also know that memory is padded to word size,
        // so we can just copy directly word by word, which has the lowest gas cost.
        if layout.pointer_count < layout.field_order.len() {
            let mut byte_offs = layout
                .offsets
                .get(&layout.field_order[layout.pointer_count])
                .unwrap()
                .0;
            assert_eq!(
                byte_offs % 32,
                0,
                "first non-pointer field on word boundary"
            );
            while byte_offs < layout.size {
                self.call_builtin(
                    ctx,
                    YulFunction::AlignedStorageStore,
                    vec![
                        format!("add({}, {})", dst, byte_offs),
                        format!("mload(add({}, {}))", src, byte_offs),
                    ]
                    .into_iter(),
                );
                byte_offs += 32
            }
        }

        // Free the memory allocated by this struct.
        self.call_builtin(
            ctx,
            YulFunction::Free,
            vec![src, layout.size.to_string()].into_iter(),
        )
    }

    /// Move resource from storage to local.
    fn move_from(
        &mut self,
        ctx: &Context,
        target: &FunctionTarget,
        struct_id: QualifiedInstId<StructId>,
        dst: TempIndex,
        addr: String,
    ) {
        ctx.emit_block(|| {
            // Obtain the storage base offset for this resource.
            emitln!(
                ctx.writer,
                "let $base_offset := {}",
                self.type_storage_base(
                    ctx,
                    &struct_id.to_type(),
                    "${RESOURCE_STORAGE_CATEGORY}",
                    addr,
                )
            );
            let base_offset = "$base_offset";

            // At the base offset we store a boolean indicating whether the resource exists. Check this
            // and if it is not set, abort. Otherwise clear this bit.
            let exists_call = self.call_builtin_str(
                ctx,
                YulFunction::StorageLoadU8,
                std::iter::once(base_offset.to_string()),
            );
            let abort_call =
                self.call_builtin_str(ctx, YulFunction::AbortBuiltin, std::iter::empty());
            emitln!(
                ctx.writer,
                "if not({}) {{\n  {}\n}}",
                exists_call,
                abort_call
            );
            self.call_builtin(
                ctx,
                YulFunction::StorageStoreU8,
                vec![base_offset.to_string(), "false".to_string()].into_iter(),
            );

            // Move the struct out of storage into memory
            ctx.emit_block(|| {
                // The actual resource data starts at base_offset + 32. Set the src address
                // to this.
                emitln!(
                    ctx.writer,
                    "let $src := add({}, ${{RESOURCE_EXISTS_FLAG_SIZE}})",
                    base_offset
                );

                // Perform the move and assign the result.
                emitln!(ctx.writer, "let $dst");
                self.move_struct_to_memory(ctx, &struct_id, "$src".to_string(), "$dst".to_string());
                self.assign(ctx, target, dst, "$dst".to_string())
            })
        })
    }

    /// Move a struct from storage to memory, zeroing all associated storage. This recursively
    /// moves linked data like nested structs and vectors.
    fn move_struct_to_memory(
        &mut self,
        ctx: &Context,
        struct_id: &QualifiedInstId<StructId>,
        src: String,
        dst: String,
    ) {
        // Allocate struct.
        let layout = self.get_or_compute_struct_layout(ctx, struct_id).clone();
        emitln!(
            ctx.writer,
            "{} := {}",
            dst,
            self.call_builtin_str(
                ctx,
                YulFunction::Malloc,
                std::iter::once(layout.size.to_string()),
            )
        );

        // Copy fields. By invariant we know that the leading fields are pointer fields.
        for field_offs in layout.field_order.iter().take(layout.pointer_count) {
            let (byte_offs, ty) = layout.offsets.get(field_offs).unwrap();
            assert_eq!(byte_offs % 32, 0, "pointer fields are on word boundary");
            if ty.is_vector() {
                ctx.env
                    .error(&self.contract_loc, "vectors not yet implemented");
                continue;
            }
            ctx.emit_block(|| {
                let field_src_ptr = format!("add({}, {})", src, byte_offs);
                let field_dst_ptr = format!("add({}, {})", dst, byte_offs);
                // Load the pointer to the linked storage.
                let load_call = self.call_builtin_str(
                    ctx,
                    YulFunction::AlignedStorageLoad,
                    std::iter::once(field_src_ptr.clone()),
                );
                emitln!(ctx.writer, "let $linked_src := {}", load_call);
                // Declare where to store the result and recursively move
                emitln!(ctx.writer, "let $linked_dst");
                let field_struct_id = ty.get_struct_id(ctx.env).expect("struct");
                self.move_struct_to_memory(
                    ctx,
                    &field_struct_id,
                    "$linked_src".to_string(),
                    "$linked_dst".to_string(),
                );
                // Store the result at the destination.
                assert_eq!(byte_offs % 32, 0);
                emitln!(ctx.writer, "mstore({}, $linked_dst)", field_dst_ptr,);
                // Clear the storage to get a refund
                self.call_builtin(
                    ctx,
                    YulFunction::AlignedStorageStore,
                    vec![field_src_ptr, 0.to_string()].into_iter(),
                )
            })
        }

        // The remaining fields are all primitive. We also know that memory is padded to word size,
        // so we can just copy directly word by word, which has the lowest gas cost.
        if layout.pointer_count < layout.field_order.len() {
            let mut byte_offs = layout
                .offsets
                .get(&layout.field_order[layout.pointer_count])
                .unwrap()
                .0;
            assert_eq!(
                byte_offs % 32,
                0,
                "first non-pointer field on word boundary"
            );
            while byte_offs < layout.size {
                let field_src_ptr = format!("add({}, {})", src, byte_offs);
                let field_dst_ptr = format!("add({}, {})", dst, byte_offs);
                let load_call = self.call_builtin_str(
                    ctx,
                    YulFunction::AlignedStorageLoad,
                    std::iter::once(field_src_ptr.clone()),
                );
                emitln!(ctx.writer, "mstore({}, {})", field_dst_ptr, load_call);
                self.call_builtin(
                    ctx,
                    YulFunction::AlignedStorageStore,
                    vec![field_src_ptr, 0.to_string()].into_iter(),
                );
                byte_offs += 32
            }
        }
    }

    /// Borrow a resource.
    fn borrow_global(
        &mut self,
        ctx: &Context,
        target: &FunctionTarget,
        struct_id: QualifiedInstId<StructId>,
        dst: TempIndex,
        addr: String,
    ) {
        ctx.emit_block(|| {
            // Obtain the storage base offset for this resource.
            emitln!(
                ctx.writer,
                "let $base_offset := {}",
                self.type_storage_base(
                    ctx,
                    &struct_id.to_type(),
                    "${RESOURCE_STORAGE_CATEGORY}",
                    addr,
                )
            );
            let base_offset = "$base_offset";

            // At the base offset check the flag whether the resource exists.
            let exists_call = self.call_builtin_str(
                ctx,
                YulFunction::StorageLoadU8,
                std::iter::once(base_offset.to_string()),
            );
            let abort_call =
                self.call_builtin_str(ctx, YulFunction::AbortBuiltin, std::iter::empty());
            emitln!(
                ctx.writer,
                "if not({}) {{\n  {}\n}}",
                exists_call,
                abort_call
            );

            // Skip the existence flag and create a pointer.
            let make_ptr = self.call_builtin_str(
                ctx,
                YulFunction::MakePtr,
                vec![
                    "true".to_string(),
                    format!("add({}, ${{RESOURCE_EXISTS_FLAG_SIZE}})", base_offset),
                ]
                .into_iter(),
            );
            self.assign(ctx, target, dst, make_ptr)
        })
    }

    /// Make a pointer into storage for the given type and instance.
    fn type_storage_base(
        &mut self,
        ctx: &Context,
        ty: &Type,
        category: &str,
        instance: String,
    ) -> String {
        let hash = self.type_hash(ctx, ty);
        self.call_builtin_str(
            ctx,
            YulFunction::MakeTypeStorageBase,
            vec![category.to_string(), format!("0x{:x}", hash), instance].into_iter(),
        )
    }

    /// Derive a 4 byte hash for a type. If this hash creates a collision in the current
    /// contract, create an error.
    fn type_hash(&mut self, ctx: &Context, ty: &Type) -> u32 {
        let sig = ctx.mangle_type(ty);
        let mut keccak = Keccak256::new();
        keccak.update(sig.as_bytes());
        let digest = keccak.finalize();
        let hash = u32::from_le_bytes([digest[0], digest[1], digest[2], digest[3]]);
        if let Some(old_ty) = self.type_sig_map.insert(hash, ty.clone()) {
            if old_ty != *ty {
                let ty_ctx = &TypeDisplayContext::WithEnv {
                    env: ctx.env,
                    type_param_names: None,
                };
                ctx.env.error(
                    &self.contract_loc,
                    &format!(
                        "collision of type hash for types `{}` and `{}`\n\
                         (resolution via attribute not yet implemented)",
                        ty.display(ty_ctx),
                        old_ty.display(ty_ctx)
                    ),
                )
            }
        }
        hash
    }

    /// Get the layout of the instantiated struct in linear memory.
    fn get_or_compute_struct_layout(
        &mut self,
        ctx: &Context,
        st: &QualifiedInstId<StructId>,
    ) -> &StructLayout {
        if self.struct_layout.get(st).is_none() {
            // Compute the fields such that the larger appear first, and pointer fields
            // precede non-pointer fields.
            let s_or_v = |ty: &Type| ty.is_vector() || ty.is_struct();
            let struct_env = ctx.env.get_struct(st.to_qualified_id());
            let ordered_fields = struct_env
                .get_fields()
                .map(|field| {
                    let field_type = field.get_type().instantiate(&st.inst);
                    let field_size = self.type_size(ctx, &field_type);
                    (field.get_offset(), field_size, field_type)
                })
                .sorted_by(|(_, s1, ty1), (_, s2, ty2)| {
                    if s1 > s2 {
                        std::cmp::Ordering::Less
                    } else if s2 > s1 {
                        std::cmp::Ordering::Greater
                    } else if s_or_v(ty1) && !s_or_v(ty2) {
                        std::cmp::Ordering::Less
                    } else if s_or_v(ty2) && !s_or_v(ty1) {
                        std::cmp::Ordering::Greater
                    } else {
                        std::cmp::Ordering::Equal
                    }
                });
            let mut result = StructLayout::default();
            for (logical_offs, field_size, ty) in ordered_fields {
                result.field_order.push(logical_offs);
                if s_or_v(&ty) {
                    result.pointer_count += 1
                }
                result.offsets.insert(logical_offs, (result.size, ty));
                result.size += field_size
            }
            self.struct_layout.insert(st.clone(), result);
        }
        self.struct_layout.get(st).unwrap()
    }

    /// Calculate the size, in bytes, for the memory layout of this type.
    fn type_size(&mut self, _ctx: &Context, ty: &Type) -> usize {
        use PrimitiveType::*;
        use Type::*;
        match ty {
            Primitive(p) => match p {
                Bool | U8 => 1,
                U64 => 8,
                U128 => 16,
                Address | Signer => 20,
                Num | Range | EventStore => {
                    panic!("unexpected field type")
                }
            },
            Struct(..) | Vector(..) => 32,
            Tuple(_)
            | TypeParameter(_)
            | Reference(_, _)
            | Fun(_, _)
            | TypeDomain(_)
            | ResourceDomain(_, _, _)
            | Error
            | Var(_) => {
                panic!("unexpected field type")
            }
        }
    }

    fn load_builtin_fun(&mut self, ctx: &Context, ty: &Type) -> YulFunction {
        match self.type_size(ctx, ty.skip_reference()) {
            1 => YulFunction::LoadU8,
            8 => YulFunction::LoadU64,
            16 => YulFunction::LoadU128,
            32 => YulFunction::LoadU256,
            _ => panic!("unexpected type size"),
        }
    }

    fn store_builtin_fun(&mut self, ctx: &Context, ty: &Type) -> YulFunction {
        match self.type_size(ctx, ty.skip_reference()) {
            1 => YulFunction::StoreU8,
            8 => YulFunction::StoreU64,
            16 => YulFunction::StoreU128,
            32 => YulFunction::StoreU256,
            _ => panic!("unexpected type size"),
        }
    }

    fn memory_load_builtin_fun(&mut self, ctx: &Context, ty: &Type) -> YulFunction {
        match self.type_size(ctx, ty.skip_reference()) {
            1 => YulFunction::MemoryLoadU8,
            8 => YulFunction::MemoryLoadU64,
            16 => YulFunction::MemoryLoadU128,
            32 => YulFunction::MemoryLoadU256,
            _ => panic!("unexpected type size"),
        }
    }

    fn memory_store_builtin_fun(&mut self, ctx: &Context, ty: &Type) -> YulFunction {
        match self.type_size(ctx, ty.skip_reference()) {
            1 => YulFunction::MemoryStoreU8,
            8 => YulFunction::MemoryStoreU64,
            16 => YulFunction::MemoryStoreU128,
            32 => YulFunction::MemoryStoreU256,
            _ => panic!("unexpected type size"),
        }
    }

    #[allow(dead_code)]
    fn storage_load_builtin_fun(&mut self, ctx: &Context, ty: &Type) -> YulFunction {
        match self.type_size(ctx, ty.skip_reference()) {
            1 => YulFunction::StorageLoadU8,
            8 => YulFunction::StorageLoadU64,
            16 => YulFunction::StorageLoadU128,
            32 => YulFunction::StorageLoadU256,
            _ => panic!("unexpected type size"),
        }
    }

    #[allow(dead_code)]
    fn storage_store_builtin_fun(&mut self, ctx: &Context, ty: &Type) -> YulFunction {
        match self.type_size(ctx, ty.skip_reference()) {
            1 => YulFunction::StorageStoreU8,
            8 => YulFunction::StorageStoreU64,
            16 => YulFunction::StorageStoreU128,
            32 => YulFunction::StorageStoreU256,
            _ => panic!("unexpected type size"),
        }
    }
    fn type_allocates_memory(&self, ty: &Type) -> bool {
        matches!(ty, Type::Vector(..) | Type::Struct(..))
    }
}

// ================================================================================================
// Helpers

impl<'a> Context<'a> {
    /// Check whether given Move function has no generics; report error otherwise.
    fn check_no_generics(&self, fun: &FunctionEnv<'_>) {
        if fun.get_type_parameter_count() > 0 {
            self.env.error(
                &fun.get_loc(),
                "#[callable] or #[create] functions cannot be generic",
            )
        }
    }

    /// Make the name of a contract.
    fn make_contract_name(&self, module: &ModuleEnv) -> String {
        let mod_name = module.get_name();
        let mod_sym = module.symbol_pool().string(mod_name.name());
        format!("A{}_{}", mod_name.addr().to_str_radix(16), mod_sym)
    }

    /// Make the name of function.
    fn make_function_name(&self, fun_id: &QualifiedInstId<FunId>) -> String {
        let fun = self.env.get_function(fun_id.to_qualified_id());
        let fun_sym = fun.symbol_pool().string(fun.get_name());
        format!(
            "{}_{}{}",
            self.make_contract_name(&fun.module_env),
            fun_sym,
            self.mangle_types(&fun_id.inst)
        )
    }

    /// Mangle a type for being part of name.
    ///
    /// Note that the mangled type representation is also used to create a hash for types
    /// in `Generator::type_hash` which is used to index storage. Therefor the representation here
    /// cannot be changed without creating versioning problems for existing storage of contracts.
    fn mangle_type(&self, ty: &Type) -> String {
        use PrimitiveType::*;
        use Type::*;
        match ty {
            Primitive(p) => match p {
                U8 => "u8".to_string(),
                U64 => "u64".to_string(),
                U128 => "u128".to_string(),
                Num => "num".to_string(),
                Address => "address".to_string(),
                Signer => "signer".to_string(),
                Bool => "bool".to_string(),
                Range => "range".to_string(),
                _ => format!("<<unsupported {:?}>>", ty),
            },
            Vector(et) => format!("vec{}", self.mangle_types(&[et.as_ref().to_owned()])),
            Struct(mid, sid, inst) => {
                self.mangle_struct(&mid.qualified(*sid).instantiate(inst.clone()))
            }
            TypeParameter(..) | Fun(..) | Tuple(..) | TypeDomain(..) | ResourceDomain(..)
            | Error | Var(..) | Reference(..) => format!("<<unsupported {:?}>>", ty),
        }
    }

    /// Mangle a struct.
    fn mangle_struct(&self, struct_id: &QualifiedInstId<StructId>) -> String {
        let struct_env = &self.env.get_struct(struct_id.to_qualified_id());
        let module_name = self.make_contract_name(&struct_env.module_env);
        format!(
            "{}_{}{}",
            module_name,
            struct_env.get_name().display(struct_env.symbol_pool()),
            self.mangle_types(&struct_id.inst)
        )
    }

    /// Mangle a slice of types.
    fn mangle_types(&self, tys: &[Type]) -> String {
        if tys.is_empty() {
            "".to_owned()
        } else {
            format!("${}$", tys.iter().map(|ty| self.mangle_type(ty)).join("_"))
        }
    }

    /// Make name for a local.
    fn make_local_name(&self, target: &FunctionTarget, idx: TempIndex) -> String {
        target
            .get_local_name(idx)
            .display(target.symbol_pool())
            .to_string()
            .replace("#", "_")
    }

    /// Make name for a result.
    fn make_result_name(&self, target: &FunctionTarget, idx: usize) -> String {
        if target.get_return_count() == 1 {
            "$result".to_string()
        } else {
            format!("$result{}", idx)
        }
    }

    /// Emits a Yul block.
    fn emit_block(&self, blk: impl FnOnce()) {
        emitln!(self.writer, "{");
        self.writer.indent();
        blk();
        self.writer.unindent();
        emitln!(self.writer, "}");
    }

    /// Get the types inside a struct
    fn get_or_compute_struct_types(&self, mid: ModuleId, sid: StructId) -> Vec<Type> {
        let mut tys = vec![];
        let module_env = self.env.get_module(mid);
        let struct_env = module_env.get_struct(sid);
        for field in struct_env.get_fields() {
            tys.push(field.get_type());
        }
        tys
    }

    /// Check whether ty is a static type in the sense of serialization
    fn is_static_type(&self, ty: &Type) -> bool {
        use PrimitiveType::*;
        use Type::*;

        let conjunction = |tys: &[Type]| {
            tys.iter()
                .map(|t| self.is_static_type(t))
                .collect::<Vec<_>>()
                .into_iter()
                .all(|t| t)
        };
        match ty {
            Primitive(p) => match p {
                Bool | U8 | U64 | U128 | Address | Signer => true,
                _ => {
                    panic!("unexpected field type")
                }
            },
            Vector(_) => false,
            Tuple(tys) => conjunction(tys),
            Struct(mid, sid, _) => {
                if is_u256(self.env, *mid, *sid) {
                    true
                } else {
                    let tys = self.get_or_compute_struct_types(*mid, *sid);
                    conjunction(&tys)
                }
            }
            TypeParameter(_)
            | Reference(_, _)
            | Fun(_, _)
            | TypeDomain(_)
            | ResourceDomain(_, _, _)
            | Error
            | Var(_) => {
                panic!("unexpected field type")
            }
        }
    }

    /// Compute the sum of data size of tys
    fn type_head_sizes_sum(&self, tys: &[Type], padded: bool) -> usize {
        let size_vec = self.type_head_sizes_vec(tys, padded);
        size_vec.iter().map(|(_, size)| size).sum()
    }

    /// Compute the data size of all types in tys
    fn type_head_sizes_vec(&self, tys: &[Type], padded: bool) -> Vec<(Type, usize)> {
        tys.iter()
            .map(|ty_| (ty_.clone(), self.type_head_size(ty_, padded)))
            .collect_vec()
    }

    /// Compute the data size of ty on the stack
    fn type_head_size(&self, ty: &Type, padded: bool) -> usize {
        use PrimitiveType::*;
        use Type::*;
        if self.is_static_type(ty) {
            match ty {
                Primitive(p) => match p {
                    Bool => {
                        if padded {
                            32
                        } else {
                            1
                        }
                    }
                    U8 => {
                        if padded {
                            32
                        } else {
                            1
                        }
                    }
                    U64 => {
                        if padded {
                            32
                        } else {
                            8
                        }
                    }
                    U128 => {
                        if padded {
                            32
                        } else {
                            16
                        }
                    }
                    Address | Signer => {
                        if padded {
                            32
                        } else {
                            20
                        }
                    }
                    Num | Range | EventStore => {
                        panic!("unexpected field type")
                    }
                },
                Tuple(tys) => self.type_head_sizes_sum(tys, padded),
                Struct(mid, sid, _) => {
                    if is_u256(self.env, *mid, *sid) {
                        32
                    } else {
                        let tys = self.get_or_compute_struct_types(*mid, *sid);
                        self.type_head_sizes_sum(&tys, padded)
                    }
                }
                _ => panic!("unexpected field type"),
            }
        } else {
            // Dynamic types
            32
        }
    }

    /// Print out generated function code
    fn print_out_generated_func(&self, funs: &mut BTreeMap<String, Vec<String>>) {
        for value in funs.values() {
            for code_line in value {
                emitln!(self.writer, code_line);
            }
        }
    }

    fn generate_marshalling_function(
        &self,
        function_name: String,
        funs: &mut BTreeMap<String, Vec<String>>,
        generate_fun: impl FnOnce(&mut BTreeMap<String, Vec<String>>, &mut Vec<String>),
    ) -> String {
        if funs.contains_key(&function_name) {
            return function_name;
        }
        let mut generate_encoding_funs = vec![];
        generate_fun(funs, &mut generate_encoding_funs);
        generate_encoding_funs.push("}".to_string());
        funs.insert(function_name.clone(), generate_encoding_funs);
        function_name
    }
}

/// Check whether a simple attribute is present in an attribute list.
fn has_simple_attr(env: &GlobalEnv, attrs: &[Attribute], name: &str) -> bool {
    attrs.iter().any(|a| match a {
        Attribute::Apply(_, s, args)
            if args.is_empty() && env.symbol_pool().string(*s).as_str() == name =>
        {
            true
        }
        _ => false,
    })
}

/// Check whether the module has a `#[contract]` attribute.
fn is_contract_module(module: &ModuleEnv<'_>) -> bool {
    has_simple_attr(module.env, module.get_attributes(), CONTRACT_ATTR)
}

/// Check whether the function has a `#[callable]` attribute.
fn is_callable_fun(fun: &FunctionEnv<'_>) -> bool {
    has_simple_attr(fun.module_env.env, fun.get_attributes(), CALLABLE_ATTR)
}

/// Check whether the function has a `#[create]` attribute.
fn is_create_fun(fun: &FunctionEnv<'_>) -> bool {
    has_simple_attr(fun.module_env.env, fun.get_attributes(), CREATE_ATTR)
}

/// Check whether the function has a `#[payable]` attribute.
fn is_payable_fun(fun: &FunctionEnv<'_>) -> bool {
    has_simple_attr(fun.module_env.env, fun.get_attributes(), PAYABLE_ATTR)
}

/// Check whether the function has a `#[receive]` attribute.
fn is_receive_fun(fun: &FunctionEnv<'_>) -> bool {
    has_simple_attr(fun.module_env.env, fun.get_attributes(), RECEIVE_ATTR)
}

/// Check whether the function has a `#[fallback]]` attribute.
fn is_fallback_fun(fun: &FunctionEnv<'_>) -> bool {
    has_simple_attr(fun.module_env.env, fun.get_attributes(), RECEIVE_FALLBACK)
}

/// Returns whether a module has a `#[evm_arith]` attribute.
pub(crate) fn is_evm_arith_module(module: &ModuleEnv<'_>) -> bool {
    *module.self_address() == CORE_CODE_ADDRESS
        && has_simple_attr(module.env, module.get_attributes(), EVM_ARITH_ATTR)
}

/// Returns whether the struct identified by module_id and struct_id is the native U256 struct.
fn is_u256(env: &GlobalEnv, module_id: ModuleId, struct_id: StructId) -> bool {
    let module_env = env.get_module(module_id);
    let struct_env = module_env.get_struct(struct_id);
    is_evm_arith_module(&module_env) && struct_env.is_native()
}

/// return num of indents as a string
fn ident_str(num: usize) -> String {
    (0..num).map(|_| "    ").collect::<String>()
}
