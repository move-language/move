// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
};

use itertools::Itertools;

use move_model::{
    ast::{Attribute, TempIndex},
    code_writer::CodeWriter,
    emitln,
    model::{FunId, FunctionEnv, GlobalEnv, ModuleEnv, QualifiedInstId, StructId},
    ty::{PrimitiveType, Type},
};
use move_stackless_bytecode::{
    function_target::FunctionTarget,
    function_target_pipeline::{FunctionTargetPipeline, FunctionTargetsHolder, FunctionVariant},
    livevar_analysis::LiveVarAnalysisProcessor,
    reaching_def_analysis::ReachingDefProcessor,
    stackless_bytecode::{Bytecode, Constant, Label, Operation},
    stackless_control_flow_graph::{BlockContent, BlockId, StacklessControlFlowGraph},
};

use crate::{yul_functions::YulFunction, Options};

const CONTRACT_ATTR: &str = "contract";
const CREATE_ATTR: &str = "create";
const CALLABLE_ATTR: &str = "callable";

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
pub struct Generator {
    /// Move functions, including type instantiation, needed in the currently generated code block.
    needed_move_functions: Vec<QualifiedInstId<FunId>>,
    /// Move functions for which code has been emitted.
    done_move_functions: BTreeSet<QualifiedInstId<FunId>>,
    /// Yule functions needed in the currently generated code block.
    needed_yul_functions: BTreeSet<YulFunction>,
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
        (
            Context {
                options,
                env,
                targets: Self::create_bytecode(options, env),
                writer: CodeWriter::new(env.unknown_loc()),
            },
            Self {
                needed_move_functions: Default::default(),
                done_move_functions: Default::default(),
                needed_yul_functions: Default::default(),
            },
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
                if is_callable_fun(&fun) || is_create_fun(&fun) {
                    Self::add_fun(&mut targets, &fun)
                }
            }
        }
        // Run a minimal transformation pipeline. For now, we do reaching-def and live-var
        // to clean up some churn created by the conversion from stack to stackless bytecode.
        let mut pipeline = FunctionTargetPipeline::default();
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
        let contract_name = ctx.make_contract_name(module);
        emitln!(ctx.writer, "object \"{}\" {{", contract_name);
        ctx.writer.indent();

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
        emitln!(ctx.writer, "object \"{}\" {{", contract_deployed_name);
        ctx.writer.indent();
        self.begin_code_block(ctx);
        self.callable_functions(ctx, module);
        self.end_code_block(ctx);
        ctx.writer.unindent();
        emitln!(ctx.writer, "}");

        // Close contract object
        ctx.writer.unindent();
        emitln!(ctx.writer, "}");
    }

    /// Generate optional creator (contract constructor).
    /// TODO: fully implement
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
            // TODO: implement creators
            ctx.env
                .error(&creators[1].get_loc(), "[NYI] #[create] functions")
        }
    }

    /// Generate Yul definitions for all callable functions.
    fn callable_functions(&mut self, ctx: &Context, module: &ModuleEnv<'_>) {
        let callables = module
            .get_functions()
            .filter(|f| is_callable_fun(f))
            .collect_vec();
        // TODO: generate dispatcher and marshalling
        for callable in callables {
            ctx.check_no_generics(&callable);
            self.function(
                ctx,
                &callable
                    .module_env
                    .get_id()
                    .qualified(callable.get_id())
                    .instantiate(vec![]),
            )
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
            self.done_move_functions.insert(fun_id.clone());
            self.function(ctx, &fun_id)
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
        let fun = &ctx.env.get_function(fun_id.to_qualified_id());
        // Emit function header
        let params = (0..fun.get_parameter_count())
            .map(|idx| ctx.make_local_name(fun, idx))
            .join(", ");
        let results = match fun.get_return_count() {
            0 => "".to_string(),
            1 => format!(" -> {}", ctx.make_result_name(fun, 0)),
            _ => format!(
                " -> ({})",
                (0..fun.get_return_count())
                    .map(|i| ctx.make_result_name(fun, i))
                    .join(", ")
            ),
        };
        emitln!(
            ctx.writer,
            "function {}({}){} {{",
            ctx.make_function_name(fun_id),
            params,
            results,
        );
        ctx.writer.indent();

        // Emit function locals
        let target = ctx.targets.get_target(fun, &FunctionVariant::Baseline);
        let locals = (target.get_parameter_count()..target.get_local_count())
            .map(|idx| ctx.make_local_name(fun, idx))
            .join(", ");
        if !locals.is_empty() {
            emitln!(ctx.writer, "let {}", locals);
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
                    self.bytecode(ctx, &target, &label_map, &code[offs as usize], false);
                }
            } else {
                panic!("effective entry block is not basic")
            }
        } else {
            emitln!(ctx.writer, "let $block := {}", entry_bb);
            emitln!(ctx.writer, "for {} true {} {");
            ctx.writer.indent();
            emitln!(ctx.writer, "switch $block");
            for blk_id in &cfg.blocks() {
                if let BlockContent::Basic { lower, upper } = cfg.content(*blk_id) {
                    // Emit code for this basic block.
                    emitln!(ctx.writer, "case {} {{", blk_id);
                    ctx.writer.indent();
                    for offs in *lower..*upper + 1 {
                        self.bytecode(ctx, &target, &label_map, &code[offs as usize], true);
                    }
                    ctx.writer.unindent();
                    emitln!(ctx.writer, "}")
                }
            }
            ctx.writer.unindent();
            emitln!(ctx.writer, "}"); // for
        }
        ctx.writer.unindent();
        emitln!(ctx.writer, "}\n"); // function
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

    /// Compute a map from labels to block ids which those labels start
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
        target: &FunctionTarget,
        label_map: &BTreeMap<Label, BlockId>,
        bc: &Bytecode,
        has_flow: bool,
    ) {
        use Bytecode::*;
        let fun = target.func_env;
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
        let print_nyi = || {
            emitln!(
                ctx.writer,
                "// NYI: {}",
                bc.display(target, &BTreeMap::default())
            );
        };
        let get_block = |l| label_map.get(l).expect("label has corresponding block");
        let local_name = |l: &TempIndex| ctx.make_local_name(fun, *l);
        let mut builtin = |yul_fun: YulFunction, dest: &[TempIndex], srcs: &[TempIndex]| {
            print_loc();
            emitln!(
                ctx.writer,
                "{} := {}",
                local_name(&dest[0]),
                self.call_builtin_str(ctx, yul_fun, srcs.iter().map(local_name))
            )
        };
        let mut builtin_typed = |yul_fun_u8: YulFunction,
                                 yul_fun_u64: YulFunction,
                                 yul_fun_u128: YulFunction,
                                 dest: &[TempIndex],
                                 srcs: &[TempIndex]| {
            use PrimitiveType::*;
            use Type::*;
            match target.get_local_type(srcs[0]) {
                Primitive(U8) => builtin(yul_fun_u8, dest, srcs),
                Primitive(U64) => builtin(yul_fun_u64, dest, srcs),
                Primitive(U128) => builtin(yul_fun_u128, dest, srcs),
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
                    "if {} {{ $block := {} }} else {{ $block := {} }}",
                    local_name(cond),
                    get_block(if_t),
                    get_block(if_f)
                )
            }
            Assign(_, dest, src, _) => {
                print_loc();
                emitln!(ctx.writer, "{} := {}", local_name(dest), local_name(src));
            }
            Load(_, dest, cons) => {
                emitln!(
                    ctx.writer,
                    "{} := {}",
                    local_name(dest),
                    self.constant(ctx, cons)
                )
            }
            Ret(_, results) => {
                print_loc();
                for (idx, result) in results.iter().enumerate() {
                    emitln!(
                        ctx.writer,
                        "{} := {}",
                        ctx.make_result_name(fun, idx),
                        local_name(result)
                    );
                }
                if has_flow {
                    emitln!(ctx.writer, "leave")
                }
            }
            Abort(_, code) => {
                self.call_builtin(ctx, YulFunction::Abort, std::iter::once(local_name(code)))
            }
            Call(_, dest, op, srcs, _) => {
                use Operation::*;
                match op {
                    // Move function call
                    Function(m, f, targs) => self.move_call(
                        ctx,
                        dest.iter().map(local_name),
                        m.qualified(*f).instantiate(targs.clone()),
                        srcs.iter().map(local_name),
                    ),

                    // Packing and unpacking of structs
                    Pack(_, _, _) => print_nyi(),
                    Unpack(_, _, _) => print_nyi(),

                    // Resource management
                    MoveTo(_, _, _) => print_nyi(),
                    MoveFrom(_, _, _) => print_nyi(),
                    Exists(_, _, _) => print_nyi(),

                    // Borrowing
                    BorrowLoc => print_nyi(),
                    BorrowField(_, _, _, _) => print_nyi(),
                    BorrowGlobal(_, _, _) => print_nyi(),
                    ReadRef => print_nyi(),
                    WriteRef => print_nyi(),

                    // Arithmetics
                    CastU8 => print_nyi(),
                    CastU64 => print_nyi(),
                    CastU128 => print_nyi(),
                    Not => builtin(YulFunction::LogicalNot, dest, srcs),
                    Add => builtin_typed(
                        YulFunction::AddU8,
                        YulFunction::AddU64,
                        YulFunction::AddU128,
                        dest,
                        srcs,
                    ),
                    Sub => builtin(YulFunction::Sub, dest, srcs),
                    Mul => builtin_typed(
                        YulFunction::MulU8,
                        YulFunction::MulU64,
                        YulFunction::MulU128,
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
                    | Destroy
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
                    | Stop => {}
                }
            }

            Label(_, _) | Nop(_) | SaveMem(_, _, _) | SaveSpecVar(_, _, _) | Prop(_, _, _) => {
                // These opcodes are not needed, ignore them
            }
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
        mut dest: impl Iterator<Item = String>,
        fun: QualifiedInstId<FunId>,
        mut args: impl Iterator<Item = String>,
    ) {
        let fun_name = ctx.make_function_name(&fun);
        if !self.done_move_functions.contains(&fun) {
            self.needed_move_functions.push(fun)
        }
        emitln!(
            ctx.writer,
            "{} := {}({})",
            dest.join(", "),
            fun_name,
            args.join(",")
        )
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

    /// Create the string representing call to builtin function.
    fn call_builtin_str(
        &mut self,
        _ctx: &Context,
        fun: YulFunction,
        mut args: impl Iterator<Item = String>,
    ) -> String {
        self.needed_yul_functions.insert(fun);
        for dep in fun.yule_deps() {
            self.needed_yul_functions.insert(dep);
        }
        format!("{}({})", fun.yule_name(), args.join(", "))
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
    fn make_local_name(&self, fun: &FunctionEnv, idx: TempIndex) -> String {
        fun.get_local_name(idx)
            .display(fun.symbol_pool())
            .to_string()
            .replace("#", "_")
    }

    /// Make name for a result.
    fn make_result_name(&self, fun: &FunctionEnv, idx: usize) -> String {
        if fun.get_return_count() == 1 {
            "$result".to_string()
        } else {
            format!("$result{}", idx)
        }
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
