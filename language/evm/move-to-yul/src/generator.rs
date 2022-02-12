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
    emit, emitln,
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

use crate::{
    yul_functions::{substitute_placeholders, YulFunction, WORD_SIZE},
    Options,
};

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
#[derive(Default)]
pub struct Generator {
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
}

/// Information about the layout of a struct in linear memory.
#[derive(Default, Clone)]
struct StructLayout {
    /// The size, in bytes, of this struct.
    size: usize,
    /// Offsets in linear memory for each field.
    offsets: BTreeMap<usize, (usize, usize)>,
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
        emitln!(
            ctx.writer,
            "mstore(${MEM_SIZE_LOC}, memoryguard(${USED_MEM}))"
        );
        self.callable_functions(ctx, module);
        self.end_code_block(ctx);
        ctx.writer.unindent();
        emitln!(ctx.writer, "}");

        // Close contract object
        ctx.writer.unindent();
        emitln!(ctx.writer, "}");
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

    /// Generate Yul definitions for all callable functions.
    fn callable_functions(&mut self, ctx: &Context, module: &ModuleEnv<'_>) {
        let callables = module
            .get_functions()
            .filter(|f| is_callable_fun(f))
            .collect_vec();
        // TODO: generate dispatcher and marshalling
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
        emitln!(
            ctx.writer,
            "function {}({}){} {{",
            ctx.make_function_name(fun_id),
            params,
            results,
        );
        ctx.writer.indent();

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
                    self.bytecode(ctx, target, &label_map, &code[offs as usize], false);
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
                        self.bytecode(ctx, target, &label_map, &code[offs as usize], true);
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
        let print_nyi = || {
            emitln!(
                ctx.writer,
                "// NYI: {}",
                bc.display(target, &BTreeMap::default())
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
                self.call_builtin(ctx, YulFunction::Abort, std::iter::once(local(code)))
            }
            Call(_, dest, op, srcs, _) => {
                use Operation::*;
                match op {
                    // Move function call
                    Function(m, f, targs) => self.move_call(
                        ctx,
                        target,
                        dest,
                        m.qualified(*f).instantiate(targs.clone()),
                        srcs.iter().map(local),
                    ),

                    // Packing and unpacking of structs
                    Pack(m, s, inst) => self.pack(
                        ctx,
                        target,
                        m.qualified(*s).instantiate(inst.clone()),
                        dest[0],
                        srcs.iter().map(local),
                    ),
                    Unpack(m, s, inst) => self.unpack(
                        ctx,
                        m.qualified(*s).instantiate(inst.clone()),
                        dest.iter().map(local),
                        local(&srcs[0]),
                    ),

                    // Resource management
                    MoveTo(_, _, _) => print_nyi(),
                    MoveFrom(_, _, _) => print_nyi(),
                    Exists(_, _, _) => print_nyi(),

                    // References
                    BorrowLoc => self.borrow_loc(ctx, target, dest, srcs),
                    BorrowField(m, s, inst, f) => self.borrow_field(
                        ctx,
                        target.get_local_type(dest[0]).skip_reference(),
                        m.qualified(*s).instantiate(inst.clone()),
                        *f,
                        local(&dest[0]),
                        local(&srcs[0]),
                    ),
                    BorrowGlobal(_, _, _) => print_nyi(),
                    ReadRef => self.read_ref(
                        ctx,
                        target.get_local_type(dest[0]),
                        local(&dest[0]),
                        local(&srcs[0]),
                    ),
                    WriteRef => self.write_ref(
                        ctx,
                        target.get_local_type(srcs[1]),
                        local(&srcs[0]),
                        local(&srcs[1]),
                    ),

                    // Arithmetics
                    CastU8 => builtin(YulFunction::CastU8, dest, srcs),
                    CastU64 => builtin(YulFunction::CastU64, dest, srcs),
                    CastU128 => builtin(YulFunction::CastU128, dest, srcs),
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
                    emitln!(ctx.writer, "{");
                    ctx.writer.indent();
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
                    ctx.writer.unindent();
                    emitln!(ctx.writer, "}");
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
        &self,
        ctx: &Context,
        target: &FunctionTarget,
        dest: &[TempIndex],
        srcs: &[TempIndex],
    ) {
        emitln!(
            ctx.writer,
            "{} := {}",
            ctx.make_local_name(target, dest[0]),
            Self::local_ptr(&self.borrowed_locals, srcs[0]).expect("local evaded to memory")
        )
    }

    /// Read the value of reference.
    fn read_ref(&mut self, ctx: &Context, ty: &Type, dest: String, src: String) {
        let yul_fun = match self.type_size(ctx, ty.skip_reference()) {
            1 => YulFunction::LoadU8,
            8 => YulFunction::LoadU64,
            32 => YulFunction::LoadU256,
            _ => {
                ctx.env
                    .error(&ctx.env.unknown_loc(), "NYI: types of 16 bytes size");
                YulFunction::LoadU256
            }
        };
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
        let yul_fun = match self.type_size(ctx, ty.skip_reference()) {
            1 => YulFunction::StoreU8,
            8 => YulFunction::StoreU64,
            32 => YulFunction::StoreU256,
            _ => {
                ctx.env
                    .error(&ctx.env.unknown_loc(), "NYI: types of 16 bytes size");
                YulFunction::StoreU256
            }
        };
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

        emitln!(ctx.writer, "{");
        ctx.writer.indent();

        // Allocate memory
        let mem = "$mem".to_string();
        self.call_builtin_with_result(
            ctx,
            "let ",
            std::iter::once(mem.clone()),
            YulFunction::Malloc,
            std::iter::once(layout.size.to_string()),
        );

        // Initialize fields
        let struct_env = &ctx.env.get_struct(struct_id.to_qualified_id());
        for (logical_offs, field_exp) in srcs.enumerate() {
            use PrimitiveType::*;
            use Type::*;
            let (byte_offset, _) = *layout.offsets.get(&logical_offs).unwrap();
            let field_ptr = format!("add({}, {})", mem, byte_offset);
            match struct_env
                .get_field_by_offset(logical_offs)
                .get_type()
                .instantiate(&struct_id.inst)
            {
                Primitive(p) => match p {
                    Bool | U8 => self.call_builtin(
                        ctx,
                        YulFunction::MemoryStoreU8,
                        vec![field_ptr, field_exp].into_iter(),
                    ),
                    U64 => self.call_builtin(
                        ctx,
                        YulFunction::MemoryStoreU64,
                        vec![field_ptr, field_exp].into_iter(),
                    ),
                    U128 | Address | Signer => ctx
                        .env
                        .error(&ctx.env.unknown_loc(), "NYI: u128, address, signer"),
                    Num | Range | EventStore => {
                        panic!("unexpected primitive field type")
                    }
                },
                Struct(..) | Vector(..) => self.call_builtin(
                    ctx,
                    YulFunction::MemoryStoreU256,
                    vec![field_ptr, field_exp].into_iter(),
                ),
                _ => panic!("unexpected field type"),
            }
        }
        // Store result
        self.assign(ctx, target, dest, mem);
        ctx.writer.unindent();
        emitln!(ctx.writer, "}")
    }

    fn unpack(
        &self,
        _ctx: &Context,
        _struct_id: QualifiedInstId<StructId>,
        _dest: impl Iterator<Item = String>,
        _src: String,
    ) {
        todo!()
    }

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
        emitln!(
            ctx.writer,
            "{} := add({}, {})",
            dest,
            load_struct_ptr,
            byte_offs
        )
    }

    /// Get the layout of the instantiated struct in linear memory.
    fn get_or_compute_struct_layout(
        &mut self,
        ctx: &Context,
        st: &QualifiedInstId<StructId>,
    ) -> &StructLayout {
        if self.struct_layout.get(st).is_none() {
            let mut offsets = BTreeMap::new();
            let mut size = 0;
            let struct_env = ctx.env.get_struct(st.to_qualified_id());
            for field in struct_env.get_fields() {
                let field_size = self.type_size(ctx, &field.get_type().instantiate(&st.inst));
                offsets.insert(field.get_offset(), (size, field_size));
                size += field_size
            }
            self.struct_layout
                .insert(st.clone(), StructLayout { size, offsets });
        }
        self.struct_layout.get(st).unwrap()
    }

    fn type_size(&mut self, _ctx: &Context, ty: &Type) -> usize {
        use PrimitiveType::*;
        use Type::*;
        match ty {
            Primitive(p) => match p {
                Bool | U8 => 1,
                U64 => 8,
                U128 => 16,
                Address => 16,
                Signer => 16,
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
