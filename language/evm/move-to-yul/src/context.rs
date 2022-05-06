// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    attributes,
    attributes::{
        extract_contract_name, is_contract_fun, is_create_fun, is_event_struct,
        is_evm_contract_module, is_storage_struct,
    },
    events::EventSignature,
    evm_transformation::EvmTransformationProcessor,
    native_functions::NativeFunctions,
    yul_functions,
    yul_functions::YulFunction,
    Options,
};
use codespan::FileId;
use itertools::Itertools;
use move_model::{
    ast::{ModuleName, TempIndex},
    code_writer::CodeWriter,
    emitln,
    model::{
        FunId, FunctionEnv, GlobalEnv, ModuleEnv, ModuleId, QualifiedId, QualifiedInstId,
        StructEnv, StructId,
    },
    ty::{PrimitiveType, Type},
};
use move_stackless_bytecode::{
    function_target::FunctionTarget,
    function_target_pipeline::{FunctionTargetPipeline, FunctionTargetsHolder, FunctionVariant},
    livevar_analysis::LiveVarAnalysisProcessor,
    reaching_def_analysis::ReachingDefProcessor,
};
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
    path::PathBuf,
};

/// Address at which the EVM modules are stored.
const EVM_MODULE_ADDRESS: &str = "0x2";

/// Immutable context passed through the compilation.
pub(crate) struct Context<'a> {
    /// The program options.
    pub options: &'a Options,
    /// The global environment, containing the Move model.
    pub env: &'a GlobalEnv,
    /// The function target data containing the stackless bytecode.
    pub targets: FunctionTargetsHolder,
    /// A code writer where we emit Yul code to.
    pub writer: CodeWriter,
    /// Cached memory layout info.
    pub struct_layout: RefCell<BTreeMap<QualifiedInstId<StructId>, StructLayout>>,
    /// Native function info.
    pub native_funs: NativeFunctions,
    /// Mapping of file_id to file number and path.
    pub(crate) file_id_map: BTreeMap<FileId, (usize, String)>,
    /// Mapping of event structs to corresponding event signatures
    pub(crate) event_signature_map: RefCell<BTreeMap<QualifiedInstId<StructId>, EventSignature>>,
    /// A code writer where we emit JSON-ABI.
    pub abi_writer: CodeWriter,
}

/// Information about the layout of a struct in linear memory.
#[derive(Default, Clone)]
pub(crate) struct StructLayout {
    /// The size, in bytes, of this struct.
    pub size: usize,
    /// Offsets in linear memory and type for each field, indexed by logical offsets, i.e.
    /// position in the struct definition.
    pub offsets: BTreeMap<usize, (usize, Type)>,
    /// Field order (in terms of logical offset), optimized for best memory representation.
    pub field_order: Vec<usize>,
    /// The number of leading fields which are pointers to linked data. Those fields always
    /// appear first in the field_order.
    pub pointer_count: usize,
}

/// Describes a contract as identified via attribute analysis of the model.
pub(crate) struct Contract {
    /// The name of the contract.
    pub name: String,
    /// The module defining the contract.
    pub module: ModuleId,
    /// Optional struct representing storage root.
    pub storage: Option<StructId>,
    /// Optional constructor function.
    pub constructor: Option<FunId>,
    /// Functions which are callable, receive, or fallback entry.
    pub functions: Vec<FunId>,
}

impl<'a> Context<'a> {
    // --------------------------------------------------------------------------------------------
    // Creation

    /// Create a new context.
    pub fn new(options: &'a Options, env: &'a GlobalEnv, for_test: bool) -> Self {
        let writer = CodeWriter::new(env.unknown_loc());
        let abi_writer = CodeWriter::new(env.unknown_loc());

        writer.set_emit_hook(yul_functions::substitute_placeholders);
        let targets = Self::create_bytecode(options, env, for_test);
        let file_id_map: BTreeMap<FileId, (usize, String)> = targets
            .get_funs()
            .map(|f| env.get_function(f).get_loc().file_id())
            .collect::<BTreeSet<_>>()
            .into_iter()
            .map(|id| (id, Self::get_relative_path(env, id)))
            // Sort this by file path to ensure deterministic output
            .sorted_by(|(_, p1), (_, p2)| p2.cmp(p1))
            // Assign position and collect
            .enumerate()
            .map(|(pos, (id, path))| (id, (pos + 1, path)))
            .collect();
        let mut ctx = Self {
            options,
            env,
            targets,
            file_id_map,
            writer,
            struct_layout: Default::default(),
            native_funs: NativeFunctions::default(),
            event_signature_map: Default::default(),
            abi_writer,
        };
        ctx.native_funs = NativeFunctions::create(&ctx);
        ctx.build_event_signature_map();
        ctx
    }

    /// Helper to get relative path of a file id.
    fn get_relative_path(env: &GlobalEnv, file_id: FileId) -> String {
        let file_path = env.get_file(file_id).to_string_lossy().to_string();
        let current_dir = std::env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("."))
            .to_string_lossy()
            .to_string()
            + &std::path::MAIN_SEPARATOR.to_string();
        if file_path.starts_with(&current_dir) {
            file_path[current_dir.len()..].to_string()
        } else {
            file_path
        }
    }

    /// Helper to create the stackless bytecode.
    fn create_bytecode(
        options: &Options,
        env: &GlobalEnv,
        for_test: bool,
    ) -> FunctionTargetsHolder {
        // Populate the targets holder with all needed functions.
        let mut targets = FunctionTargetsHolder::default();
        let is_used_fun = |fun: &FunctionEnv| {
            if for_test {
                attributes::is_evm_test_fun(fun) || attributes::is_test_fun(fun)
            } else {
                attributes::is_callable_fun(fun)
                    || attributes::is_create_fun(fun)
                    || attributes::is_receive_fun(fun)
                    || attributes::is_fallback_fun(fun)
            }
        };
        let external_name = ModuleName::from_str("0x2", env.symbol_pool().make("ExternalResult"));
        for module in env.get_modules() {
            if *module.get_name() == external_name {
                for fun in module.get_functions() {
                    Self::add_fun(&mut targets, &fun)
                }
            }
            if !module.is_target() {
                continue;
            }
            for fun in module.get_functions() {
                if is_used_fun(&fun) {
                    Self::add_fun(&mut targets, &fun)
                }
            }
        }
        // Run a minimal transformation pipeline. For now, we do some evm pre-processing,
        // and reaching-def and live-var to clean up some churn created by the conversion from
        // stack to stackless bytecode.
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

    // --------------------------------------------------------------------------------------------
    // Contract Analysis

    /// Derive the EVM contracts defined in this context. This contains contracts
    /// defined both in target (i.e. currently compiled) and dependency modules.
    ///
    /// This function will produce errors in the global env if attributes are misused,
    /// and should only be called once because of this.
    pub fn derive_contracts(&self) -> Vec<Contract> {
        self.env
            .get_modules()
            .into_iter()
            .filter_map(|ref m| {
                if is_evm_contract_module(m) {
                    Some(self.extract_contract(m))
                } else {
                    None
                }
            })
            .collect()
    }

    /// Extract information about a contract from a module.
    fn extract_contract(&self, module: &ModuleEnv) -> Contract {
        // Identity name.
        let name = if let Some(name) = extract_contract_name(module) {
            name
        } else {
            self.make_contract_name(module)
        };
        // Identify storage struct
        let storage_cands = module
            .get_structs()
            .filter(is_storage_struct)
            .take(2)
            .collect::<Vec<_>>();
        let storage = match storage_cands.len() {
            0 => None,
            1 => Some(storage_cands[0].get_id()),
            _ => {
                self.env.error(
                    &storage_cands[1].get_loc(),
                    "only one #[storage] struct allowed per contract module",
                );
                None
            }
        };
        // Identify constructor.
        let ctor_cands = module
            .get_functions()
            .filter(is_create_fun)
            .take(2)
            .collect::<Vec<_>>();
        let constructor = match ctor_cands.len() {
            0 => None,
            1 => Some(ctor_cands[0].get_id()),
            _ => {
                self.env.error(
                    &ctor_cands[1].get_loc(),
                    "only one #[create] function allowed per contract module",
                );
                None
            }
        };
        // Identify functions.
        let functions = module
            .get_functions()
            .filter(is_contract_fun)
            .map(|s| s.get_id())
            .collect();

        // Check conditions.
        if storage.is_some() && constructor.is_none() {
            self.env.error(
                &module.get_loc(),
                "contract declares #[storage] struct but misses #[creator] function",
            )
        }

        Contract {
            name,
            module: module.get_id(),
            storage,
            constructor,
            functions,
        }
    }

    // --------------------------------------------------------------------------------------------
    // Queries

    /// Return iterator for all functions in the environment which stem from a target module
    /// and which satisfy predicate.
    pub fn get_target_functions(&self, p: impl Fn(&FunctionEnv) -> bool) -> Vec<FunctionEnv<'a>> {
        self.env
            .get_modules()
            .filter(|m| m.is_target())
            .map(|m| m.into_functions().filter(|f| p(f)))
            .flatten()
            .collect()
    }

    /// Return iterator for all structs in the environment which stem from a target module
    /// and which satisfy predicate.
    fn get_target_structs(&self, p: impl Fn(&StructEnv) -> bool) -> Vec<StructEnv<'a>> {
        self.env
            .get_modules()
            .filter(|m| m.is_target())
            .map(|m| m.into_structs().filter(|f| p(f)))
            .flatten()
            .collect()
    }

    // --------------------------------------------------------------------------------------------
    // Signature Event Map

    /// Build the event signature map
    pub fn build_event_signature_map(&self) {
        let event_structs_vec = self.get_target_structs(is_event_struct);
        let mut event_signature_map_ref = self.event_signature_map.borrow_mut();
        for st_env in event_structs_vec {
            if !self.check_no_generics_struct(&st_env) {
                break;
            }
            let mut sig = EventSignature::create_default_event_signature(self, &st_env);
            let ev_sig_str_opt = attributes::extract_event_signature(&st_env);
            if let Some(ev_sig_str) = ev_sig_str_opt {
                let parsed_sig_opt =
                    EventSignature::parse_into_event_signature(self, &ev_sig_str, &st_env);
                if let Ok(parsed_sig) = parsed_sig_opt {
                    sig = parsed_sig;
                } else if let Err(msg) = parsed_sig_opt {
                    self.env.error(&st_env.get_loc(), &format!("{}", msg));
                }
            }
            let st_id = &st_env.get_qualified_id().instantiate(vec![]);
            if event_signature_map_ref.get(st_id).is_none() {
                event_signature_map_ref.insert(st_id.clone(), sig.clone());
            }
        }
    }

    // --------------------------------------------------------------------------------------------
    // Checks

    /// Check whether given Move struct has no generics; report error otherwise.
    pub fn check_no_generics_struct(&self, st: &StructEnv<'_>) -> bool {
        if !st.get_type_parameters().is_empty() {
            self.env
                .error(&st.get_loc(), "#[event] structs cannot be generic");
            return false;
        }
        true
    }
    /// Check whether given Move function has no generics; report error otherwise.
    pub fn check_no_generics(&self, fun: &FunctionEnv<'_>) {
        if fun.get_type_parameter_count() > 0 {
            self.env.error(
                &fun.get_loc(),
                "#[callable] or #[create] functions cannot be generic",
            )
        }
    }

    // --------------------------------------------------------------------------------------------
    // Name Generation

    /// Make the name of a contract.
    pub fn make_contract_name(&self, module: &ModuleEnv) -> String {
        let mod_name = module.get_name();
        let mod_sym = module.symbol_pool().string(mod_name.name());
        format!("A{}_{}", mod_name.addr().to_str_radix(16), mod_sym)
    }

    /// Make the name of function.
    pub fn make_function_name(&self, fun_id: &QualifiedInstId<FunId>) -> String {
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
    /// in `Generator::type_hash` which is used to index storage. Therefore the representation here
    /// cannot be changed without creating versioning problems for existing storage of contracts.
    pub fn mangle_type(&self, ty: &Type) -> String {
        use move_model::ty::{PrimitiveType::*, Type::*};
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
    pub(crate) fn mangle_struct(&self, struct_id: &QualifiedInstId<StructId>) -> String {
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
    pub fn mangle_types(&self, tys: &[Type]) -> String {
        if tys.is_empty() {
            "".to_owned()
        } else {
            format!("${}$", tys.iter().map(|ty| self.mangle_type(ty)).join("_"))
        }
    }

    /// Make name for a local.
    pub fn make_local_name(&self, target: &FunctionTarget, idx: TempIndex) -> String {
        target
            .get_local_name(idx)
            .display(target.symbol_pool())
            .to_string()
            .replace("#", "_")
    }

    /// Make name for a result.
    pub fn make_result_name(&self, target: &FunctionTarget, idx: usize) -> String {
        if target.get_return_count() == 1 {
            "$result".to_string()
        } else {
            format!("$result{}", idx)
        }
    }

    // --------------------------------------------------------------------------------------------
    // Code Generation

    /// Emits a Yul block.
    pub fn emit_block(&self, blk: impl FnOnce()) {
        emitln!(self.writer, "{");
        self.writer.indent();
        blk();
        self.writer.unindent();
        emitln!(self.writer, "}");
    }

    // --------------------------------------------------------------------------------------------
    // Types

    /// Returns whether the struct identified by module_id and struct_id is the native U256 struct.
    pub fn is_u256(&self, struct_id: QualifiedId<StructId>) -> bool {
        let struct_env = self.env.get_struct(struct_id);
        struct_env.get_full_name_with_address() == format!("{}::U256::U256", EVM_MODULE_ADDRESS)
    }

    pub fn is_u256_ty(&self, ty: &Type) -> bool {
        match ty {
            Type::Struct(m, s, _) => {
                let struct_id = m.qualified(*s);
                self.is_u256(struct_id)
            }
            _ => false,
        }
    }

    pub fn is_unit_ty(&self, ty: &Type) -> bool {
        match ty {
            Type::Struct(m, s, _) => {
                let struct_id = m.qualified(*s);
                let struct_env = self.env.get_struct(struct_id);
                struct_env.get_full_name_with_address()
                    == format!("{}::Evm::Unit", EVM_MODULE_ADDRESS)
            }
            _ => false,
        }
    }

    pub fn is_unit_opt_ty(&self, ty: Option<Type>) -> bool {
        if let Some(Type::Struct(m, s, _)) = ty {
            let struct_id = m.qualified(s);
            let struct_env = self.env.get_struct(struct_id);
            return struct_env.get_full_name_with_address()
                == format!("{}::Evm::Unit", EVM_MODULE_ADDRESS);
        }
        false
    }

    pub fn extract_external_result(&self, ty: &Type) -> (bool, Option<Type>) {
        if let Type::Struct(m, s, insts) = ty {
            let struct_id = m.qualified(*s);
            let struct_env = self.env.get_struct(struct_id);
            if struct_env.get_full_name_with_address()
                == format!("{}::ExternalResult::ExternalResult", EVM_MODULE_ADDRESS)
            {
                assert!(insts.len() == 1);
                return (true, Some(insts[0].clone()));
            }
        };
        (false, None)
    }

    /// Returns whether the struct identified by module_id and struct_id is the native Table struct.
    pub fn is_table(&self, struct_id: QualifiedId<StructId>) -> bool {
        let struct_env = self.env.get_struct(struct_id);
        struct_env.get_full_name_with_address() == format!("{}::Table::Table", EVM_MODULE_ADDRESS)
    }

    /// Returns whether the struct identified by module_id and struct_id is the native String struct.
    pub fn is_string(&self, struct_id: QualifiedId<StructId>) -> bool {
        let struct_env = self.env.get_struct(struct_id);
        format!(
            "{}",
            struct_env.get_name().display(struct_env.symbol_pool())
        ) == "String"
    }

    /// Get the field types of a struct as a vector.
    pub fn get_field_types(&self, id: QualifiedId<StructId>) -> Vec<Type> {
        self.env
            .get_struct(id)
            .get_fields()
            .map(|f| f.get_type())
            .collect()
    }

    /// Get the layout of the instantiated struct in linear memory. The result will be cached
    /// for future calls.
    pub fn get_struct_layout(&self, st: &QualifiedInstId<StructId>) -> StructLayout {
        let mut layouts_ref = self.struct_layout.borrow_mut();
        if layouts_ref.get(st).is_none() {
            // Compute the fields such that the larger appear first, and pointer fields
            // precede non-pointer fields.
            let s_or_v = |ty: &Type| ty.is_vector() || self.type_is_struct(ty);
            let struct_env = self.env.get_struct(st.to_qualified_id());
            let ordered_fields = struct_env
                .get_fields()
                .map(|field| {
                    let field_type = field.get_type().instantiate(&st.inst);
                    let field_size = self.type_size(&field_type);
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
            layouts_ref.insert(st.clone(), result);
        }
        layouts_ref.get(st).unwrap().clone()
    }

    /// Calculate the size, in bytes, for the memory layout of this type.
    pub fn type_size(&self, ty: &Type) -> usize {
        use PrimitiveType::*;
        use Type::*;
        match ty {
            Primitive(p) => match p {
                Bool | U8 => 1,
                U64 => 8,
                U128 => 16,
                // TODO: optimize for 20 bytes? Then we need primitives like LoadU160 etc.
                Address | Signer => 32,
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

    /// Returns the Load function for a given type.
    pub fn load_builtin_fun(&self, ty: &Type) -> YulFunction {
        match self.type_size(ty.skip_reference()) {
            1 => YulFunction::LoadU8,
            8 => YulFunction::LoadU64,
            16 => YulFunction::LoadU128,
            32 => YulFunction::LoadU256,
            _ => panic!("unexpected type size"),
        }
    }

    /// Returns the Store function for a given type.
    pub fn store_builtin_fun(&self, ty: &Type) -> YulFunction {
        match self.type_size(ty.skip_reference()) {
            1 => YulFunction::StoreU8,
            8 => YulFunction::StoreU64,
            16 => YulFunction::StoreU128,
            32 => YulFunction::StoreU256,
            _ => panic!("unexpected type size"),
        }
    }

    /// Returns the MemoryLoad function for a given type.
    pub fn memory_load_builtin_fun(&self, ty: &Type) -> YulFunction {
        match self.type_size(ty.skip_reference()) {
            1 => YulFunction::MemoryLoadU8,
            8 => YulFunction::MemoryLoadU64,
            16 => YulFunction::MemoryLoadU128,
            32 => YulFunction::MemoryLoadU256,
            _ => panic!("unexpected type size"),
        }
    }

    /// Returns the MemoryStore function for a given type.
    pub fn memory_store_builtin_fun(&self, ty: &Type) -> YulFunction {
        match self.type_size(ty.skip_reference()) {
            1 => YulFunction::MemoryStoreU8,
            8 => YulFunction::MemoryStoreU64,
            16 => YulFunction::MemoryStoreU128,
            32 => YulFunction::MemoryStoreU256,
            _ => panic!("unexpected type size"),
        }
    }

    /// Returns the StorageLoad function for a given type.
    #[allow(dead_code)]
    pub fn storage_load_builtin_fun(&self, ty: &Type) -> YulFunction {
        match self.type_size(ty.skip_reference()) {
            1 => YulFunction::StorageLoadU8,
            8 => YulFunction::StorageLoadU64,
            16 => YulFunction::StorageLoadU128,
            32 => YulFunction::StorageLoadU256,
            _ => panic!("unexpected type size"),
        }
    }

    /// Returns the StorageStore function for a given type.
    #[allow(dead_code)]
    pub fn storage_store_builtin_fun(&self, ty: &Type) -> YulFunction {
        match self.type_size(ty.skip_reference()) {
            1 => YulFunction::StorageStoreU8,
            8 => YulFunction::StorageStoreU64,
            16 => YulFunction::StorageStoreU128,
            32 => YulFunction::StorageStoreU256,
            _ => panic!("unexpected type size"),
        }
    }

    /// Returns true of the type allocates memory.
    pub fn type_allocates_memory(&self, ty: &Type) -> bool {
        use Type::*;
        match ty {
            Struct(m, s, _) => !self.is_u256(m.qualified(*s)) && !self.is_table(m.qualified(*s)),
            Vector(_) => true,
            _ => false,
        }
    }

    pub fn type_is_struct(&self, ty: &Type) -> bool {
        use Type::*;
        match ty {
            Struct(m, s, _) => !self.is_u256(m.qualified(*s)) && !self.is_table(m.qualified(*s)),
            _ => false,
        }
    }
}
