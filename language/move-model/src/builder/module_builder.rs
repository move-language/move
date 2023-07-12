// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    ast::{
        Attribute, AttributeValue, Condition, ConditionKind, Exp, ExpData, GlobalInvariant,
        ModuleName, Operation, PropertyBag, PropertyValue, QualifiedSymbol, Spec, SpecBlockInfo,
        SpecBlockTarget, SpecFunDecl, SpecVarDecl, Value,
    },
    builder::{
        exp_builder::ExpTranslator,
        model_builder::{ConstEntry, FunEntry, LocalVarEntry, ModelBuilder, SpecFunEntry},
    },
    exp_rewriter::{ExpRewriter, ExpRewriterFunctions, RewriteTarget},
    intrinsics::process_intrinsic_declaration,
    model::{
        FieldData, FieldId, FunId, FunctionData, Loc, ModuleId, MoveIrLoc, NamedConstantData,
        NamedConstantId, NodeId, Parameter, QualifiedId, QualifiedInstId, SchemaId, SpecFunId,
        SpecVarId, StructData, StructId, TypeParameter, TypeParameterKind,
    },
    options::ModelBuilderOptions,
    pragmas::{
        is_pragma_valid_for_block, is_property_valid_for_condition, CONDITION_ABSTRACT_PROP,
        CONDITION_CONCRETE_PROP, CONDITION_DEACTIVATED_PROP, CONDITION_EXPORT_PROP,
        CONDITION_INJECTED_PROP, OPAQUE_PRAGMA, VERIFY_PRAGMA,
    },
    symbol::{Symbol, SymbolPool},
    ty::{PrimitiveType, Type, BOOL_TYPE},
};
use codespan_reporting::diagnostic::Severity;
use itertools::Itertools;
use move_binary_format::{
    access::ModuleAccess,
    file_format::{Ability, AbilitySet, Constant, Visibility},
    CompiledModule,
};
use move_bytecode_source_map::source_map::SourceMap;
use move_compiler::{
    compiled_unit::{FunctionInfo, SpecInfo},
    expansion::ast as EA,
    parser::ast as PA,
    shared::{unique_map::UniqueMap, Identifier, Name},
};
use move_ir_types::{ast::ConstantName, location::Spanned};
use regex::Regex;
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
    default::Default,
    fmt,
};

#[derive(Debug)]
pub(crate) struct ModuleBuilder<'env, 'translator> {
    pub parent: &'translator mut ModelBuilder<'env>,
    /// Id of the currently build module.
    pub module_id: ModuleId,
    /// Name of the currently build module.
    pub module_name: ModuleName,
    /// Translated specification functions.
    pub spec_funs: Vec<SpecFunDecl>,
    /// During the definition analysis, the index into `spec_funs` we are currently
    /// handling
    pub spec_fun_index: usize,
    /// Translated specification variables.
    pub spec_vars: Vec<SpecVarDecl>,
    /// Translated function specifications.
    pub fun_specs: BTreeMap<Symbol, Spec>,
    /// Translated function definitions, if we are compiling Move code
    pub fun_defs: BTreeMap<Symbol, Exp>,
    /// Translated struct specifications.
    pub struct_specs: BTreeMap<Symbol, Spec>,
    /// Translated module spec
    pub module_spec: Spec,
    /// Spec block infos.
    pub spec_block_infos: Vec<SpecBlockInfo>,
    /// Let bindings for the current spec block, characterized by a boolean indicating whether
    /// post state is active and the node id of the original expression of the let.
    pub spec_block_lets: BTreeMap<Symbol, (bool, NodeId)>,
    /// Whether during model building, we actuallly compile the full Move source. If this is not
    /// set, we assume a compiled bytecode module as input. We still need the AST of the spec
    /// language part.
    pub compile_move: bool,
}

/// Represents information about a module already compiled into bytecode by the legacy
/// Move compiler.
#[derive(Debug)]
pub(crate) struct BytecodeModule {
    pub compiled_module: CompiledModule,
    pub source_map: SourceMap,
    pub function_infos: UniqueMap<PA::FunctionName, FunctionInfo>,
}

/// A value which we pass in to spec block analyzers, describing the resolved target of the spec
/// block.
#[derive(Debug)]
pub enum SpecBlockContext<'a> {
    Module,
    Struct(QualifiedSymbol),
    Function(QualifiedSymbol),
    FunctionCode(QualifiedSymbol, &'a SpecInfo),
    Schema(QualifiedSymbol),
}

impl<'a> fmt::Display for SpecBlockContext<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use SpecBlockContext::*;
        match self {
            Module => write!(f, "module context")?,
            Struct(..) => write!(f, "struct context")?,
            Function(..) => write!(f, "function context")?,
            FunctionCode(..) => write!(f, "code context")?,
            Schema(..) => write!(f, "schema context")?,
        }
        Ok(())
    }
}

/// # Entry Points

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    pub fn new(
        parent: &'translator mut ModelBuilder<'env>,
        module_id: ModuleId,
        module_name: ModuleName,
    ) -> Self {
        let compile_move = parent
            .env
            .get_extension::<ModelBuilderOptions>()
            .map(|o| o.compile_via_model)
            .unwrap_or_default();
        Self {
            parent,
            module_id,
            module_name,
            spec_funs: vec![],
            spec_fun_index: 0,
            spec_vars: vec![],
            fun_specs: BTreeMap::new(),
            fun_defs: BTreeMap::new(),
            struct_specs: BTreeMap::new(),
            module_spec: Spec::default(),
            spec_block_infos: Default::default(),
            spec_block_lets: BTreeMap::new(),
            compile_move,
        }
    }

    /// Translates the given module definition from the Move compiler's expansion phase,
    /// optionally combined with a compiled module (bytecode) and a source map, and enters it into
    /// this global environment. Any type check or others errors encountered will be collected
    /// in the environment for later processing. Dependencies of this module are guaranteed to
    /// have been analyzed and being already part of the environment.
    ///
    /// If no `BytecodeModule` is provided, the Move function definitions will be translated
    /// as well.
    ///
    /// Translation happens in three phases:
    ///
    /// 1. In the *declaration analysis*, we collect all information about structs, functions,
    ///    spec functions, spec vars, and schemas in a module. We do not yet analyze function
    ///    bodies, conditions, and invariants, which we can only analyze after we know all
    ///    global declarations (declaration of globals is order independent, and they can have
    ///    cyclic references).
    /// 2. In the *definition analysis*, we visit the definitions we have skipped in step (1),
    ///    specifically analyzing and type checking expressions and schema inclusions.
    /// 3. In the *population phase*, we populate the global environment with the information
    ///    from this module.
    pub fn translate(
        &mut self,
        loc: Loc,
        module_def: EA::ModuleDefinition,
        compiled_module: Option<BytecodeModule>,
    ) {
        self.decl_ana(&module_def, &compiled_module);
        self.def_ana(&module_def, &compiled_module);
        self.collect_spec_block_infos(&module_def);
        let attrs = self.translate_attributes(&module_def.attributes);
        self.populate_and_finalize_env(loc, attrs, compiled_module);
    }
}

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    /// Shortcut for accessing the symbol pool.
    pub fn symbol_pool(&self) -> &SymbolPool {
        self.parent.env.symbol_pool()
    }

    /// Qualifies the given symbol by the current module.
    pub fn qualified_by_module(&self, sym: Symbol) -> QualifiedSymbol {
        QualifiedSymbol {
            module_name: self.module_name.clone(),
            symbol: sym,
        }
    }

    /// Qualifies the given name by the current module.
    fn qualified_by_module_from_name(&self, name: &Name) -> QualifiedSymbol {
        let sym = self.symbol_pool().make(&name.value);
        self.qualified_by_module(sym)
    }

    /// Converts a ModuleAccess into its parts, an optional ModuleName and base name.
    pub fn module_access_to_parts(
        &self,
        access: &EA::ModuleAccess,
    ) -> (Option<ModuleName>, Symbol) {
        match &access.value {
            EA::ModuleAccess_::Name(n) => (None, self.symbol_pool().make(n.value.as_str())),
            EA::ModuleAccess_::ModuleAccess(m, n) => {
                let loc = self.parent.to_loc(&m.loc);
                let addr_bytes = self.parent.resolve_address(&loc, &m.value.address);
                let module_name = ModuleName::from_address_bytes_and_name(
                    addr_bytes,
                    self.symbol_pool().make(m.value.module.0.value.as_str()),
                );
                (Some(module_name), self.symbol_pool().make(n.value.as_str()))
            },
        }
    }

    /// Converts a ModuleAccess into a qualified symbol which can be used for lookup of
    /// types or functions.
    pub fn module_access_to_qualified(&self, access: &EA::ModuleAccess) -> QualifiedSymbol {
        let (module_name_opt, symbol) = self.module_access_to_parts(access);
        let module_name = module_name_opt.unwrap_or_else(|| self.module_name.clone());
        QualifiedSymbol {
            module_name,
            symbol,
        }
    }

    /// Creates a SpecBlockContext from the given SpecBlockTarget. The context is used during
    /// definition analysis when visiting a schema block member (condition, invariant, etc.).
    /// This returns None if the SpecBlockTarget cannnot be resolved; error reporting happens
    /// at caller side.
    fn get_spec_block_context<'pa>(
        &self,
        target: &'pa EA::SpecBlockTarget,
    ) -> Option<SpecBlockContext<'pa>> {
        match &target.value {
            EA::SpecBlockTarget_::Code => None,
            EA::SpecBlockTarget_::Member(name, _) => {
                let qsym = self.qualified_by_module_from_name(name);
                if self.parent.fun_table.contains_key(&qsym) {
                    Some(SpecBlockContext::Function(qsym))
                } else if self.parent.struct_table.contains_key(&qsym) {
                    Some(SpecBlockContext::Struct(qsym))
                } else {
                    None
                }
            },
            EA::SpecBlockTarget_::Schema(name, _) => {
                let qsym = self.qualified_by_module_from_name(name);
                if self.parent.spec_schema_table.contains_key(&qsym) {
                    Some(SpecBlockContext::Schema(qsym))
                } else {
                    None
                }
            },
            EA::SpecBlockTarget_::Module => Some(SpecBlockContext::Module),
        }
    }
}

/// # Abilitity Analysis

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    pub(crate) fn translate_abilities(&self, set: &EA::AbilitySet) -> AbilitySet {
        let mut abilities = AbilitySet::EMPTY;
        if set.has_ability_(PA::Ability_::Key) {
            abilities = abilities.add(Ability::Key)
        }
        if set.has_ability_(PA::Ability_::Store) {
            abilities = abilities.add(Ability::Store)
        }
        if set.has_ability_(PA::Ability_::Copy) {
            abilities = abilities.add(Ability::Copy)
        }
        if set.has_ability_(PA::Ability_::Drop) {
            abilities = abilities.add(Ability::Drop)
        }
        abilities
    }
}

/// # Attribute Analysis

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    pub fn translate_attributes(&mut self, attrs: &EA::Attributes) -> Vec<Attribute> {
        attrs
            .iter()
            .map(|(_, _, attr)| self.translate_attribute(attr))
            .collect()
    }

    pub fn translate_attribute(&mut self, attr: &EA::Attribute) -> Attribute {
        let node_id = self
            .parent
            .env
            .new_node(self.parent.to_loc(&attr.loc), Type::Tuple(vec![]));
        match &attr.value {
            EA::Attribute_::Name(n) => {
                let sym = self.symbol_pool().make(n.value.as_str());
                Attribute::Apply(node_id, sym, vec![])
            },
            EA::Attribute_::Parameterized(n, vs) => {
                let sym = self.symbol_pool().make(n.value.as_str());
                Attribute::Apply(node_id, sym, self.translate_attributes(vs))
            },
            EA::Attribute_::Assigned(n, v) => {
                let value_node_id = self
                    .parent
                    .env
                    .new_node(self.parent.to_loc(&v.loc), Type::Tuple(vec![]));
                let v = match &v.value {
                    EA::AttributeValue_::Value(val) => {
                        let val =
                            if let Some((val, _)) = ExpTranslator::new(self).translate_value(val) {
                                val
                            } else {
                                // Error reported
                                Value::Bool(false)
                            };
                        AttributeValue::Value(value_node_id, val)
                    },
                    EA::AttributeValue_::Module(mident) => {
                        let addr_bytes = self.parent.resolve_address(
                            &self.parent.to_loc(&mident.loc),
                            &mident.value.address,
                        );
                        let module_name = ModuleName::from_address_bytes_and_name(
                            addr_bytes,
                            self.symbol_pool()
                                .make(mident.value.module.0.value.as_str()),
                        );
                        // TODO support module attributes more than via empty string
                        AttributeValue::Name(
                            value_node_id,
                            Some(module_name),
                            self.symbol_pool().make(""),
                        )
                    },
                    EA::AttributeValue_::ModuleAccess(macc) => match macc.value {
                        EA::ModuleAccess_::Name(n) => AttributeValue::Name(
                            value_node_id,
                            None,
                            self.symbol_pool().make(n.value.as_str()),
                        ),
                        EA::ModuleAccess_::ModuleAccess(mident, n) => {
                            let addr_bytes = self.parent.resolve_address(
                                &self.parent.to_loc(&macc.loc),
                                &mident.value.address,
                            );
                            let module_name = ModuleName::from_address_bytes_and_name(
                                addr_bytes,
                                self.symbol_pool()
                                    .make(mident.value.module.0.value.as_str()),
                            );
                            AttributeValue::Name(
                                value_node_id,
                                Some(module_name),
                                self.symbol_pool().make(n.value.as_str()),
                            )
                        },
                    },
                };
                Attribute::Assign(node_id, self.symbol_pool().make(n.value.as_str()), v)
            },
        }
    }
}

/// # Declaration Analysis

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    fn decl_ana(
        &mut self,
        module_def: &EA::ModuleDefinition,
        compiled_module: &Option<BytecodeModule>,
    ) {
        for (name, struct_def) in module_def.structs.key_cloned_iter() {
            self.decl_ana_struct(&name, struct_def);
        }
        for (name, fun_def) in module_def.functions.key_cloned_iter() {
            self.decl_ana_fun(&name, fun_def);
        }
        for (name, const_def) in module_def.constants.key_cloned_iter() {
            self.decl_ana_const(&name, const_def, compiled_module);
        }
        for spec in &module_def.specs {
            self.decl_ana_spec_block(spec);
        }
    }

    fn decl_ana_const(
        &mut self,
        name: &PA::ConstantName,
        def: &EA::Constant,
        compiled_module: &Option<BytecodeModule>,
    ) {
        let qsym = self.qualified_by_module_from_name(&name.0);
        if self.parent.const_table.contains_key(&qsym) {
            self.parent.env.error(
                &self.parent.to_loc(&name.loc()),
                &format!("duplicate declaration of `{}`", &name.value()),
            )
        }
        let name = qsym.symbol;
        let const_name = ConstantName(self.symbol_pool().string(name).to_string().into());
        let mut et = ExpTranslator::new(self);
        let loc = et.to_loc(&def.loc);
        let ty = et.translate_type(&def.signature);
        let value = if let Some(BytecodeModule {
            compiled_module,
            source_map,
            ..
        }) = compiled_module
        {
            // Get the already assigned constant index.
            let const_idx = source_map
                .constant_map
                .get(&const_name)
                .expect("constant not in source map");
            let move_value = Constant::deserialize_constant(
                &compiled_module.constant_pool()[*const_idx as usize],
            )
            .unwrap();
            et.translate_from_move_value(&loc, &ty, &move_value)
        } else {
            // Type check the constant.
            let exp = et.translate_exp(&def.value, &ty);
            if let ExpData::Value(_, value) = exp {
                value
            } else {
                et.error(
                    &et.parent.parent.env.get_node_loc(exp.node_id()),
                    "expected a constant expression",
                );
                // Use dummy value. It does not matter its not matching the type, we never
                // continue after type checking.
                Value::Bool(false)
            }
        };
        et.parent
            .parent
            .define_const(qsym, ConstEntry { loc, ty, value });
    }

    fn decl_ana_struct(&mut self, name: &PA::StructName, def: &EA::StructDefinition) {
        let qsym = self.qualified_by_module_from_name(&name.0);
        if self.parent.struct_table.contains_key(&qsym) {
            self.parent.env.error(
                &self.parent.to_loc(&name.loc()),
                &format!("duplicate declaration of `{}`", &name.value()),
            )
        }
        let struct_id = StructId::new(qsym.symbol);
        let attrs = self.translate_attributes(&def.attributes);
        let abilities = self.translate_abilities(&def.abilities);
        let mut et = ExpTranslator::new(self);
        let type_params = et.analyze_and_add_type_params(
            def.type_parameters
                .iter()
                .map(|s| (&s.name, &s.constraints)),
        );
        et.parent.parent.define_struct(
            et.to_loc(&def.loc),
            attrs,
            qsym,
            et.parent.module_id,
            struct_id,
            abilities,
            type_params,
            None, // will be filled in during definition analysis
        );
    }

    fn decl_ana_fun(&mut self, name: &PA::FunctionName, def: &EA::Function) {
        let qsym = self.qualified_by_module_from_name(&name.0);
        if self.parent.fun_table.contains_key(&qsym) {
            self.parent.env.error(
                &self.parent.to_loc(&name.loc()),
                &format!("duplicate declaration of `{}`", &name.value()),
            )
        }
        let fun_id = FunId::new(qsym.symbol);
        let attrs = self.translate_attributes(&def.attributes);
        let mut et = ExpTranslator::new(self);
        et.enter_scope();
        let type_params = et
            .analyze_and_add_type_params(def.signature.type_parameters.iter().map(|(n, a)| (n, a)));
        et.enter_scope();
        let params = et.analyze_and_add_params(&def.signature.parameters, true);
        let result_type = et.translate_type(&def.signature.return_type);
        let is_entry = def.entry.is_some();
        let is_inline = def.inline;
        let visibility = match def.visibility {
            EA::Visibility::Public(_) => Visibility::Public,
            EA::Visibility::Friend(_) => Visibility::Friend,
            EA::Visibility::Internal => Visibility::Private,
        };
        let is_native = matches!(def.body.value, EA::FunctionBody_::Native);
        let loc = et.to_loc(&def.loc);
        et.parent.parent.define_fun(
            loc.clone(),
            attrs,
            qsym.clone(),
            et.parent.module_id,
            fun_id,
            visibility,
            is_native,
            is_entry,
            is_inline,
            type_params.clone(),
            params.clone(),
            result_type.clone(),
            def.specs.clone(),
        );

        // Add function as a spec fun entry as well.
        let spec_fun_id = SpecFunId::new(self.spec_funs.len());
        self.parent.define_spec_fun(qsym, SpecFunEntry {
            loc: loc.clone(),
            oper: Operation::Function(self.module_id, spec_fun_id, None),
            type_params: type_params.clone(),
            params: params.clone(),
            result_type: result_type.clone(),
        });

        // Add $ to the name so the spec version does not name clash with the Move version.
        let spec_fun_name = self.symbol_pool().make(&format!("${}", name.0.value));
        let mut fun_decl = SpecFunDecl {
            loc,
            name: spec_fun_name,
            type_params,
            params,
            context_params: None,
            result_type,
            used_memory: BTreeSet::new(),
            uninterpreted: false,
            is_move_fun: true,
            is_native: false,
            body: None,
            callees: Default::default(),
            is_recursive: Default::default(),
        };
        if let EA::FunctionBody_::Native = def.body.value {
            fun_decl.is_native = true;
        }
        self.spec_funs.push(fun_decl);
    }

    fn decl_ana_spec_block(&mut self, block: &EA::SpecBlock) {
        for member in &block.value.members {
            self.decl_ana_spec_block_member(member)
        }
        // If this is a schema spec block, process its declaration.
        if let EA::SpecBlockTarget_::Schema(name, type_params) = &block.value.target.value {
            self.decl_ana_schema(block, name, type_params.iter().map(|(n, a)| (n, a)));
        }
    }

    /// Process any spec block members which introduce global declarations.
    fn decl_ana_spec_block_member(&mut self, member: &EA::SpecBlockMember) {
        use EA::SpecBlockMember_::*;
        let loc = self.parent.env.to_loc(&member.loc);
        match &member.value {
            Function {
                uninterpreted,
                name,
                signature,
                ..
            } => self.decl_ana_spec_fun(&loc, *uninterpreted, name, signature),
            Variable {
                is_global: true,
                name,
                type_,
                type_parameters,
                init: _,
            } => self.decl_ana_global_var(
                &loc,
                name,
                type_parameters.iter().map(|(n, a)| (n, a)),
                type_,
            ),
            _ => {},
        }
    }

    fn decl_ana_spec_fun(
        &mut self,
        loc: &Loc,
        uninterpreted: bool,
        name: &PA::FunctionName,
        signature: &EA::FunctionSignature,
    ) {
        let name = self.symbol_pool().make(&name.0.value);
        let (type_params, params, result_type) = self.decl_ana_signature(signature, false);

        // Add the function to the symbol table.
        let fun_id = SpecFunId::new(self.spec_funs.len());
        self.parent
            .define_spec_fun(self.qualified_by_module(name), SpecFunEntry {
                loc: loc.clone(),
                oper: Operation::Function(self.module_id, fun_id, None),
                type_params: type_params.clone(),
                params: params.clone(),
                result_type: result_type.clone(),
            });

        // Add a prototype of the SpecFunDecl to the module build. This
        // will for now have an empty body which we fill in during a 2nd pass.
        let fun_decl = SpecFunDecl {
            loc: loc.clone(),
            name,
            type_params,
            params,
            context_params: None,
            result_type,
            used_memory: BTreeSet::new(),
            uninterpreted,
            is_move_fun: false,
            is_native: false,
            body: None,
            callees: Default::default(),
            is_recursive: Default::default(),
        };
        self.spec_funs.push(fun_decl);
    }

    fn decl_ana_signature(
        &mut self,
        signature: &EA::FunctionSignature,
        for_move_fun: bool,
    ) -> (Vec<TypeParameter>, Vec<Parameter>, Type) {
        let et = &mut ExpTranslator::new(self);
        let type_params =
            et.analyze_and_add_type_params(signature.type_parameters.iter().map(|(n, a)| (n, a)));
        et.enter_scope();
        let params = et.analyze_and_add_params(&signature.parameters, for_move_fun);
        let result_type = et.translate_type(&signature.return_type);
        et.finalize_types();
        (type_params, params, result_type)
    }

    fn decl_ana_global_var<'a, I>(
        &mut self,
        loc: &Loc,
        name: &Name,
        type_params: I,
        type_: &EA::Type,
    ) where
        I: IntoIterator<Item = (&'a Name, &'a EA::AbilitySet)>,
    {
        let name = self.symbol_pool().make(name.value.as_str());
        let (type_params, type_) = {
            let et = &mut ExpTranslator::new(self);
            let type_params = et.analyze_and_add_type_params(type_params);
            let type_ = et.translate_type(type_);
            (type_params, type_)
        };
        if type_.is_reference() {
            self.parent.error(
                loc,
                &format!(
                    "`{}` cannot have reference type",
                    name.display(self.symbol_pool())
                ),
            )
        }
        // Add the variable to the symbol table.
        let var_id = SpecVarId::new(self.spec_vars.len());
        self.parent.define_spec_var(
            loc,
            self.qualified_by_module(name),
            self.module_id,
            var_id,
            type_params.clone(),
            type_.clone(),
        );
        // Add the variable to the module builder. For now, the init expression stays unset.
        let var_decl = SpecVarDecl {
            loc: loc.clone(),
            name,
            type_params,
            type_,
            init: None,
        };
        self.spec_vars.push(var_decl);
    }

    fn decl_ana_schema<'a, I>(&mut self, block: &EA::SpecBlock, name: &Name, type_params: I)
    where
        I: IntoIterator<Item = (&'a Name, &'a EA::AbilitySet)>,
    {
        let qsym = self.qualified_by_module_from_name(name);
        let mut et = ExpTranslator::new(self);
        et.enter_scope();
        let type_params = et.analyze_and_add_type_params(type_params);
        // Extract local variables.
        let mut vars = vec![];
        for member in &block.value.members {
            if let EA::SpecBlockMember_::Variable {
                is_global: false,
                name,
                type_,
                type_parameters,
                init: _,
            } = &member.value
            {
                if !type_parameters.is_empty() {
                    et.error(
                        &et.to_loc(&member.loc),
                        "schema variable cannot have type parameters",
                    );
                }
                let name = et.symbol_pool().make(&name.value);
                let type_ = et.translate_type(type_);
                vars.push(Parameter(name, type_));
            }
        }
        // Add schema declaration prototype to the symbol table.
        let loc = et.to_loc(&block.loc);
        self.parent
            .define_spec_schema(&loc, qsym, self.module_id, type_params, vars);
    }
}

/// # Definition Analysis

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    fn def_ana(
        &mut self,
        module_def: &EA::ModuleDefinition,
        compiled_module: &Option<BytecodeModule>,
    ) {
        // Analyze all structs.
        for (name, def) in module_def.structs.key_cloned_iter() {
            self.def_ana_struct(&name, def);
        }

        // Analyze all functions.
        for (idx, (name, fun_def)) in module_def.functions.key_cloned_iter().enumerate() {
            self.def_ana_fun(&name, &fun_def.body, idx);
        }

        // Propagate the impurity of functions: a Move function which calls an
        // impure Move function is also considered impure.
        let mut visited = BTreeMap::new();
        for (idx, (name, _)) in module_def.functions.key_cloned_iter().enumerate() {
            let is_pure = self.propagate_function_impurity(&mut visited, SpecFunId::new(idx));
            let full_name = self.qualified_by_module_from_name(&name.0);
            if is_pure {
                // Modify the types of parameters, return values and expressions
                // of pure Move functions so they no longer have references.
                self.deref_move_fun_types(full_name.clone(), idx);
            }
            self.parent
                .fun_table
                .entry(full_name)
                .and_modify(|e| e.is_pure = is_pure);
        }

        // Analyze all schemas. This must be done before other things because schemas need to be
        // ready for inclusion. We also must do this recursively, so use a visited set to detect
        // cycles.
        {
            let schema_defs: BTreeMap<QualifiedSymbol, &EA::SpecBlock> = module_def
                .specs
                .iter()
                .filter_map(|block| {
                    if let EA::SpecBlockTarget_::Schema(name, ..) = &block.value.target.value {
                        let qsym = self.qualified_by_module_from_name(name);
                        Some((qsym, block))
                    } else {
                        None
                    }
                })
                .collect();
            let mut visited = BTreeSet::new();
            let mut visiting = vec![];
            for (name, block) in schema_defs.iter() {
                self.def_ana_schema(
                    &schema_defs,
                    &mut visited,
                    &mut visiting,
                    name.clone(),
                    block,
                );
            }
        }

        // TODO: we should re-visit this decision once we have high-order function ready on
        // the compiled bytecode (i.e., file format) level. Before that, the rule is:
        // - an inline function can have in-body spec blocks
        // - an inline function cannot have function spec (i.e., pre/post-conditions)
        //
        // On the verification side:
        // - we do not verify the correctness of in-body spec blocks in the inline function
        // - instead, we inline these in-body spec blocks into the caller and verify these
        //   specs in the context of caller.

        // Analyze all module level spec blocks (except schemas)
        for spec in &module_def.specs {
            if matches!(spec.value.target.value, EA::SpecBlockTarget_::Schema(..)) {
                continue;
            }
            match self.get_spec_block_context(&spec.value.target) {
                Some(context) => {
                    match &context {
                        SpecBlockContext::Function(qsym) => {
                            let fun_decl = self
                                .parent
                                .fun_table
                                .get(qsym)
                                .expect("function defined")
                                .clone();
                            let loc = self.parent.to_loc(&spec.value.target.loc);

                            // Validate that the provided signature matches the declaration
                            // This is needed to separate spec and code in different compilation unit
                            if let EA::SpecBlockTarget_::Member(_, Some(signature)) =
                                &spec.value.target.value
                            {
                                self.validate_target_signature(&fun_decl, &loc, signature);
                            }

                            // TODO: to be revisited once we have high-order function
                            if fun_decl.is_inline {
                                self.parent.error(
                                    &loc,
                                    "functional spec blocks for inline functions are not supported yet",
                                );
                            }
                        },
                        SpecBlockContext::Struct(..) | SpecBlockContext::Module => (),
                        SpecBlockContext::Schema(..) => {
                            unreachable!("schema spec blocks should be filtered early");
                        },
                        SpecBlockContext::FunctionCode(..) => {
                            unreachable!("unexpected inline spec block appearing at module level");
                        },
                    }

                    // the actual analysis
                    self.def_ana_spec_block(&context, spec)
                },
                None => {
                    let loc = self.parent.to_loc(&spec.value.target.loc);
                    self.parent.error(&loc, "unresolved spec target");
                },
            }
        }

        // If we compile from bytecode, analyze in-function spec blocks.
        if let Some(compiled_module) = compiled_module {
            self.def_ana_code_specs(module_def, compiled_module);
        }

        // Perform post analyzes of state usage in spec functions.
        self.compute_state_usage();

        // Perform post reduction of module invariants.
        self.process_module_invariants();

        // Apply tweaks after all specs are analyzed
        self.apply_tweaks(module_def);
    }

    /// Analyze specifications embedded in code, for the case we do not compile the code ourselves,
    /// but have it provided from bytecode.
    fn def_ana_code_specs(
        &mut self,
        module_def: &EA::ModuleDefinition,
        compiled_module: &BytecodeModule,
    ) {
        for (name, fun_def) in module_def.functions.key_cloned_iter() {
            // TODO: to be revisited once we have full support for high order function
            if fun_def.inline {
                continue;
            }

            let fun_name_loc = self.parent.to_loc(&name.loc());
            let fun_spec_info = &compiled_module.function_infos.get(&name).unwrap().spec_info;

            for spec_info in fun_spec_info.values() {
                // locate the spec block
                let origin = &spec_info.origin;
                let spec_block_opt = match origin.module {
                    None => {
                        // inline spec in a script function
                        fun_def.specs.get(&origin.id)
                    },
                    Some(module_ident) => {
                        // inline spec in a normal function
                        let module_addr = self
                            .parent
                            .resolve_address(&fun_name_loc, &module_ident.address);
                        let module_name = ModuleName::from_address_bytes_and_name(
                            module_addr,
                            self.symbol_pool()
                                .make(module_ident.module.0.value.as_str()),
                        );
                        let origin_symbol = QualifiedSymbol {
                            module_name,
                            symbol: self.symbol_pool().make(origin.function.as_str()),
                        };
                        self.parent
                            .fun_table
                            .get(&origin_symbol)
                            .and_then(|entry| entry.inline_specs.get(&origin.id))
                    },
                };
                let spec_block = match spec_block_opt {
                    None => {
                        self.parent.error(&fun_name_loc, "unresolved spec anchor");
                        continue;
                    },
                    Some(block) => block.clone(),
                };

                // all conditions are analyzed under this context
                let qsym = self.qualified_by_module_from_name(&name.0);
                let context = SpecBlockContext::FunctionCode(qsym, spec_info);
                for member in &spec_block.value.members {
                    let loc = &self.parent.env.to_loc(&member.loc);
                    match &member.value {
                        EA::SpecBlockMember_::Condition {
                            kind,
                            properties,
                            exp,
                            additional_exps,
                        } => {
                            if let Some(kind) = self.convert_condition_kind(kind, &context) {
                                let properties =
                                    self.translate_properties(properties, &|_, _, prop| {
                                        if !is_property_valid_for_condition(&kind, prop) {
                                            Some(loc.clone())
                                        } else {
                                            None
                                        }
                                    });
                                self.def_ana_condition(
                                    loc,
                                    &context,
                                    kind,
                                    properties,
                                    exp,
                                    additional_exps,
                                );
                            }
                        },
                        EA::SpecBlockMember_::Update { lhs, rhs } => {
                            self.def_ana_global_var_update(loc, &context, lhs, rhs)
                        },
                        _ => {
                            self.parent.error(loc, "item not allowed");
                        },
                    }
                }
            }
        }
    }

    /// Validates whether a function signature provided with a spec block target matches the
    /// function declaration. Currently we require literal matching. We may want to allow
    /// matching modulo renaming to make specs more independent from the code, but this
    /// requires some changes on the APIs has parameter names in specs are currently hardwired to be
    /// discovered via function declarations.
    fn validate_target_signature(
        &mut self,
        fun_decl: &FunEntry,
        loc: &Loc,
        signature: &EA::FunctionSignature,
    ) {
        let (type_params, params, result_type) = self.decl_ana_signature(signature, true);
        let generic_msg = "provided function signature must match function declaration";
        if fun_decl.type_params != type_params {
            self.parent
                .error(loc, &format!("{}: type parameter mismatch", generic_msg));
        }
        if fun_decl.params != params {
            self.parent
                .error(loc, &format!("{}: parameter mismatch", generic_msg));
        }
        if fun_decl.result_type != result_type {
            self.parent
                .error(loc, &format!("{}: return type mismatch", generic_msg));
        }
    }
}

/// ## Struct Definition Analysis

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    fn def_ana_struct(&mut self, name: &PA::StructName, def: &EA::StructDefinition) {
        let qsym = self.qualified_by_module_from_name(&name.0);
        let type_params = self
            .parent
            .struct_table
            .get(&qsym)
            .expect("struct invalid")
            .type_params
            .clone();
        let mut et = ExpTranslator::new(self);
        let loc = et.to_loc(&name.0.loc);
        et.define_type_params(&loc, &type_params);
        let fields = match &def.fields {
            EA::StructFields::Defined(fields) => {
                let mut field_map = BTreeMap::new();
                for (_name_loc, field_name_, (idx, ty)) in fields {
                    let field_sym = et.symbol_pool().make(field_name_);
                    let field_ty = et.translate_type(ty);
                    field_map.insert(field_sym, (*idx, field_ty));
                }
                if field_map.is_empty() {
                    // The legacy Move compiler inserts a `dummy_field: bool` here, we need to
                    // simulate this behavior for now, as that is what we find in the bytecode
                    // generated by the compiler.
                    let field_sym = et.parent.dummy_field_name();
                    let field_ty = Type::new_prim(PrimitiveType::Bool);
                    field_map.insert(field_sym, (0, field_ty));
                }
                Some(field_map)
            },
            EA::StructFields::Native(_) => None,
        };
        self.parent
            .struct_table
            .get_mut(&qsym)
            .expect("struct invalid")
            .fields = fields;
    }

    /// The name of a dummy field the legacy Move compilers adds to zero-arity structs.
    pub(crate) fn dummy_field_name(&self) -> Symbol {
        self.symbol_pool().make("dummy_field")
    }
}

/// ## Move Function Definition Analysis

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    /// Definition analysis for Move functions.
    /// If the function is pure, we translate its body.
    /// If we are operating as a Move compiler, we also translate its body.
    fn def_ana_fun(&mut self, name: &PA::FunctionName, body: &EA::FunctionBody, fun_idx: usize) {
        if let EA::FunctionBody_::Defined(seq) = &body.value {
            let full_name = self.qualified_by_module_from_name(&name.0);
            let entry = self
                .parent
                .fun_table
                .get(&full_name)
                .expect("function defined");
            let type_params = entry.type_params.clone();
            let params = entry.params.clone();
            let result_type = entry.result_type.clone();

            let body_translator = |et: &mut ExpTranslator, as_spec_fun: bool| {
                if as_spec_fun {
                    et.translate_fun_as_spec_fun()
                } else {
                    et.translate_move_fun()
                }
                let loc = et.to_loc(&body.loc);
                for (pos, TypeParameter(name, _)) in type_params.iter().enumerate() {
                    et.define_type_param(&loc, *name, Type::new_param(pos));
                }
                et.enter_scope();
                for (idx, Parameter(n, ty)) in params.iter().enumerate() {
                    et.define_local(&loc, *n, ty.clone(), None, Some(idx));
                }
                let result = et.translate_seq(&loc, seq, &result_type);
                et.finalize_types();
                result
            };

            // Attempt to translate as specification function
            let mut et = ExpTranslator::new(self);
            let translated = body_translator(&mut et, true);
            if !*et.errors_generated.borrow() {
                // Rewrite all type annotations in expressions to skip references.
                for node_id in translated.node_ids() {
                    let ty = et.get_node_type(node_id);
                    et.update_node_type(node_id, ty.skip_reference().clone());
                }
                et.called_spec_funs.iter().for_each(|(mid, fid)| {
                    self.parent.add_edge_to_move_fun_call_graph(
                        self.module_id.qualified(SpecFunId::new(fun_idx)),
                        mid.qualified(*fid),
                    );
                });
                self.spec_funs[self.spec_fun_index].body = Some(translated.into_exp());
            }

            if self.compile_move {
                // Also translate as regular Move function
                let mut et = ExpTranslator::new(self);
                let translated = body_translator(&mut et, false);
                assert!(self
                    .fun_defs
                    .insert(full_name.symbol, translated.into_exp())
                    .is_none());
            }
        }
        self.spec_fun_index += 1; // TODO: why is this at the end? Document or move close to use
    }

    /// Propagate the impurity of Move functions from callees to callers so
    /// that we can detect pure-looking Move functions which calls impure
    /// Move functions.
    fn propagate_function_impurity(
        &mut self,
        visited: &mut BTreeMap<SpecFunId, bool>,
        spec_fun_id: SpecFunId,
    ) -> bool {
        if let Some(is_pure) = visited.get(&spec_fun_id) {
            return *is_pure;
        }
        let spec_fun_idx = spec_fun_id.as_usize();
        let body = if self.spec_funs[spec_fun_idx].body.is_some() {
            std::mem::replace(&mut self.spec_funs[spec_fun_idx].body, None).unwrap()
        } else {
            // If the function is native and contains no mutable references
            // as parameters, consider it pure.
            // Otherwise the function is non-native, its body cannot be parsed
            // so we consider it impure.
            // TODO(emmazzz) right now all the native Move functions without
            // parameters of type mutable references are considered pure.
            // In the future we might want to only allow a certain subset of the
            // native Move functions, through something similar to an allow list or
            // a pragma.
            let no_mut_ref_param = self.spec_funs[spec_fun_idx]
                .params
                .iter()
                .map(|Parameter(_, ty)| !ty.is_mutable_reference())
                .all(|b| b); // `no_mut_ref_param` if none of the types are mut refs.
            return self.spec_funs[spec_fun_idx].is_native && no_mut_ref_param;
        };
        let mut is_pure = true;
        body.visit(&mut |e: &ExpData| {
            if let ExpData::Call(_, Operation::Function(mid, fid, _), _) = e {
                if mid.to_usize() < self.module_id.to_usize() {
                    // This is calling a function from another module we already have
                    // translated. In this case, the impurity has already been propagated
                    // in translate_call.
                } else {
                    // This is calling a function from the module we are currently translating.
                    // Need to recursively ensure we have propagated impurity because of
                    // arbitrary call graphs, including cyclic.
                    if !self.propagate_function_impurity(visited, *fid) {
                        is_pure = false;
                    }
                }
            }
        });
        if is_pure {
            // Restore the function body if the Move function is pure.
            self.spec_funs[spec_fun_idx].body = Some(body);
        }
        visited.insert(spec_fun_id, is_pure);
        is_pure
    }

    fn deref_move_fun_types(&mut self, full_name: QualifiedSymbol, spec_fun_idx: usize) {
        self.parent.spec_fun_table.entry(full_name).and_modify(|e| {
            assert!(e.len() == 1);
            e[0].params = e[0]
                .params
                .iter()
                .map(|Parameter(n, ty)| Parameter(*n, ty.skip_reference().clone()))
                .collect_vec();
            e[0].result_type = e[0].result_type.skip_reference().clone();
        });

        let spec_fun_decl = &mut self.spec_funs[spec_fun_idx];
        spec_fun_decl.params = spec_fun_decl
            .params
            .iter()
            .map(|Parameter(s, ty)| Parameter(*s, ty.skip_reference().clone()))
            .collect_vec();
        spec_fun_decl.result_type = spec_fun_decl.result_type.skip_reference().clone();
    }
}

/// ## Spec Block Definition Analysis

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    fn def_ana_spec_block(&mut self, context: &SpecBlockContext<'_>, block: &EA::SpecBlock) {
        let block_loc = self.parent.env.to_loc(&block.loc);
        self.update_spec(context, move |spec| spec.loc = Some(block_loc));

        assert!(self.spec_block_lets.is_empty());

        // Sort members so that lets are processed first. This is needed so that lets included
        // from schemas are properly renamed on name clash.
        let let_sorted_members = block.value.members.iter().sorted_by(|m1, m2| {
            let m1_is_let = matches!(m1.value, EA::SpecBlockMember_::Let { .. });
            let m2_is_let = matches!(m2.value, EA::SpecBlockMember_::Let { .. });
            match (m1_is_let, m2_is_let) {
                (true, true) | (false, false) => std::cmp::Ordering::Equal,
                (true, false) => std::cmp::Ordering::Less,
                (false, true) => std::cmp::Ordering::Greater,
            }
        });

        for member in let_sorted_members {
            self.def_ana_spec_block_member(context, member)
        }

        // clear the let bindings stored in the build.
        self.spec_block_lets.clear();
    }

    fn def_ana_spec_block_member(
        &mut self,
        context: &SpecBlockContext,
        member: &EA::SpecBlockMember,
    ) {
        use EA::SpecBlockMember_::*;
        let loc = &self.parent.env.to_loc(&member.loc);
        match &member.value {
            Condition {
                kind,
                properties,
                exp,
                additional_exps,
            } => {
                if let Some(kind) = self.convert_condition_kind(kind, context) {
                    let properties = self.translate_properties(properties, &|_, _, prop| {
                        if !is_property_valid_for_condition(&kind, prop) {
                            Some(loc.clone())
                        } else {
                            None
                        }
                    });
                    self.def_ana_condition(loc, context, kind, properties, exp, additional_exps)
                }
            },
            Function {
                uninterpreted,
                signature,
                body,
                ..
            } => self.def_ana_spec_fun(*uninterpreted, signature, body),
            Let {
                name,
                post_state,
                def,
            } => self.def_ana_let(context, loc, *post_state, name, def),
            Include { properties, exp } => {
                let properties = self.translate_properties(properties, &|_, _, _| None);
                self.def_ana_schema_inclusion_outside_schema(loc, context, None, properties, exp)
            },
            Apply {
                exp,
                patterns,
                exclusion_patterns,
            } => self.def_ana_schema_apply(loc, context, exp, patterns, exclusion_patterns),
            Pragma { properties } => self.def_ana_pragma(loc, context, properties),
            Variable {
                is_global: true,
                name,
                init,
                ..
            } => self.def_ana_global_var(loc, name, init.as_ref()),
            Variable {
                is_global: false, ..
            } => { /* nothing to do right now */ },
            Update { lhs, rhs } => self.def_ana_global_var_update(loc, context, lhs, rhs),
        }
    }
}

/// ## Let Definition Analysis

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    fn def_ana_let(
        &mut self,
        context: &SpecBlockContext<'_>,
        loc: &Loc,
        post_state: bool,
        name: &Name,
        def: &EA::Exp,
    ) {
        // Check the expression and extract results.
        let sym = self.symbol_pool().make(&name.value);
        let kind = if post_state {
            ConditionKind::LetPost(sym)
        } else {
            ConditionKind::LetPre(sym)
        };
        let mut et = self.exp_translator_for_context(loc, context, &kind);
        let (_, def) = et.translate_exp_free(def);
        et.finalize_types();

        // Check whether a let of this name is already defined, and add it to the
        // map which tracks lets in this block.
        if self
            .spec_block_lets
            .insert(sym, (post_state, def.node_id()))
            .is_some()
        {
            self.parent.error(
                &self.parent.to_loc(&name.loc),
                &format!("duplicate declaration of `{}`", name.value),
            );
        }

        // Add the let to the context spec.
        self.update_spec(context, |spec| {
            spec.conditions.push(Condition {
                loc: loc.clone(),
                kind,
                properties: Default::default(),
                exp: def.into_exp(),
                additional_exps: vec![],
            })
        })
    }
}

/// ## Pragma Definition Analysis

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    /// Definition analysis for a pragma.
    fn def_ana_pragma(
        &mut self,
        loc: &Loc,
        context: &SpecBlockContext,
        properties: &[EA::PragmaProperty],
    ) {
        let mut properties = self.translate_properties(properties, &|symbols, bag, prop| {
            if !is_pragma_valid_for_block(symbols, bag, context, prop) {
                Some(loc.clone())
            } else {
                None
            }
        });

        // extra processing on concrete pragma declarations
        process_intrinsic_declaration(self, loc, context, &mut properties);

        self.update_spec(context, move |spec| {
            spec.properties.extend(properties);
        });
    }

    /// Translate properties (of conditions or in pragmas), using the provided function
    /// to check their validness.
    fn translate_properties<F>(
        &mut self,
        properties: &[EA::PragmaProperty],
        check_prop: &F,
    ) -> PropertyBag
    where
        // Returns the location if not valid
        F: Fn(&SymbolPool, &PropertyBag, &str) -> Option<Loc>,
    {
        let mut props = PropertyBag::default();
        for prop in properties {
            self.process_one_property(&mut props, prop, check_prop);
        }
        props
    }

    fn process_one_property<F>(
        &mut self,
        bag: &mut PropertyBag,
        prop: &EA::PragmaProperty,
        check_prop: &F,
    ) where
        // Returns the location if not valid
        F: Fn(&SymbolPool, &PropertyBag, &str) -> Option<Loc>,
    {
        let prop_str = prop.value.name.value.as_str();
        if let Some(loc) = check_prop(self.symbol_pool(), bag, prop_str) {
            self.parent.error(
                &loc,
                &format!("property `{}` is not valid in this context", prop_str),
            );
            return;
        }

        let name = self.symbol_pool().make(&prop.value.name.value);
        let value = match &prop.value.value {
            None => PropertyValue::Value(Value::Bool(true)),
            Some(EA::PragmaValue::Literal(ev)) => {
                let mut et = ExpTranslator::new(self);
                match et.translate_value(ev) {
                    None => {
                        // Error reported
                        return;
                    },
                    Some((v, _)) => PropertyValue::Value(v),
                }
            },
            Some(EA::PragmaValue::Ident(ema)) => match self.module_access_to_parts(ema) {
                (None, sym) => PropertyValue::Symbol(sym),
                _ => PropertyValue::QualifiedSymbol(self.module_access_to_qualified(ema)),
            },
        };

        if bag.insert(name, value).is_some() {
            self.parent.error(
                &self.parent.to_loc(&prop.loc),
                &format!(
                    "property `{}` specified more than once in the same pragma declaration",
                    prop_str
                ),
            );
        }
    }

    fn add_bool_property(&self, mut properties: PropertyBag, name: &str, val: bool) -> PropertyBag {
        let sym = self.symbol_pool().make(name);
        properties.insert(sym, PropertyValue::Value(Value::Bool(val)));
        properties
    }
}

/// ## General Helpers for Definition Analysis

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    /// Updates the Spec of a given context via an update function.
    fn update_spec<F>(&mut self, context: &SpecBlockContext, update: F)
    where
        F: FnOnce(&mut Spec),
    {
        use SpecBlockContext::*;
        match context {
            Function(name) => update(
                self.fun_specs
                    .entry(name.symbol)
                    .or_insert_with(Spec::default),
            ),
            FunctionCode(name, spec_info) => update(
                self.fun_specs
                    .entry(name.symbol)
                    .or_insert_with(Spec::default)
                    .on_impl
                    .entry(spec_info.offset)
                    .or_insert_with(Spec::default),
            ),
            Schema(name) => update(
                &mut self
                    .parent
                    .spec_schema_table
                    .get_mut(name)
                    .expect("schema defined")
                    .spec,
            ),
            Struct(name) => update(
                self.struct_specs
                    .entry(name.symbol)
                    .or_insert_with(Spec::default),
            ),
            Module => update(&mut self.module_spec),
        }
    }

    /// Sets up an expression translator for the given spec block context. If kind
    /// is given, includes all the symbols which can be consumed by the condition,
    /// otherwise only defines type parameters.
    fn exp_translator_for_context<'module_translator>(
        &'module_translator mut self,
        loc: &Loc,
        context: &SpecBlockContext,
        kind: &ConditionKind,
    ) -> ExpTranslator<'env, 'translator, 'module_translator> {
        use SpecBlockContext::*;
        let allows_old = kind.allows_old();
        let mut et = match context {
            Function(name) => {
                let entry = &self
                    .parent
                    .fun_table
                    .get(name)
                    .expect("invalid spec block context")
                    .clone();
                let mut et = ExpTranslator::new_with_old(self, allows_old);
                for (pos, TypeParameter(name, _)) in entry.type_params.iter().enumerate() {
                    et.define_type_param(loc, *name, Type::new_param(pos));
                }

                et.enter_scope();
                for (idx, Parameter(n, ty)) in entry.params.iter().enumerate() {
                    et.define_local(loc, *n, ty.clone(), None, Some(idx));
                }
                // Define the placeholders for the result values of a function if this is an
                // Ensures condition.
                if matches!(kind, ConditionKind::Ensures | ConditionKind::LetPost(..)) {
                    et.enter_scope();
                    if let Type::Tuple(ts) = &entry.result_type {
                        for (i, ty) in ts.iter().enumerate() {
                            let name = et.symbol_pool().make(&format!("result_{}", i + 1));
                            let oper = Some(Operation::Result(i));
                            et.define_local(loc, name, ty.clone(), oper, None);
                        }
                    } else {
                        let name = et.symbol_pool().make("result");
                        let oper = Some(Operation::Result(0));
                        et.define_local(loc, name, entry.result_type.clone(), oper, None);
                    }
                }

                et
            },
            FunctionCode(name, spec_info) => {
                let entry = &self
                    .parent
                    .fun_table
                    .get(name)
                    .expect("invalid spec block context")
                    .clone();
                let mut et = ExpTranslator::new_with_old(self, allows_old);
                for (pos, TypeParameter(name, _)) in entry.type_params.iter().enumerate() {
                    et.define_type_param(loc, *name, Type::new_param(pos));
                }

                et.enter_scope();
                for (_n_loc, n_, info) in &spec_info.used_locals {
                    let sym = et.symbol_pool().make(n_);
                    let ty = et.translate_hlir_single_type(&info.type_);
                    if ty == Type::Error {
                        et.error(
                            loc,
                            "[internal] error in translating hlir type to prover type",
                        );
                    }
                    et.define_local(loc, sym, ty, None, Some(info.index as usize));
                }

                for (orig_name, (remapped_name, preset_args)) in &spec_info.used_lambda_funs {
                    let orig_sym = et.symbol_pool().make(orig_name);
                    let remapped_sym = et.symbol_pool().make(remapped_name);
                    let preset_arg_syms = preset_args
                        .iter()
                        .map(|v| {
                            let sym = et.symbol_pool().make(v.value().as_str());
                            if et.lookup_local(sym, false).is_none() {
                                et.error(
                                loc,
                                "[internal] error in finding used local variables in lambda calls",
                            );
                            }
                            sym
                        })
                        .collect();
                    et.fun_ptrs_table
                        .insert(orig_sym, (remapped_sym, preset_arg_syms));
                }

                et
            },
            Struct(name) => {
                let entry = &self
                    .parent
                    .struct_table
                    .get(name)
                    .expect("invalid spec block context")
                    .clone();

                let mut et = ExpTranslator::new_with_old(self, allows_old);
                et.define_type_params(loc, &entry.type_params);
                if let Some(fields) = &entry.fields {
                    et.enter_scope();
                    for (n, (_, ty)) in fields {
                        et.define_local(
                            loc,
                            *n,
                            ty.clone(),
                            Some(Operation::Select(
                                entry.module_id,
                                entry.struct_id,
                                FieldId::new(*n),
                            )),
                            None,
                        );
                    }
                }

                et
            },
            Module => {
                let mut et = ExpTranslator::new_with_old(self, allows_old);

                // define the type params
                match kind {
                    ConditionKind::GlobalInvariant(ty_params)
                    | ConditionKind::GlobalInvariantUpdate(ty_params) => {
                        et.define_type_params(loc, &TypeParameter::from_symbols(ty_params.iter()))
                    },
                    _ => (),
                }

                et
            },
            Schema(name) => {
                let entry = self
                    .parent
                    .spec_schema_table
                    .get(name)
                    .expect("schema defined");
                // Unfortunately need to clone elements from the entry because we need mut borrow
                // of self for expression build.
                let type_params = entry.type_params.clone();
                let all_vars = entry.all_vars.clone();
                let mut et = ExpTranslator::new_with_old(self, allows_old);
                et.define_type_params(loc, &type_params);
                et.enter_scope();
                for (n, entry) in all_vars {
                    et.define_local(loc, n, entry.type_, None, None);
                }

                et
            },
        };

        // Add lets to translator.
        if !et.parent.spec_block_lets.is_empty() {
            // Put them into a new scope, they can shadow outer names.
            et.enter_scope();
            for (name, (post_state, node_id)) in et.parent.spec_block_lets.clone() {
                // If allow_old is true, we are looking at a condition in a post state like ensures.
                // In this case all lets are available. If allow_old is false, only !post_state
                // lets are available.
                if allows_old || !post_state {
                    let ty = et.parent.parent.env.get_node_type(node_id);
                    let loc = et.parent.parent.env.get_node_loc(node_id);
                    et.define_let_local(&loc, name, ty);
                }
            }
        }

        et
    }
}

/// ## Condition Definition Analysis

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    /// Check whether the condition is allowed in the given context. Return true if so, otherwise
    /// report an error and return false.
    fn check_condition_is_valid(
        &mut self,
        context: &SpecBlockContext,
        loc: &Loc,
        cond: &Condition,
        detail: &str,
    ) -> bool {
        use SpecBlockContext::*;
        let notes = vec![];
        let mut ok = match context {
            Module => cond.kind.allowed_on_module(),
            Struct(_) => cond.kind.allowed_on_struct(),
            Function(name) => {
                let entry = self.parent.fun_table.get(name).expect("function defined");
                cond.kind.allowed_on_fun_decl(entry.visibility)
            },
            FunctionCode(_, _) => cond.kind.allowed_on_fun_impl(),
            Schema(_) => true,
        };
        if !ok {
            self.parent.error_with_notes(
                loc,
                &format!("`{}` not allowed in {} {}", cond.kind, context, detail),
                notes,
            );
        }
        if !cond.kind.allows_old() {
            // Check whether the inclusion is correct regards usage of post state.

            // First check for lets.
            for (name, _) in cond.exp.free_vars(self.parent.env) {
                if let Some((true, id)) = self.spec_block_lets.get(&name) {
                    let label_cond = (cond.loc.clone(), "not allowed to use post state".to_owned());
                    let label_let = (
                        self.parent.env.get_node_loc(*id),
                        "let defined here".to_owned(),
                    );
                    self.parent.env.diag_with_labels(
                        Severity::Error,
                        loc,
                        &format!(
                            "let bound `{}` propagated via schema inclusion is referring to post state",
                            name.display(self.parent.env.symbol_pool())
                        ),
                        vec![label_cond, label_let],
                    );
                    ok = false;
                }
            }

            // Next check for old(..) and Operation::Result
            let mut visitor = |e: &ExpData| {
                if let ExpData::Call(id, Operation::Old, ..)
                | ExpData::Call(id, Operation::Result(..), ..) = e
                {
                    let label_cond = (
                        cond.loc.clone(),
                        "not allowed to refer to post state".to_owned(),
                    );
                    let label_exp = (
                        self.parent.env.get_node_loc(*id),
                        "expression referring to post state".to_owned(),
                    );
                    self.parent.env.diag_with_labels(
                        Severity::Error,
                        loc,
                        "invalid reference to post state",
                        vec![label_cond, label_exp],
                    );
                    ok = false;
                }
            };
            cond.exp.visit(&mut visitor);
        } else if let FunctionCode(name, _) = context {
            // Restrict accesses to function arguments only for `old(..)` in in-spec block
            let entry = self.parent.fun_table.get(name).expect("function defined");
            let mut visitor = |e: &ExpData| {
                if let ExpData::Call(_, Operation::Old, args) = e {
                    let arg = &args[0];
                    match args[0].as_ref() {
                        ExpData::Temporary(_, idx) if *idx < entry.params.len() => (),
                        _ => {
                            let label_cond = (
                                cond.loc.clone(),
                                "only a function parameter is allowed in old(..) expressions \
                                in inline spec block"
                                    .to_owned(),
                            );
                            let label_exp = (
                                self.parent.env.get_node_loc(arg.node_id()),
                                "this expression is not a function parameter".to_owned(),
                            );
                            self.parent.env.diag_with_labels(
                                Severity::Error,
                                loc,
                                "invalid old(..) expression in inline spec block",
                                vec![label_cond, label_exp],
                            );
                            ok = false;
                        },
                    };
                }
            };
            cond.exp.visit(&mut visitor);
        }
        ok
    }

    /// Add the given conditions to the context, after checking whether they are valid in the
    /// context. Reports errors for invalid conditions. Also detects name clashes of let-bound
    /// names.
    fn add_conditions_to_context(
        &mut self,
        context: &SpecBlockContext,
        loc: &Loc,
        conditions: Vec<Condition>,
        context_properties: PropertyBag,
        error_msg: &str,
    ) {
        use ConditionKind::*;
        // Compute the let-bound names in the context block. (We misuse the update_spec function
        // to get hold of them.)
        let mut bound_lets = BTreeSet::new();
        self.update_spec(context, |spec| {
            bound_lets = spec
                .conditions
                .iter()
                .filter_map(|c| match &c.kind {
                    LetPost(name) | LetPre(name) => Some(*name),
                    _ => None,
                })
                .collect()
        });

        // We build a substitution for imported let names which clash with names in the context.
        let mut let_substitution = BTreeMap::new();
        for mut cond in conditions {
            if !let_substitution.is_empty() {
                // If there is a non-empty let_substitution, apply it to all expressions in the
                // condition.
                let Condition {
                    loc,
                    kind,
                    properties,
                    exp,
                    additional_exps,
                } = cond;
                let mut replacer = |id: NodeId, target: RewriteTarget| {
                    if let RewriteTarget::LocalVar(name) = target {
                        if let Some(unique_name) = let_substitution.get(&name) {
                            return Some(ExpData::LocalVar(id, *unique_name).into_exp());
                        }
                    }
                    None
                };
                let mut rewriter = ExpRewriter::new(self.parent.env, &mut replacer);
                let exp = rewriter.rewrite_exp(exp);
                let additional_exps = additional_exps
                    .into_iter()
                    .map(|e| rewriter.rewrite_exp(e))
                    .collect_vec();
                cond = Condition {
                    loc,
                    kind,
                    properties,
                    exp,
                    additional_exps,
                }
            }

            // If this is a let, check for name collision.
            match &cond.kind {
                LetPost(name) | LetPre(name) => {
                    let name = *name;
                    if bound_lets.contains(&name) {
                        // Find a new name by appending #0, #1, .. to this name.
                        let mut cnt = 1;
                        let new_name = loop {
                            let symbol_pool = self.parent.env.symbol_pool();
                            let new_name =
                                symbol_pool.make(&format!("{}#{}", name.display(symbol_pool), cnt));
                            if !bound_lets.contains(&new_name) {
                                break new_name;
                            }
                            cnt += 1;
                        };
                        let_substitution.insert(name, new_name);
                        if matches!(&cond.kind, LetPost(..)) {
                            cond.kind = LetPost(new_name)
                        } else {
                            cond.kind = LetPre(new_name)
                        }
                        bound_lets.insert(new_name);
                    } else {
                        bound_lets.insert(name);
                    }
                },
                _ => {},
            }

            // If this is a schema invariant, convert the kind based on its application context
            if cond.kind == ConditionKind::SchemaInvariant {
                let new_kind = match context {
                    SpecBlockContext::Module => ConditionKind::GlobalInvariant(vec![]),
                    SpecBlockContext::Struct(..) => ConditionKind::StructInvariant,
                    SpecBlockContext::Function(..) => ConditionKind::FunctionInvariant,
                    SpecBlockContext::FunctionCode(..) => ConditionKind::LoopInvariant,
                    SpecBlockContext::Schema(..) => {
                        // this is the initial pass that put the condition into the schema context
                        cond.kind.clone()
                    },
                };
                cond.kind = new_kind;
            }

            // Expand invariants on functions in requires/ensures
            let derived_conds = if matches!(context, SpecBlockContext::Function(..))
                && matches!(cond.kind, FunctionInvariant)
            {
                let mut ensures = cond.clone();
                ensures.kind = ConditionKind::Ensures;
                cond.kind = ConditionKind::Requires;
                vec![cond, ensures]
            } else {
                vec![cond]
            };

            for mut derived_cond in derived_conds {
                // Merge context properties.
                derived_cond.properties.extend(context_properties.clone());

                // Add condition to context.
                if self.check_condition_is_valid(context, loc, &derived_cond, error_msg)
                    && !self
                        .parent
                        .env
                        .is_property_true(&derived_cond.properties, CONDITION_DEACTIVATED_PROP)
                        .unwrap_or(false)
                {
                    self.update_spec(context, |spec| spec.conditions.push(derived_cond));
                }
            }
        }
    }

    /// Definition analysis for a condition.
    fn def_ana_condition(
        &mut self,
        loc: &Loc,
        context: &SpecBlockContext,
        kind: ConditionKind,
        properties: PropertyBag,
        exp: &EA::Exp,
        additional_exps: &[EA::Exp],
    ) {
        if matches!(kind, ConditionKind::Decreases | ConditionKind::SucceedsIf) {
            self.parent.error(loc, "condition kind is not supported");
            return;
        }
        let expected_type = self.expected_type_for_condition(&kind);
        let mut et = self.exp_translator_for_context(loc, context, &kind);
        let (translated, translated_additional) = match kind {
            ConditionKind::AbortsIf => (
                et.translate_exp(exp, &expected_type).into_exp(),
                additional_exps
                    .iter()
                    .map(|code| {
                        et.translate_exp(code, &Type::Primitive(PrimitiveType::Num))
                            .into_exp()
                    })
                    .collect_vec(),
            ),
            ConditionKind::AbortsWith => {
                // Parser has created a dummy exp, codes are all in additional_exps
                let mut exps = additional_exps
                    .iter()
                    .map(|code| {
                        et.translate_exp(code, &Type::Primitive(PrimitiveType::Num))
                            .into_exp()
                    })
                    .collect_vec();
                let first = exps.remove(0);
                (first, exps)
            },
            ConditionKind::Modifies => {
                // Parser has created a dummy exp, targets are all in additional_exps
                let mut exps = additional_exps
                    .iter()
                    .map(|target| et.translate_modify_target(target).into_exp())
                    .collect_vec();
                let first = exps.remove(0);
                (first, exps)
            },
            ConditionKind::Emits => {
                // TODO: `first` is the "message" part, and `second` is the "handle" part.
                //       `second` should have type std::event::EventHandle<T>, and `first`
                //       should have type T.
                let (_, first) = et.translate_exp_free(exp);
                let (_, second) = et.translate_exp_free(&additional_exps[0]);
                let mut exps = vec![second.into_exp()];
                if additional_exps.len() > 1 {
                    exps.push(et.translate_exp(&additional_exps[1], &BOOL_TYPE).into_exp());
                }
                (first.into_exp(), exps)
            },
            ConditionKind::Axiom(ref type_params) => {
                et.define_type_params(loc, &TypeParameter::from_symbols(type_params.iter()));
                (et.translate_exp(exp, &expected_type).into_exp(), vec![])
            },
            _ => {
                if !additional_exps.is_empty() {
                    et.error(
                          loc,
                          "additional expressions only allowed with `aborts_if`, `aborts_with`, `modifies`, or `emits`",
                      );
                }
                (et.translate_exp(exp, &expected_type).into_exp(), vec![])
            },
        };
        et.finalize_types();
        self.add_conditions_to_context(
            context,
            loc,
            vec![Condition {
                loc: loc.clone(),
                kind,
                properties,
                exp: translated,
                additional_exps: translated_additional,
            }],
            PropertyBag::default(),
            "",
        );
    }

    /// Compute the expected type for the expression in a condition.
    fn expected_type_for_condition(&mut self, _kind: &ConditionKind) -> Type {
        BOOL_TYPE.clone()
    }

    /// Convert a condition kind from AST into the ConditionKind known by the move model.
    fn convert_condition_kind(
        &mut self,
        kind: &EA::SpecConditionKind,
        context: &SpecBlockContext,
    ) -> Option<ConditionKind> {
        // Defines a type local with duplication check
        fn define_type_param(
            builder: &mut ModuleBuilder,
            ty_params_defined: &mut BTreeSet<Symbol>,
            name: &Name,
        ) -> Option<Symbol> {
            let symbol = builder.symbol_pool().make(&name.value);
            if !ty_params_defined.insert(symbol) {
                builder.parent.env.error(
                    &builder.parent.to_loc(&name.loc),
                    &format!("duplicate declaration of `{}`", &name.value),
                );
                None
            } else {
                Some(symbol)
            }
        }

        fn define_type_params(
            builder: &mut ModuleBuilder,
            type_params: &[(Name, EA::AbilitySet)],
        ) -> Option<Vec<Symbol>> {
            let mut ty_params_defined = BTreeSet::new();
            type_params
                .iter()
                .map(|(name, _)| define_type_param(builder, &mut ty_params_defined, name))
                .collect()
        }

        use ConditionKind::*;
        use EA::SpecConditionKind_ as PK;
        let converted = match &kind.value {
            PK::Assert => Assert,
            PK::Assume => Assume,
            PK::Decreases => Decreases,
            PK::Modifies => Modifies,
            PK::Emits => Emits,
            PK::Ensures => Ensures,
            PK::Requires => Requires,
            PK::AbortsIf => AbortsIf,
            PK::AbortsWith => AbortsWith,
            PK::SucceedsIf => SucceedsIf,
            PK::Invariant(ty_params) => {
                let tys = define_type_params(self, ty_params)?;
                match context {
                    SpecBlockContext::Module => GlobalInvariant(tys),
                    SpecBlockContext::Struct(..) => {
                        if !tys.is_empty() {
                            self.parent.env.error(
                                &self.parent.to_loc(&kind.loc),
                                "type parameters are not allowed in struct invariants",
                            )
                        }
                        StructInvariant
                    },
                    SpecBlockContext::Function(..) => {
                        if !tys.is_empty() {
                            self.parent.env.error(
                                &self.parent.to_loc(&kind.loc),
                                "type parameters are not allowed in function invariants",
                            )
                        }
                        FunctionInvariant
                    },
                    SpecBlockContext::FunctionCode(..) => {
                        if !tys.is_empty() {
                            self.parent.env.error(
                                &self.parent.to_loc(&kind.loc),
                                "type parameters are not allowed in loop invariants",
                            )
                        }
                        LoopInvariant
                    },
                    SpecBlockContext::Schema(..) => {
                        if !tys.is_empty() {
                            self.parent.env.error(
                                &self.parent.to_loc(&kind.loc),
                                "type parameters are not allowed in schema invariants",
                            )
                        }
                        SchemaInvariant
                    },
                }
            },
            PK::InvariantUpdate(ty_params) => {
                let tys = define_type_params(self, ty_params)?;
                if !matches!(context, SpecBlockContext::Module) {
                    self.parent.env.error(
                        &self.parent.to_loc(&kind.loc),
                        "update invariants are only allowed in module specs",
                    )
                }
                GlobalInvariantUpdate(tys)
            },
            PK::Axiom(ty_params) => Axiom(define_type_params(self, ty_params)?),
        };
        Some(converted)
    }
}

/// ## Spec Function Definition Analysis

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    /// Definition analysis for a specification helper function.
    fn def_ana_spec_fun(
        &mut self,
        uninterpreted: bool,
        _signature: &EA::FunctionSignature,
        body: &EA::FunctionBody,
    ) {
        match &body.value {
            EA::FunctionBody_::Defined(seq) => {
                let entry = &self.spec_funs[self.spec_fun_index];
                let type_params = entry.type_params.clone();
                let params = entry.params.clone();
                let result_type = entry.result_type.clone();
                let mut et = ExpTranslator::new(self);
                let loc = et.to_loc(&body.loc);
                et.define_type_params(&loc, &type_params);
                et.enter_scope();
                for Parameter(n, ty) in params {
                    et.define_local(&loc, n, ty, None, None);
                }
                let translated = et.translate_seq(&loc, seq, &result_type);
                et.finalize_types();
                self.spec_funs[self.spec_fun_index].body = Some(translated.into_exp());
            },
            EA::FunctionBody_::Native => {
                if !uninterpreted {
                    self.spec_funs[self.spec_fun_index].is_native = true
                }
            },
        }
        self.spec_fun_index += 1;
    }
}

/// ## Global Variable Definition Analysis

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    /// Definition analysis for a specification variable function.
    fn def_ana_global_var(&mut self, loc: &Loc, name: &Name, init: Option<&EA::Exp>) {
        if let Some(exp) = init {
            // Type check and translate the initialization expression.
            let sym = self.qualified_by_module_from_name(name);
            let entry = &self
                .parent
                .spec_var_table
                .get(&sym)
                .expect("spec var defined")
                .clone();
            let mut et = ExpTranslator::new(self);
            et.define_type_params(loc, &entry.type_params);
            let translated = et.translate_exp(exp, &entry.type_);
            et.finalize_types();
            // Store the translated init expression into the declaration.
            let decl = self
                .spec_vars
                .iter_mut()
                .find(|d| d.name == sym.symbol)
                .expect("spec var defined");
            decl.init = Some(translated.into_exp())
        }
    }

    fn def_ana_global_var_update(
        &mut self,
        loc: &Loc,
        context: &SpecBlockContext,
        lhs: &EA::Exp,
        rhs: &EA::Exp,
    ) {
        // Type check and translate lhs and rhs. They must have the same type.
        let mut et = self.exp_translator_for_context(loc, context, &ConditionKind::Requires);
        let (expected_ty, lhs) = et.translate_exp_free(lhs);
        let rhs = et.translate_exp(rhs, &expected_ty);
        et.finalize_types();
        if lhs.extract_ghost_mem_access(self.parent.env).is_some() {
            // Add as a condition to the context.
            self.add_conditions_to_context(
                context,
                loc,
                vec![Condition {
                    loc: loc.clone(),
                    kind: ConditionKind::Update,
                    properties: Default::default(),
                    exp: rhs.into_exp(),
                    additional_exps: vec![lhs.into_exp()],
                }],
                PropertyBag::default(),
                "",
            );
        } else {
            self.parent.error(
                &self.parent.env.get_node_loc(lhs.node_id()),
                "target of `update` restricted to specification variables",
            )
        }
    }
}

/// ## Schema Definition Analysis

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    /// Definition analysis for a schema. This proceeds in two steps: first we ensure recursively
    /// that all included schemas are analyzed, checking for cycles. Then we actually analyze this
    /// schema's content.
    fn def_ana_schema(
        &mut self,
        schema_defs: &BTreeMap<QualifiedSymbol, &EA::SpecBlock>,
        visited: &mut BTreeSet<QualifiedSymbol>,
        visiting: &mut Vec<QualifiedSymbol>,
        name: QualifiedSymbol,
        block: &EA::SpecBlock,
    ) {
        if !visited.insert(name.clone()) {
            // Already analyzed.
            return;
        }
        visiting.push(name.clone());

        // First recursively visit all schema includes and ensure they are analyzed.
        for included_name in
            self.iter_schema_includes(&block.value.members)
                .flat_map(|(_, _, exp)| {
                    let mut res = vec![];
                    extract_schema_access(exp, &mut res);
                    res
                })
        {
            let included_loc = self.parent.env.to_loc(&included_name.loc);
            let included_name = self.module_access_to_qualified(included_name);
            if included_name.module_name == self.module_name {
                // A schema in the module we are currently analyzing. We need to check
                // for cycles before recursively analyzing it.
                if visiting.contains(&included_name) {
                    self.parent.error(
                        &included_loc,
                        &format!(
                            "cyclic schema dependency: {} -> {}",
                            visiting
                                .iter()
                                .map(|name| format!("{}", name.display_simple(self.parent.env)))
                                .join(" -> "),
                            included_name.display_simple(self.parent.env)
                        ),
                    )
                } else if let Some(included_block) = schema_defs.get(&included_name) {
                    // Recursively analyze it, if its defined. If not, we report an undeclared
                    // error in 2nd phase.
                    self.def_ana_schema(
                        schema_defs,
                        visited,
                        visiting,
                        included_name,
                        included_block,
                    );
                }
            }
        }

        // Now actually analyze this schema.
        self.def_ana_schema_content(name, block);

        // Remove from visiting list
        visiting.pop();
    }

    /// Analysis of schema after it is ensured that all included schemas are fully analyzed.
    fn def_ana_schema_content(&mut self, name: QualifiedSymbol, block: &EA::SpecBlock) {
        let loc = self.parent.env.to_loc(&block.loc);
        let entry = self
            .parent
            .spec_schema_table
            .get(&name)
            .expect("schema defined");
        let type_params = entry.type_params.clone();
        let mut all_vars: BTreeMap<Symbol, LocalVarEntry> = entry
            .vars
            .iter()
            .map(|Parameter(n, ty)| {
                (*n, LocalVarEntry {
                    loc: loc.clone(),
                    type_: ty.clone(),
                    operation: None,
                    temp_index: None,
                })
            })
            .collect();
        let mut included_spec = Spec::default();

        // Store back all_vars computed so far (which does not include those coming from
        // included schemas). This is needed so we can analyze lets.
        {
            let entry = self
                .parent
                .spec_schema_table
                .get_mut(&name)
                .expect("schema defined");
            entry.all_vars = all_vars.clone();
        }

        // Process all lets. We need to do this before includes so we have them available
        // in schema arguments of includes. This unfortunately means we can't refer in
        // lets to variables included from schemas, but this seems to be a rare use case.
        assert!(self.spec_block_lets.is_empty());
        for member in &block.value.members {
            let member_loc = self.parent.to_loc(&member.loc);
            if let EA::SpecBlockMember_::Let {
                name: let_name,
                post_state,
                def,
            } = &member.value
            {
                let context = SpecBlockContext::Schema(name.clone());
                self.def_ana_let(&context, &member_loc, *post_state, let_name, def);
            }
        }

        // Process all schema includes. We need to do this before we type check expressions to have
        // all variables from includes in the environment.
        for (_, included_props, included_exp) in self.iter_schema_includes(&block.value.members) {
            let included_props = self.translate_properties(included_props, &|_, _, _| None);
            self.def_ana_schema_exp(
                &type_params,
                &mut all_vars,
                &mut included_spec,
                true,
                &included_props,
                included_exp,
            );
        }
        // Store the results back to the schema entry.
        {
            let entry = self
                .parent
                .spec_schema_table
                .get_mut(&name)
                .expect("schema defined");
            entry.all_vars = all_vars;
            entry.included_spec = included_spec;
        }

        // Now process all conditions and invariants.
        for member in &block.value.members {
            let member_loc = self.parent.to_loc(&member.loc);
            match &member.value {
                EA::SpecBlockMember_::Variable {
                    is_global: false, ..
                } => { /* handled during decl analysis */ },
                EA::SpecBlockMember_::Include { .. } => { /* handled above */ },
                EA::SpecBlockMember_::Let { .. } => { /* handled above */ },
                EA::SpecBlockMember_::Condition {
                    kind,
                    properties,
                    exp,
                    additional_exps,
                } => {
                    let context = SpecBlockContext::Schema(name.clone());
                    if let Some(kind) = self.convert_condition_kind(kind, &context) {
                        let properties = self.translate_properties(properties, &|_, _, prop| {
                            if !is_property_valid_for_condition(&kind, prop) {
                                Some(member_loc.clone())
                            } else {
                                None
                            }
                        });
                        self.def_ana_condition(
                            &member_loc,
                            &context,
                            kind,
                            properties,
                            exp,
                            additional_exps,
                        );
                    }
                },
                _ => {
                    self.parent.error(&member_loc, "item not allowed in schema");
                },
            };
        }
        self.spec_block_lets.clear();
    }

    /// Extracts all schema inclusions from a list of spec block members.
    fn iter_schema_includes<'a>(
        &self,
        members: &'a [EA::SpecBlockMember],
    ) -> impl Iterator<Item = (&'a MoveIrLoc, &'a Vec<EA::PragmaProperty>, &'a EA::Exp)> {
        members.iter().filter_map(|m| {
            if let EA::SpecBlockMember_::Include { properties, exp } = &m.value {
                Some((&m.loc, properties, exp))
            } else {
                None
            }
        })
    }

    /// Analyzes a schema expression. Depending on whether `allow_new_vars` is true, this will
    /// add new variables to `vars` and match types of existing ones. All conditions
    /// from the schema are rewritten for the inclusion context and added to the provided spec.
    ///
    /// We accept a very restricted set of Move expressions for schemas:
    ///
    /// - `P ==> SchemaExp`: all conditions in the schema will be prefixed with `P ==> ..`.
    ///   Conditions which are not based on boolean expressions (as VarUpdate et. al) will
    ///   be rejected.
    /// - `if (P) SchemaExp else SchemaExp`: this is treated similar as one include for
    ///   `P ==> SchemaExp` and one for `!P ==> SchemaExp`.
    /// - `SchemaExp1 && SchemaExp2`: this is treated as two includes for the both expressions.
    /// - `SchemaExp1 || SchemaExp2`: this could be treated as
    ///   `exists b: bool :: if (b) SchemaExp1 else SchemaExp2` (but as we do not have the
    ///   existential quantifier yet in the spec language, it is actually not supported..)
    ///
    /// The implementation works via a recursive function which accumulates a path condition
    /// leading to a Move "pack" expression which is interpreted as a schema reference.
    fn def_ana_schema_exp(
        &mut self,
        context_type_params: &[TypeParameter],
        vars: &mut BTreeMap<Symbol, LocalVarEntry>,
        spec: &mut Spec,
        allow_new_vars: bool,
        properties: &PropertyBag,
        exp: &EA::Exp,
    ) {
        self.def_ana_schema_exp_oper(
            context_type_params,
            vars,
            spec,
            allow_new_vars,
            None,
            properties,
            exp,
        )
    }

    /// Analyzes operations in schema expressions. This extends the path condition as needed
    /// and continues recursively.
    fn def_ana_schema_exp_oper(
        &mut self,
        context_type_params: &[TypeParameter],
        vars: &mut BTreeMap<Symbol, LocalVarEntry>,
        spec: &mut Spec,
        allow_new_vars: bool,
        path_cond: Option<Exp>,
        properties: &PropertyBag,
        exp: &EA::Exp,
    ) {
        let loc = self.parent.to_loc(&exp.loc);
        match &exp.value {
            EA::Exp_::BinopExp(
                lhs,
                Spanned {
                    value: PA::BinOp_::Implies,
                    ..
                },
                rhs,
            ) => {
                let mut et = self.exp_translator_for_schema(&loc, context_type_params, vars);
                let lhs_exp = et.translate_exp(lhs, &BOOL_TYPE).into_exp();
                et.finalize_types();
                let path_cond = Some(self.extend_path_condition(&loc, path_cond, lhs_exp));
                self.def_ana_schema_exp_oper(
                    context_type_params,
                    vars,
                    spec,
                    allow_new_vars,
                    path_cond,
                    properties,
                    rhs,
                );
            },
            EA::Exp_::BinopExp(
                lhs,
                Spanned {
                    value: PA::BinOp_::And,
                    ..
                },
                rhs,
            ) => {
                self.def_ana_schema_exp_oper(
                    context_type_params,
                    vars,
                    spec,
                    allow_new_vars,
                    path_cond.clone(),
                    properties,
                    lhs,
                );
                self.def_ana_schema_exp_oper(
                    context_type_params,
                    vars,
                    spec,
                    allow_new_vars,
                    path_cond,
                    properties,
                    rhs,
                );
            },
            EA::Exp_::IfElse(c, t, e) => {
                let mut et = self.exp_translator_for_schema(&loc, context_type_params, vars);
                let c_exp = et.translate_exp(c, &BOOL_TYPE).into_exp();
                et.finalize_types();
                let t_path_cond =
                    Some(self.extend_path_condition(&loc, path_cond.clone(), c_exp.clone()));
                self.def_ana_schema_exp_oper(
                    context_type_params,
                    vars,
                    spec,
                    allow_new_vars,
                    t_path_cond,
                    properties,
                    t,
                );
                let node_id = self.parent.env.new_node(loc.clone(), BOOL_TYPE.clone());
                let not_c_exp = ExpData::Call(node_id, Operation::Not, vec![c_exp]).into_exp();
                let e_path_cond = Some(self.extend_path_condition(&loc, path_cond, not_c_exp));
                self.def_ana_schema_exp_oper(
                    context_type_params,
                    vars,
                    spec,
                    allow_new_vars,
                    e_path_cond,
                    properties,
                    e,
                );
            },
            EA::Exp_::Name(maccess, type_args_opt) => self.def_ana_schema_exp_leaf(
                context_type_params,
                vars,
                spec,
                allow_new_vars,
                path_cond,
                properties,
                &loc,
                maccess,
                type_args_opt,
                None,
            ),
            EA::Exp_::Pack(maccess, type_args_opt, fields) => self.def_ana_schema_exp_leaf(
                context_type_params,
                vars,
                spec,
                allow_new_vars,
                path_cond,
                properties,
                &loc,
                maccess,
                type_args_opt,
                Some(fields),
            ),
            _ => self
                .parent
                .error(&loc, "expression construct not supported for schemas"),
        }
    }

    /// Analyzes a schema leaf expression.
    fn def_ana_schema_exp_leaf(
        &mut self,
        context_type_params: &[TypeParameter],
        vars: &mut BTreeMap<Symbol, LocalVarEntry>,
        spec: &mut Spec,
        allow_new_vars: bool,
        path_cond: Option<Exp>,
        schema_properties: &PropertyBag,
        loc: &Loc,
        maccess: &EA::ModuleAccess,
        type_args_opt: &Option<Vec<EA::Type>>,
        args_opt: Option<&EA::Fields<EA::Exp>>,
    ) {
        let schema_name = self.module_access_to_qualified(maccess);

        // Remove schema from unused table since it is used in an expression
        self.parent.unused_schema_set.remove(&schema_name);

        // We need to temporarily detach the schema entry from the parent table because of
        // borrowing problems, as we need to traverse it while at the same time mutate self.
        let schema_entry = if let Some(e) = self.parent.spec_schema_table.remove(&schema_name) {
            e
        } else {
            self.parent.error(
                loc,
                &format!(
                    "schema `{}` undeclared",
                    schema_name.display(self.parent.env)
                ),
            );
            return;
        };

        // Translate type arguments
        let mut et = self.exp_translator_for_schema(loc, context_type_params, vars);
        let type_arguments = &et.translate_types_opt(type_args_opt);
        if schema_entry.type_params.len() != type_arguments.len() {
            self.parent.error(
                loc,
                &format!(
                    "wrong number of type arguments (expected {}, got {})",
                    schema_entry.type_params.len(),
                    type_arguments.len()
                ),
            );
            // Don't forget to put schema back.
            self.parent
                .spec_schema_table
                .insert(schema_name, schema_entry);
            return;
        }

        // Translate schema arguments.
        let mut argument_map: BTreeMap<Symbol, Exp> = args_opt
            .map(|args| {
                args.iter()
                    .map(|(var_loc, schema_var_, (_, exp))| {
                        let pool = et.symbol_pool();
                        let schema_sym = pool.make(schema_var_);
                        let schema_type = if let Some(LocalVarEntry { type_, .. }) =
                            schema_entry.all_vars.get(&schema_sym)
                        {
                            type_.instantiate(type_arguments)
                        } else {
                            et.error(
                                &et.to_loc(&var_loc),
                                &format!("`{}` not declared in schema", schema_sym.display(pool)),
                            );
                            Type::Error
                        };
                        // Check the expression in the argument list.
                        // Note we currently only use the vars defined so far in this context. Variables
                        // which are introduced by schemas after the inclusion of this one are not in scope.
                        let exp = et.translate_exp(exp, &schema_type).into_exp();
                        et.finalize_types();
                        (schema_sym, exp)
                    })
                    .collect()
            })
            .unwrap_or_default();

        // Go over all variables in the schema which are not in the argument map and either match
        // them against existing one or declare new, if allowed.
        for (name, LocalVarEntry { type_, .. }) in &schema_entry.all_vars {
            if argument_map.contains_key(name) {
                continue;
            }
            let ty = type_.instantiate(type_arguments);
            let pool = et.symbol_pool();
            if let Some(entry) = vars.get(name) {
                // Name already exists in inclusion context, check its type.
                et.check_type(
                    loc,
                    &ty,
                    &entry.type_,
                    &format!(
                        "for `{}` included from schema",
                        name.display(et.symbol_pool())
                    ),
                );
                // Put into argument map.
                let node_id = et.new_node_id_with_type_loc(&entry.type_, loc);
                let exp = if let Some(oper) = &entry.operation {
                    ExpData::Call(node_id, oper.clone(), vec![])
                } else if let Some(index) = &entry.temp_index {
                    ExpData::Temporary(node_id, *index)
                } else {
                    ExpData::LocalVar(node_id, *name)
                };
                argument_map.insert(*name, exp.into_exp());
            } else if allow_new_vars {
                // Name does not yet exists in inclusion context, but is allowed to be introduced.
                // This happens if we include a schema in another schema.
                vars.insert(*name, LocalVarEntry {
                    loc: loc.clone(),
                    type_: ty.clone(),
                    operation: None,
                    temp_index: None,
                });
            } else {
                et.error(
                    loc,
                    &format!(
                        "`{}` cannot be matched to an existing name in inclusion context",
                        name.display(pool)
                    ),
                );
            }
        }
        // Done with expression build; ensure all types are inferred correctly.
        et.finalize_types();

        // Go over all conditions in the schema, rewrite them, and add to the inclusion conditions.
        for Condition {
            loc,
            kind,
            properties,
            exp,
            additional_exps,
        } in schema_entry
            .spec
            .conditions
            .iter()
            .chain(schema_entry.included_spec.conditions.iter())
        {
            let mut replacer = |_, target: RewriteTarget| {
                if let RewriteTarget::LocalVar(sym) = target {
                    argument_map.get(&sym).cloned()
                } else {
                    None
                }
            };
            let mut rewriter =
                ExpRewriter::new(self.parent.env, &mut replacer).set_type_args(type_arguments);
            let mut exp = rewriter.rewrite_exp(exp.to_owned());
            let mut additional_exps = rewriter.rewrite_vec(additional_exps);
            if let Some(cond) = &path_cond {
                // There is a path condition to be added.
                if kind == &ConditionKind::Emits {
                    let cond_exp = if additional_exps.len() < 2 {
                        cond.clone()
                    } else {
                        self.make_path_expr(
                            Operation::And,
                            cond.node_id(),
                            cond.clone(),
                            additional_exps.pop().unwrap(),
                        )
                    };
                    additional_exps.push(cond_exp);
                } else if matches!(kind, ConditionKind::LetPre(..) | ConditionKind::LetPost(..)) {
                    // Ignore path condition for lets.
                } else {
                    // In case of AbortsIf, the path condition is combined with the predicate using
                    // &&, otherwise ==>.
                    exp = self.make_path_expr(
                        if kind == &ConditionKind::AbortsIf {
                            Operation::And
                        } else {
                            Operation::Implies
                        },
                        cond.node_id(),
                        cond.clone(),
                        exp,
                    );
                }
            }
            let mut effective_properties = schema_properties.clone();
            effective_properties.extend(properties.clone());
            spec.conditions.push(Condition {
                loc: loc.clone(),
                kind: kind.clone(),
                properties: effective_properties,
                exp,
                additional_exps,
            });

            // If a formal argument is bound to an expression that contains a name
            // that conflicts with variables defined in the condition, return an error
            for bound_expr in argument_map.values() {
                let exp_loc = self.parent.env.get_node_loc(bound_expr.node_id());
                for loc_sym in bound_expr.free_local_vars_with_node_id().keys() {
                    match kind {
                        ConditionKind::LetPost(name) | ConditionKind::LetPre(name) => {
                            if name == loc_sym {
                                self.parent.error(
                                    &exp_loc,
                                    &format!("Variable `{}` conflicts with a specification variable in the schema {}", name.display(self.symbol_pool()),
                                             schema_name.display(self.parent.env))
                                );
                            }
                        },
                        _ => {},
                    }
                }
            }

            match kind {
                ConditionKind::LetPost(name) | ConditionKind::LetPre(name) => {
                    // If a let name is introduced by this condition, remove it from argument_map
                    // as it shadows schema arguments.
                    argument_map.remove(name);
                },
                _ => {},
            }
        }

        // Put schema entry back.
        self.parent
            .spec_schema_table
            .insert(schema_name, schema_entry);
    }

    /// Make a path expression.
    fn make_path_expr(&mut self, oper: Operation, node_id: NodeId, cond: Exp, exp: Exp) -> Exp {
        let env = &self.parent.env;
        let path_cond_loc = env.get_node_loc(node_id);
        let new_node_id = env.new_node(path_cond_loc, BOOL_TYPE.clone());
        ExpData::Call(new_node_id, oper, vec![cond, exp]).into_exp()
    }

    /// Creates an expression translator for use in schema expression. This defines the context
    /// type parameters and the variables.
    fn exp_translator_for_schema<'module_translator>(
        &'module_translator mut self,
        loc: &Loc,
        context_type_params: &[TypeParameter],
        vars: &mut BTreeMap<Symbol, LocalVarEntry>,
    ) -> ExpTranslator<'env, 'translator, 'module_translator> {
        let mut et = ExpTranslator::new_with_old(self, true);
        et.define_type_params(loc, context_type_params);
        et.enter_scope();
        for (n, entry) in vars.iter() {
            et.define_local(
                &entry.loc,
                *n,
                entry.type_.clone(),
                entry.operation.clone(),
                entry.temp_index,
            );
        }
        et.enter_scope();
        for (n, id) in et
            .parent
            .spec_block_lets
            .iter()
            .map(|(n, (_, id))| (*n, *id))
            .collect_vec()
        {
            let ty = et.parent.parent.env.get_node_type(id);
            let loc = et.parent.parent.env.get_node_loc(id);
            et.define_let_local(&loc, n, ty);
        }
        et
    }

    /// Extends a path condition for schema expression analysis.
    fn extend_path_condition(&mut self, loc: &Loc, path_cond: Option<Exp>, exp: Exp) -> Exp {
        if let Some(cond) = path_cond {
            let node_id = self.parent.env.new_node(loc.clone(), BOOL_TYPE.clone());
            ExpData::Call(node_id, Operation::And, vec![cond, exp]).into_exp()
        } else {
            exp
        }
    }

    /// Analyze schema inclusion in the spec block for a function, struct or module. This
    /// instantiates the schema and adds all conditions and invariants it contains to the context.
    ///
    /// The `alt_context_type_params` allows to use different type parameter names as would
    /// otherwise be inferred from the SchemaBlockContext. This is used for the apply weaving
    /// operator which allows to use different type parameter names than the function declarations
    /// to which it is applied to.
    fn def_ana_schema_inclusion_outside_schema(
        &mut self,
        loc: &Loc,
        context: &SpecBlockContext,
        alt_context_type_params: Option<&[TypeParameter]>,
        context_properties: PropertyBag,
        exp: &EA::Exp,
    ) {
        // Compute the type parameters and variables this spec block uses. We do this by constructing
        // an expression translator and immediately extracting  from it. Depending on whether in
        // function or struct context, we use a condition kind which defines the maximum
        // of available symbols. We need to potentially revise this to only declare variables which
        // have a proper use in a condition/invariant, depending on what is actually included in
        // the block.
        let (mut vars, context_type_params) = match context {
            SpecBlockContext::Function(..) | SpecBlockContext::FunctionCode(..) => {
                let et = self.exp_translator_for_context(loc, context, &ConditionKind::Ensures);
                (et.extract_var_map(), et.get_type_params_with_name())
            },
            SpecBlockContext::Struct(..) => {
                let et =
                    self.exp_translator_for_context(loc, context, &ConditionKind::StructInvariant);
                (et.extract_var_map(), et.get_type_params_with_name())
            },
            SpecBlockContext::Module => (BTreeMap::new(), vec![]),
            SpecBlockContext::Schema { .. } => panic!("unexpected schema context"),
        };
        let mut spec = Spec::default();

        // Analyze the schema inclusion. This will instantiate conditions for
        // this block.
        let context_type_params = context_type_params
            .iter()
            .map(|(n, _)| TypeParameter(*n, TypeParameterKind::default()))
            .collect::<Vec<_>>();
        self.def_ana_schema_exp(
            if let Some(type_params) = alt_context_type_params {
                type_params
            } else {
                &context_type_params
            },
            &mut vars,
            &mut spec,
            false,
            &PropertyBag::default(),
            exp,
        );

        // Write the conditions to the context item.
        self.add_conditions_to_context(
            context,
            loc,
            spec.conditions,
            context_properties,
            "(included from schema)",
        );
    }

    /// Analyzes a schema apply weaving operator.
    fn def_ana_schema_apply(
        &mut self,
        loc: &Loc,
        context: &SpecBlockContext,
        exp: &EA::Exp,
        patterns: &[PA::SpecApplyPattern],
        exclusion_patterns: &[PA::SpecApplyPattern],
    ) {
        if !matches!(context, SpecBlockContext::Module) {
            self.parent.error(
                loc,
                "the `apply` schema weaving operator can only be used inside a `spec module` block",
            );
            return;
        }
        for fun_name in self.parent.fun_table.keys().cloned().collect_vec() {
            // Note we need the vector clone above to avoid borrowing self for the
            // whole loop.
            let entry = self.parent.fun_table.get(&fun_name).unwrap();
            if entry.module_id != self.module_id {
                // Not a function from this module
                continue;
            }
            let is_public = matches!(entry.visibility, Visibility::Public);
            let type_arg_count = entry.type_params.len();
            let is_excluded = exclusion_patterns.iter().any(|p| {
                self.apply_pattern_matches(fun_name.symbol, is_public, type_arg_count, true, p)
            });
            if is_excluded {
                // Explicitly excluded from matching.
                continue;
            }
            if let Some(matched) = patterns.iter().find(|p| {
                self.apply_pattern_matches(fun_name.symbol, is_public, type_arg_count, false, p)
            }) {
                // This is a match, so apply this schema to this function.
                let type_params = {
                    let mut et = ExpTranslator::new(self);
                    let ability_set = EA::AbilitySet::empty();
                    et.analyze_and_add_type_params(
                        matched
                            .value
                            .type_parameters
                            .iter()
                            .map(|(n, _)| (n, &ability_set)),
                    );
                    et.get_type_params()
                };
                // Create a property marking this as injected.
                let mut context_properties =
                    self.add_bool_property(PropertyBag::default(), CONDITION_INJECTED_PROP, true);
                context_properties =
                    self.add_bool_property(context_properties, CONDITION_EXPORT_PROP, true);
                self.def_ana_schema_inclusion_outside_schema(
                    loc,
                    &SpecBlockContext::Function(fun_name),
                    Some(&type_params),
                    context_properties,
                    exp,
                );
            }
        }
    }

    /// Returns true if the pattern matches the function of name, type arity, and
    /// visibility.
    ///
    /// The `ignore_type_args` parameter is used for exclusion matches. In exclusion matches we
    /// do not want to include type args because its to easy for a user to get this wrong, so
    /// we match based only on visibility and name pattern. On the other hand, we want a user
    /// in inclusion matches to use a pattern like `*<X>` to match any generic function with
    /// one type argument.
    fn apply_pattern_matches(
        &self,
        name: Symbol,
        is_public: bool,
        type_arg_count: usize,
        ignore_type_args: bool,
        pattern: &PA::SpecApplyPattern,
    ) -> bool {
        if !ignore_type_args && pattern.value.type_parameters.len() != type_arg_count {
            return false;
        }
        if let Some(v) = &pattern.value.visibility {
            match v {
                PA::Visibility::Public(..) => {
                    if !is_public {
                        return false;
                    }
                },
                PA::Visibility::Internal => {
                    if is_public {
                        return false;
                    }
                },
                PA::Visibility::Script(..) => {
                    // TODO: model script visibility properly
                    unimplemented!("Script visibility not supported yet")
                },
                PA::Visibility::Friend(..) => {
                    // TODO: model friend visibility properly
                    unimplemented!("Friend visibility not supported yet")
                },
            }
        }
        let rex = Regex::new(&format!(
            "^{}$",
            pattern
                .value
                .name_pattern
                .iter()
                .map(|p| match &p.value {
                    PA::SpecApplyFragment_::Wildcard => ".*".to_string(),
                    PA::SpecApplyFragment_::NamePart(n) => n.value.to_string(),
                })
                .join("")
        ))
        .expect("regex valid");
        rex.is_match(self.symbol_pool().string(name).as_str())
    }
}

/// ## Spec Var Usage Analysis

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    /// Compute state usage of spec funs.
    fn compute_state_usage(&mut self) {
        let mut visited = BTreeSet::new();
        for idx in 0..self.spec_funs.len() {
            self.compute_state_usage_and_callees_for_fun(&mut visited, idx);
        }
        // Check for purity requirements. All data invariants must be pure expressions and
        // not depend on global state.
        let check_uses_memory = |mid: ModuleId, fid: SpecFunId| {
            if mid.to_usize() < self.parent.env.get_module_count() {
                // This is calling a function from another module we already have
                // translated.
                let module_env = self.parent.env.get_module(mid);
                let fun_decl = module_env.get_spec_fun(fid);
                fun_decl.used_memory.is_empty()
            } else {
                // This is calling a function from the module we are currently translating.
                let fun_decl = &self.spec_funs[fid.as_usize()];
                fun_decl.used_memory.is_empty()
            }
        };

        for struct_spec in self.struct_specs.values() {
            for cond in &struct_spec.conditions {
                if matches!(cond.kind, ConditionKind::StructInvariant)
                    && !cond.exp.uses_memory(&check_uses_memory)
                {
                    self.parent.error(
                        &cond.loc,
                        "data invariants cannot depend on global state \
                        (directly or indirectly uses a global spec var or resource storage).",
                    );
                }
            }
        }
    }

    /// Compute state usage for a given spec fun, defined via its index into the spec_funs
    /// vector of the currently translated module. This recursively computes the values for
    /// functions called from this one; the visited set is there to break cycles.
    fn compute_state_usage_and_callees_for_fun(
        &mut self,
        visited: &mut BTreeSet<usize>,
        fun_idx: usize,
    ) {
        if !visited.insert(fun_idx) {
            return;
        }

        // Detach the current SpecFunDecl body so we can traverse it while at the same time mutating
        // the full self. Rust requires us to do so (at least the author doesn't know better yet),
        // but moving it should be not too expensive.
        let body = if self.spec_funs[fun_idx].body.is_some() {
            std::mem::replace(&mut self.spec_funs[fun_idx].body, None).unwrap()
        } else {
            // No body: assume it is pure.
            return;
        };

        let (used_memory, callees) =
            self.compute_state_usage_and_callees_for_exp(Some(visited), &body);
        let fun_decl = &mut self.spec_funs[fun_idx];
        fun_decl.body = Some(body);
        fun_decl.used_memory = used_memory;
        fun_decl.callees = callees;
    }

    /// Computes state usage and called functions for an expression. If the visited_opt is
    /// available, this recurses to compute the usage for any functions called. Otherwise
    /// it assumes this information is already computed.
    fn compute_state_usage_and_callees_for_exp(
        &mut self,
        mut visited_opt: Option<&mut BTreeSet<usize>>,
        exp: &ExpData,
    ) -> (
        BTreeSet<QualifiedInstId<StructId>>,
        BTreeSet<QualifiedId<SpecFunId>>,
    ) {
        let mut used_memory = BTreeSet::new();
        let mut callees = BTreeSet::new();
        exp.visit(&mut |e: &ExpData| {
            match e {
                ExpData::Call(id, Operation::Function(mid, fid, _), _) => {
                    callees.insert(mid.qualified(*fid));
                    let inst = self.parent.env.get_node_instantiation(*id);
                    // Extend used memory with that of called functions, after applying type
                    // instantiation of this call.
                    if mid.to_usize() < self.parent.env.get_module_count() {
                        // This is calling a function from another module we already have
                        // translated.
                        let module_env = self.parent.env.get_module(*mid);
                        let fun_decl = module_env.get_spec_fun(*fid);
                        used_memory.extend(
                            fun_decl
                                .used_memory
                                .iter()
                                .map(|id| id.instantiate_ref(&inst)),
                        );
                    } else {
                        // This is calling a function from the module we are currently translating.
                        // Need to recursively ensure we have computed used_spec_vars because of
                        // arbitrary call graphs, including cyclic. If visted_opt is not set,
                        // we know we already computed this.
                        if let Some(visited) = &mut visited_opt {
                            self.compute_state_usage_and_callees_for_fun(visited, fid.as_usize());
                        }
                        let fun_decl = &self.spec_funs[fid.as_usize()];
                        used_memory.extend(
                            fun_decl
                                .used_memory
                                .iter()
                                .map(|id| id.instantiate_ref(&inst)),
                        );
                    }
                },
                ExpData::Call(node_id, Operation::Global(_), _)
                | ExpData::Call(node_id, Operation::Exists(_), _) => {
                    if !self.parent.env.has_errors() {
                        // We would crash if the type is not valid, so only do this if no errors
                        // have been reported so far.
                        let ty = &self.parent.env.get_node_instantiation(*node_id)[0];
                        let (mid, sid, inst) = ty.require_struct();
                        used_memory.insert(mid.qualified_inst(sid, inst.to_owned()));
                    }
                },
                _ => {},
            }
        });
        (used_memory, callees)
    }
}

/// ## Module Invariants

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    /// Process module invariants, attaching them to the global env.
    fn process_module_invariants(&mut self) {
        for cond in self.module_spec.conditions.iter().cloned().collect_vec() {
            if matches!(
                cond.kind,
                ConditionKind::GlobalInvariant(..) | ConditionKind::GlobalInvariantUpdate(..)
            ) {
                let (mem_usage, _) = self.compute_state_usage_and_callees_for_exp(None, &cond.exp);
                let id = self.parent.env.new_global_id();
                let Condition { loc, exp, .. } = cond;
                self.parent.env.add_global_invariant(GlobalInvariant {
                    id,
                    loc,
                    kind: cond.kind,
                    mem_usage,
                    declaring_module: self.module_id,
                    cond: exp,
                    properties: cond.properties.clone(),
                });
            }
        }
    }
}

/// # Spec Block Infos

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    /// Collect location and target information for all spec blocks. This is used for documentation
    /// generation.
    fn collect_spec_block_infos(&mut self, module_def: &EA::ModuleDefinition) {
        for block in &module_def.specs {
            let block_loc = self.parent.to_loc(&block.loc);
            let member_locs = block
                .value
                .members
                .iter()
                .map(|m| self.parent.to_loc(&m.loc))
                .collect_vec();
            let target = match self.get_spec_block_context(&block.value.target) {
                Some(SpecBlockContext::Module) => SpecBlockTarget::Module,
                Some(SpecBlockContext::Function(qsym)) => {
                    SpecBlockTarget::Function(self.module_id, FunId::new(qsym.symbol))
                },
                Some(SpecBlockContext::FunctionCode(qsym, info)) => SpecBlockTarget::FunctionCode(
                    self.module_id,
                    FunId::new(qsym.symbol),
                    info.offset as usize,
                ),
                Some(SpecBlockContext::Struct(qsym)) => {
                    SpecBlockTarget::Struct(self.module_id, StructId::new(qsym.symbol))
                },
                Some(SpecBlockContext::Schema(qsym)) => {
                    let entry = self
                        .parent
                        .spec_schema_table
                        .get(&qsym)
                        .expect("schema defined");
                    SpecBlockTarget::Schema(
                        self.module_id,
                        SchemaId::new(qsym.symbol),
                        entry.type_params.clone(),
                    )
                },
                None => {
                    // This has been reported as an error. Choose a dummy target.
                    SpecBlockTarget::Module
                },
            };
            self.spec_block_infos.push(SpecBlockInfo {
                loc: block_loc,
                member_locs,
                target,
            })
        }
    }
}

/// # Tweak application

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    /// Tweak the specifications at the AST level based on `ModuleBuilderOptions`.
    fn apply_tweaks(&mut self, module_def: &EA::ModuleDefinition) {
        self.tweak_pragma_opaque(module_def);
    }

    /// If the `ignore_pragma_opaque_*` options are set, the opaque pragma will be
    /// removed from the function spec property bag according to the options.
    fn tweak_pragma_opaque(&mut self, module_def: &EA::ModuleDefinition) {
        let env = &self.parent.env;
        let options = env
            .get_extension::<ModelBuilderOptions>()
            .unwrap_or_default();
        if !(options.ignore_pragma_opaque_when_possible
            || options.ignore_pragma_opaque_internal_only)
        {
            return;
        }

        for spec in &module_def.specs {
            if matches!(spec.value.target.value, EA::SpecBlockTarget_::Schema(..)) {
                continue;
            }
            if let Some(SpecBlockContext::Function(fun_name)) =
                self.get_spec_block_context(&spec.value.target)
            {
                if let Some(spec) = self.fun_specs.get_mut(&fun_name.symbol) {
                    // if the spec does not have "pragma opaque;" do nothing,
                    let has_pragma_opaque = env
                        .is_property_true(&spec.properties, OPAQUE_PRAGMA)
                        .unwrap_or(false);
                    if !has_pragma_opaque {
                        continue;
                    }

                    // if the spec has `pragma verify = false;` do not remove its `opaque` mark
                    let is_verified = env
                        .is_property_true(&spec.properties, VERIFY_PRAGMA)
                        .unwrap_or(true)
                        && env
                            .is_property_true(&self.module_spec.properties, VERIFY_PRAGMA)
                            .unwrap_or(true);
                    if !is_verified {
                        continue;
                    }

                    // if the spec has `[concrete]` or `[abstract]` properties, do not remove its
                    // `opaque` mark
                    let has_opaque_prop = spec.any(|cond| {
                        env.is_property_true(&cond.properties, CONDITION_CONCRETE_PROP)
                            .unwrap_or(false)
                            || env
                                .is_property_true(&cond.properties, CONDITION_ABSTRACT_PROP)
                                .unwrap_or(false)
                    });
                    if has_opaque_prop {
                        continue;
                    }

                    // if the function may have unknown callers, respect the option
                    // `ignore_pragma_opaque_internal_only`.
                    let fun_entry = self.parent.fun_table.get(&fun_name).unwrap_or_else(|| {
                        panic!(
                            "Unable to find function `{}`",
                            fun_name.display(self.parent.env)
                        )
                    });
                    let has_unknown_caller =
                        matches!(fun_entry.visibility, Visibility::Public) || fun_entry.is_entry;
                    if has_unknown_caller && options.ignore_pragma_opaque_internal_only {
                        continue;
                    }

                    // everything is cleared, we can remove the `opaque` mark now
                    let opaque_symbol = env.symbol_pool().make(OPAQUE_PRAGMA);
                    spec.properties.remove(&opaque_symbol);
                }
            }
        }
    }
}

/// # Environment Population and finalization

impl<'env, 'translator> ModuleBuilder<'env, 'translator> {
    fn populate_and_finalize_env(
        &mut self,
        loc: Loc,
        attributes: Vec<Attribute>,
        compiled_module: Option<BytecodeModule>,
    ) {
        let mut struct_data: BTreeMap<StructId, StructData> = Default::default();
        for (name, entry) in &self.parent.struct_table {
            if name.module_name != self.module_name {
                continue;
            }
            // New struct in this module
            let spec = self.struct_specs.remove(&name.symbol).unwrap_or_default();
            let field_data = if let Some(fields) = &entry.fields {
                fields
                    .iter()
                    .map(|(name, (offset, ty))| {
                        (FieldId::new(*name), FieldData {
                            name: *name,
                            offset: *offset,
                            ty: ty.clone(),
                        })
                    })
                    .collect::<BTreeMap<_, _>>()
            } else {
                BTreeMap::new()
            };
            let data = StructData {
                name: name.symbol,
                loc: entry.loc.clone(),
                def_idx: None,
                attributes: entry.attributes.clone(),
                type_params: entry.type_params.clone(),
                abilities: entry.abilities,
                spec_var_opt: None,
                field_data,
                spec,
            };
            struct_data.insert(StructId::new(name.symbol), data);
        }
        let mut function_data: BTreeMap<FunId, FunctionData> = Default::default();
        for (name, entry) in &self.parent.fun_table {
            if name.module_name != self.module_name {
                continue;
            }
            // New function
            let spec = self.fun_specs.remove(&name.symbol).unwrap_or_default();
            let def = self.fun_defs.remove(&name.symbol);
            let data = FunctionData {
                name: name.symbol,
                loc: entry.loc.clone(),
                def_idx: None,
                handle_idx: None,
                visibility: entry.visibility,
                is_native: entry.is_native,
                is_entry: entry.is_entry,
                is_inline: entry.is_inline,
                attributes: entry.attributes.clone(),
                type_params: entry.type_params.clone(),
                params: entry.params.clone(),
                result_type: entry.result_type.clone(),
                spec: spec.into(),
                def,
                called_funs: None,
                calling_funs: RefCell::default(),
                transitive_closure_of_called_funs: RefCell::default(),
            };
            function_data.insert(FunId::new(name.symbol), data);
        }

        let mut named_constants: BTreeMap<NamedConstantId, NamedConstantData> = Default::default();
        for (name, const_entry) in &self.parent.const_table {
            if name.module_name != self.module_name {
                continue;
            }
            // New constant
            let ConstEntry { loc, value, ty } = const_entry.clone();
            let data = NamedConstantData {
                name: name.symbol,
                loc,
                type_: ty,
                value,
            };
            named_constants.insert(NamedConstantId::new(name.symbol), data);
        }

        let module_id = self.parent.env.add(
            loc,
            self.module_name.clone(),
            attributes,
            named_constants,
            struct_data,
            function_data,
            std::mem::take(&mut self.spec_vars),
            std::mem::take(&mut self.spec_funs),
            std::mem::take(&mut self.module_spec),
            std::mem::take(&mut self.spec_block_infos),
        );

        if let Some(BytecodeModule {
            compiled_module,
            source_map,
            ..
        }) = compiled_module
        {
            self.parent
                .env
                .attach_compiled_module(module_id, compiled_module, source_map)
        }
    }
}

/// Extract all accesses of a schema from a schema expression.
pub(crate) fn extract_schema_access<'a>(exp: &'a EA::Exp, res: &mut Vec<&'a EA::ModuleAccess>) {
    match &exp.value {
        EA::Exp_::Name(maccess, _) => res.push(maccess),
        EA::Exp_::Pack(maccess, ..) => res.push(maccess),
        EA::Exp_::BinopExp(_, _, rhs) => extract_schema_access(rhs, res),
        EA::Exp_::IfElse(_, t, e) => {
            extract_schema_access(t, res);
            extract_schema_access(e, res);
        },
        _ => {},
    }
}
