use crate::context::MultiProject;

// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0
use super::item::*;
use super::scopes::*;
use super::types::*;
use super::utils::*;
use anyhow::{Ok, Result};
use move_command_line_common::files::FileHash;
use move_compiler::parser::ast::Definition;
use move_compiler::parser::ast::*;
use move_compiler::shared::Identifier;
use move_compiler::shared::*;
use move_core_types::account_address::*;
use move_ir_types::location::Loc;
use move_ir_types::location::Spanned;
use move_package::source_package::layout::SourcePackageLayout;
use move_package::source_package::manifest_parser::*;
use move_symbol_pool::Symbol;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::path::PathBuf;
use std::rc::Rc;
use walkdir::WalkDir;

/// All Modules.
pub struct Project {
    pub(crate) modules: HashMap<
        PathBuf, /* this is a Move.toml like xxxx/Move.toml  */
        Rc<RefCell<SourceDefs>>,
    >,
    /// a field contains the root manifest file
    /// if Modules construct successful this field is never None.
    pub(crate) manifests: Vec<move_package::source_package::parsed_manifest::SourceManifest>,
    pub(crate) hash_file: Rc<RefCell<PathBufHashMap>>,
    pub(crate) file_line_mapping: Rc<RefCell<FileLineMapping>>,
    pub(crate) manifest_paths: Vec<PathBuf>,
    pub(crate) scopes: Scopes,
}
impl Project {
    pub(crate) fn mk_multi_project_key(&self) -> im::HashSet<PathBuf> {
        use im::HashSet;
        let mut v = HashSet::default();
        for x in self.manifest_paths.iter() {
            v.insert(x.clone());
        }
        v
    }
}

/// Various ast access methods.
pub trait AstProvider: Clone {
    fn get_module_addr(
        &self,
        addr: Option<LeadingNameAccess>,
        m: &ModuleDefinition,
    ) -> AccountAddress;

    fn with_definition(&self, call_back: impl FnMut(&Definition));
    fn with_module(&self, mut call_back: impl FnMut(AccountAddress, &ModuleDefinition)) {
        self.with_definition(|x| match x {
            Definition::Module(module) => {
                call_back(self.get_module_addr(module.address, module), module);
            }
            Definition::Address(a) => {
                for module in a.modules.iter() {
                    call_back(self.get_module_addr(module.address, module), module);
                }
            }
            _ => {}
        })
    }

    fn found_in_test(&self) -> bool;

    fn with_module_member(
        &self,
        mut call_back: impl FnMut(AccountAddress, Symbol, &ModuleMember, bool /* if is_spec */),
    ) {
        self.with_definition(|x| match x {
            Definition::Module(module) => {
                for m in module.members.iter() {
                    call_back(
                        self.get_module_addr(module.address, module),
                        module.name.0.value,
                        m,
                        module.is_spec_module,
                    );
                }
            }
            Definition::Address(a) => {
                for module in a.modules.iter() {
                    for m in module.members.iter() {
                        call_back(
                            self.get_module_addr(module.address, module),
                            module.name.0.value,
                            m,
                            module.is_spec_module,
                        );
                    }
                }
            }
            _ => {}
        });
    }

    fn with_const(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &Constant)) {
        self.with_module_member(|addr, module_name, member, _| match member {
            ModuleMember::Constant(c) => call_back(addr, module_name, c),
            _ => {}
        });
    }

    fn with_struct(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &StructDefinition)) {
        self.with_module_member(|addr, module_name, member, _| match member {
            ModuleMember::Struct(c) => call_back(addr, module_name, c),
            _ => {}
        });
    }
    fn with_script(&self, mut call_back: impl FnMut(&Script)) {
        self.with_definition(|x| match x {
            Definition::Script(x) => {
                call_back(x);
            }
            _ => {}
        })
    }
    fn with_use_decl(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &UseDecl, bool)) {
        self.with_module_member(|addr, module_name, member, is_spec| match member {
            ModuleMember::Use(c) => call_back(addr, module_name, c, is_spec),
            _ => {}
        });
    }
    fn with_function(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &Function)) {
        self.with_module_member(|addr, module_name, member, _| match member {
            ModuleMember::Function(c) => call_back(addr, module_name, c),
            _ => {}
        });
    }
    fn with_friend(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &FriendDecl)) {
        self.with_module_member(|addr, module_name, member, _| match member {
            ModuleMember::Friend(c) => call_back(addr, module_name, c),
            _ => {}
        });
    }
    fn with_spec(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &SpecBlock, bool)) {
        self.with_module_member(|addr, module_name, member, is_spec_module| match member {
            ModuleMember::Spec(c) => call_back(addr, module_name, c, is_spec_module),
            _ => {}
        });
    }
    fn with_spec_schema(
        &self,
        mut call_back: impl FnMut(AccountAddress, Symbol, Name, &SpecBlock, bool),
    ) {
        self.with_module_member(|addr, module_name, member, is_spec_module| match member {
            ModuleMember::Spec(c) => match &c.value.target.value {
                SpecBlockTarget_::Schema(name, _) => {
                    call_back(addr, module_name, name.clone(), c, is_spec_module);
                }
                _ => {}
            },
            _ => {}
        });
    }
}

#[derive(Clone)]
pub struct VecDefAstProvider<'a> {
    /// The actual Definition.
    defs: &'a Vec<Definition>,
    /// Help for convert name to addr.
    modules: &'a Project,
    layout: SourcePackageLayout,
}

impl<'a> VecDefAstProvider<'a> {
    pub(crate) fn new(
        defs: &'a Vec<Definition>,
        modules: &'a Project,
        layout: SourcePackageLayout,
    ) -> Self {
        Self {
            defs,
            modules,
            layout,
        }
    }
}

impl<'a> AstProvider for VecDefAstProvider<'a> {
    fn get_module_addr(
        &self,
        addr: Option<LeadingNameAccess>,
        m: &ModuleDefinition,
    ) -> AccountAddress {
        self.modules.get_module_addr(addr, m)
    }
    fn with_definition(&self, mut call_back: impl FnMut(&Definition)) {
        for d in self.defs.iter() {
            call_back(d);
        }
    }
    fn found_in_test(&self) -> bool {
        self.layout == SourcePackageLayout::Tests
    }
}
#[derive(Clone)]
pub struct ModulesAstProvider<'a> {
    modules: &'a Project,
    layout: SourcePackageLayout,
    manifest_path: PathBuf,
}

impl<'a> ModulesAstProvider<'a> {
    pub(crate) fn new(
        modules: &'a Project,
        manifest_path: PathBuf,
        kind: SourcePackageLayout,
    ) -> Self {
        Self {
            modules,
            layout: kind,
            manifest_path,
        }
    }
}

impl<'a> AstProvider for ModulesAstProvider<'a> {
    fn get_module_addr(
        &self,
        addr: Option<LeadingNameAccess>,
        m: &ModuleDefinition,
    ) -> AccountAddress {
        self.modules.get_module_addr(addr, m)
    }
    fn with_definition(&self, mut call_back: impl FnMut(&Definition)) {
        let empty = Default::default();
        let b = self
            .modules
            .modules
            .get(&self.manifest_path)
            .unwrap_or(&empty)
            .as_ref()
            .borrow();

        for (_, m) in if self.layout == SourcePackageLayout::Sources {
            &b.sources
        } else if self.layout == SourcePackageLayout::Tests {
            &b.tests
        } else if self.layout == SourcePackageLayout::Scripts {
            &b.scripts
        } else {
            unreachable!()
        }
        .iter()
        {
            for d in m.iter() {
                call_back(d);
            }
        }
    }

    fn found_in_test(&self) -> bool {
        self.layout == SourcePackageLayout::Tests
    }
}

impl Project {
    pub fn new(root_dir: impl Into<PathBuf>, multi: &mut MultiProject) -> Result<Self> {
        let working_dir = root_dir.into();
        log::info!("scan modules at {:?}", &working_dir);
        let mut modules = Self {
            modules: Default::default(),
            manifests: Default::default(),
            hash_file: multi.hash_file.clone(),
            file_line_mapping: multi.file_line_mapping.clone(),
            manifest_paths: Default::default(),
            scopes: Scopes::new(),
        };
        modules.load_project(&working_dir, multi)?;
        let mut dummy = DummyVisitor;
        modules.run_full_visitor(&mut dummy);
        Ok(modules)
    }

    pub fn update_defs(&mut self, file_path: &PathBuf, old_defs: Option<&Vec<Definition>>) {
        let manifest = super::utils::discover_manifest_and_kind(file_path.as_path());
        if manifest.is_none() {
            log::error!("path can't find manifest file:{:?}", file_path);
            return;
        }
        let (manifest, layout) = manifest.unwrap();
        log::info!(
            "update defs for {:?} manifest:{:?} layout:{:?}",
            file_path.as_path(),
            manifest.as_path(),
            layout
        );
        // delete old items.
        if let Some(defs) = old_defs.as_ref() {
            let x = VecDefAstProvider::new(&defs, self, layout.clone());
            x.with_module(|addr, d| {
                self.scopes
                    .delete_module_items(addr, d.name.value(), d.is_spec_module);
            });
        };
        // Update defs.
        let mut dummy = DummyVisitor;
        self.run_visitor_for_file(&mut dummy, &manifest, file_path, layout);
    }

    /// Load a Move.toml project.
    pub(crate) fn load_project(
        &mut self,
        manifest_path: &PathBuf,
        multi: &mut MultiProject,
    ) -> Result<()> {
        let manifest_path = normal_path(&manifest_path.as_path());
        if self.modules.get(&manifest_path).is_some() {
            log::info!("manifest '{:?}' loaded before skipped.", &manifest_path);
            return Ok(());
        }
        self.manifest_paths.push(manifest_path.clone());
        log::info!("load manifest file at {:?}", &manifest_path);
        let manifest = parse_move_manifest_from_file(&manifest_path)?;

        self.manifests.push(manifest.clone());

        // load depends.
        for (dep_name, de) in manifest
            .dependencies
            .iter()
            .chain(manifest.dev_dependencies.iter())
        {
            let de_path = match &de.kind {
                DependencyKind::Local(local) => local.clone(),
                DependencyKind::Git(x) => {
                    path_concat(repository_path(&de.kind).as_path(), x.subdir.as_path())
                }
                DependencyKind::Custom(_) => todo!(),
            };
            let p = path_concat(manifest_path.as_path(), &de_path);
            log::info!(
                "load dependency for '{:?}' dep_name '{}'",
                &manifest_path,
                dep_name
            );
            self.load_project(&p, multi)?;
        }
        if let Some(x) = multi.asts.get(&manifest_path) {
            self.modules.insert(manifest_path, x.clone());
        } else {
            let d: Rc<RefCell<SourceDefs>> = Default::default();
            self.modules.insert(manifest_path.clone(), d.clone());
            multi.asts.insert(manifest_path.clone(), d.clone());
            self.load_layout_files(&manifest_path, SourcePackageLayout::Sources);
            self.load_layout_files(&manifest_path, SourcePackageLayout::Tests);
            self.load_layout_files(&manifest_path, SourcePackageLayout::Scripts);
        }
        Ok(())
    }

    /// Load move files  locate in sources and tests ...
    pub(crate) fn load_layout_files(&mut self, manifest_path: &PathBuf, kind: SourcePackageLayout) {
        use super::syntax::parse_file_string;
        use std::fs;
        let mut env = CompilationEnv::new(Flags::testing());
        let mut p = manifest_path.clone();
        p.push(kind.location_str());
        for item in WalkDir::new(&p) {
            let file = match item {
                std::result::Result::Err(_e) => continue,
                std::result::Result::Ok(x) => x,
            };
            if file.file_type().is_file()
                && match file.file_name().to_str() {
                    Some(s) => s.ends_with(".move"),
                    None => continue,
                }
            {
                let file_content = fs::read_to_string(file.path()).unwrap();
                log::info!("load source file {:?}", file.path());
                let file_hash = FileHash::new(file_content.as_str());

                // This is a move file.
                let defs = parse_file_string(&mut env, file_hash, file_content.as_str());
                let defs = match defs {
                    std::result::Result::Ok(x) => x,
                    std::result::Result::Err(diags) => {
                        let mut m = HashMap::new();
                        m.insert(
                            file_hash,
                            (
                                Symbol::from(file.path().to_str().unwrap()),
                                file_content.clone(),
                            ),
                        );
                        let buffer =
                            move_compiler::diagnostics::report_diagnostics_to_buffer(&m, diags);
                        let s = String::from_utf8_lossy(buffer.as_slice());
                        log::error!("{}", s);
                        continue;
                    }
                };

                let defs = defs.0;

                if kind == SourcePackageLayout::Sources {
                    self.modules
                        .get_mut(manifest_path)
                        .unwrap()
                        .as_ref()
                        .borrow_mut()
                        .sources
                        .insert(file.path().clone().to_path_buf(), defs);
                } else if kind == SourcePackageLayout::Tests {
                    self.modules
                        .get_mut(manifest_path)
                        .unwrap()
                        .as_ref()
                        .borrow_mut()
                        .tests
                        .insert(file.path().clone().to_path_buf(), defs);
                } else {
                    self.modules
                        .get_mut(manifest_path)
                        .unwrap()
                        .as_ref()
                        .borrow_mut()
                        .scripts
                        .insert(file.path().clone().to_path_buf(), defs);
                }
                // update hash
                self.hash_file
                    .as_ref()
                    .borrow_mut()
                    .update(file.path().to_path_buf(), file_hash);
                // update line mapping.
                self.file_line_mapping
                    .as_ref()
                    .borrow_mut()
                    .update(file.path().to_path_buf(), file_content.as_str());
            }
        }
    }

    pub(crate) fn name_to_addr_impl(&self, name: Symbol) -> AccountAddress {
        for x in self.manifests.iter() {
            if let Some(ref x) = x.dev_address_assignments {
                match x.get(&name) {
                    Some(x) => return x.clone(),
                    None => {}
                }
            }
            if let Some(ref x) = x.addresses {
                match x.get(&name) {
                    Some(x) => match x {
                        Some(x) => return x.clone(),
                        _ => {}
                    },
                    None => {}
                }
            }
        }
        return *ERR_ADDRESS;
    }

    pub(crate) fn get_spec_build_in_call_type(
        &self,
        scopes: &Scopes,
        b: SpecBuildInFun,
        type_args: &Option<Vec<Type>>,
        exprs: &Spanned<Vec<Exp>>, // TODO need use _expr.
    ) -> ResolvedType {
        let exprs_types: Vec<_> = exprs
            .value
            .iter()
            .map(|e| self.get_expr_type(e, scopes))
            .collect();
        // vec<T>(x): vector<T> returns a singleton vector.
        // A lot of those build in function.
        let t_in_vector = exprs_types
            .get(0)
            .map(|x| x.clone())
            .map(|x| match x {
                ResolvedType::Vec(x) => x.as_ref().clone(),
                _ => x,
            })
            .unwrap_or(ResolvedType::UnKnown);
        let first_t = exprs_types
            .get(0)
            .map(|x| x.clone())
            .unwrap_or(ResolvedType::UnKnown);

        match b {
            SpecBuildInFun::Exists => ResolvedType::new_build_in(BuildInType::Bool),
            SpecBuildInFun::Global => {
                if let Some(type_args) = type_args {
                    if let Some(ty) = type_args.get(0) {
                        scopes.resolve_type(ty, self)
                    } else {
                        ResolvedType::UnKnown
                    }
                } else {
                    ResolvedType::UnKnown
                }
            }
            SpecBuildInFun::Len => ResolvedType::new_build_in(BuildInType::NumType),
            SpecBuildInFun::Update => {
                if let Some(type_args) = type_args {
                    if let Some(ty) = type_args.get(0) {
                        ResolvedType::new_vector(scopes.resolve_type(ty, self))
                    } else {
                        ResolvedType::new_vector(t_in_vector)
                    }
                } else {
                    ResolvedType::new_vector(t_in_vector)
                }
            }
            SpecBuildInFun::Vec => {
                if let Some(type_args) = type_args {
                    if let Some(ty) = type_args.get(0) {
                        ResolvedType::new_vector(scopes.resolve_type(ty, self))
                    } else {
                        // TODO infer from expr.
                        ResolvedType::new_vector(first_t)
                    }
                } else {
                    ResolvedType::new_vector(first_t)
                }
            }
            SpecBuildInFun::Concat => {
                if let Some(type_args) = type_args {
                    if let Some(ty) = type_args.get(0) {
                        ResolvedType::new_vector(scopes.resolve_type(ty, self))
                    } else {
                        ResolvedType::new_vector(t_in_vector)
                    }
                } else {
                    ResolvedType::new_vector(t_in_vector)
                }
            }
            SpecBuildInFun::Contains => ResolvedType::new_build_in(BuildInType::Bool),
            SpecBuildInFun::IndexOf => ResolvedType::new_build_in(BuildInType::NumType),
            SpecBuildInFun::Range => ResolvedType::Range,
            SpecBuildInFun::InRange => ResolvedType::new_build_in(BuildInType::Bool),
            SpecBuildInFun::UpdateField => first_t,
            SpecBuildInFun::Old => first_t,
            SpecBuildInFun::TRACE => first_t,
        }
    }

    /// return type for `borrow_global`  ...
    pub(crate) fn get_move_build_in_call_type(
        &self,
        scopes: &Scopes,
        b: MoveBuildInFun,
        type_args: &Option<Vec<Type>>,
        _exprs: &Spanned<Vec<Exp>>, // TODO need use _expr.
    ) -> ResolvedType {
        match b {
            MoveBuildInFun::MoveTo => ResolvedType::new_unit(),
            MoveBuildInFun::MoveFrom => {
                if let Some(type_args) = type_args {
                    if let Some(ty) = type_args.get(0) {
                        let ty = scopes.resolve_type(ty, self);
                        ty
                    } else {
                        ResolvedType::UnKnown
                    }
                } else {
                    ResolvedType::UnKnown
                }
            }
            MoveBuildInFun::BorrowGlobalMut | MoveBuildInFun::BorrowGlobal => {
                if let Some(type_args) = type_args {
                    if let Some(ty) = type_args.get(0) {
                        let ty = scopes.resolve_type(ty, self);
                        ResolvedType::new_ref(b == MoveBuildInFun::BorrowGlobalMut, ty)
                    } else {
                        ResolvedType::UnKnown
                    }
                } else {
                    ResolvedType::UnKnown
                }
            }
            MoveBuildInFun::Exits => ResolvedType::new_build_in(BuildInType::Bool),
        }
    }

    /// Get A Type for expr if possible otherwise Unknown is return.
    pub(crate) fn get_expr_type(&self, expr: &Exp, scopes: &Scopes) -> ResolvedType {
        match &expr.value {
            Exp_::Value(ref x) => match &x.value {
                Value_::Address(_) => ResolvedType::new_build_in(BuildInType::Address),
                Value_::Num(x) => {
                    let b = BuildInType::num_types()
                        .into_iter()
                        .find(|b| x.as_str().ends_with(b.to_static_str()));
                    ResolvedType::new_build_in(b.unwrap_or(BuildInType::NumType))
                }
                Value_::Bool(_) => ResolvedType::new_build_in(BuildInType::Bool),
                Value_::HexString(_) => ResolvedType::new_build_in(BuildInType::NumType),
                Value_::ByteString(_) => ResolvedType::new_build_in(BuildInType::String),
            },
            Exp_::Move(x) | Exp_::Copy(x) => scopes.find_var_type(x.0.value),
            Exp_::Name(name, _ /*  TODO this is a error. */) => {
                let (item, _) = scopes.find_name_chain_item(name, self);
                return item.unwrap_or_default().to_type().unwrap_or_default();
            }
            Exp_::Call(name, is_macro, ref type_args, exprs) => {
                if *is_macro {
                    let c = MacroCall::from_chain(name).unwrap_or_default();
                    match c {
                        MacroCall::Assert => return ResolvedType::new_unit(),
                    }
                }
                match &name.value {
                    NameAccessChain_::One(name) => {
                        if name.value.as_str() == crate::modules_visitor::SPEC_DOMAIN {
                            return exprs
                                .value
                                .get(0)
                                .map(|x| self.get_expr_type(x, scopes))
                                .unwrap_or(ResolvedType::UnKnown);
                        }
                    }
                    _ => {}
                }
                let (item, _) = scopes.find_name_chain_item(name, self);
                match item.unwrap_or_default() {
                    Item::SpecBuildInFun(b) => {
                        return self.get_spec_build_in_call_type(scopes, b, type_args, exprs)
                    }
                    Item::MoveBuildInFun(b) => {
                        return self.get_move_build_in_call_type(scopes, b, type_args, exprs)
                    }
                    _ => {}
                }
                let (fun_type, _) = scopes.find_name_chain_item(name, self);
                let fun_type = fun_type.unwrap_or_default().to_type().unwrap_or_default();
                match &fun_type {
                    ResolvedType::Fun(x) => {
                        let type_parameters = &x.type_parameters;
                        let parameters = &x.parameters;
                        let type_args: Option<Vec<ResolvedType>> =
                            if let Some(type_args) = type_args {
                                Some(
                                    type_args
                                        .iter()
                                        .map(|x| scopes.resolve_type(x, self))
                                        .collect(),
                                )
                            } else {
                                None
                            };
                        let mut fun_type = fun_type.clone();
                        let mut types = HashMap::new();
                        if let Some(ref ts) = type_args {
                            for (para, args) in type_parameters.iter().zip(ts.iter()) {
                                types.insert(para.0.value, args.clone());
                            }
                        } else if type_parameters.len() > 0 {
                            //
                            let exprs_types: Vec<_> = exprs
                                .value
                                .iter()
                                .map(|e| self.get_expr_type(e, scopes))
                                .collect();
                            infer_type_parameter_on_expression(
                                &mut types,
                                &parameters.iter().map(|(_, t)| t.clone()).collect(),
                                &exprs_types,
                                scopes,
                            );
                        }
                        fun_type.bind_type_parameter(&types, scopes);
                        match &fun_type {
                            ResolvedType::Fun(x) => x.ret_type.as_ref().clone(),
                            _ => unreachable!(),
                        }
                    }

                    _ => ResolvedType::UnKnown,
                }
            }

            Exp_::Pack(name, type_args, fields) => {
                let (struct_ty, _) = scopes.find_name_chain_item(name, self);

                let struct_ty = struct_ty.unwrap_or_default().to_type().unwrap_or_default();
                let mut struct_ty = struct_ty.struct_ref_to_struct(scopes);
                let mut types = HashMap::new();
                let mut struct_ty = match &struct_ty {
                    ResolvedType::Struct(ItemStruct {
                        name: _,
                        type_parameters,
                        type_parameters_ins: _,
                        fields: struct_fields,
                        is_test: _is_test,
                    }) => {
                        let type_args: Option<Vec<ResolvedType>> =
                            if let Some(type_args) = type_args {
                                Some(
                                    type_args
                                        .iter()
                                        .map(|x| scopes.resolve_type(x, self))
                                        .collect(),
                                )
                            } else {
                                None
                            };
                        if type_args.is_none() {
                            // try infer on field.
                            let fields_exprs: Vec<_> = fields
                                .iter()
                                .map(|(field, expr)| {
                                    (field.clone(), self.get_expr_type(expr, scopes))
                                })
                                .collect();
                            let fields_exp_map = {
                                let mut m = HashMap::new();
                                fields_exprs.iter().for_each(|(f, t)| {
                                    m.insert(f.0.value, t.clone());
                                });
                                m
                            };
                            let parameters: Vec<_> =
                                struct_fields.iter().map(|(_, ty)| ty.clone()).collect();
                            let expression_types: Vec<_> = struct_fields
                                .iter()
                                .map(|(f, _)| {
                                    fields_exp_map
                                        .get(&f.0.value)
                                        .unwrap_or(&UNKNOWN_TYPE)
                                        .clone()
                                })
                                .collect();

                            infer_type_parameter_on_expression(
                                &mut types,
                                &parameters,
                                &expression_types,
                                scopes,
                            )
                        }
                        if let Some(ref ts) = type_args {
                            for (para, args) in type_parameters.iter().zip(ts.iter()) {
                                types.insert(para.name.value, args.clone());
                            }
                        }
                        struct_ty.bind_type_parameter(&types, scopes);

                        struct_ty
                    }
                    _ => UNKNOWN_TYPE.clone(),
                };
                match &mut struct_ty {
                    ResolvedType::Struct(ItemStruct {
                        name: _,
                        ref type_parameters,
                        ref mut type_parameters_ins,
                        fields: _,
                        is_test: _is_test,
                    }) => {
                        let ins: Vec<ResolvedType> = type_parameters
                            .iter()
                            .map(|x| types.get(&x.name.value).unwrap_or(&UNKNOWN_TYPE).clone())
                            .collect();
                        let _ = std::mem::replace(type_parameters_ins, ins);
                    }
                    _ => {}
                };

                struct_ty
            }
            Exp_::Vector(_, ty, exprs) => {
                let mut ty = if let Some(ty) = ty {
                    if let Some(ty) = ty.get(0) {
                        Some(scopes.resolve_type(ty, self))
                    } else {
                        None
                    }
                } else {
                    None
                };
                if option_ty_is_valid(&ty) {
                    for e in exprs.value.iter() {
                        let ty2 = self.get_expr_type(e, scopes);
                        if !ty2.is_unknown() {
                            ty = Some(ty2);
                            break;
                        }
                    }
                }
                ty.unwrap_or(ResolvedType::UnKnown)
            }
            Exp_::IfElse(_, then_, else_) => {
                let mut ty = self.get_expr_type(then_.as_ref(), scopes);
                if ty.is_err() {
                    if let Some(else_) = else_ {
                        ty = self.get_expr_type(else_, scopes);
                    }
                }
                ty
            }
            Exp_::While(_, _) | Exp_::Loop(_) => ResolvedType::new_unit(),
            Exp_::Block(b) => {
                if let Some(expr) = b.3.as_ref() {
                    scopes.enter_scope(|scopes| {
                        let mut visitor = DummyVisitor;
                        self.visit_block(&b, scopes, &mut visitor);
                        self.get_expr_type(expr, scopes)
                    })
                } else {
                    ResolvedType::new_unit()
                }
            }
            Exp_::Lambda(_, _) => {
                // TODO.
                ResolvedType::UnKnown
            }
            Exp_::Quant(_, _, _, _, _) => {
                // TODO.
                ResolvedType::UnKnown
            }
            Exp_::ExpList(e) => {
                let tys: Vec<_> = e.iter().map(|x| self.get_expr_type(x, scopes)).collect();
                ResolvedType::Multiple(tys)
            }

            Exp_::Unit => ResolvedType::new_unit(),
            Exp_::Assign(_, _) => ResolvedType::new_unit(),
            Exp_::Return(_) => ResolvedType::new_unit(),
            Exp_::Abort(_) => ResolvedType::new_unit(),
            Exp_::Break => ResolvedType::new_unit(),
            Exp_::Continue => ResolvedType::new_unit(),
            Exp_::Dereference(e) => {
                let ty = self.get_expr_type(e, scopes);
                match &ty {
                    ResolvedType::Ref(_, t) => t.as_ref().clone(),
                    _ => ty,
                }
            }
            Exp_::UnaryExp(_, e) => {
                let ty = self.get_expr_type(e, scopes);
                ty
            }
            Exp_::BinopExp(left, op, right) => {
                let left_ty = self.get_expr_type(left, scopes);
                let right_ty = self.get_expr_type(right, scopes);
                let binary_type = || {
                    if !left_ty.is_err() {
                        left_ty.clone()
                    } else if !right_ty.is_err() {
                        right_ty.clone()
                    } else {
                        ResolvedType::new_build_in(BuildInType::NumType)
                    }
                };
                match op.value {
                    BinOp_::Add => binary_type(),
                    BinOp_::Sub => binary_type(),
                    BinOp_::Mul => binary_type(),
                    BinOp_::Mod => binary_type(),
                    BinOp_::Div => binary_type(),
                    BinOp_::BitOr => binary_type(),
                    BinOp_::BitAnd => binary_type(),
                    BinOp_::Xor => binary_type(),
                    BinOp_::Shl => binary_type(),
                    BinOp_::Shr => binary_type(),
                    BinOp_::Range => ResolvedType::Range,
                    BinOp_::Implies => ResolvedType::new_unit(),
                    BinOp_::Iff => ResolvedType::new_unit(),
                    BinOp_::And => ResolvedType::new_build_in(BuildInType::Bool),
                    BinOp_::Or => ResolvedType::new_build_in(BuildInType::Bool),
                    BinOp_::Eq
                    | BinOp_::Neq
                    | BinOp_::Lt
                    | BinOp_::Gt
                    | BinOp_::Le
                    | BinOp_::Ge => ResolvedType::new_build_in(BuildInType::Bool),
                }
            }
            Exp_::Borrow(is_mut, e) => {
                let ty = self.get_expr_type(e, scopes);
                ResolvedType::new_ref(*is_mut, ty)
            }
            Exp_::Dot(e, name) => {
                let ty = self.get_expr_type(e, scopes);
                if let Some(field) = ty.find_filed_by_name(name.value) {
                    field.1.clone()
                } else {
                    ty
                }
            }
            Exp_::Index(e, _index) => {
                let ty = self.get_expr_type(e, scopes);
                let ty = match &ty {
                    ResolvedType::Ref(_, x) => x.as_ref().clone(),
                    _ => ty,
                };
                match &ty {
                    ResolvedType::Vec(x) => x.as_ref().clone(),
                    _ => ty,
                }
            }

            Exp_::Cast(_, ty) => {
                let ty = scopes.resolve_type(ty, self);
                ty
            }
            Exp_::Annotate(_, ty) => scopes.resolve_type(ty, self),
            Exp_::Spec(_) => ResolvedType::new_unit(),
            Exp_::UnresolvedError => {
                // Nothings. didn't know what to do.
                ResolvedType::UnKnown
            }
        }
    }

    pub(crate) fn visit_struct_tparam(
        &self,
        t: &StructTypeParameter,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        self.visit_tparam(&(t.name.clone(), t.constraints.clone()), scopes, visitor);
    }

    pub(crate) fn visit_tparam(
        &self,
        t: &(Name, Vec<Ability>),
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        let (name, v) = t;
        let item = ItemOrAccess::Item(Item::TParam(name.clone(), v.clone()));
        visitor.handle_item_or_access(self, scopes, &item);
        if visitor.finished() {
            return;
        }
        // Enter this.
        scopes.enter_types(self, name.value, item);
    }
    pub(crate) fn visit_signature(
        &self,
        signature: &FunctionSignature,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        for t in signature.type_parameters.iter() {
            self.visit_tparam(t, scopes, visitor);
            if visitor.finished() {
                return;
            }
        }
        for (v, t) in signature.parameters.iter() {
            self.visit_type_apply(t, scopes, visitor);
            let t = scopes.resolve_type(t, self);
            let item = ItemOrAccess::Item(Item::Parameter(v.clone(), t));
            // found
            visitor.handle_item_or_access(self, scopes, &item);
            if visitor.finished() {
                return;
            }
            scopes.enter_item(self, v.value(), item)
        }
        self.visit_type_apply(&signature.return_type, scopes, visitor);
    }
}

/// Check is option is Some and ResolvedType is not unknown and not a error.
fn option_ty_is_valid(x: &Option<ResolvedType>) -> bool {
    if let Some(ref x) = x {
        !x.is_err()
    } else {
        false
    }
}

#[derive(Debug, Clone, Default)]
pub struct SourceDefs {
    pub(crate) sources: HashMap<
        PathBuf, /*  file path  xxxx/abc.move  */
        Vec<move_compiler::parser::ast::Definition>,
    >,

    /*
        TODO tests.
    */
    pub(crate) tests: HashMap<
        PathBuf, /*  file path  xxxx/abc.move  */
        Vec<move_compiler::parser::ast::Definition>,
    >,

    pub(crate) scripts: HashMap<
        PathBuf, /*  file path  xxxx/abc.move  */
        Vec<move_compiler::parser::ast::Definition>,
    >,
}

pub(crate) const UNKNOWN_TYPE: ResolvedType = ResolvedType::UnKnown;

pub(crate) fn get_name_from_value(v: &Value) -> Option<&Name> {
    match &v.value {
        Value_::Address(ref x) => match &x.value {
            LeadingNameAccess_::AnonymousAddress(_) => None,
            LeadingNameAccess_::Name(ref name) => Some(name),
        },
        _ => None,
    }
}

pub(crate) fn infer_type_parameter_on_expression(
    ret: &mut HashMap<Symbol /*  name like T or ... */, ResolvedType>,
    parameters: &Vec<ResolvedType>,
    expression_types: &Vec<ResolvedType>,
    scopes: &Scopes,
) {
    for (p, expr_type) in parameters.iter().zip(expression_types.iter()) {
        bind(ret, &p, expr_type, scopes);
    }
    fn bind(
        ret: &mut HashMap<Symbol, ResolvedType>,
        // may be a type have type parameter.
        parameter_type: &ResolvedType,
        // a type that is certain.
        expr_type: &ResolvedType,
        scopes: &Scopes,
    ) {
        match &parameter_type {
            ResolvedType::UnKnown => {}
            ResolvedType::Struct(ItemStruct {
                fields,
                type_parameters_ins,
                ..
            }) => match &expr_type {
                ResolvedType::Struct(ItemStruct {
                    fields: fields2,
                    type_parameters_ins: type_parameters_ins2,
                    ..
                }) => {
                    type_parameters_ins
                        .iter()
                        .zip(type_parameters_ins2.iter())
                        .for_each(|(x, y)| {
                            bind(ret, x, y, scopes);
                        });
                    for (l, r) in fields.iter().zip(fields2.iter()) {
                        bind(ret, &l.1, &r.1, scopes);
                    }
                }
                ResolvedType::StructRef(_, _) => {
                    let expr_type = expr_type.clone().struct_ref_to_struct(scopes);
                    match &expr_type {
                        ResolvedType::Struct(_) => bind(ret, parameter_type, &expr_type, scopes),
                        _ => unreachable!(),
                    }
                }
                _ => {}
            },
            ResolvedType::BuildInType(_) => {}
            ResolvedType::TParam(name, _) => {
                ret.insert(name.value, expr_type.clone());
            }
            ResolvedType::Ref(_, l) => match &expr_type {
                ResolvedType::Ref(_, r) => bind(ret, l.as_ref(), r.as_ref(), scopes),
                _ => {}
            },
            ResolvedType::Unit => {}
            ResolvedType::Multiple(x) => match &expr_type {
                ResolvedType::Multiple(y) => {
                    for (index, l) in x.iter().enumerate() {
                        if let Some(r) = y.get(index) {
                            bind(ret, l, r, scopes);
                        } else {
                            break;
                        }
                    }
                }
                _ => {}
            },
            //  function is not expression
            ResolvedType::Fun(_) => {}
            ResolvedType::Vec(x) => match &expr_type {
                ResolvedType::Vec(y) => {
                    bind(ret, x.as_ref(), y.as_ref(), scopes);
                }
                _ => {}
            },

            ResolvedType::StructRef(_, _) => {
                let parameter_type = parameter_type.clone().struct_ref_to_struct(scopes);
                match &parameter_type {
                    ResolvedType::Struct(_) => {
                        bind(ret, &parameter_type, expr_type, scopes);
                    }
                    _ => {
                        unreachable!("");
                    }
                }
            }
            ResolvedType::Range => {}
        }
    }
}

pub trait ConvertLoc {
    fn convert_file_hash_filepath(&self, hash: &FileHash) -> Option<PathBuf>;
    fn convert_loc_range(&self, loc: &Loc) -> Option<FileRange>;
}

impl ConvertLoc for Project {
    fn convert_file_hash_filepath(&self, hash: &FileHash) -> Option<PathBuf> {
        self.hash_file
            .as_ref()
            .borrow()
            .get_path(hash)
            .map(|x| x.clone())
    }
    fn convert_loc_range(&self, loc: &Loc) -> Option<FileRange> {
        self.convert_file_hash_filepath(&loc.file_hash())
            .map(|file| {
                self.file_line_mapping
                    .as_ref()
                    .borrow()
                    .translate(&file, loc.start(), loc.end())
            })
            .flatten()
    }
}

pub trait Name2Addr {
    fn name_2_addr(&self, name: Symbol) -> AccountAddress;
}
impl Name2Addr for Project {
    fn name_2_addr(&self, name: Symbol) -> AccountAddress {
        self.name_to_addr_impl(name)
    }
}

/// Scoped analyze based visitor.
pub trait ScopeVisitor: std::fmt::Display {
    /// Handle this item.
    fn handle_item_or_access(
        &mut self,
        services: &dyn HandleItemService,
        scopes: &Scopes,
        item: &ItemOrAccess,
    );

    /// Need not visit function or spec body.
    /// Sometimes you want visit function body But not all the function Body.
    fn function_or_spec_body_should_visit(&self, range: &FileRange) -> bool;
    fn visit_fun_or_spec_body(&self) -> bool;

    /// Visitor should finished.
    fn finished(&self) -> bool;
}

pub trait HandleItemService: ConvertLoc + GetAllAddrs + Name2Addr {}
impl HandleItemService for Project {}

#[allow(dead_code)]
pub(crate) struct Ending {
    pub(crate) msg: String,
}
impl Drop for Ending {
    fn drop(&mut self) {
        log::info!("ending {}", self.msg.as_str());
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct DummyVisitor;

impl ScopeVisitor for DummyVisitor {
    fn handle_item_or_access(
        &mut self,
        _services: &dyn HandleItemService,
        _scopes: &Scopes,
        _item: &ItemOrAccess,
    ) {
    }
    fn function_or_spec_body_should_visit(&self, _range: &FileRange) -> bool {
        false
    }
    fn finished(&self) -> bool {
        false
    }
    fn visit_fun_or_spec_body(&self) -> bool {
        false
    }
}

impl std::fmt::Display for DummyVisitor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

lazy_static! {
    pub(crate) static ref ERR_ADDRESS: AccountAddress = AccountAddress::random();
}

pub trait GetAllAddrs {
    fn get_all_addrs(&self, scopes: &Scopes) -> HashSet<AddressSpace>;
}

impl GetAllAddrs for Project {
    fn get_all_addrs(&self, scopes: &Scopes) -> HashSet<AddressSpace> {
        let mut addrs: HashSet<AddressSpace> = HashSet::new();
        let empty = Default::default();
        let empty2 = Default::default();
        for x in self.manifests.iter() {
            for (name, addr) in x.addresses.as_ref().unwrap_or(&empty).iter() {
                addrs.insert(AddressSpace::from(name.clone()));
                if let Some(addr) = addr {
                    addrs.insert(AddressSpace::from(addr.clone()));
                }
            }
            for (name, addr) in x.dev_address_assignments.as_ref().unwrap_or(&empty2).iter() {
                addrs.insert(AddressSpace::from(name.clone()));
                addrs.insert(AddressSpace::from(addr.clone()));
            }
        }
        scopes.visit_address(|addresss| {
            addresss.address.keys().for_each(|addr| {
                addrs.insert(AddressSpace::from(addr.clone()));
            })
        });
        addrs
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum AddressSpace {
    Addr(AccountAddress),
    Name(Symbol),
}

impl From<Symbol> for AddressSpace {
    fn from(x: Symbol) -> Self {
        Self::Name(x)
    }
}

impl From<AccountAddress> for AddressSpace {
    fn from(x: AccountAddress) -> Self {
        Self::Addr(x)
    }
}

pub(crate) fn attributes_has_test(x: &Vec<Attributes>) -> IsFunTest {
    use IsFunTest::*;
    let mut is = No;
    x.iter().for_each(|x| {
        x.value.iter().for_each(|x| match &x.value {
            Attribute_::Name(name) => match name.value.as_str() {
                "test" => is = Test,
                "test_only" => is = TestOnly,
                _ => {}
            },
            Attribute_::Assigned(_, _) => {}
            Attribute_::Parameterized(name, _) => match name.value.as_str() {
                "test" => is = Test,
                "test_only" => is = TestOnly,
                _ => {}
            },
        })
    });
    is
}

use move_command_line_common::env::MOVE_HOME;
use move_package::source_package::parsed_manifest::*;

/// The local location of the repository containing the dependency of kind `kind` (and potentially
/// other, related dependencies).
fn repository_path(kind: &DependencyKind) -> PathBuf {
    match kind {
        DependencyKind::Local(path) => path.clone(),

        // Downloaded packages are of the form <sanitized_git_url>_<rev_name>
        DependencyKind::Git(GitInfo {
            git_url,
            git_rev,
            subdir: _,
        }) => [
            &*MOVE_HOME,
            &format!(
                "{}_{}",
                url_to_file_name(git_url.as_str()),
                git_rev.replace('/', "__"),
            ),
        ]
        .iter()
        .collect(),

        // Downloaded packages are of the form <sanitized_node_url>_<address>_<package>
        DependencyKind::Custom(CustomDepInfo {
            node_url,
            package_address,
            package_name,
        }) => [
            &*MOVE_HOME,
            &format!(
                "{}_{}_{}",
                url_to_file_name(node_url.as_str()),
                package_address.as_str(),
                package_name.as_str(),
            ),
        ]
        .iter()
        .collect(),
    }
}

fn url_to_file_name(url: &str) -> String {
    regex::Regex::new(r"/|:|\.|@")
        .unwrap()
        .replace_all(url, "_")
        .to_string()
}
