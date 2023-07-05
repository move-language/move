// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use super::{item::*, project_context::*, types::*, utils::*};
use crate::context::MultiProject;
use anyhow::{Ok, Result};
use move_package::source_package::parsed_manifest::{CustomDepInfo, DependencyKind, GitInfo};
use once_cell::sync::Lazy;

use move_command_line_common::files::FileHash;
use move_compiler::{
    parser::ast::{Definition, *},
    shared::{Identifier, *},
};
use move_core_types::account_address::*;

use move_ir_types::location::*;
use move_package::source_package::{layout::SourcePackageLayout, manifest_parser::*};
use move_symbol_pool::Symbol;
use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::{HashMap, HashSet},
    fs,
    path::Path,
    time::SystemTime,
};

use std::{hash::Hash, path::PathBuf, rc::Rc};
use walkdir::WalkDir;

pub struct Project {
    pub(crate) modules: HashMap<
        PathBuf, // manifest path.
        Rc<RefCell<SourceDefs>>,
    >,
    pub(crate) manifests: Vec<move_package::source_package::parsed_manifest::SourceManifest>,
    pub(crate) hash_file: Rc<RefCell<PathBufHashMap>>,
    pub(crate) file_line_mapping: Rc<RefCell<FileLineMapping>>,
    pub(crate) manifest_paths: Vec<PathBuf>,
    pub(crate) project_context: ProjectContext,
    pub(crate) manifest_not_exists: HashSet<PathBuf>,
    pub(crate) manifest_load_failures: HashSet<PathBuf>,
    pub(crate) manifest_mod_time: HashMap<PathBuf, Option<SystemTime>>,
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
    pub fn load_ok(&self) -> bool {
        self.manifest_not_exists.is_empty() && self.manifest_load_failures.is_empty()
    }
}

impl Project {
    pub fn new(
        root_dir: impl Into<PathBuf>,
        multi: &mut MultiProject,
        report_err: impl FnMut(String) + Clone,
    ) -> Result<Self> {
        let working_dir = root_dir.into();
        log::info!("scan modules at {:?}", &working_dir);
        let mut modules = Self {
            modules: Default::default(),
            manifests: Default::default(),
            hash_file: multi.hash_file.clone(),
            file_line_mapping: multi.file_line_mapping.clone(),
            manifest_paths: Default::default(),
            project_context: ProjectContext::new(),
            manifest_not_exists: Default::default(),
            manifest_load_failures: Default::default(),
            manifest_mod_time: Default::default(),
        };
        modules.load_project(&working_dir, multi, report_err)?;
        let mut dummy = DummyHandler;
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
            let x = VecDefAstProvider::new(defs, self, layout);
            x.with_module(|addr, d| {
                self.project_context
                    .delete_module_items(addr, d.name.value(), d.is_spec_module);
            });
        };
        // Update defs.
        let mut dummy = DummyHandler;
        let _ = self.run_visitor_for_file(&mut dummy, file_path, true);
    }

    /// Load a Move.toml project.
    pub(crate) fn load_project(
        &mut self,
        manifest_path: &Path,
        multi: &mut MultiProject,
        mut report_err: impl FnMut(String) + Clone,
    ) -> Result<()> {
        let manifest_path = normal_path(manifest_path);
        if self.modules.get(&manifest_path).is_some() {
            log::info!("manifest '{:?}' loaded before skipped.", &manifest_path);
            return Ok(());
        }
        self.manifest_paths.push(manifest_path.clone());
        eprintln!("load manifest file at {:?}", &manifest_path);
        if let Some(x) = multi.asts.get(&manifest_path) {
            self.modules.insert(manifest_path.clone(), x.clone());
        } else {
            let d: Rc<RefCell<SourceDefs>> = Default::default();
            self.modules.insert(manifest_path.clone(), d.clone());
            multi.asts.insert(manifest_path.clone(), d);
            self.load_layout_files(&manifest_path, SourcePackageLayout::Sources);
            self.load_layout_files(&manifest_path, SourcePackageLayout::Tests);
            self.load_layout_files(&manifest_path, SourcePackageLayout::Scripts);
        }
        if !manifest_path.exists() {
            self.manifest_not_exists.insert(manifest_path);
            return anyhow::Result::Ok(());
        }
        {
            let mut file = manifest_path.clone();
            file.push(PROJECT_FILE_NAME);

            self.manifest_mod_time
                .insert(file.clone(), file_modify_time(file.as_path()));
        }

        let manifest = match parse_move_manifest_from_file(&manifest_path) {
            std::result::Result::Ok(x) => x,
            std::result::Result::Err(err) => {
                report_err(format!(
                    "parse manifest '{:?} 'failed.\n addr must exactly 32 length or start with '0x' like '0x2'\n{:?}",
                    manifest_path,
                    err
                ));
                log::error!("parse_move_manifest_from_file failed,err:{:?}", err);
                self.manifest_load_failures.insert(manifest_path.clone());
                return anyhow::Result::Ok(());
            }
        };
        self.manifests.push(manifest.clone());
        // load depends.
        for (dep_name, de) in manifest
            .dependencies
            .iter()
            .chain(manifest.dev_dependencies.iter())
        {
            let move_home = Lazy::new(|| {
                std::env::var("MOVE_HOME").unwrap_or_else(|_| {
                    format!(
                        "{}/.move",
                        dirs_next::home_dir()
                            .expect("user's home directory not found")
                            .to_str()
                            .unwrap()
                    )
                })
            });

            let repository_path = |kind: &DependencyKind| -> PathBuf {
                match kind {
                    DependencyKind::Local(path) => path.clone(),

                    // Downloaded packages are of the form <sanitized_git_url>_<rev_name>
                    DependencyKind::Git(GitInfo {
                        git_url,
                        git_rev,
                        subdir: _,
                    }) => [
                        &*move_home,
                        &format!(
                            "{}_{}",
                            regex::Regex::new(r"/|:|\.|@")
                                .unwrap()
                                .replace_all(git_url.as_str(), "_"),
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
                        subdir: _,
                    }) => [
                        &*move_home,
                        &format!(
                            "{}_{}_{}",
                            regex::Regex::new(r"/|:|\.|@")
                                .unwrap()
                                .replace_all(node_url.as_str(), "_"),
                            package_address.as_str(),
                            package_name.as_str(),
                        ),
                    ]
                    .iter()
                    .collect(),
                }
            };

            let local_path = |kind: &DependencyKind| -> PathBuf {
                let mut repo_path = repository_path(kind);

                if let DependencyKind::Git(GitInfo { subdir, .. })
                | DependencyKind::Custom(CustomDepInfo { subdir, .. }) = kind
                {
                    repo_path.push(subdir);
                }

                repo_path
            };

            use move_package::source_package::parsed_manifest::Dependency;
            let de_path = match &de {
                Dependency::External(_) => {
                    // TODO
                    continue;
                }
                Dependency::Internal(x) => local_path(&x.kind),
            };
            let p = path_concat(manifest_path.as_path(), &de_path);
            log::info!(
                "load dependency for '{:?}' dep_name '{}'",
                &manifest_path,
                dep_name
            );
            self.load_project(&p, multi, report_err.clone())?;
        }
        Ok(())
    }

    /// Load move files locate in sources and tests ...
    pub(crate) fn load_layout_files(&mut self, manifest_path: &PathBuf, kind: SourcePackageLayout) {
        use super::syntax::parse_file_string;
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
                if file
                    .file_name()
                    .to_str()
                    .map(|x| x.starts_with('.'))
                    .unwrap_or(false)
                {
                    continue;
                }
                let file_content = fs::read_to_string(file.path())
                    .unwrap_or_else(|_| panic!("'{:?}' can't read_to_string", file.path()));
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
                        .insert(file.path().to_path_buf().clone(), defs);
                } else if kind == SourcePackageLayout::Tests {
                    self.modules
                        .get_mut(manifest_path)
                        .unwrap()
                        .as_ref()
                        .borrow_mut()
                        .tests
                        .insert(file.path().to_path_buf().clone(), defs);
                } else {
                    self.modules
                        .get_mut(manifest_path)
                        .unwrap()
                        .as_ref()
                        .borrow_mut()
                        .scripts
                        .insert(file.path().to_path_buf().clone(), defs);
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
                if let Some(x) = x.get(&name) {
                    return *x;
                }
            }
            if let Some(ref x) = x.addresses {
                if let Some(Some(x)) = x.get(&name) {
                    return *x;
                }
            }
        }
        *ERR_ADDRESS
    }

    pub(crate) fn get_spec_build_in_call_type(
        &self,
        project_context: &ProjectContext,
        b: SpecBuildInFun,
        type_args: &Option<Vec<Type>>,
        exprs: &Spanned<Vec<Exp>>, // TODO need use _expr.
    ) -> ResolvedType {
        let exprs_types: Vec<_> = exprs
            .value
            .iter()
            .map(|e| self.get_expr_type(e, project_context))
            .collect();
        // vec<T>(x): vector<T> returns a singleton vector.
        // A lot of those build in function.
        let t_in_vector = exprs_types
            .get(0)
            .cloned()
            .map(|x| match x {
                ResolvedType::Vec(x) => x.as_ref().clone(),
                _ => x,
            })
            .unwrap_or(ResolvedType::UnKnown);
        let first_t = exprs_types.get(0).cloned().unwrap_or(ResolvedType::UnKnown);

        match b {
            SpecBuildInFun::Exists => ResolvedType::new_build_in(BuildInType::Bool),
            SpecBuildInFun::Global => {
                if let Some(type_args) = type_args {
                    if let Some(ty) = type_args.get(0) {
                        project_context.resolve_type(ty, self)
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
                        ResolvedType::new_vector(project_context.resolve_type(ty, self))
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
                        ResolvedType::new_vector(project_context.resolve_type(ty, self))
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
                        ResolvedType::new_vector(project_context.resolve_type(ty, self))
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
        project_context: &ProjectContext,
        b: MoveBuildInFun,
        type_args: &Option<Vec<Type>>,
        _exprs: &Spanned<Vec<Exp>>, // TODO need use _expr.
    ) -> ResolvedType {
        match b {
            MoveBuildInFun::MoveTo => ResolvedType::new_unit(),
            MoveBuildInFun::MoveFrom => {
                if let Some(type_args) = type_args {
                    if let Some(ty) = type_args.get(0) {
                        project_context.resolve_type(ty, self)
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
                        let ty = project_context.resolve_type(ty, self);
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

    pub(crate) fn initialize_fun_call(
        &self,
        project_context: &ProjectContext,
        name: &NameAccessChain,
        type_args: &Option<Vec<Type>>,
        exprs: &Spanned<Vec<Exp>>,
    ) -> Option<ResolvedType> {
        let (fun_type, _) = project_context.find_name_chain_item(name, self);
        let fun_type = fun_type.unwrap_or_default().to_type().unwrap_or_default();
        match &fun_type {
            ResolvedType::Lambda { .. } => Some(fun_type),
            ResolvedType::Fun(x) => {
                let type_parameters = &x.type_parameters;
                let parameters = &x.parameters;
                let type_args: Option<Vec<ResolvedType>> = type_args.as_ref().map(|type_args| {
                    type_args
                        .iter()
                        .map(|x| project_context.resolve_type(x, self))
                        .collect()
                });
                let mut fun_type = fun_type.clone();
                let mut types = HashMap::new();
                if let Some(ref ts) = type_args {
                    for (para, args) in type_parameters.iter().zip(ts.iter()) {
                        types.insert(para.0.value, args.clone());
                    }
                } else if !type_parameters.is_empty() {
                    let exprs_types: Vec<_> = exprs
                        .value
                        .iter()
                        .map(|e| self.get_expr_type(e, project_context))
                        .collect();
                    infer_type_parameter_on_expression(
                        &mut types,
                        &parameters
                            .iter()
                            .map(|(_, t)| t.clone())
                            .collect::<Vec<_>>(),
                        &exprs_types,
                    );
                }
                fun_type.bind_type_parameter(&types);
                Some(fun_type)
            }
            _ => None,
        }
    }

    /// Get A Type for exprme if possible otherwise Unknown is return.
    pub(crate) fn get_expr_type(
        &self,
        expr: &Exp,
        project_context: &ProjectContext,
    ) -> ResolvedType {
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
            Exp_::Move(x) | Exp_::Copy(x) => project_context.find_var_type(x.0.value),
            Exp_::Name(name, _ /*  TODO this is a error. */) => {
                let (item, _) = project_context.find_name_chain_item(name, self);
                item.unwrap_or_default().to_type().unwrap_or_default()
            }
            Exp_::Call(name, is_macro, ref type_args, exprs) => {
                if *is_macro {
                    let c = MacroCall::from_chain(name).unwrap_or_default();
                    match c {
                        MacroCall::Assert => return ResolvedType::new_unit(),
                    }
                }
                if let NameAccessChain_::One(name) = &name.value {
                    if name.value.as_str() == crate::project_visitor::SPEC_DOMAIN {
                        return exprs
                            .value
                            .get(0)
                            .map(|x| self.get_expr_type(x, project_context))
                            .unwrap_or(ResolvedType::UnKnown);
                    }
                }
                let (item, _) = project_context.find_name_chain_item(name, self);
                match item.unwrap_or_default() {
                    Item::SpecBuildInFun(b) => {
                        return self.get_spec_build_in_call_type(
                            project_context,
                            b,
                            type_args,
                            exprs,
                        )
                    }
                    Item::MoveBuildInFun(b) => {
                        return self.get_move_build_in_call_type(
                            project_context,
                            b,
                            type_args,
                            exprs,
                        )
                    }
                    _ => {}
                }
                let ty = self.initialize_fun_call(project_context, name, type_args, exprs);
                match ty.unwrap_or_default() {
                    ResolvedType::Fun(x) => x.ret_type.as_ref().clone(),
                    ResolvedType::Lambda { ret_ty, .. } => ret_ty.as_ref().clone(),
                    _ => ResolvedType::UnKnown,
                }
            }

            Exp_::Pack(name, type_args, fields) => {
                let (struct_item, _) = project_context.find_name_chain_item(name, self);
                let struct_item = match struct_item {
                    Some(x) => match x {
                        Item::Struct(x) => x,
                        Item::StructNameRef(_) => {
                            unreachable!()
                        }
                        _ => {
                            return ResolvedType::UnKnown;
                        }
                    },
                    None => return ResolvedType::UnKnown,
                };

                let mut types = HashMap::new();
                let type_args: Option<Vec<ResolvedType>> = type_args.as_ref().map(|type_args| {
                    type_args
                        .iter()
                        .map(|x| project_context.resolve_type(x, self))
                        .collect()
                });

                if type_args.is_none() {
                    // try infer on field.
                    let fields_exprs: Vec<_> = fields
                        .iter()
                        .map(|(field, expr)| (*field, self.get_expr_type(expr, project_context)))
                        .collect();
                    let fields_exp_map = {
                        let mut m = HashMap::new();
                        fields_exprs.iter().for_each(|(f, t)| {
                            m.insert(f.0.value, t.clone());
                        });
                        m
                    };
                    let parameters: Vec<_> = struct_item
                        .fields
                        .iter()
                        .map(|(_, ty)| ty.clone())
                        .collect();
                    let expression_types: Vec<_> = fields
                        .iter()
                        .map(|(f, _)| {
                            fields_exp_map
                                .get(&f.0.value)
                                .unwrap_or(&UNKNOWN_TYPE)
                                .clone()
                        })
                        .collect();
                    infer_type_parameter_on_expression(&mut types, &parameters, &expression_types)
                }

                if let Some(ref ts) = type_args {
                    for (para, args) in struct_item.type_parameters.iter().zip(ts.iter()) {
                        types.insert(para.name.value, args.clone());
                    }
                }
                ResolvedType::Struct(
                    struct_item.to_struct_ref(),
                    struct_item
                        .type_parameters
                        .iter()
                        .map(|x| types.get(&x.name.value).cloned().unwrap_or_default())
                        .collect(),
                )
            }
            Exp_::Vector(_, ty, exprs) => {
                let mut ty = if let Some(ty) = ty {
                    ty.get(0).map(|ty| project_context.resolve_type(ty, self))
                } else {
                    None
                };
                if !option_ty_is_valid(&ty) {
                    for e in exprs.value.iter() {
                        let ty2 = self.get_expr_type(e, project_context);
                        if !ty2.is_err() {
                            ty = Some(ty2);
                            break;
                        }
                    }
                }
                ResolvedType::new_vector(ty.unwrap_or_default())
            }
            Exp_::IfElse(_, then_, else_) => {
                let mut ty = self.get_expr_type(then_.as_ref(), project_context);
                if ty.is_err() {
                    if let Some(else_) = else_ {
                        ty = self.get_expr_type(else_, project_context);
                    }
                }
                ty
            }
            Exp_::While(_, _) | Exp_::Loop(_) => ResolvedType::new_unit(),
            Exp_::Block(b) => {
                if let Some(expr) = b.3.as_ref() {
                    project_context.enter_scope(|scopes| {
                        let mut handler = DummyHandler;
                        self.visit_block(b, scopes, &mut handler);
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
            Exp_::Quant(_, _, _, _, _) => ResolvedType::UnKnown,
            Exp_::ExpList(e) => {
                let tys: Vec<_> = e
                    .iter()
                    .map(|x| self.get_expr_type(x, project_context))
                    .collect();
                ResolvedType::Multiple(tys)
            }

            Exp_::Unit => ResolvedType::new_unit(),
            Exp_::Assign(_, _) => ResolvedType::new_unit(),
            Exp_::Return(_) => ResolvedType::new_unit(),
            Exp_::Abort(_) => ResolvedType::new_unit(),
            Exp_::Break => ResolvedType::new_unit(),
            Exp_::Continue => ResolvedType::new_unit(),
            Exp_::Dereference(e) => {
                let ty = self.get_expr_type(e, project_context);
                match &ty {
                    ResolvedType::Ref(_, t) => t.as_ref().clone(),
                    _ => ty,
                }
            }
            Exp_::UnaryExp(_, e) => self.get_expr_type(e, project_context),
            Exp_::BinopExp(left, op, right) => {
                let left_ty = self.get_expr_type(left, project_context);
                let right_ty = self.get_expr_type(right, project_context);
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
                let ty = self.get_expr_type(e, project_context);
                ResolvedType::new_ref(*is_mut, ty)
            }
            Exp_::Dot(_, _) => ResolvedType::UnKnown,

            Exp_::Index(e, _index) => {
                let ty = self.get_expr_type(e, project_context);
                let ty = match &ty {
                    ResolvedType::Ref(_, x) => x.as_ref().clone(),
                    _ => ty,
                };
                match &ty {
                    ResolvedType::Vec(x) => x.as_ref().clone(),
                    _ => ty,
                }
            }

            Exp_::Cast(_, ty) => project_context.resolve_type(ty, self),
            Exp_::Annotate(_, ty) => project_context.resolve_type(ty, self),
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
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        self.visit_tparam(&(t.name, t.constraints.clone()), project_context, visitor);
    }

    pub(crate) fn visit_tparam(
        &self,
        t: &(Name, Vec<Ability>),
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        let (name, v) = t;
        let item = ItemOrAccess::Item(Item::TParam(*name, v.clone()));
        visitor.handle_item_or_access(self, project_context, &item);
        if visitor.finished() {
            return;
        }
        // Enter this.
        project_context.enter_types(self, name.value, item);
    }
    pub(crate) fn visit_signature(
        &self,
        signature: &FunctionSignature,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        for t in signature.type_parameters.iter() {
            self.visit_tparam(t, project_context, visitor);
            if visitor.finished() {
                return;
            }
        }
        for (v, t) in signature.parameters.iter() {
            self.visit_type_apply(t, project_context, visitor);
            let t = project_context.resolve_type(t, self);
            let item = ItemOrAccess::Item(Item::Parameter(*v, t));
            // found
            visitor.handle_item_or_access(self, project_context, &item);
            if visitor.finished() {
                return;
            }
            project_context.enter_item(self, v.value(), item)
        }
        self.visit_type_apply(&signature.return_type, project_context, visitor);
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
    pub(crate) sources: HashMap<PathBuf, Vec<move_compiler::parser::ast::Definition>>,
    pub(crate) tests: HashMap<PathBuf, Vec<move_compiler::parser::ast::Definition>>,
    pub(crate) scripts: HashMap<PathBuf, Vec<move_compiler::parser::ast::Definition>>,
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
    parameters: &[ResolvedType],
    expression_types: &[ResolvedType],
) {
    for (p, expr_type) in parameters.iter().zip(expression_types.iter()) {
        bind(ret, p, expr_type);
    }
    fn bind(
        ret: &mut HashMap<Symbol, ResolvedType>,
        // may be a type have type parameter.
        parameter_type: &ResolvedType,
        // a type that is certain.
        expr_type: &ResolvedType,
    ) {
        match &parameter_type {
            ResolvedType::UnKnown => {}
            ResolvedType::BuildInType(_) => {}
            ResolvedType::TParam(name, _) => {
                ret.insert(name.value, expr_type.clone());
            }
            ResolvedType::Ref(_, l) => {
                if let ResolvedType::Ref(_, r) = expr_type {
                    bind(ret, l.as_ref(), r.as_ref())
                }
            }
            ResolvedType::Unit => {}
            ResolvedType::Multiple(x) => {
                if let ResolvedType::Multiple(y) = expr_type {
                    for (index, l) in x.iter().enumerate() {
                        if let Some(r) = y.get(index) {
                            bind(ret, l, r);
                        } else {
                            break;
                        }
                    }
                }
            }
            //  function is not expression
            ResolvedType::Fun(_) => {}
            ResolvedType::Vec(x) => {
                if let ResolvedType::Vec(y) = expr_type {
                    bind(ret, x.as_ref(), y.as_ref());
                }
            }
            ResolvedType::Struct(_, ptys) => {
                if let ResolvedType::Struct(_, etypes) = expr_type {
                    for (p, e) in ptys.iter().zip(etypes.iter()) {
                        bind(ret, p, e);
                    }
                }
            }
            ResolvedType::Range => {}
            ResolvedType::Lambda { args, ret_ty } => {
                if let ResolvedType::Lambda {
                    args: args2,
                    ret_ty: ret_ty2,
                } = expr_type
                {
                    for (a1, a2) in args.iter().zip(args2.iter()) {
                        bind(ret, a1, a2);
                    }
                    bind(ret, ret_ty.as_ref(), ret_ty2.as_ref());
                }
            }
        }
    }
}

pub trait ConvertLoc {
    fn convert_file_hash_filepath(&self, hash: &FileHash) -> Option<PathBuf>;
    fn convert_loc_range(&self, loc: &Loc) -> Option<FileRange>;
}

impl_convert_loc!(Project);

pub trait Name2Addr {
    fn name_2_addr(&self, name: Symbol) -> AccountAddress;
}
impl Name2Addr for Project {
    fn name_2_addr(&self, name: Symbol) -> AccountAddress {
        self.name_to_addr_impl(name)
    }
}

/// Handler a `ItemOrAccess` producced By `Project`.
pub trait ItemOrAccessHandler: std::fmt::Display {
    /// Handle this item.
    fn handle_item_or_access(
        &mut self,
        _services: &dyn HandleItemService,
        _project_context: &ProjectContext,
        _item: &ItemOrAccess,
    ) {
    }

    /// Need visit function or spec body or not.
    /// Sometimes you want visit function body But not all the function Body.
    fn function_or_spec_body_should_visit(&self, range: &FileRange) -> bool;
    fn visit_fun_or_spec_body(&self) -> bool;

    /// Visitor should finished.
    fn finished(&self) -> bool;

    // need Expr type ??
    fn need_expr_type(&self) -> bool {
        false
    }
    // handle expr type.
    fn handle_expr_typ(&mut self, _exp: &Exp, _ty: ResolvedType) {}

    fn need_call_pair(&self) -> bool {
        false
    }
    fn handle_call_pair(&mut self, _from: FunID, _to: FunID) {}
    fn need_para_arg_pair(&self) -> bool {
        false
    }
    fn handle_para_arg_pair(&mut self, _services: &dyn HandleItemService, _para: Name, _exp: &Exp) {
    }
}

#[derive(Clone, serde::Serialize, Debug)]
pub struct FunID {
    pub(crate) addr: AccountAddress,
    pub(crate) addr_name: String,
    pub(crate) module_name: Symbol,
    pub(crate) function_name: Symbol,
}

pub trait HandleItemService: ConvertLoc + GetAllAddrs + Name2Addr {}
impl HandleItemService for Project {}

#[derive(Debug, Clone, Copy)]
pub(crate) struct DummyHandler;

impl ItemOrAccessHandler for DummyHandler {
    fn handle_item_or_access(
        &mut self,
        _services: &dyn HandleItemService,
        _project_context: &ProjectContext,
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

impl std::fmt::Display for DummyHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub static ERR_ADDRESS: once_cell::sync::Lazy<AccountAddress> =
    once_cell::sync::Lazy::new(AccountAddress::random);

pub trait GetAllAddrs {
    fn get_all_addrs(&self, project_context: &ProjectContext) -> HashSet<AddressSpace>;
}

impl GetAllAddrs for Project {
    fn get_all_addrs(&self, project_context: &ProjectContext) -> HashSet<AddressSpace> {
        let mut addrs: HashSet<AddressSpace> = HashSet::new();
        let empty = Default::default();
        let empty2 = Default::default();
        for x in self.manifests.iter() {
            for (name, addr) in x.addresses.as_ref().unwrap_or(&empty).iter() {
                addrs.insert(AddressSpace::from(*name));
                if let Some(addr) = addr {
                    addrs.insert(AddressSpace::from(*addr));
                }
            }
            for (name, addr) in x.dev_address_assignments.as_ref().unwrap_or(&empty2).iter() {
                addrs.insert(AddressSpace::from(*name));
                addrs.insert(AddressSpace::from(*addr));
            }
        }
        project_context.visit_address(|addresss| {
            addresss.address.keys().for_each(|addr| {
                addrs.insert(AddressSpace::from(*addr));
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

impl ToString for AddressSpace {
    fn to_string(&self) -> String {
        match self {
            AddressSpace::Addr(addr) => addr.to_hex_literal(),
            AddressSpace::Name(x) => x.as_str().to_string(),
        }
    }
}
impl From<AccountAddress> for AddressSpace {
    fn from(x: AccountAddress) -> Self {
        Self::Addr(x)
    }
}

pub(crate) fn attributes_has_test(x: &[Attributes]) -> AttrTest {
    use AttrTest::*;
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
                    call_back(self.get_module_addr(Some(a.addr), module), module);
                }
            }
            _ => {}
        })
    }

    fn found_in_test(&self) -> bool {
        *self.layout() == SourcePackageLayout::Tests
    }
    fn found_in_scripts(&self) -> bool {
        *self.layout() == SourcePackageLayout::Scripts
    }
    fn layout(&self) -> &SourcePackageLayout;

    fn with_module_member(
        &self,
        mut call_back: impl FnMut(
            AccountAddress,
            Symbol,
            &ModuleMember,
            bool, /* if is_spec_module */
        ),
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
                            self.get_module_addr(Some(a.addr), module),
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
        self.with_module_member(|addr, module_name, member, _| {
            if let ModuleMember::Constant(c) = member {
                call_back(addr, module_name, c)
            }
        });
    }

    fn with_struct(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &StructDefinition)) {
        self.with_module_member(|addr, module_name, member, _| {
            if let ModuleMember::Struct(c) = member {
                call_back(addr, module_name, c)
            }
        });
    }
    fn with_script(&self, mut call_back: impl FnMut(&Script)) {
        self.with_definition(|x| {
            if let Definition::Script(x) = x {
                call_back(x);
            }
        })
    }
    fn with_use_decl(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &UseDecl, bool)) {
        self.with_module_member(|addr, module_name, member, is_spec| {
            if let ModuleMember::Use(c) = member {
                call_back(addr, module_name, c, is_spec)
            }
        });
    }
    fn with_function(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &Function)) {
        self.with_module_member(|addr, module_name, member, _| {
            if let ModuleMember::Function(c) = member {
                call_back(addr, module_name, c)
            }
        });
    }
    fn with_friend(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &FriendDecl)) {
        self.with_module_member(|addr, module_name, member, _| {
            if let ModuleMember::Friend(c) = member {
                call_back(addr, module_name, c)
            }
        });
    }
    fn with_spec(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &SpecBlock, bool)) {
        self.with_module_member(|addr, module_name, member, is_spec_module| {
            if let ModuleMember::Spec(c) = member {
                call_back(addr, module_name, c, is_spec_module)
            }
        });
    }
    fn with_spec_schema(
        &self,
        mut call_back: impl FnMut(AccountAddress, Symbol, Name, &SpecBlock, bool),
    ) {
        self.with_module_member(|addr, module_name, member, is_spec_module| {
            if let ModuleMember::Spec(c) = member {
                if let SpecBlockTarget_::Schema(name, _) = &c.value.target.value {
                    call_back(addr, module_name, *name, c, is_spec_module);
                }
            }
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
    fn layout(&self) -> &SourcePackageLayout {
        &self.layout
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
    fn layout(&self) -> &SourcePackageLayout {
        &self.layout
    }
}

pub(crate) fn file_modify_time(x: &Path) -> Option<SystemTime> {
    use std::result::Result::*;

    match x.metadata() {
        Ok(x) => match x.modified() {
            Ok(x) => Some(x),
            Err(_) => None,
        },
        Err(_) => None,
    }
}

impl Project {
    pub(crate) fn manifest_beed_modified(&self) -> bool {
        self.manifest_mod_time.iter().any(|(k, v)| {
            if file_modify_time(k.as_path()).cmp(v) != Ordering::Equal {
                eprintln!(
                    "going to reload project becasue of modify of '{:?}' {:?} {:?}",
                    k.as_path(),
                    file_modify_time(k.as_path()),
                    v
                );
                true
            } else {
                false
            }
        })
    }
}
