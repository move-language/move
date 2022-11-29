// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use super::item::*;
use super::scope::*;
use super::scopes::*;
use super::types::*;
use super::utils::*;
use move_core_types::account_address::*;

use anyhow::{Ok, Result};

use move_command_line_common::files::FileHash;

use move_compiler::parser::ast::Definition;

use move_compiler::shared::Identifier;
use move_compiler::{parser::ast::*, shared::*};

use move_ir_types::location::Loc;

use move_ir_types::location::Spanned;
use move_package::source_package::layout::SourcePackageLayout;
use move_package::source_package::manifest_parser::*;

use move_package::*;
use move_symbol_pool::Symbol;
use petgraph::visit;

use std::cell::RefCell;
use std::collections::btree_map::BTreeMap;
use std::collections::HashMap;

use std::fmt::format;
use std::slice::SliceIndex;

use std::{path::PathBuf, rc::Rc};

use walkdir::WalkDir;

/// All Modules.
#[derive(Default, Debug)]
pub struct Modules {
    modules: HashMap<PathBuf /* this is a Move.toml like xxxx/Move.toml  */, IDEModule>,
    /// a field contains the root manifest file
    /// if Modules construct successful this field is never None.
    root_manifest: Option<move_package::source_package::parsed_manifest::SourceManifest>,
    hash_file: PathBufHashMap,
    file_line_mapping: FileLineMapping,
    manifests: Vec<PathBuf>,
}

impl Modules {
    pub fn new(working_dir: impl Into<PathBuf>) -> Self {
        let working_dir = working_dir.into();
        log::info!("scan modules at {:?}", &working_dir);
        let mut modules = Self::default();
        modules.load_project(&working_dir).unwrap();
        modules
    }

    fn load_project(&mut self, manifest_path: &PathBuf) -> Result<()> {
        let manifest_path = normal_path(&manifest_path.as_path());
        if self.modules.get(&manifest_path).is_some() {
            log::info!("manifest '{:?}' loaded before skipped.", &manifest_path);
            return Ok(());
        }
        self.manifests.push(manifest_path.clone());
        log::info!("load manifest file at {:?}", &manifest_path);
        let manifest = parse_move_manifest_from_file(&manifest_path).unwrap();
        let build_cfg = BuildConfig {
            dev_mode: true,
            test_mode: true,
            generate_docs: false,
            generate_abis: false,
            install_dir: None,
            force_recompilation: false,
            additional_named_addresses: BTreeMap::new(),
            architecture: None,
            fetch_deps_only: true,
            skip_fetch_latest_git_deps: false,
        };
        if self.root_manifest.is_none() {
            self.root_manifest = Some(manifest.clone());
        }
        // load depends.
        for (dep_name, de) in manifest
            .dependencies
            .iter()
            .chain(manifest.dev_dependencies.iter())
        {
            let p = path_concat(manifest_path.as_path(), de.local.as_path());
            log::info!(
                "load dependency for '{:?}' dep_name '{}'",
                &manifest_path,
                dep_name
            );
            self.load_project(&p).unwrap();
        }
        self.load_layout_files(&manifest_path, SourcePackageLayout::Sources);

        // TODO::load
        // self.load_layout_files(&manifest_path, SourcePackageLayout::Tests);
        // self.load_layout_files(&manifest_path, SourcePackageLayout::Scripts);

        Ok(())
    }

    /// Load move files  locate in sources and tests ...
    fn load_layout_files(&mut self, manifest_path: &PathBuf, kind: SourcePackageLayout) {
        use move_compiler::parser::syntax::parse_file_string;
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
                let x = parse_file_string(&mut env, file_hash, file_content.as_str());
                let x = match x {
                    std::result::Result::Ok(x) => x,
                    std::result::Result::Err(e) => {
                        log::error!("parse file failed:{:?} d:{:?}", file.path(), e);
                        continue;
                    }
                };
                let x = x.0;
                if self.modules.get(manifest_path).is_none() {
                    self.modules
                        .insert(manifest_path.clone(), Default::default());
                }
                self.modules
                    .get_mut(manifest_path)
                    .unwrap()
                    .sources
                    .insert(file.path().clone().to_path_buf(), x);

                //  update hash
                self.hash_file.update(file.path().to_path_buf(), file_hash);
                // update line mapping.
                self.file_line_mapping
                    .update(file.path().to_path_buf(), file_content.as_str());
            }
        }
    }

    fn enter_module_constant(
        &self,
        scopes: &Scopes,
        module: &ModuleDefinition,
        addr: Option<LeadingNameAccess>,
        visitor: &mut dyn ScopeVisitor,
    ) {
        let add = self.get_module_addr(addr, module);
        for c in module.members.iter() {
            match c {
                ModuleMember::Constant(c) => {
                    self.visit_const(addr, module.name.0.value, c, scopes, visitor)
                }
                _ => {}
            }
        }
    }

    /// Entrance for `ScopeVisitor` base on analyze.
    pub fn run_visitor(&self, visitor: &mut dyn ScopeVisitor) {
        let scopes = Scopes::new();
        log::info!("run visitor for {} ", visitor);
        // visit should `rev`.
        let x: Vec<_> = self.manifests.iter().rev().map(|x| x.clone()).collect();
        for m in x.iter() {
            self.run_visitor_for_manifest(&scopes, visitor, m);
            if visitor.finished() {
                return;
            }
        }
    }

    /// Entrance for `ScopeVisitor` base on analyze.
    pub fn run_visitor_for_manifest(
        &self,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
        manifest: &PathBuf,
    ) {
        log::info!("run visitor for {:?} ", manifest);
        let _guard = scopes.enter_scope_guard();
        // Handle imports.
        for (_, ds) in self.modules.get(manifest).unwrap().sources.iter() {
            for d in ds.iter() {
                match d {
                    Definition::Module(ref m) => {
                        for m in m.members.iter() {
                            match &m {
                                ModuleMember::Use(u) => self.visit_use_decl(u, &scopes, visitor),
                                _ => {}
                            }
                        }
                    }
                    Definition::Address(ref a) => {
                        for m in a.modules.iter() {
                            for m in m.members.iter() {
                                match &m {
                                    ModuleMember::Use(u) => {
                                        self.visit_use_decl(u, &scopes, visitor)
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        // Handle constant.
        for (_, ds) in self.modules.get(manifest).unwrap().sources.iter() {
            for d in ds.iter() {
                match d {
                    Definition::Module(ref m) => {
                        self.enter_module_constant(&scopes, m, None, visitor);
                    }
                    Definition::Address(ref a) => {
                        for m in a.modules.iter() {
                            self.enter_module_constant(&scopes, m, Some(a.addr.clone()), visitor);
                        }
                    }
                    _ => {}
                }
            }
        }

        // Handle all struct type.
        {
            // Collect all struct defini tion.
            let mut all: Vec<(StructDefinition, Option<AddressDefinition>, ModuleName)> =
                Default::default();

            for (_, modules) in self.modules.iter() {
                for (_, ds) in modules.sources.iter() {
                    for d in ds.iter() {
                        match d {
                            Definition::Module(ref m) => {
                                for member in m.members.iter() {
                                    match member {
                                        ModuleMember::Struct(x) => {
                                            all.push((x.clone(), m.address.clone(), m.name));
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            Definition::Address(ref a) => {
                                for m in a.modules.iter() {
                                    for member in m.members.iter() {
                                        match member {
                                            ModuleMember::Struct(x) => {
                                                all.insert((x.clone(), Some(a.addr), m.name));
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }

            let all_struct_with_module = Rc::new(RefCell::new(HashMap::new()));
            for (_, s) in all.iter() {
                scopes.enter_item(
                    self,
                    s.name.value(),
                    Item::StructName(s.0.name.clone(), all_struct_with_module.clone()),
                );
            }

            for (s, _, _) in all.iter() {
                let fields = match &s.fields {
                    StructFields::Defined(x) => {
                        let mut fields = Vec::with_capacity(x.len());
                        for (f, ty) in x.iter() {
                            let ty = scopes.resolve_type(ty, self);
                            {
                                let item = ItemOrAccess::Item(Item::Field(f.clone(), ty.clone()));
                                visitor.handle_item(self, scopes, &item);
                                if visitor.finished() {
                                    return;
                                }
                            }
                            fields.push((f.clone(), ty));
                        }
                        fields
                    }
                    StructFields::Native(_) => vec![],
                };
                let is = ItemStruct {
                    name: s.name.clone(),
                    type_parameters: s.type_parameters.clone(),
                    fields,
                };
                {
                    let item = ItemOrAccess::Item(Item::Struct(is.clone()));
                    visitor.handle_item(self, scopes, &item);
                    if visitor.finished() {
                        return;
                    }
                }

                let item = Item::Struct(is.clone());
                scopes.enter_top_item(self, is.name.value(), item);

                all_struct_with_module
                    .as_ref()
                    .borrow_mut()
                    .insert(is.name.0.value, is.clone());
            }
        }

        // Enter function Item.
        for (_, ds) in self.modules.get(manifest).unwrap().sources.iter() {
            let enter_function = |modules: &Modules,
                                  f: &Function,
                                  scopes: &Scopes,
                                  visitor: &mut dyn ScopeVisitor,
                                  address: AccountAddress,
                                  module_name: Symbol| {
                let s = &f.signature;
                let ts = s.type_parameters.clone();
                let params: Vec<_> = s
                    .parameters
                    .iter()
                    .map(|(var, ty)| (var.clone(), scopes.resolve_type(ty, self)))
                    .collect();
                let ret = scopes.resolve_type(&s.return_type, self);
                let item = Item::Fun(ItemFunction {
                    name: f.name.clone(),
                    type_parameters: ts,
                    parameters: params,
                    ret_type: Box::new(ret),
                });
                let item = ItemOrAccess::Item(item);
                visitor.handle_item(modules, scopes, &item);
                scopes.enter_top_item(self, address, module_name, f.name.value(), item);
            };

            for d in ds.iter() {
                match d {
                    Definition::Module(ref m) => {
                        for member in m.members.iter() {
                            match member {
                                ModuleMember::Function(f) => {
                                    let addr = self.get_module_addr(None, m);
                                    let name = m.name.0.value;

                                    enter_function(self, f, scopes, visitor, addr, name)
                                }
                                _ => {}
                            }
                        }
                    }
                    Definition::Address(ref a) => {
                        for m in a.modules.iter() {
                            for member in m.members.iter() {
                                match member {
                                    ModuleMember::Function(f) => {
                                        let addr = self.get_module_addr(Some(a.addr), m);
                                        let name = m.name.0.value;
                                        enter_function(self, f, scopes, visitor);
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        for (_f, ds) in self.modules.get(manifest).unwrap().sources.iter() {
            // We have match the files.
            for d in ds.iter() {
                match d {
                    Definition::Module(x) => {
                        if !visitor.file_should_visit(
                            match self.convert_file_hash_filepath(&x.loc.file_hash()) {
                                Some(x) => x,
                                None => continue,
                            },
                        ) {
                            continue;
                        }
                        for v in x.members.iter() {
                            // Right now we only need visit function body.
                            // All toplevel struct must be visited no matter what.
                            match v {
                                ModuleMember::Function(x) => {
                                    self.visit_function(x, &scopes, visitor);
                                }
                                _ => {}
                            }
                        }
                    }
                    Definition::Address(_) => todo!(),
                    Definition::Script(_) => todo!(),
                }
            }
        }
    }

    fn visit_const(
        &self,
        address: AccountAddress,
        module: Symbol,
        c: &Constant,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        self.visit_type_apply(&c.signature, scopes, visitor);
        if visitor.finished() {
            return;
        }
        // const can only be declared at top scope
        let ty = scopes.resolve_type(&c.signature, self);
        let item = ItemOrAccess::Item(Item::Const(c.name.clone(), ty));
        visitor.handle_item(self, scopes, &item);
        let item: Item = item.into();
        scopes.enter_top_item(self, address, module, c.name.value(), item.clone());
    }

    fn get_module_addr(
        &self,
        addr: Option<LeadingNameAccess>,
        m: &ModuleDefinition,
    ) -> AccountAddress {
        match addr {
            Some(x) => match x.value {
                LeadingNameAccess_::AnonymousAddress(x) => x.bytes,
                LeadingNameAccess_::Name(name) => self.name_to_addr(name.value),
            },
            None => match m.address {
                Some(x) => match x.value {
                    LeadingNameAccess_::AnonymousAddress(x) => x.bytes,
                    LeadingNameAccess_::Name(name) => self.name_to_addr(name.value),
                },
                None => ERR_ADDRESS.clone(),
            },
        }
    }

    ///
    fn visit_function(&self, function: &Function, scopes: &Scopes, visitor: &mut dyn ScopeVisitor) {
        return scopes.enter_scope(|s| {
            self.visit_signature(&function.signature, s, visitor);
            if visitor.finished() {
                return;
            }
            match function.body.value {
                FunctionBody_::Native => {}
                FunctionBody_::Defined(ref seq) => self.visit_block(seq, scopes, visitor),
            }
        });
    }

    fn visit_block(&self, seq: &Sequence, scopes: &Scopes, visitor: &mut dyn ScopeVisitor) {
        scopes.enter_scope(|scopes| {
            for u in seq.0.iter() {
                self.visit_use_decl(u, scopes, visitor);
                if visitor.finished() {
                    return;
                }
            }
            for s in seq.1.iter() {
                self.visit_sequence_item(s, scopes, visitor);
                if visitor.finished() {
                    return;
                }
            }
            if let Some(ref exp) = seq.3.as_ref() {
                self.visit_expr(exp, scopes, visitor);
            }
        });
    }

    fn visit_sequence_item(
        &self,
        seq: &SequenceItem,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        match seq.value {
            SequenceItem_::Seq(ref e) => {
                self.visit_expr(e, scopes, visitor);
                if visitor.finished() {
                    return;
                }
            }
            SequenceItem_::Declare(ref list, ref ty) => {
                self.visit_bind_list(list, ty, None, scopes, visitor);
                if visitor.finished() {
                    return;
                }
            }
            SequenceItem_::Bind(ref list, ref ty, ref expr) => {
                self.visit_bind_list(list, ty, Some(expr), scopes, visitor);
                if visitor.finished() {
                    return;
                }
            }
        }
    }

    fn visit_bind_list(
        &self,
        bind_list: &BindList,
        ty: &Option<Type>,
        expr: Option<&Box<Exp>>,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        let ty = if let Some(ty) = ty {
            scopes.resolve_type(ty, self)
        } else if let Some(expr) = expr {
            let ty = self.get_expr_type(expr, scopes);
            self.visit_expr(expr, scopes, visitor);
            if visitor.finished() {
                return;
            }
            ty
        } else {
            ResolvedType::new_unknown()
        };

        for (index, bind) in bind_list.value.iter().enumerate() {
            let ty = ty.nth_ty(index);
            let unknown = ResolvedType::new_unknown();
            let ty = ty.unwrap_or(&unknown);
            self.visit_bind(bind, ty, scopes, None, visitor);
            if visitor.finished() {
                return;
            }
        }
    }

    fn visit_bind(
        &self,
        bind: &Bind,
        ty: &ResolvedType,
        scopes: &Scopes,
        field: Option<&'_ Field>,
        visitor: &mut dyn ScopeVisitor,
    ) {
        match &bind.value {
            Bind_::Var(var) => {
                let item = ItemOrAccess::Item(Item::Var(var.clone(), ty.clone()));
                visitor.handle_item(self, scopes, &item);
                if visitor.finished() {
                    return;
                }
                scopes.enter_item(self, var.0.value, item);
                return;
            }
            Bind_::Unpack(_, _, _) => todo!(),
        }
    }

    fn visit_type_apply(&self, ty: &Type, scopes: &Scopes, visitor: &mut dyn ScopeVisitor) {
        match &ty.value {
            Type_::Apply(chain, types) => {
                let ty = scopes.find_name_chain_type(chain.as_ref(), &mut None, self, true);
                let item =
                    ItemOrAccess::Access(Access::ApplyType(chain.as_ref().clone(), Box::new(ty)));
                visitor.handle_item(self, scopes, &item);
                if visitor.finished() {
                    return;
                }
                for t in types.iter() {
                    self.visit_type_apply(t, scopes, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
            Type_::Ref(_, ty) => self.visit_type_apply(ty, scopes, visitor),
            Type_::Fun(_, _) => {}
            Type_::Unit => {}
            Type_::Multiple(types) => {
                for t in types.iter() {
                    self.visit_type_apply(t, scopes, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
        }
    }

    fn name_to_addr(&self, name: Symbol) -> AccountAddress {
        if let Some(ref x) = self.root_manifest {
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
        return ERR_ADDRESS;
    }

    /// Get A Type for expr if possible otherwise Unknown is return.
    fn get_expr_type(&self, expr: &Exp, scopes: &Scopes) -> ResolvedType {
        match &expr.value {
            Exp_::Value(ref x) => match &x.value {
                Value_::Address(_) => ResolvedType::new_build_in(BuildInType::Address),
                Value_::Num(_) => ResolvedType::new_build_in(BuildInType::NumType),
                Value_::Bool(_) => ResolvedType::new_build_in(BuildInType::Bool),
                Value_::HexString(_) | Value_::ByteString(_) => {
                    ResolvedType::new_build_in(BuildInType::String)
                }
            },
            Exp_::Move(x) | Exp_::Copy(x) => scopes.find_var_type(x.0.value),
            Exp_::Name(name, _ /*  TODO this is a error. */) => {
                let ty = scopes.find_name_chain_type(name, &mut None, self, false);
                return ty;
            }
            Exp_::Call(name, is_macro, ref type_args, exprs) => {
                if *is_macro {
                    let c = MacroCall::from_chain(name);
                    match c {
                        MacroCall::Assert => ResolvedType::new_unit(),
                    }
                } else {
                    let fun_type = scopes.find_name_chain_type(name, &mut None, self, false);
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
                                    type_parameters,
                                    parameters,
                                    &exprs_types,
                                );
                            }
                            fun_type.bind_type_parameter(&types);
                            match &fun_type {
                                ResolvedType::Fun(x) => x.ret_type.as_ref().clone(),
                                _ => unreachable!(),
                            }
                        }
                        // This maybe is a error.
                        _ => return UNKNOWN_TYPE.clone(),
                    }
                }
            }

            Exp_::Pack(name, type_args, _) => {
                let mut struct_ty = scopes.find_name_chain_type(name, &mut None, self, false);

                match &struct_ty {
                    ResolvedType::Struct(ItemStruct {
                        name: _,
                        type_parameters,
                        fields,
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
                        let mut types = HashMap::new();
                        if let Some(ref ts) = type_args {
                            for (para, args) in type_parameters.iter().zip(ts.iter()) {
                                types.insert(para.name.value, args.clone());
                            }
                        }
                        struct_ty.bind_type_parameter(&types);

                        struct_ty
                    }
                    _ => UNKNOWN_TYPE.clone(),
                }
            }
            Exp_::Vector(loc, ty, exprs) => {
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
                ty.unwrap_or(ResolvedType::new_unknown())
            }
            Exp_::IfElse(_, _then, else_) => {
                let mut ty = self.get_expr_type(expr, scopes);
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
            Exp_::Lambda(_, _) => todo!(),
            Exp_::Quant(_, _, _, _, _) => todo!(),
            Exp_::ExpList(_) => todo!(),
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
                let pick = |prefer_left: bool| {
                    if prefer_left && !left_ty.is_err() {
                        left_ty.clone()
                    } else {
                        right_ty.clone()
                    }
                };
                match op.value {
                    BinOp_::Add => pick(true),
                    BinOp_::Sub => pick(true),
                    BinOp_::Mul => pick(true),
                    BinOp_::Mod => pick(true),
                    BinOp_::Div => pick(true),
                    BinOp_::BitOr => pick(true),
                    BinOp_::BitAnd => pick(true),
                    BinOp_::Xor => pick(true),
                    BinOp_::Shl => pick(true),
                    BinOp_::Shr => pick(true),
                    BinOp_::Range => todo!(),
                    BinOp_::Implies => todo!(),
                    BinOp_::Iff => todo!(),
                    BinOp_::And => todo!(),
                    BinOp_::Or => todo!(),
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
                if let Some(v) = ty.is_vector() {
                    v.clone()
                } else {
                    ty
                }
            }
            Exp_::Cast(_, ty) => {
                let ty = scopes.resolve_type(ty, self);
                ty
            }
            Exp_::Annotate(_, ty) => scopes.resolve_type(ty, self),
            Exp_::Spec(_) => todo!(),
            Exp_::UnresolvedError => {
                // Nothings. didn't know what to do.
                ResolvedType::new_unknown()
            }
        }
    }

    fn visit_expr(&self, exp: &Exp, scopes: &Scopes, visitor: &mut dyn ScopeVisitor) {
        log::trace!("visit_expr:{:?}", exp);
        match &exp.value {
            Exp_::Value(ref v) => {
                if let Some(name) = get_name_from_value(v) {
                    let item = ItemOrAccess::Access(Access::ExprAddressName(name.clone()));
                    visitor.handle_item(self, scopes, &item);
                }
            }
            Exp_::Move(var) | Exp_::Copy(var) => {
                let mut item = Some(Item::new_dummy());
                scopes.find_name_chain_type(
                    &Spanned {
                        loc: var.loc(),
                        value: NameAccessChain_::One(var.0.clone()),
                    },
                    &mut item,
                    self,
                    false,
                );
                let item =
                    ItemOrAccess::Access(Access::ExprVar(var.clone(), Box::new(item.unwrap())));
                visitor.handle_item(self, scopes, &item);
            }
            Exp_::Name(
                chain,
                _ty, /*
                     yuyang:
                      TODO How to use _ty,
                      looks like _ty is not used. */
            ) => {
                let mut item = Some(Item::new_dummy());
                scopes.find_name_chain_type(chain, &mut item, self, false);
                let item = ItemOrAccess::Access(Access::ExprAccessChain(
                    chain.clone(),
                    Box::new(item.unwrap()),
                ));
                visitor.handle_item(self, scopes, &item);
            }
            Exp_::Call(ref chain, is_macro, ref types, ref exprs) => {
                if *is_macro {
                    let c = MacroCall::from_chain(chain);
                    let item = ItemOrAccess::Access(Access::MacroCall(c));
                    visitor.handle_item(self, scopes, &item);
                } else {
                    let mut item = Some(Item::new_dummy());
                    scopes.find_name_chain_type(chain, &mut item, self, false);
                    let item = ItemOrAccess::Access(Access::ExprAccessChain(
                        chain.clone(),
                        Box::new(item.unwrap()),
                    ));
                    visitor.handle_item(self, scopes, &item);
                }
                if visitor.finished() {
                    return;
                }
                if let Some(ref types) = types {
                    for t in types.iter() {
                        self.visit_type_apply(t, scopes, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                }
                for expr in exprs.value.iter() {
                    self.visit_expr(expr, scopes, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
            Exp_::Pack(ref chain, ref types, fields) => {
                let ty = scopes.find_name_chain_type(chain, &mut None, self, false);
                let item =
                    ItemOrAccess::Access(Access::ApplyType(chain.clone(), Box::new(ty.clone())));
                visitor.handle_item(self, scopes, &item);
                if visitor.finished() {
                    return;
                }
                if let Some(types) = types {
                    for t in types.iter() {
                        self.visit_type_apply(t, scopes, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                    //TODO bind type.
                }

                for f in fields.iter() {
                    self.visit_expr(&f.1, scopes, visitor);
                    if visitor.finished() {
                        return;
                    }

                    let field_type = ty.find_filed_by_name(f.0.value());
                    if let Some(field_type) = field_type {
                        let item = ItemOrAccess::Access(Access::AccessFiled(
                            f.0.clone(),
                            field_type.0.clone(),
                            field_type.1.clone(),
                        ));
                        visitor.handle_item(self, scopes, &item);
                    }
                }
            }

            Exp_::Vector(_loc, ref ty, ref exprs) => {
                if let Some(ty) = ty {
                    for t in ty.iter() {
                        self.visit_type_apply(t, scopes, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                }
                for e in exprs.value.iter() {
                    self.visit_expr(e, scopes, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }

            Exp_::IfElse(condition, then_, else_) => {
                self.visit_expr(condition, scopes, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_expr(then_, scopes, visitor);
                if visitor.finished() {
                    return;
                }
                if let Some(else_) = else_ {
                    self.visit_expr(else_.as_ref(), scopes, visitor);
                }
            }
            Exp_::While(condition, body) => {
                self.visit_expr(condition, scopes, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_expr(body.as_ref(), scopes, visitor);
            }

            Exp_::Loop(e) => {
                self.visit_expr(e.as_ref(), scopes, visitor);
            }
            Exp_::Block(b) => self.visit_block(b, scopes, visitor),
            Exp_::Lambda(_, _) => todo!(),
            Exp_::Quant(_, _, _, _, _) => todo!(),
            Exp_::ExpList(list) => {
                for e in list.iter() {
                    self.visit_expr(e, scopes, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
            Exp_::Unit => {
                // Nothing.
            }
            Exp_::Assign(left, right) => {
                self.visit_expr(left, scopes, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_expr(right, scopes, visitor);
            }
            Exp_::Return(e) => {
                if let Some(e) = e {
                    self.visit_expr(e, scopes, visitor);
                }
            }
            Exp_::Abort(e) => self.visit_expr(e.as_ref(), scopes, visitor),
            Exp_::Break => {
                let item = ItemOrAccess::Access(Access::KeyWords("break"));
                visitor.handle_item(self, scopes, &item);
            }
            Exp_::Continue => {
                let item = ItemOrAccess::Access(Access::KeyWords("continue"));
                visitor.handle_item(self, scopes, &item);
            }
            Exp_::Dereference(x) => {
                self.visit_expr(x.as_ref(), scopes, visitor);
            }
            Exp_::UnaryExp(_, e) => {
                self.visit_expr(e.as_ref(), scopes, visitor);
            }
            Exp_::BinopExp(left, _, right) => {
                self.visit_expr(left, scopes, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_expr(right, scopes, visitor);
            }
            Exp_::Borrow(_, e) => {
                self.visit_expr(e.as_ref(), scopes, visitor);
            }
            Exp_::Dot(e, field) => {
                self.visit_expr(e.as_ref(), scopes, visitor);
                if visitor.finished() {
                    return;
                }
                let ty = self.get_expr_type(e, scopes);
                if let Some(field) = ty.find_filed_by_name(field.value) {
                    let item = ItemOrAccess::Access(Access::AccessFiled(
                        todo!(),
                        field.0.clone(),
                        field.1.clone(),
                    ));
                    visitor.handle_item(self, scopes, &item);
                }
            }
            Exp_::Index(_, _) => todo!(),
            Exp_::Cast(e, ty) => {
                self.visit_expr(e.as_ref(), scopes, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_type_apply(ty, scopes, visitor);
            }
            Exp_::Annotate(e, ty) => {
                self.visit_expr(e.as_ref(), scopes, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_type_apply(ty, scopes, visitor);
            }
            Exp_::Spec(_) => todo!(),
            Exp_::UnresolvedError => {
                //
            }
        }
    }

    fn visit_use_decl(&self, use_decl: &UseDecl, scopes: &Scopes, visitor: &mut dyn ScopeVisitor) {
        match &use_decl.use_ {
            Use::Module(module, alias) => {
                let mut name = module.value.module.0.value;
                if let Some(alias) = alias {
                    name = alias.0.value;
                }
                let r = scopes.visit_top_scope(|top| -> Option<Rc<RefCell<Scope>>> {
                    let x = top
                        .address
                        .get(&match &module.value.address.value {
                            LeadingNameAccess_::AnonymousAddress(num) => num.bytes,
                            LeadingNameAccess_::Name(name) => self.name_to_addr(name.value),
                        })?
                        .modules
                        .get(&module.value.module.0.value)?
                        .clone();
                    Some(x)
                });
                if r.is_none() {
                    return;
                }
                let r = r.unwrap();
                let item = Item::ImportedModule(module.clone(), r);
                scopes.enter_item(self, name, item);
            }
            Use::Members(module, members) => {
                let r = scopes.visit_top_scope(|top| -> Option<Rc<RefCell<Scope>>> {
                    let x = top
                        .address
                        .get(&match &module.value.address.value {
                            LeadingNameAccess_::AnonymousAddress(num) => num.bytes,
                            LeadingNameAccess_::Name(name) => self.name_to_addr(name.value),
                        })?
                        .modules
                        .get(&module.value.module.0.value)?
                        .clone();
                    Some(x)
                });
                if r.is_none() {
                    return;
                }
                let r = r.unwrap();
                for (member, alias) in members.iter() {
                    let mut name = member;
                    if let Some(alias) = alias {
                        name = alias;
                    }
                    if let Some(i) = r.as_ref().borrow().items.get(&member.value) {
                        let item = ItemOrAccess::Access(Access::UseMember(
                            name.clone(),
                            Box::new(i.clone()),
                        ));
                        visitor.handle_item(self, scopes, &item);
                        if visitor.finished() {
                            return;
                        }
                        let item = Item::ImportedMember(name.clone(), Box::new(i.clone()));
                        scopes.enter_item(self, name.value, item);
                    }
                }
            }
        }
    }

    fn visit_signature(
        &self,
        signature: &FunctionSignature,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        for (name, v) in signature.type_parameters.iter() {
            let item = ItemOrAccess::Item(Item::TParam(name.clone(), v.clone()));
            visitor.handle_item(self, scopes, &item);
            if visitor.finished() {
                return;
            }
            // Enter this.
            scopes.enter_item(self, name.value, item);
        }

        for (v, t) in signature.parameters.iter() {
            self.visit_type_apply(t, scopes, visitor);
            let t = scopes.resolve_type(t, self);
            let item = ItemOrAccess::Item(Item::Parameter(v.clone(), t));
            // found
            visitor.handle_item(self, scopes, &item);
            if visitor.finished() {
                return;
            }
            scopes.enter_item(self, v.value(), item)
        }
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
pub struct IDEModule {
    sources: HashMap<
        PathBuf, /*  file path  xxxx/abc.move  */
        Vec<move_compiler::parser::ast::Definition>,
    >,

    tests: HashMap<
        PathBuf, /*  file path  xxxx/abc.move  */
        Vec<move_compiler::parser::ast::Definition>,
    >,

    scripts: HashMap<
        PathBuf, /*  file path  xxxx/abc.move  */
        Vec<move_compiler::parser::ast::Definition>,
    >,
}

const UNKNOWN_TYPE: ResolvedType = ResolvedType::new_unknown();

fn get_name_from_value(v: &Value) -> Option<&Name> {
    match &v.value {
        Value_::Address(ref x) => match &x.value {
            LeadingNameAccess_::AnonymousAddress(_) => None,
            LeadingNameAccess_::Name(ref name) => Some(name),
        },
        _ => None,
    }
}

fn infer_type_parameter_on_expression(
    ret: &mut HashMap<Symbol /*  name like T or ... */, ResolvedType>,
    _type_parameters: &Vec<(Name, Vec<Ability>)>,
    parameters: &Vec<(Var, ResolvedType)>,
    expression_types: &Vec<ResolvedType>,
) {
    for (index, p) in parameters.iter().enumerate() {
        if let Some(expr_type) = expression_types.get(index) {
            bind(ret, &p.1, expr_type);
        } else {
            break;
        }
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
            ResolvedType::Struct(ItemStruct { fields, .. }) => match &expr_type {
                ResolvedType::Struct(ItemStruct {
                    fields: fields2, ..
                }) => {
                    for (l, r) in fields.iter().zip(fields2.iter()) {
                        bind(ret, &l.1, &r.1);
                    }
                }
                _ => {}
            },
            ResolvedType::BuildInType(_) => {}
            ResolvedType::TParam(name, _) => {
                ret.insert(name.value, expr_type.clone());
            }
            ResolvedType::ApplyTParam(_, _, _) => {}
            ResolvedType::Ref(_, l) => match &expr_type {
                ResolvedType::Ref(_, r) => bind(ret, l.as_ref(), r.as_ref()),
                _ => {}
            },
            ResolvedType::Unit => {}
            ResolvedType::Multiple(x) => match &expr_type {
                ResolvedType::Multiple(y) => {
                    for (index, l) in x.iter().enumerate() {
                        if let Some(r) = y.get(index) {
                            bind(ret, l, r);
                        } else {
                            break;
                        }
                    }
                }
                _ => {}
            },
            /// function is not expression
            ResolvedType::Fun(_) => {}
            ResolvedType::Vec(x) => match &expr_type {
                ResolvedType::Vec(y) => {
                    bind(ret, x.as_ref(), y.as_ref());
                }
                _ => {}
            },
            ResolvedType::ResolvedFailed(_) => {}
            ResolvedType::StructName(_, _) => {}
        }
    }
}

pub trait ConvertLoc {
    fn convert_file_hash_filepath(&self, hash: &FileHash) -> Option<&'_ PathBuf>;
    fn convert_loc_range(&self, loc: &Loc) -> Option<FileRange>;
}

impl ConvertLoc for Modules {
    fn convert_file_hash_filepath(&self, hash: &FileHash) -> Option<&'_ PathBuf> {
        self.hash_file.get_path(hash)
    }
    fn convert_loc_range(&self, loc: &Loc) -> Option<FileRange> {
        self.convert_file_hash_filepath(&loc.file_hash())
            .map(|file| {
                self.file_line_mapping
                    .translate(file, loc.start(), loc.end())
            })
            .flatten()
    }
}

pub trait ConvertName2AccountAddress {
    fn convert(&self, name: Symbol) -> AccountAddress;
}
impl ConvertName2AccountAddress for Modules {
    fn convert(&self, name: Symbol) -> AccountAddress {
        self.name_to_addr(name)
    }
}

/// Visit scopes for inner to outer.
pub trait ScopeVisitor: std::fmt::Display {
    /// Handle this item.
    /// If `should_finish` return true. All `enter_scope` and enter_scope called function will return.
    fn handle_item(&mut self, services: &dyn ConvertLoc, scopes: &Scopes, item: &ItemOrAccess);
    /// Need not visit this structure???
    fn file_should_visit(&self, p: &PathBuf) -> bool;
    /// Visitor should finished.
    fn finished(&self) -> bool;
}

static ERR_ADDRESS: AccountAddress = AccountAddress::ONE;

pub(crate) struct Ending {
    pub(crate) msg: String,
}
impl Drop for Ending {
    fn drop(&mut self) {
        log::info!("ending {}", self.msg.as_str());
    }
}

#[derive(Debug)]
pub(crate) struct DummyVisitor;

impl ScopeVisitor for DummyVisitor {
    fn handle_item(&mut self, _services: &dyn ConvertLoc, _scopes: &Scopes, _item: &ItemOrAccess) {}

    fn file_should_visit(&self, _p: &PathBuf) -> bool {
        unreachable!();
    }

    fn finished(&self) -> bool {
        false
    }
}

impl std::fmt::Display for DummyVisitor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
