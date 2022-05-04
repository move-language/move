// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

//! This module is responsible for building symbolication information
//! on top of compiler IR, including identifier definitions to be used
//! for implementing go-to-def and go-to-references language server
//! commands.

use crate::context::Context;
use anyhow::Result;
use codespan_reporting::files::{Files, SimpleFiles};
use lsp_server::{Request, RequestId};
use lsp_types::{GotoDefinitionParams, Location, Position, Range, ReferenceParams};
use std::{
    cmp,
    collections::{BTreeMap, BTreeSet, VecDeque},
    fs,
    path::{Path, PathBuf},
};
use tempfile::tempdir;
use url::Url;

use move_command_line_common::files::FileHash;
use move_compiler::{
    diagnostics,
    expansion::ast::{Fields, ModuleIdent_},
    naming::ast::{StructFields, Type},
    parser::ast::StructName,
    shared::Identifier,
    typing::ast::{
        Exp, Function, FunctionBody_, LValue, LValueList, LValue_, ModuleDefinition, SequenceItem,
        SequenceItem_, UnannotatedExp_,
    },
    PASS_TYPING,
};
use move_ir_types::location::Loc;
use move_package::compilation::build_plan::BuildPlan;
use move_symbol_pool::Symbol;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
struct DefLoc {
    /// File where the definition of the identifier starts
    fhash: FileHash,
    /// Location where the definition of the identifier starts
    start: Position,
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
struct UseLoc {
    /// File where this use identifier starts
    fhash: FileHash,
    /// Location where this use identifier starts
    start: Position,
    /// Column (on the same line as start)  where this use identifier ends
    col_end: u32,
}

#[derive(Debug, Clone, Eq)]
pub struct UseDef {
    /// Column where the (use) identifier location starts on a given
    /// line (use this field for sorting uses on the line)
    col_start: u32,
    /// Column where the (use) identifier location ends on a given
    /// line
    col_end: u32,
    /// Location of the definition
    def_loc: DefLoc,
}

struct FieldDef {
    name: Symbol,
    start: Position,
}

struct StructDef {
    name_start: Position,
    field_defs: Vec<FieldDef>,
}

struct ModuleDefs {
    structs: BTreeMap<Symbol, StructDef>,
    constants: BTreeMap<Symbol, Position>,
    functions: BTreeMap<Symbol, Position>,
}

pub struct Symbolicator {
    /// Outermost definitions in a module (structs, consts, functions)
    mod_outer_defs: BTreeMap<ModuleIdent_, ModuleDefs>,
    /// A mapping from file names to file content (used to obtain
    /// source file locations)
    files: SimpleFiles<Symbol, String>,
    /// A mapping from file hashes to file IDs (used to obtain source
    /// file locations)
    file_id_mapping: BTreeMap<FileHash, usize>,
    /// Scope to contain type params where relevant (e.g. when
    /// processing function definition)
    type_params: Scope,
}

pub struct Symbols {
    /// A map from def locations to all the references (uses)
    references: BTreeMap<DefLoc, BTreeSet<UseLoc>>,
    /// A mapping from uses to definitions in a module
    mod_use_defs: BTreeMap<ModuleIdent_, UseDefMap>,
    /// A mapping from paths to module IDs
    mod_ident_map: BTreeMap<PathBuf, ModuleIdent_>,
    /// A mapping from file hashes to file names
    file_name_mapping: BTreeMap<FileHash, Symbol>,
}

/// Scope for the purpose of definition resolution
type Scope = BTreeMap<Symbol, DefLoc>;

impl UseDef {
    fn new(
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_fhash: FileHash,
        use_start: Position,
        def_fhash: FileHash,
        def_start: Position,
        use_name: &Symbol,
    ) -> Self {
        let def_loc = DefLoc {
            fhash: def_fhash,
            start: def_start,
        };
        let col_end = use_start.character + use_name.len() as u32;
        let use_def = Self {
            col_start: use_start.character,
            col_end,
            def_loc: def_loc.clone(),
        };

        let use_loc = UseLoc {
            fhash: use_fhash,
            start: use_start,
            col_end,
        };

        match references.get_mut(&def_loc) {
            Some(v) => {
                v.insert(use_loc);
            }
            None => {
                let mut s = BTreeSet::new();
                s.insert(use_loc);
                references.insert(def_loc, s);
            }
        }
        use_def
    }
}

impl Ord for UseDef {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.col_start.cmp(&other.col_start)
    }
}

impl PartialOrd for UseDef {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for UseDef {
    fn eq(&self, other: &Self) -> bool {
        self.col_start == other.col_start
    }
}

/// Maps a line number to a list of use-def pairs on a given line
/// (use-def set is sorted by col_start)
#[derive(Debug)]
struct UseDefMap(BTreeMap<u32, BTreeSet<UseDef>>);

impl UseDefMap {
    fn new() -> Self {
        Self(BTreeMap::new())
    }

    fn insert(&mut self, key: u32, val: UseDef) {
        match self.0.get_mut(&key) {
            Some(v) => {
                v.insert(val);
            }
            None => {
                let mut s = BTreeSet::new();
                s.insert(val);
                self.0.insert(key, s);
            }
        }
    }

    fn get(&self, key: u32) -> Option<BTreeSet<UseDef>> {
        self.0.get(&key).cloned()
    }
}

impl Symbolicator {
    pub fn empty_symbols() -> Symbols {
        Symbols {
            mod_use_defs: BTreeMap::new(),
            mod_ident_map: BTreeMap::new(),
            references: BTreeMap::new(),
            file_name_mapping: BTreeMap::new(),
        }
    }
    pub fn get_symbols(pkg_path: &Path) -> Result<Symbols> {
        let build_config = move_package::BuildConfig {
            test_mode: true,
            install_dir: Some(tempdir().unwrap().path().to_path_buf()),
            ..Default::default()
        };

        eprintln!("symbolicating {:?}", pkg_path);

        let resolution_graph = build_config.resolution_graph_for_package(pkg_path)?;

        // get source files to be able to correlate positions (in terms of
        // byte offsets) with actual file locations (in terms of
        // line/column numbers)
        let source_files = &resolution_graph.file_sources();
        let mut files = SimpleFiles::new();
        let mut file_id_mapping = BTreeMap::new();
        let mut file_name_mapping = BTreeMap::new();
        for (fhash, (fname, source)) in source_files {
            let id = files.add(*fname, source.clone());
            file_id_mapping.insert(*fhash, id);
            file_name_mapping.insert(*fhash, *fname);
        }

        let build_plan = BuildPlan::create(resolution_graph)?;
        let mut typed_ast = vec![];
        let pkg = build_plan.compile_with_driver(&mut std::io::sink(), |compiler| {
            let (files, comments_and_compiler_res) = compiler.run::<PASS_TYPING>().unwrap();
            let (_, compiler) =
                diagnostics::unwrap_or_report_diagnostics(&files, comments_and_compiler_res);
            let (compiler, typed_program) = compiler.into_ast();
            typed_ast.push(typed_program.clone());
            let compilation_result = compiler.at_typing(typed_program).build();

            let (units, _) = diagnostics::unwrap_or_report_diagnostics(&files, compilation_result);
            Ok((files, units))
        })?;

        debug_assert!(typed_ast.len() == 1);
        let modules = &typed_ast.get(0).unwrap().modules;

        let mut mod_outer_defs = BTreeMap::new();
        let mut references = BTreeMap::new();
        let mut mod_use_defs = BTreeMap::new();
        let mut mod_ident_map = BTreeMap::new();
        for (pos, module_ident, module_def) in modules {
            let (defs, symbols) =
                Self::get_mod_outer_defs(&mut references, module_def, &files, &file_id_mapping);
            mod_outer_defs.insert(*module_ident, defs);
            mod_use_defs.insert(*module_ident, symbols);
            match source_files.get(&pos.file_hash()) {
                Some(v) => {
                    mod_ident_map.insert(
                        // if canonicalization fails, simply use
                        // "regular" path to continue processing
                        fs::canonicalize(v.0.as_str())
                            .unwrap_or_else(|_| PathBuf::from(v.0.as_str())),
                        *module_ident,
                    );
                }
                None => continue,
            }
        }

        let mut symbolicator = Symbolicator {
            mod_outer_defs,
            files,
            file_id_mapping,
            type_params: Scope::new(),
        };

        for (_, module_ident, module_def) in modules {
            let mut use_defs = mod_use_defs.get_mut(module_ident).unwrap();
            symbolicator.mod_symbols(module_def, &mut references, &mut use_defs);
        }

        Ok(Symbols {
            references,
            mod_use_defs,
            mod_ident_map,
            file_name_mapping,
        })
    }

    fn get_start_loc(
        pos: &Loc,
        files: &SimpleFiles<Symbol, String>,
        file_id_mapping: &BTreeMap<FileHash, usize>,
    ) -> Option<Position> {
        let id = match file_id_mapping.get(&pos.file_hash()) {
            Some(v) => v,
            None => return None,
        };
        match files.location(*id, pos.start() as usize) {
            Ok(v) => Some(Position {
                // we need 0-based column location
                line: v.line_number as u32 - 1,
                character: v.column_number as u32 - 1,
            }),
            Err(_) => None,
        }
    }

    fn get_mod_outer_defs(
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        mod_def: &ModuleDefinition,
        files: &SimpleFiles<Symbol, String>,
        file_id_mapping: &BTreeMap<FileHash, usize>,
    ) -> (ModuleDefs, UseDefMap) {
        let mut structs = BTreeMap::new();
        let mut constants = BTreeMap::new();
        let mut functions = BTreeMap::new();
        let mut use_def_map = UseDefMap::new();

        for (pos, name, def) in &mod_def.structs {
            match Self::get_start_loc(&pos, files, file_id_mapping) {
                Some(name_start) => {
                    let mut field_defs = vec![];
                    if let StructFields::Defined(fields) = &def.fields {
                        for (fpos, fname, _) in fields {
                            match Self::get_start_loc(&fpos, files, file_id_mapping) {
                                Some(start) => {
                                    field_defs.push(FieldDef {
                                        name: *fname,
                                        start,
                                    });
                                    // enter self-definition for field name
                                    use_def_map.insert(
                                        start.line,
                                        UseDef::new(
                                            references,
                                            fpos.file_hash(),
                                            start,
                                            fpos.file_hash(),
                                            start,
                                            fname,
                                        ),
                                    )
                                }
                                None => {
                                    debug_assert!(false);
                                    continue;
                                }
                            };
                        }
                    };

                    structs.insert(
                        *name,
                        StructDef {
                            name_start,
                            field_defs,
                        },
                    );
                    // enter self-definition for struct name
                    use_def_map.insert(
                        name_start.line,
                        UseDef::new(
                            references,
                            pos.file_hash(),
                            name_start,
                            pos.file_hash(),
                            name_start,
                            name,
                        ),
                    );
                }
                None => {
                    debug_assert!(false);
                    continue;
                }
            };
        }

        for (pos, name, _) in &mod_def.constants {
            match Self::get_start_loc(&pos, files, file_id_mapping) {
                Some(name_start) => {
                    constants.insert(*name, name_start);
                    // enter self-definition for const name
                    use_def_map.insert(
                        name_start.line,
                        UseDef::new(
                            references,
                            pos.file_hash(),
                            name_start,
                            pos.file_hash(),
                            name_start,
                            name,
                        ),
                    );
                }
                None => {
                    debug_assert!(false);
                    continue;
                }
            };
        }

        for (pos, name, _) in &mod_def.functions {
            match Self::get_start_loc(&pos, files, file_id_mapping) {
                Some(name_start) => {
                    functions.insert(*name, name_start);
                    // enter self-definition for const name
                    use_def_map.insert(
                        name_start.line,
                        UseDef::new(
                            references,
                            pos.file_hash(),
                            name_start,
                            pos.file_hash(),
                            name_start,
                            name,
                        ),
                    );
                }
                None => {
                    debug_assert!(false);
                    continue;
                }
            };
        }

        (
            ModuleDefs {
                structs,
                constants,
                functions,
            },
            use_def_map,
        )
    }

    fn mod_symbols(
        &mut self,
        mod_def: &ModuleDefinition,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        for (_, _, fun) in &mod_def.functions {
            self.fun_symbols(fun, references, use_defs);
        }
    }

    fn fun_symbols(
        &mut self,
        fun: &Function,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        // create scope designated to contain type parameters (if any)
        let mut tp_scope = Scope::new();
        for tp in &fun.signature.type_parameters {
            let exists = match Self::get_start_loc(
                &tp.user_specified_name.loc,
                &self.files,
                &self.file_id_mapping,
            ) {
                Some(loc) => tp_scope.insert(
                    tp.user_specified_name.value,
                    DefLoc {
                        fhash: tp.user_specified_name.loc.file_hash(),
                        start: loc,
                    },
                ),
                None => {
                    debug_assert!(false);
                    continue;
                }
            };
            debug_assert!(exists.is_none());
        }
        self.type_params = tp_scope;

        let mut scope_stack = VecDeque::new();
        // scope for the main function scope (for parameters and
        // function body)
        let mut fn_scope = Scope::new();

        for p in &fun.signature.parameters {
            // TODO: process parameter types
            if let Some(def_loc) = self.add_def(
                &p.0.loc(),
                &p.0.value(),
                &mut fn_scope,
                references,
                use_defs,
            ) {
                let exists = fn_scope.insert(p.0.value(), def_loc);
                // TODO: enable assertion when all scoping rules are in place
                //                debug_assert!(exists.is_none());
            }
        }
        scope_stack.push_front(fn_scope);

        match &fun.body.value {
            FunctionBody_::Defined(s) => {
                for seq_item in s {
                    self.seq_item_symbols(&mut scope_stack, seq_item, references, use_defs);
                }
            }
            FunctionBody_::Native => (),
        }

        // pop the main function scope
        scope_stack.pop_front();
        debug_assert!(scope_stack.len() == 0);

        // after processing the function body, create a scope to process return types
        let mut ret_scope = Scope::new();
        scope_stack.push_front(ret_scope);

        // TODO: process return types

        // pop the return parameters scope
        scope_stack.pop_front();
        debug_assert!(scope_stack.len() == 0);

        // clear type params from the scope
        self.type_params.clear();
    }

    fn seq_item_symbols(
        &self,
        scope_stack: &mut VecDeque<Scope>,
        seq_item: &SequenceItem,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        use SequenceItem_ as I;
        match &seq_item.value {
            I::Seq(e) => self.exp_symbols(e, scope_stack, references, use_defs),
            I::Declare(lvalues) => {
                self.lvalue_list_symbols(lvalues, scope_stack, references, use_defs)
            }
            I::Bind(lvalues, _, e) => {
                // process RHS first to avoid accidentally binding its
                // identifiers to LHS (which now will be put into the
                // current scope only after RHS is processed)
                self.exp_symbols(e, scope_stack, references, use_defs);
                self.lvalue_list_symbols(lvalues, scope_stack, references, use_defs);
            }
        }
    }

    fn lvalue_list_symbols(
        &self,
        lvalues: &LValueList,
        scope_stack: &mut VecDeque<Scope>,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        let mut scope = scope_stack.pop_front().unwrap(); // scope stack guaranteed non-empty
        for lval in &lvalues.value {
            self.lvalue_symbols(lval, &mut scope, references, use_defs);
        }
        scope_stack.push_front(scope);
    }

    fn lvalue_symbols(
        &self,
        lval: &LValue,
        scope: &mut Scope,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        match &lval.value {
            LValue_::Var(var, _) => {
                self.add_def(&var.loc(), &var.value(), scope, references, use_defs);
            }
            LValue_::Unpack(ident, name, _, fields) => {
                self.unpack_symbols(&ident.value, name, fields, scope, references, use_defs);
            }
            LValue_::BorrowUnpack(_, ident, name, _, fields) => {
                self.unpack_symbols(&ident.value, name, fields, scope, references, use_defs);
            }
            LValue_::Ignore => (),
        }
    }

    fn unpack_symbols(
        &self,
        ident: &ModuleIdent_,
        name: &StructName,
        fields: &Fields<(Type, LValue)>,
        scope: &mut Scope,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        // add use of the struct name
        self.add_struct_def(ident, &name.value(), &name.loc(), references, use_defs);
        for (fpos, fname, lval) in fields {
            // add use of the field name
            self.add_field_def(ident, &name.value(), fname, &fpos, references, use_defs);
            // add definition of a variable used for struct field unpacking
            self.lvalue_symbols(&lval.1 .1, scope, references, use_defs);
        }
    }

    fn add_struct_def(
        &self,
        module_ident: &ModuleIdent_,
        use_name: &Symbol,
        use_pos: &Loc,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        let name_start = match Self::get_start_loc(use_pos, &self.files, &self.file_id_mapping) {
            Some(v) => v,
            None => {
                debug_assert!(false);
                return;
            }
        };

        let mod_defs = match self.mod_outer_defs.get(module_ident) {
            Some(v) => v,
            None => {
                debug_assert!(false);
                return;
            }
        };

        match mod_defs.structs.get(use_name) {
            Some(def) => {
                use_defs.insert(
                    name_start.line,
                    UseDef::new(
                        references,
                        use_pos.file_hash(),
                        name_start,
                        module_ident.module.loc().file_hash(),
                        def.name_start,
                        use_name,
                    ),
                );
            }
            None => debug_assert!(false),
        };
    }

    fn add_field_def(
        &self,
        module_ident: &ModuleIdent_,
        struct_name: &Symbol,
        use_name: &Symbol,
        use_pos: &Loc,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        let name_start = match Self::get_start_loc(use_pos, &self.files, &self.file_id_mapping) {
            Some(v) => v,
            None => {
                debug_assert!(false);
                return;
            }
        };

        let mod_defs = match self.mod_outer_defs.get(module_ident) {
            Some(v) => v,
            None => {
                debug_assert!(false);
                return;
            }
        };

        match mod_defs.structs.get(struct_name) {
            Some(def) => {
                for fdef in &def.field_defs {
                    if fdef.name == *use_name {
                        use_defs.insert(
                            name_start.line,
                            UseDef::new(
                                references,
                                use_pos.file_hash(),
                                name_start,
                                module_ident.module.loc().file_hash(),
                                fdef.start,
                                use_name,
                            ),
                        );
                    }
                }
            }
            None => debug_assert!(false),
        };
    }

    fn add_def(
        &self,
        pos: &Loc,
        name: &Symbol,
        scope: &mut Scope,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) -> Option<DefLoc> {
        match Self::get_start_loc(pos, &self.files, &self.file_id_mapping) {
            Some(name_start) => {
                let def_loc = DefLoc {
                    fhash: pos.file_hash(),
                    start: name_start,
                };
                let exists = scope.insert(*name, def_loc.clone());
                // should be only one def with the same name in a given scope

                // TODO: enable assertion when all scoping rules are in place
                //                debug_assert!(exists.is_none());

                // enter self-definition for def name
                use_defs.insert(
                    name_start.line,
                    UseDef::new(
                        references,
                        pos.file_hash(),
                        name_start,
                        pos.file_hash(),
                        name_start,
                        name,
                    ),
                );
                Some(def_loc)
            }
            None => {
                debug_assert!(false);
                None
            }
        }
    }

    fn add_use_def(
        &self,
        use_name: &Symbol,
        use_pos: &Loc,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        scope_stack: &VecDeque<Scope>,
        use_defs: &mut UseDefMap,
    ) {
        let name_start = match Self::get_start_loc(use_pos, &self.files, &self.file_id_mapping) {
            Some(v) => v,
            None => {
                debug_assert!(false);
                return;
            }
        };

        for s in scope_stack {
            if let Some(def_loc) = s.get(use_name) {
                use_defs.insert(
                    name_start.line,
                    UseDef::new(
                        references,
                        use_pos.file_hash(),
                        name_start,
                        def_loc.fhash,
                        def_loc.start,
                        use_name,
                    ),
                );
                return;
            }
        }
        debug_assert!(false);
    }

    fn exp_symbols(
        &self,
        exp: &Exp,
        scope_stack: &mut VecDeque<Scope>,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        use UnannotatedExp_ as E;
        match &exp.exp.value {
            E::Move {
                from_user: _,
                var: v,
            } => self.add_use_def(&v.value(), &v.loc(), references, scope_stack, use_defs),
            E::Copy {
                from_user: _,
                var: v,
            } => self.add_use_def(&v.value(), &v.loc(), references, scope_stack, use_defs),
            E::Pack(ident, name, _, fields) => {
                self.pack_symbols(
                    &ident.value,
                    &name,
                    &fields,
                    scope_stack,
                    references,
                    use_defs,
                );
            }

            _ => (),
        }
    }

    fn pack_symbols(
        &self,
        ident: &ModuleIdent_,
        name: &StructName,
        fields: &Fields<(Type, Exp)>,
        scope_stack: &mut VecDeque<Scope>,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        // add use of the struct name
        self.add_struct_def(ident, &name.value(), &name.loc(), references, use_defs);
        for (fpos, fname, lval) in fields {
            // add use of the field name
            self.add_field_def(ident, &name.value(), fname, &fpos, references, use_defs);
            // add field initialization expression
            self.exp_symbols(&lval.1 .1, scope_stack, references, use_defs);
        }
    }
}

pub fn on_go_to_def_request(context: &Context, request: &Request, symbols: &Symbols) {
    let parameters = serde_json::from_value::<GotoDefinitionParams>(request.params.clone())
        .expect("could not deserialize go-to-def request");

    let fpath = parameters
        .text_document_position_params
        .text_document
        .uri
        .path();
    let loc = parameters.text_document_position_params.position;
    let line = loc.line;
    let col = loc.character;

    on_use_request(
        context,
        symbols,
        fpath,
        line,
        col,
        request.id.clone(),
        |u| {
            // TODO: Do we need beginning and end of the
            // definition? Does not seem to make a
            // difference from the IDE perspective as the
            // cursor goes to the beginning anyway (at
            // least in VSCode).
            let range = Range {
                start: u.def_loc.start,
                end: u.def_loc.start,
            };
            let path = symbols.file_name_mapping.get(&u.def_loc.fhash).unwrap();
            let loc = Location {
                uri: Url::from_file_path(path.as_str()).unwrap(),
                range,
            };
            Some(serde_json::to_value(loc).unwrap())
        },
    );
}

pub fn on_references_request(context: &Context, request: &Request, symbols: &Symbols) {
    let parameters = serde_json::from_value::<ReferenceParams>(request.params.clone())
        .expect("could not deserialize references request");

    let fpath = parameters.text_document_position.text_document.uri.path();
    let loc = parameters.text_document_position.position;
    let line = loc.line;
    let col = loc.character;
    let include_decl = parameters.context.include_declaration;

    on_use_request(
        context,
        symbols,
        fpath,
        line,
        col,
        request.id.clone(),
        |u| match symbols.references.get(&u.def_loc) {
            Some(s) => {
                let mut locs = vec![];
                for ref_loc in s {
                    if include_decl
                        || !(u.def_loc.start == ref_loc.start && u.def_loc.fhash == ref_loc.fhash)
                    {
                        let end_pos = Position {
                            line: ref_loc.start.line,
                            character: ref_loc.col_end,
                        };
                        let range = Range {
                            start: ref_loc.start,
                            end: end_pos,
                        };
                        let path = symbols.file_name_mapping.get(&ref_loc.fhash).unwrap();
                        locs.push(Location {
                            uri: Url::from_file_path(path.as_str()).unwrap(),
                            range,
                        });
                    }
                }
                if locs.is_empty() {
                    Some(serde_json::to_value(Option::<lsp_types::Location>::None).unwrap())
                } else {
                    Some(serde_json::to_value(locs).unwrap())
                }
            }
            None => Some(serde_json::to_value(Option::<lsp_types::Location>::None).unwrap()),
        },
    );
}

pub fn on_use_request(
    context: &Context,
    symbols: &Symbols,
    use_fpath: &str,
    use_line: u32,
    use_col: u32,
    id: RequestId,
    use_def_action: impl Fn(&UseDef) -> Option<serde_json::Value>,
) {
    let mut result = Option::<serde_json::Value>::None;

    let mut use_def_found = false;
    if let Some(mod_ident) = symbols.mod_ident_map.get(&PathBuf::from(use_fpath)) {
        if let Some(mod_symbols) = symbols.mod_use_defs.get(mod_ident) {
            if let Some(uses) = mod_symbols.get(use_line) {
                for u in uses {
                    if use_col >= u.col_start && use_col <= u.col_end {
                        result = use_def_action(&u);
                        use_def_found = true;
                    }
                }
            }
        }
    }
    if !use_def_found {
        result = Some(serde_json::to_value(Option::<lsp_types::Location>::None).unwrap());
    }

    eprintln!("about to send use response");
    // unwrap will succeed based on the logic above which the compiler
    // is unable to figure out without using Option
    let response = lsp_server::Response::new_ok(id, result.unwrap());
    context
        .connection
        .sender
        .send(lsp_server::Message::Response(response))
        .expect("could not send use response");
}

fn assert_use_def(
    mod_symbols: &UseDefMap,
    use_idx: usize,
    use_line: u32,
    use_col: u32,
    def_line: u32,
    def_col: u32,
) {
    let uses = mod_symbols.get(use_line).unwrap();
    let use_def = uses.iter().nth(use_idx).unwrap();
    assert!(use_def.col_start == use_col);
    assert!(use_def.def_loc.start.line == def_line);
    assert!(use_def.def_loc.start.character == def_col);
}

#[test]
fn symbols_build_test() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    path.push("tests/basic");

    let symbols = Symbolicator::get_symbols(path.as_path()).unwrap();

    path.push("sources/M1.move");
    let cpath = fs::canonicalize(&path).unwrap();

    let mod_ident = symbols.mod_ident_map.get(&cpath).unwrap();
    let mod_symbols = symbols.mod_use_defs.get(mod_ident).unwrap();

    // struct def name
    assert_use_def(mod_symbols, 0, 2, 11, 2, 11);
    // const def name
    assert_use_def(mod_symbols, 0, 6, 10, 6, 10);
    // function def name
    assert_use_def(mod_symbols, 0, 9, 8, 9, 8);

    // param (unpack function)
    assert_use_def(mod_symbols, 1, 9, 15, 9, 15);
    // field name in unpack (unpack function)
    assert_use_def(mod_symbols, 0, 10, 25, 3, 8);
    // bound variable in unpack (unpack function)
    assert_use_def(mod_symbols, 1, 10, 37, 10, 37);
    // moved var in unpack assignment (unpack function)
    assert_use_def(mod_symbols, 2, 10, 47, 9, 15);

    // copied var in an assignment (cp function)
    assert_use_def(mod_symbols, 1, 15, 18, 14, 11);

    // field name in pack (pack function)
    assert_use_def(mod_symbols, 1, 20, 31, 3, 8);
}
