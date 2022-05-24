// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! This module is responsible for building symbolication information on top of compiler's typed
//! AST, in particular identifier definitions to be used for implementing go-to-def and
//! go-to-references language server commands.
//!
//! There are two main structs that are used at different phases of the process, the Symbolicator
//! struct is used when building symbolication information and the Symbols struct is summarizes the
//! symbolication results and is used by the language server find definitions and references.
//!
//! Here is a brief description of how the symbolication information is encoded. Each identifier is
//! in the source code of a given module is represented by its location (UseLoc struct): line
//! number, starting and ending column, and hash of the source file where this identifier is
//! located). A definition for each identifier (if any - e.g., built-in type definitions are
//! excluded as there is no place in source code where they are defined) is also represented by its
//! location in the source code (DefLoc struct): line, starting column and a hash of the source
//! file where it's located. The symbolication process maps each identifier with its definition - a
//! per module map is keyed on the line number where the identifier is located, and the map entry
//! contains a list of identifier/definition pairs ordered by the column where the identifier starts.
//!
//! For example consider the following code fragment (0-based line numbers on the left and 0-based
//! column numbers at the bottom):
//!
//! 7: const SOME_CONST: u64 = 42;
//! 8:
//! 9: SOME_CONST + SOME_CONST
//!    |     |  |   | |      |
//!    0     6  9  13 15    22
//!
//! Symbolication information for this code fragment would look as follows assuming that this code
//! is stored in a file with hash FHASH (note that identifier in the definition of the constant maps
//! to itself):
//!
//! [7] -> [UseLoc(7:6-13, FHASH), DefLoc(7:6, FHASH)]
//! [9] -> [UseLoc(9:0-9 , FHASH), DefLoc((7:6, FHASH)], [UseLoc(9:13-22, FHASH), DefLoc((7:6, FHASH)]
//!
//! Including line number (and file hash) with the (use) identifier location may appear redundant,
//! but it's needed to allow accumulating uses with each definition to support
//! go-to-references. This is done in a global map from an identifier location (DefLoc) to a set of
//! use locations (UseLoc) - we find a all references of a given identifier by first finding its
//! definition and then using this definition as a key to the global map.
//!
//! Symbolication algorithm first analyzes all top-level definitions from all modules and then
//! processes function bodies and struct definitions to match uses to definitions. For local
//! definitions, the symbolicator builds a scope stack, entering encountered definitions and
//! matching uses to a definition in the innermost scope.

use crate::context::Context;
use anyhow::Result;
use codespan_reporting::files::{Files, SimpleFiles};
use im::ordmap::OrdMap;
use lsp_server::{Request, RequestId};
use lsp_types::{GotoDefinitionParams, Location, Position, Range, ReferenceParams};
use std::{
    cmp,
    collections::{BTreeMap, BTreeSet},
    fs,
    path::{Path, PathBuf},
};
use tempfile::tempdir;
use url::Url;

use move_command_line_common::files::FileHash;
use move_compiler::{
    diagnostics,
    expansion::ast::{Fields, ModuleIdent, ModuleIdent_},
    naming::ast::{StructDefinition, StructFields, TParam, Type, TypeName_, Type_},
    parser::ast::StructName,
    shared::Identifier,
    typing::ast::{
        BuiltinFunction_, Exp, ExpListItem, Function, FunctionBody_, LValue, LValueList, LValue_,
        ModuleCall, ModuleDefinition, SequenceItem, SequenceItem_, UnannotatedExp_,
    },
    PASS_TYPING,
};
use move_ir_types::location::*;
use move_package::compilation::build_plan::BuildPlan;
use move_symbol_pool::Symbol;

/// Enabling/disabling the language server reporting readiness to support go-to-def and
/// go-to-references to the IDE.
pub const DEFS_AND_REFS_SUPPORT: bool = false;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Copy)]
/// Location of a definition's identifier
struct DefLoc {
    /// File where the definition of the identifier starts
    fhash: FileHash,
    /// Location where the definition of the identifier starts
    start: Position,
}

/// Location of a use's identifier
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Copy)]
struct UseLoc {
    /// File where this use identifier starts
    fhash: FileHash,
    /// Location where this use identifier starts
    start: Position,
    /// Column (on the same line as start)  where this use identifier ends
    col_end: u32,
}

/// Information about both the use identifier (source file is specified wherever an instance of this
/// struct is used) and the definition identifier
#[derive(Debug, Clone, Eq, Copy)]
pub struct UseDef {
    /// Column where the (use) identifier location starts on a given line (use this field for
    /// sorting uses on the line)
    col_start: u32,
    /// Column where the (use) identifier location ends on a given line
    col_end: u32,
    /// Location of the definition
    def_loc: DefLoc,
}

/// Definition of a struct field
#[derive(Debug)]
struct FieldDef {
    name: Symbol,
    start: Position,
}

/// Definition of a struct
#[derive(Debug)]
struct StructDef {
    name_start: Position,
    field_defs: Vec<FieldDef>,
}

/// Module-level definitions
#[derive(Debug)]
struct ModuleDefs {
    /// File where this module is located
    fhash: FileHash,
    /// Struct definitions
    structs: BTreeMap<Symbol, StructDef>,
    /// Const definitions
    constants: BTreeMap<Symbol, Position>,
    /// Function definitions
    functions: BTreeMap<Symbol, Position>,
}

/// Data used during symbolication
pub struct Symbolicator {
    /// Outermost definitions in a module (structs, consts, functions)
    mod_outer_defs: BTreeMap<ModuleIdent_, ModuleDefs>,
    /// A mapping from file names to file content (used to obtain source file locations)
    files: SimpleFiles<Symbol, String>,
    /// A mapping from file hashes to file IDs (used to obtain source file locations)
    file_id_mapping: BTreeMap<FileHash, usize>,
    /// Contains type params where relevant (e.g. when processing function definition)
    type_params: BTreeMap<Symbol, DefLoc>,
    /// Current processed module (always set before module processing starts)
    current_mod: Option<ModuleIdent_>,
}

/// Result of the symbolication process
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
        let use_loc = UseLoc {
            fhash: use_fhash,
            start: use_start,
            col_end,
        };

        references
            .entry(def_loc)
            .or_insert_with(BTreeSet::new)
            .insert(use_loc);

        Self {
            col_start: use_start.character,
            col_end,
            def_loc,
        }
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

/// Maps a line number to a list of use-def pairs on a given line (use-def set is sorted by
/// col_start)
#[derive(Debug)]
struct UseDefMap(BTreeMap<u32, BTreeSet<UseDef>>);

impl UseDefMap {
    fn new() -> Self {
        Self(BTreeMap::new())
    }

    fn insert(&mut self, key: u32, val: UseDef) {
        self.0.entry(key).or_insert_with(BTreeSet::new).insert(val);
    }

    fn get(&self, key: u32) -> Option<BTreeSet<UseDef>> {
        self.0.get(&key).cloned()
    }
}

impl Symbolicator {
    /// Main driver to get symbols for the whole package
    pub fn get_symbols(pkg_path: &Path) -> Result<Symbols> {
        let build_config = move_package::BuildConfig {
            test_mode: true,
            install_dir: Some(tempdir().unwrap().path().to_path_buf()),
            ..Default::default()
        };

        eprintln!("symbolicating {:?}", pkg_path);

        let resolution_graph = build_config.resolution_graph_for_package(pkg_path)?;

        // get source files to be able to correlate positions (in terms of byte offsets) with actual
        // file locations (in terms of line/column numbers)
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
        let mut typed_ast = None;
        build_plan.compile_with_driver(&mut std::io::sink(), |compiler| {
            let (files, comments_and_compiler_res) = compiler.run::<PASS_TYPING>().unwrap();
            let (_, compiler) =
                diagnostics::unwrap_or_report_diagnostics(&files, comments_and_compiler_res);
            let (compiler, typed_program) = compiler.into_ast();
            typed_ast = Some(typed_program.clone());
            let compilation_result = compiler.at_typing(typed_program).build();

            let (units, _) = diagnostics::unwrap_or_report_diagnostics(&files, compilation_result);
            Ok((files, units))
        })?;

        debug_assert!(typed_ast.is_some());
        let modules = &typed_ast.unwrap().modules;

        let mut mod_outer_defs = BTreeMap::new();
        let mut references = BTreeMap::new();
        let mut mod_use_defs = BTreeMap::new();
        let mut mod_ident_map = BTreeMap::new();
        for (pos, module_ident, module_def) in modules {
            let (defs, symbols) = Self::get_mod_outer_defs(
                &pos,
                &mut references,
                module_def,
                &files,
                &file_id_mapping,
            );
            mod_outer_defs.insert(*module_ident, defs);
            mod_use_defs.insert(*module_ident, symbols);

            let fpath = match source_files.get(&pos.file_hash()) {
                Some((p, _)) => p,
                None => continue,
            };

            mod_ident_map.insert(
                // if canonicalization fails, simply use "regular" path to continue processing
                fs::canonicalize(fpath.as_str()).unwrap_or_else(|_| PathBuf::from(fpath.as_str())),
                *module_ident,
            );
        }

        let mut symbolicator = Symbolicator {
            mod_outer_defs,
            files,
            file_id_mapping,
            type_params: BTreeMap::new(),
            current_mod: None,
        };

        for (_, module_ident, module_def) in modules {
            let use_defs = mod_use_defs.get_mut(module_ident).unwrap();
            symbolicator.current_mod = Some(*module_ident);
            symbolicator.mod_symbols(module_def, &mut references, use_defs);
        }

        Ok(Symbols {
            references,
            mod_use_defs,
            mod_ident_map,
            file_name_mapping,
        })
    }

    /// Get empty symbols
    pub fn empty_symbols() -> Symbols {
        Symbols {
            mod_use_defs: BTreeMap::new(),
            mod_ident_map: BTreeMap::new(),
            references: BTreeMap::new(),
            file_name_mapping: BTreeMap::new(),
        }
    }

    /// Main AST traversal functions

    /// Get symbols for outer definitions in the module (functions, structs, and consts)
    fn get_mod_outer_defs(
        loc: &Loc,
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
            // process field structs first
            let mut field_defs = vec![];
            if let StructFields::Defined(fields) = &def.fields {
                for (fpos, fname, _) in fields {
                    let start = match Self::get_start_loc(&fpos, files, file_id_mapping) {
                        Some(s) => s,
                        None => {
                            debug_assert!(false);
                            continue;
                        }
                    };
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
                    );
                }
            };

            // process the struct itself
            let name_start = match Self::get_start_loc(&pos, files, file_id_mapping) {
                Some(s) => s,
                None => {
                    debug_assert!(false);
                    continue;
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

        for (pos, name, _) in &mod_def.constants {
            let name_start = match Self::get_start_loc(&pos, files, file_id_mapping) {
                Some(s) => s,
                None => {
                    debug_assert!(false);
                    continue;
                }
            };
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

        for (pos, name, _) in &mod_def.functions {
            let name_start = match Self::get_start_loc(&pos, files, file_id_mapping) {
                Some(s) => s,
                None => {
                    debug_assert!(false);
                    continue;
                }
            };
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

        let fhash = loc.file_hash();
        let module_defs = ModuleDefs {
            fhash,
            structs,
            constants,
            functions,
        };

        (module_defs, use_def_map)
    }

    /// Get symbols for the whole module
    fn mod_symbols(
        &mut self,
        mod_def: &ModuleDefinition,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        for (_, _, fun) in &mod_def.functions {
            self.fun_symbols(fun, references, use_defs);
        }
        // TODO: for now we are skipping constants as they can only have primitive type values, but
        // this can change in the future
        for (_, _, struct_def) in &mod_def.structs {
            self.struct_symbols(struct_def, references, use_defs);
        }
    }

    /// Get symbols for function a definition
    fn struct_symbols(
        &mut self,
        struct_def: &StructDefinition,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        // create scope designated to contain type parameters (if any)
        let mut tp_scope = BTreeMap::new();
        for stp in &struct_def.type_parameters {
            self.add_type_param(&stp.param, &mut tp_scope, references, use_defs);
        }
        self.type_params = tp_scope;
        if let StructFields::Defined(fields) = &struct_def.fields {
            for (_, _, (_, t)) in fields {
                self.add_type_id_use_def(t, references, use_defs);
            }
        }
    }

    /// Get symbols for function a definition
    fn fun_symbols(
        &mut self,
        fun: &Function,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        // create scope designated to contain type parameters (if any)
        let mut tp_scope = BTreeMap::new();
        for tp in &fun.signature.type_parameters {
            self.add_type_param(tp, &mut tp_scope, references, use_defs);
        }
        self.type_params = tp_scope;

        // scope for the main function scope (for parameters and
        // function body)
        let mut scope = OrdMap::new();

        for (pname, ptype) in &fun.signature.parameters {
            self.add_type_id_use_def(ptype, references, use_defs);

            // add definition of the parameter
            self.add_def(
                &pname.loc(),
                &pname.value(),
                &mut scope,
                references,
                use_defs,
            );
        }

        match &fun.body.value {
            FunctionBody_::Defined(sequence) => {
                for seq_item in sequence {
                    self.seq_item_symbols(&mut scope, seq_item, references, use_defs);
                }
            }
            FunctionBody_::Native => (),
        }

        // process return types
        self.add_type_id_use_def(&fun.signature.return_type, references, use_defs);
        // process optional "acquires" clause
        for name in fun.acquires.keys() {
            self.add_struct_use_def(
                &self.current_mod.unwrap(),
                &name.value(),
                &name.loc(),
                references,
                use_defs,
            );
        }

        // clear type params from the scope
        self.type_params.clear();
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

    /// Get symbols for a sequence representing function body
    fn seq_item_symbols(
        &self,
        scope: &mut OrdMap<Symbol, DefLoc>,
        seq_item: &SequenceItem,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        use SequenceItem_ as I;
        match &seq_item.value {
            I::Seq(e) => self.exp_symbols(e, scope, references, use_defs),
            I::Declare(lvalues) => {
                self.lvalue_list_symbols(true, lvalues, scope, references, use_defs)
            }
            I::Bind(lvalues, opt_types, e) => {
                // process RHS first to avoid accidentally binding its identifiers to LHS (which now
                // will be put into the current scope only after RHS is processed)
                self.exp_symbols(e, scope, references, use_defs);
                for opt_t in opt_types {
                    match opt_t {
                        Some(t) => self.add_type_id_use_def(t, references, use_defs),
                        None => (),
                    }
                }
                self.lvalue_list_symbols(true, lvalues, scope, references, use_defs);
            }
        }
    }

    /// Get symbols for a list of lvalues
    fn lvalue_list_symbols(
        &self,
        define: bool,
        lvalues: &LValueList,
        scope: &mut OrdMap<Symbol, DefLoc>,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        for lval in &lvalues.value {
            self.lvalue_symbols(define, lval, scope, references, use_defs);
        }
    }

    /// Get symbols for a single lvalue
    fn lvalue_symbols(
        &self,
        define: bool,
        lval: &LValue,
        scope: &mut OrdMap<Symbol, DefLoc>,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        match &lval.value {
            LValue_::Var(var, _) => {
                if define {
                    self.add_def(&var.loc(), &var.value(), scope, references, use_defs);
                } else {
                    self.add_local_use_def(&var.value(), &var.loc(), references, scope, use_defs)
                }
            }
            LValue_::Unpack(ident, name, tparams, fields) => {
                self.unpack_symbols(
                    define,
                    &ident.value,
                    name,
                    tparams,
                    fields,
                    scope,
                    references,
                    use_defs,
                );
            }
            LValue_::BorrowUnpack(_, ident, name, tparams, fields) => {
                self.unpack_symbols(
                    define,
                    &ident.value,
                    name,
                    tparams,
                    fields,
                    scope,
                    references,
                    use_defs,
                );
            }
            LValue_::Ignore => (),
        }
    }

    /// Get symbols for the unpack statement
    fn unpack_symbols(
        &self,
        define: bool,
        ident: &ModuleIdent_,
        name: &StructName,
        tparams: &Vec<Type>,
        fields: &Fields<(Type, LValue)>,
        scope: &mut OrdMap<Symbol, DefLoc>,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        // add use of the struct name
        self.add_struct_use_def(ident, &name.value(), &name.loc(), references, use_defs);
        for (fpos, fname, (_, (_, lvalue))) in fields {
            // add use of the field name
            self.add_field_use_def(ident, &name.value(), fname, &fpos, references, use_defs);
            // add definition or use of a variable used for struct field unpacking
            self.lvalue_symbols(define, lvalue, scope, references, use_defs);
        }
        // add type params
        for t in tparams {
            self.add_type_id_use_def(t, references, use_defs);
        }
    }

    /// Get symbols for an expression
    fn exp_symbols(
        &self,
        exp: &Exp,
        scope: &mut OrdMap<Symbol, DefLoc>,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        use UnannotatedExp_ as E;
        match &exp.exp.value {
            E::Move {
                from_user: _,
                var: v,
            } => self.add_local_use_def(&v.value(), &v.loc(), references, scope, use_defs),
            E::Copy {
                from_user: _,
                var: v,
            } => self.add_local_use_def(&v.value(), &v.loc(), references, scope, use_defs),
            E::Use(v) => self.add_local_use_def(&v.value(), &v.loc(), references, scope, use_defs),
            E::Constant(mod_ident_opt, name) => self.add_const_use_def(
                mod_ident_opt,
                &name.value(),
                &name.loc(),
                references,
                use_defs,
            ),
            E::ModuleCall(mod_call) => self.mod_call_symbols(mod_call, scope, references, use_defs),
            E::Builtin(builtin_fun, exp) => {
                use BuiltinFunction_ as BF;
                match &builtin_fun.value {
                    BF::MoveTo(t) => self.add_type_id_use_def(t, references, use_defs),
                    BF::MoveFrom(t) => self.add_type_id_use_def(t, references, use_defs),
                    BF::BorrowGlobal(_, t) => self.add_type_id_use_def(t, references, use_defs),
                    BF::Exists(t) => self.add_type_id_use_def(t, references, use_defs),
                    BF::Freeze(t) => self.add_type_id_use_def(t, references, use_defs),
                    _ => (),
                }
                self.exp_symbols(exp, scope, references, use_defs);
            }
            E::Vector(_, _, t, exp) => {
                self.add_type_id_use_def(t, references, use_defs);
                self.exp_symbols(exp, scope, references, use_defs);
            }
            E::IfElse(cond, t, f) => {
                self.exp_symbols(cond, scope, references, use_defs);
                self.exp_symbols(t, scope, references, use_defs);
                self.exp_symbols(f, scope, references, use_defs);
            }
            E::While(cond, body) => {
                self.exp_symbols(cond, scope, references, use_defs);
                self.exp_symbols(body, scope, references, use_defs);
            }
            E::Loop { has_break: _, body } => {
                self.exp_symbols(body, scope, references, use_defs);
            }
            E::Block(sequence) => {
                // a block is a new var scope
                let mut new_scope = scope.clone();
                for seq_item in sequence {
                    self.seq_item_symbols(&mut new_scope, seq_item, references, use_defs);
                }
            }
            E::Assign(lvalues, opt_types, e) => {
                self.lvalue_list_symbols(false, lvalues, scope, references, use_defs);
                for opt_t in opt_types {
                    match opt_t {
                        Some(t) => self.add_type_id_use_def(t, references, use_defs),
                        None => (),
                    }
                }
                self.exp_symbols(e, scope, references, use_defs);
            }
            E::Mutate(lhs, rhs) => {
                self.exp_symbols(lhs, scope, references, use_defs);
                self.exp_symbols(rhs, scope, references, use_defs);
            }
            E::Return(exp) => {
                self.exp_symbols(exp, scope, references, use_defs);
            }
            E::Abort(exp) => {
                self.exp_symbols(exp, scope, references, use_defs);
            }
            E::Dereference(exp) => {
                self.exp_symbols(exp, scope, references, use_defs);
            }
            E::UnaryExp(_, exp) => {
                self.exp_symbols(exp, scope, references, use_defs);
            }
            E::BinopExp(lhs, _, _, rhs) => {
                self.exp_symbols(lhs, scope, references, use_defs);
                self.exp_symbols(rhs, scope, references, use_defs);
            }
            E::Pack(ident, name, tparams, fields) => {
                self.pack_symbols(
                    &ident.value,
                    name,
                    tparams,
                    fields,
                    scope,
                    references,
                    use_defs,
                );
            }
            E::ExpList(list_items) => {
                for item in list_items {
                    let exp = match item {
                        // TODO: are types important for symbolication here (and, more generally,
                        // what's a splat?)
                        ExpListItem::Single(e, _) => e,
                        ExpListItem::Splat(_, e, _) => e,
                    };
                    self.exp_symbols(exp, scope, references, use_defs);
                }
            }
            E::Borrow(_, exp, field) => {
                self.exp_symbols(exp, scope, references, use_defs);
                // get expression type to match fname to a struct def
                self.add_field_type_use_def(
                    &exp.ty,
                    &field.value(),
                    &field.loc(),
                    references,
                    use_defs,
                );
            }
            E::TempBorrow(_, exp) => {
                self.exp_symbols(exp, scope, references, use_defs);
            }
            E::BorrowLocal(_, var) => {
                self.add_local_use_def(&var.value(), &var.loc(), references, scope, use_defs)
            }
            E::Cast(exp, t) => {
                self.exp_symbols(exp, scope, references, use_defs);
                self.add_type_id_use_def(t, references, use_defs);
            }
            E::Annotate(exp, t) => {
                self.exp_symbols(exp, scope, references, use_defs);
                self.add_type_id_use_def(t, references, use_defs);
            }

            _ => (),
        }
    }

    /// Add a type for a struct field given its type
    fn add_field_type_use_def(
        &self,
        sp!(_, typ): &Type,
        use_name: &Symbol,
        use_pos: &Loc,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        match typ {
            Type_::Ref(_, t) => {
                self.add_field_type_use_def(t, use_name, use_pos, references, use_defs)
            }
            Type_::Apply(_, sp!(_, TypeName_::ModuleType(sp!(_, mod_ident), struct_name)), _) => {
                self.add_field_use_def(
                    mod_ident,
                    &struct_name.value(),
                    use_name,
                    use_pos,
                    references,
                    use_defs,
                );
            }
            _ => (),
        }
    }

    fn mod_call_symbols(
        &self,
        mod_call: &ModuleCall,
        scope: &mut OrdMap<Symbol, DefLoc>,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        // handle function name
        let sp!(_, mod_ident) = mod_call.module;
        self.add_fun_use_def(
            &mod_ident,
            &mod_call.name.value(),
            &mod_call.name.loc(),
            references,
            use_defs,
        );

        // handle type parameters
        for t in &mod_call.type_arguments {
            self.add_type_id_use_def(t, references, use_defs);
        }

        // handle arguments
        self.exp_symbols(&mod_call.arguments, scope, references, use_defs);
    }

    /// Get symbols for the pack expression
    fn pack_symbols(
        &self,
        ident: &ModuleIdent_,
        name: &StructName,
        tparams: &Vec<Type>,
        fields: &Fields<(Type, Exp)>,
        scope: &mut OrdMap<Symbol, DefLoc>,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        // add use of the struct name
        self.add_struct_use_def(ident, &name.value(), &name.loc(), references, use_defs);
        for (fpos, fname, (_, (_, init_exp))) in fields {
            // add use of the field name
            self.add_field_use_def(ident, &name.value(), fname, &fpos, references, use_defs);
            // add field initialization expression
            self.exp_symbols(init_exp, scope, references, use_defs);
        }
        // add type params
        for t in tparams {
            self.add_type_id_use_def(t, references, use_defs);
        }
    }

    /// Helper functions

    /// Add type parameter to a scope holding type params
    fn add_type_param(
        &mut self,
        tp: &TParam,
        tp_scope: &mut BTreeMap<Symbol, DefLoc>,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        match Self::get_start_loc(
            &tp.user_specified_name.loc,
            &self.files,
            &self.file_id_mapping,
        ) {
            Some(start) => {
                let tname = tp.user_specified_name.value;
                let fhash = tp.user_specified_name.loc.file_hash();
                // enter self-definition for type param
                use_defs.insert(
                    start.line,
                    UseDef::new(references, fhash, start, fhash, start, &tname),
                );
                let exists = tp_scope.insert(tname, DefLoc { fhash, start });
                debug_assert!(exists.is_none());
            }
            None => {
                debug_assert!(false);
            }
        };
    }

    /// Add use of one of identifiers defined at the module level
    fn add_outer_use_def(
        &self,
        module_ident: &ModuleIdent_,
        use_name: &Symbol,
        use_pos: &Loc,
        mut add_fn: impl FnMut(&Symbol, Position, &ModuleDefs),
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
        add_fn(use_name, name_start, mod_defs);
    }

    /// Add use of a const identifier
    fn add_const_use_def(
        &self,
        module_ident_opt: &Option<ModuleIdent>,
        use_name: &Symbol,
        use_pos: &Loc,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        let module_ident = match module_ident_opt {
            Some(v) => v.value,
            None => self.current_mod.unwrap(),
        };

        self.add_outer_use_def(
            &module_ident,
            use_name,
            use_pos,
            |use_name, name_start, mod_defs| match mod_defs.constants.get(use_name) {
                Some(def_start) => {
                    use_defs.insert(
                        name_start.line,
                        UseDef::new(
                            references,
                            use_pos.file_hash(),
                            name_start,
                            self.mod_outer_defs.get(&module_ident).unwrap().fhash,
                            *def_start,
                            use_name,
                        ),
                    );
                }
                None => debug_assert!(false),
            },
        );
    }

    /// Add use of a function identifier
    fn add_fun_use_def(
        &self,
        module_ident: &ModuleIdent_,
        use_name: &Symbol,
        use_pos: &Loc,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        self.add_outer_use_def(
            module_ident,
            use_name,
            use_pos,
            |use_name, name_start, mod_defs| match mod_defs.functions.get(use_name) {
                Some(def_start) => {
                    use_defs.insert(
                        name_start.line,
                        UseDef::new(
                            references,
                            use_pos.file_hash(),
                            name_start,
                            self.mod_outer_defs.get(module_ident).unwrap().fhash,
                            *def_start,
                            use_name,
                        ),
                    );
                }
                None => debug_assert!(false),
            },
        );
    }

    /// Add use of a struct identifier
    fn add_struct_use_def(
        &self,
        module_ident: &ModuleIdent_,
        use_name: &Symbol,
        use_pos: &Loc,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        self.add_outer_use_def(
            module_ident,
            use_name,
            use_pos,
            |use_name, name_start, mod_defs| match mod_defs.structs.get(use_name) {
                Some(def) => {
                    use_defs.insert(
                        name_start.line,
                        UseDef::new(
                            references,
                            use_pos.file_hash(),
                            name_start,
                            self.mod_outer_defs.get(module_ident).unwrap().fhash,
                            def.name_start,
                            use_name,
                        ),
                    );
                }
                None => debug_assert!(false),
            },
        );
    }

    /// Add use of a struct field identifier
    fn add_field_use_def(
        &self,
        module_ident: &ModuleIdent_,
        struct_name: &Symbol,
        use_name: &Symbol,
        use_pos: &Loc,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        self.add_outer_use_def(
            module_ident,
            use_name,
            use_pos,
            |use_name, name_start, mod_defs| match mod_defs.structs.get(struct_name) {
                Some(def) => {
                    for fdef in &def.field_defs {
                        if fdef.name == *use_name {
                            use_defs.insert(
                                name_start.line,
                                UseDef::new(
                                    references,
                                    use_pos.file_hash(),
                                    name_start,
                                    self.mod_outer_defs.get(module_ident).unwrap().fhash,
                                    fdef.start,
                                    use_name,
                                ),
                            );
                        }
                    }
                }
                None => debug_assert!(false),
            },
        );
    }

    /// Add use of a type identifier
    fn add_type_id_use_def(
        &self,
        sp!(pos, typ): &Type,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        match typ {
            Type_::Ref(_, t) => self.add_type_id_use_def(t, references, use_defs),
            Type_::Param(tparam) => {
                let sp!(use_pos, use_name) = tparam.user_specified_name;
                match Self::get_start_loc(pos, &self.files, &self.file_id_mapping) {
                    Some(name_start) => match self.type_params.get(&use_name) {
                        Some(def_loc) => {
                            use_defs.insert(
                                name_start.line,
                                UseDef::new(
                                    references,
                                    use_pos.file_hash(),
                                    name_start,
                                    def_loc.fhash,
                                    def_loc.start,
                                    &use_name,
                                ),
                            );
                        }
                        None => debug_assert!(false),
                    },
                    None => debug_assert!(false), // a type param should not be missing
                }
            }
            Type_::Apply(_, sp!(_, type_name), tparams) => {
                if let TypeName_::ModuleType(sp!(_, mod_ident), struct_name) = type_name {
                    self.add_struct_use_def(
                        mod_ident,
                        &struct_name.value(),
                        &struct_name.loc(),
                        references,
                        use_defs,
                    );
                } // otherwise nothing to be done for other type names
                for t in tparams {
                    self.add_type_id_use_def(t, references, use_defs);
                }
            }
            _ => (), // nothing to be done for the other types
        }
    }

    /// Add a "generic" definition
    fn add_def(
        &self,
        pos: &Loc,
        name: &Symbol,
        scope: &mut OrdMap<Symbol, DefLoc>,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        use_defs: &mut UseDefMap,
    ) {
        match Self::get_start_loc(pos, &self.files, &self.file_id_mapping) {
            Some(name_start) => {
                let def_loc = DefLoc {
                    fhash: pos.file_hash(),
                    start: name_start,
                };
                scope.insert(*name, def_loc);
                // in other languages only one definition is allowed per scope but in move an (and
                // in rust) a variable can be re-defined in the same scope replacing the previous
                // definition

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
            }
            None => {
                debug_assert!(false);
            }
        }
    }

    /// Add a use for and identifier whose definition is expected to be local to a function, and
    /// pair it with an appropriate definition
    fn add_local_use_def(
        &self,
        use_name: &Symbol,
        use_pos: &Loc,
        references: &mut BTreeMap<DefLoc, BTreeSet<UseLoc>>,
        scope: &OrdMap<Symbol, DefLoc>,
        use_defs: &mut UseDefMap,
    ) {
        let name_start = match Self::get_start_loc(use_pos, &self.files, &self.file_id_mapping) {
            Some(v) => v,
            None => {
                debug_assert!(false);
                return;
            }
        };

        if let Some(def_loc) = scope.get(use_name) {
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
        } else {
            debug_assert!(false);
        }
    }
}

/// Handles go-to-def request of the language server
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
            // TODO: Do we need beginning and end of the definition? Does not seem to make a
            // difference from the IDE perspective as the cursor goes to the beginning anyway (at
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

/// Handles go-to-references request of the language server
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

/// Helper function to handle language server queries related to identifier uses
pub fn on_use_request(
    context: &Context,
    symbols: &Symbols,
    use_fpath: &str,
    use_line: u32,
    use_col: u32,
    id: RequestId,
    use_def_action: impl Fn(&UseDef) -> Option<serde_json::Value>,
) {
    let mut result = None;

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
    // unwrap will succeed based on the logic above which the compiler is unable to figure out
    // without using Option
    let response = lsp_server::Response::new_ok(id, result.unwrap());
    context
        .connection
        .sender
        .send(lsp_server::Message::Response(response))
        .expect("could not send use response");
}

#[cfg(test)]
fn assert_use_def(
    mod_symbols: &UseDefMap,
    file_name_mapping: &BTreeMap<FileHash, Symbol>,
    use_idx: usize,
    use_line: u32,
    use_col: u32,
    def_line: u32,
    def_col: u32,
    def_file: &str,
) {
    let uses = mod_symbols.get(use_line).unwrap();
    let use_def = uses.iter().nth(use_idx).unwrap();
    assert!(use_def.col_start == use_col);
    assert!(use_def.def_loc.start.line == def_line);
    assert!(use_def.def_loc.start.character == def_col);
    assert!(file_name_mapping
        .get(&use_def.def_loc.fhash)
        .unwrap()
        .as_str()
        .ends_with(def_file));
}

#[test]
/// Tests if symbolication information for specific Move constructs has been constructed correctly.
fn symbols_test() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    path.push("tests/symbols");

    let symbols = Symbolicator::get_symbols(path.as_path()).unwrap();

    let mut fpath = path.clone();
    fpath.push("sources/M1.move");
    let cpath = fs::canonicalize(&fpath).unwrap();

    let mod_ident = symbols.mod_ident_map.get(&cpath).unwrap();
    let mod_symbols = symbols.mod_use_defs.get(mod_ident).unwrap();

    // struct def name
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        2,
        11,
        2,
        11,
        "M1.move",
    );
    // const def name
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        6,
        10,
        6,
        10,
        "M1.move",
    );
    // function def name
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        9,
        8,
        9,
        8,
        "M1.move",
    );
    // param var (unpack function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        9,
        15,
        9,
        15,
        "M1.move",
    );
    // struct name in param type (unpack function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        2,
        9,
        18,
        2,
        11,
        "M1.move",
    );
    // struct name in unpack (unpack function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        10,
        12,
        2,
        11,
        "M1.move",
    );
    // field name in unpack (unpack function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        10,
        25,
        3,
        8,
        "M1.move",
    );
    // bound variable in unpack (unpack function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        2,
        10,
        37,
        10,
        37,
        "M1.move",
    );
    // moved var in unpack assignment (unpack function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        3,
        10,
        47,
        9,
        15,
        "M1.move",
    );
    // copied var in an assignment (cp function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        15,
        18,
        14,
        11,
        "M1.move",
    );
    // struct name return type (pack function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        20,
        18,
        2,
        11,
        "M1.move",
    );
    // struct name in pack (pack function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        20,
        18,
        2,
        11,
        "M1.move",
    );
    // field name in pack (pack function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        2,
        20,
        31,
        3,
        8,
        "M1.move",
    );
    // const in pack (pack function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        3,
        20,
        43,
        6,
        10,
        "M1.move",
    );
    // other module struct name (other_mod_struct function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        24,
        41,
        2,
        11,
        "M2.move",
    );
    // function name in a call (other_mod_struct function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        25,
        21,
        6,
        15,
        "M2.move",
    );
    // const in param (other_mod_struct function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        25,
        39,
        6,
        10,
        "M1.move",
    );
    // other module struct name imported (other_mod_struct_import function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        30,
        35,
        2,
        11,
        "M2.move",
    );
    // struct name in acquires (acq function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        2,
        34,
        41,
        2,
        11,
        "M1.move",
    );
    // struct name in builtin type param (acq function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        35,
        32,
        2,
        11,
        "M1.move",
    );
    // param name in builtin (acq function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        2,
        35,
        44,
        34,
        12,
        "M1.move",
    );
    // const in first param (multi_arg_call function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        40,
        22,
        6,
        10,
        "M1.move",
    );
    // const in second param (multi_arg_call function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        2,
        40,
        34,
        6,
        10,
        "M1.move",
    );
    // vector constructor type (vec function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        45,
        15,
        2,
        11,
        "M1.move",
    );
    // vector constructor first element struct type (vec function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        45,
        27,
        2,
        11,
        "M1.move",
    );
    // vector constructor first element struct field (vec function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        2,
        45,
        39,
        3,
        8,
        "M1.move",
    );
    // vector constructor second element var (vec function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        3,
        45,
        57,
        44,
        12,
        "M1.move",
    );
    // borrow local (mut function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        56,
        21,
        55,
        12,
        "M1.move",
    );
    // LHS in mutation statement (mut function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        57,
        9,
        56,
        12,
        "M1.move",
    );
    // RHS in mutation statement (mut function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        57,
        13,
        6,
        10,
        "M1.move",
    );
    // returned value (ret function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        63,
        19,
        6,
        10,
        "M1.move",
    );
    // abort value (abort_call function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        69,
        14,
        6,
        10,
        "M1.move",
    );
    // dereference (deref function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        75,
        9,
        74,
        12,
        "M1.move",
    );
    // unary operator (unary function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        79,
        9,
        78,
        14,
        "M1.move",
    );
    // temp borrow (temp_borrow function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        83,
        19,
        6,
        10,
        "M1.move",
    );
    // chain access first element (chain_access function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        94,
        8,
        93,
        12,
        "M1.move",
    );
    // chain second element (chain_access function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        94,
        14,
        88,
        8,
        "M1.move",
    );
    // chain access third element (chain_access function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        2,
        94,
        26,
        3,
        8,
        "M1.move",
    );
    // chain second element after the block (chain_access_block function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        102,
        10,
        88,
        8,
        "M1.move",
    );
    // chain access first element when borrowing (chain_access_borrow function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        108,
        17,
        107,
        12,
        "M1.move",
    );
    // chain second element when borrowing (chain_access_borrow function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        2,
        108,
        23,
        88,
        8,
        "M1.move",
    );
    // chain access third element when borrowing (chain_access_borrow function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        3,
        108,
        35,
        3,
        8,
        "M1.move",
    );
    // variable in cast (cast function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        114,
        9,
        113,
        12,
        "M1.move",
    );
    // constant in an annotation (annot function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        118,
        19,
        6,
        10,
        "M1.move",
    );

    let mut fpath = path.clone();
    fpath.push("sources/M3.move");
    let cpath = fs::canonicalize(&fpath).unwrap();

    let mod_ident = symbols.mod_ident_map.get(&cpath).unwrap();
    let mod_symbols = symbols.mod_use_defs.get(mod_ident).unwrap();

    // generic type in struct definition
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        2,
        23,
        2,
        23,
        "M3.move",
    );
    // generic type in struct field definition
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        3,
        20,
        2,
        23,
        "M3.move",
    );
    // generic type in generic type definition (type_param_arg function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        6,
        23,
        6,
        23,
        "M3.move",
    );

    // generic type in param type (type_param_arg function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        3,
        6,
        33,
        6,
        23,
        "M3.move",
    );
    // generic type in return type (type_param_arg function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        4,
        6,
        37,
        6,
        23,
        "M3.move",
    );
    // generic type in struct param type (struct_type_param_arg function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        4,
        10,
        52,
        10,
        30,
        "M3.move",
    );
    // generic type in struct return type (struct_type_param_arg function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        6,
        10,
        69,
        10,
        30,
        "M3.move",
    );
    // generic type in pack (pack_type_param function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        15,
        20,
        14,
        24,
        "M3.move",
    );
    // field type in struct field definition which itself is a struct
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        23,
        20,
        2,
        11,
        "M3.move",
    );
    // generic type in struct field definition which itself is a struct
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        2,
        23,
        32,
        22,
        30,
        "M3.move",
    );

    let mut fpath = path.clone();
    fpath.push("sources/M4.move");
    let cpath = fs::canonicalize(&fpath).unwrap();

    let mod_ident = symbols.mod_ident_map.get(&cpath).unwrap();
    let mod_symbols = symbols.mod_use_defs.get(mod_ident).unwrap();

    // param name in RHS (if_cond function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        4,
        18,
        2,
        16,
        "M4.move",
    );
    // param name in RHS (if_cond function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        6,
        22,
        4,
        12,
        "M4.move",
    );
    // var in if's true branch (if_cond function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        7,
        12,
        4,
        12,
        "M4.move",
    );
    // redefined var in if's false branch (if_cond function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        10,
        12,
        9,
        16,
        "M4.move",
    );
    // var name in while loop condition (while_loop function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        20,
        15,
        18,
        12,
        "M4.move",
    );
    // var name in while loop's inner block (while_loop function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        23,
        26,
        18,
        12,
        "M4.move",
    );
    // redefined var name in while loop's inner block (while_loop function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        24,
        23,
        23,
        20,
        "M4.move",
    );
    // var name in while loop's main block (while_loop function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        26,
        12,
        18,
        12,
        "M4.move",
    );
    // redefined var name in while loop's inner block (loop function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        1,
        40,
        23,
        39,
        20,
        "M4.move",
    );
    // var name in loop's main block (loop function)
    assert_use_def(
        mod_symbols,
        &symbols.file_name_mapping,
        0,
        43,
        16,
        34,
        12,
        "M4.move",
    );
}
