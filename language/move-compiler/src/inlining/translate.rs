// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    diag,
    expansion::ast::{AbilitySet, ModuleIdent, ModuleIdent_},
    inlining::visitor::{Dispatcher, Visitor, VisitorContinuation},
    naming::ast::{StructDefinition, StructTypeParameter, TParamID, Type, Type_},
    parser::ast::{StructName, Var},
    shared::{ast_debug, unique_map::UniqueMap, CompilationEnv, Identifier, Name},
    typing::{
        ast::{
            Exp, ExpListItem, Function, FunctionBody_, LValueList, LValue_, ModuleCall, Program,
            SequenceItem, SequenceItem_, UnannotatedExp, UnannotatedExp_,
        },
        core::{infer_abilities, InferAbilityContext, Subst},
    },
};
use move_ir_types::location::{sp, Loc};
use move_symbol_pool::Symbol;
use std::collections::{BTreeMap, BTreeSet, VecDeque};

/// A globally unique function name
type GlobalFunctionName = (ModuleIdent_, Symbol);
type GlobalStructName = (ModuleIdent_, Symbol);

#[derive(Debug)]
struct Inliner<'l> {
    env: &'l mut CompilationEnv,
    current_module: Option<ModuleIdent_>,
    current_function: Symbol,
    current_function_loc: Option<Loc>,
    struct_defs: BTreeMap<GlobalStructName, StructDefinition>,
    inline_defs: BTreeMap<GlobalFunctionName, Function>,
    inline_stack: VecDeque<GlobalFunctionName>,
    rename_counter: usize,
}

// ============================================================================================
// Entry point

pub fn run_inlining(env: &mut CompilationEnv, prog: &mut Program) {
    Inliner {
        env,
        current_module: None,
        current_function: Symbol::from(""),
        current_function_loc: None,
        struct_defs: BTreeMap::new(),
        inline_defs: BTreeMap::new(),
        inline_stack: Default::default(),
        rename_counter: 0,
    }
    .run(prog)
}

impl<'l> Inliner<'l> {
    fn run(&mut self, prog: &mut Program) {
        // First collect all definitions of inlined functions so we can expand them later in the AST.
        self.visit_functions(prog, VisitingMode::All, &mut |ctx, _, fdef| {
            if fdef.inline {
                match ctx.current_module {
                    Some(mid) => {
                        let global_name = (mid, ctx.current_function);
                        ctx.inline_defs.insert(global_name, fdef.clone());
                    }
                    _ => panic!("ICE unexpected inline fun declared in script"),
                }
            }
        });
        // Also collect all structs, we need them for ability computation
        for (_, mid, mdef) in prog.modules.iter() {
            for (_, name, sdef) in mdef.structs.iter() {
                let global_name = (*mid, *name);
                self.struct_defs.insert(global_name, sdef.clone());
            }
        }

        // Next expand all inline function calls
        self.visit_functions(
            prog,
            VisitingMode::SourceOnly,
            &mut |inliner, _name, fdef| {
                if !fdef.inline {
                    let mut visitor = OuterVisitor { inliner };
                    Dispatcher::new(&mut visitor).function(fdef);
                }
            },
        );

        // Finally remove all inline functions from the program.
        for (_, _, mut mdef) in prog.modules.iter_mut() {
            mdef.functions =
                std::mem::replace(&mut mdef.functions, UniqueMap::new()).filter_map(|_, fdef| {
                    if fdef.inline {
                        None
                    } else {
                        Some(fdef)
                    }
                });
        }
    }

    /// Helper to debug print function definition
    #[allow(unused)]
    fn eprint_fdef(header: &str, fdef: &Function) {
        match &fdef.body.value {
            FunctionBody_::Defined(s) => eprintln!("{} {}", header, ast_debug::display_verbose(s)),
            _ => eprintln!("{} native", header),
        }
    }

    /// A helper to visit all functions in the program.
    fn visit_functions<V>(&mut self, prog: &mut Program, mode: VisitingMode, visitor: &mut V)
    where
        V: FnMut(&mut Inliner<'_>, &str, &mut Function),
    {
        for (_, mid_, mdef) in prog.modules.iter_mut() {
            self.current_module = Some(*mid_);
            if mode == VisitingMode::All || mdef.is_source_module {
                for (loc, fname, fdef) in mdef.functions.iter_mut() {
                    self.current_function = *fname;
                    self.current_function_loc = Some(loc);
                    (*visitor)(self, fname.as_str(), fdef)
                }
            }
        }
        for (name, sdef) in prog.scripts.iter_mut() {
            self.current_module = None;
            self.current_function = *name;
            self.current_function_loc = Some(sdef.loc);
            (*visitor)(self, name.as_str(), &mut sdef.function)
        }
    }
}

#[derive(PartialEq)]
enum VisitingMode {
    All,
    SourceOnly,
}

// =============================================================================================
// Top-level Visitor

/// The outer visitor simply visits all expressions until it encounters an inlined function.
struct OuterVisitor<'l, 'r> {
    inliner: &'l mut Inliner<'r>,
}

impl<'l, 'r> Visitor for OuterVisitor<'l, 'r> {
    fn exp(&mut self, ex: &mut Exp) -> VisitorContinuation {
        match &mut ex.exp.value {
            UnannotatedExp_::ModuleCall(mcall) => {
                if let Some(repl) = self.inliner.module_call(ex.exp.loc, mcall.as_mut()) {
                    ex.exp.value = repl;
                    VisitorContinuation::Stop
                } else {
                    VisitorContinuation::Descend
                }
            }
            _ => VisitorContinuation::Descend,
        }
    }
}

// =============================================================================================
// Substitution Visitor

/// A substitution visitor inlines a function or lambda expressions, that is expands the body
/// with substituting the parameters. It recursively invokes itself if it encounters
/// further inlined functions or lambdas. Parameter expansion is done differently for
/// regular values and lambdas: regular values are eagerly bound via a generated `let`.
/// Function parameters are passed on to be expanded in the body.
struct SubstitutionVisitor<'l, 'r> {
    inliner: &'l mut Inliner<'r>,
    type_arguments: BTreeMap<TParamID, Type>,
    bindings: BTreeMap<Symbol, UnannotatedExp>,
    shadowed: VecDeque<BTreeSet<Symbol>>,
}

impl<'l, 'r> Visitor for SubstitutionVisitor<'l, 'r> {
    fn type_(&mut self, ty: &mut Type) -> VisitorContinuation {
        visit_type(&self.type_arguments, ty)
    }

    fn exp(&mut self, ex: &mut Exp) -> VisitorContinuation {
        self.type_(&mut ex.ty);
        match &mut ex.exp.value {
            UnannotatedExp_::VarCall(var, args) => {
                if let Some(repl) = self.var_call(var, args) {
                    ex.exp.value = repl;
                }
                VisitorContinuation::Descend
            }
            UnannotatedExp_::ModuleCall(mcall) => {
                if let Some(repl) = self.inliner.module_call(ex.exp.loc, mcall) {
                    ex.exp.value = repl;
                }
                VisitorContinuation::Descend
            }
            _ => VisitorContinuation::Descend,
        }
    }

    fn enter_scope(&mut self) {
        self.shadowed.push_front(BTreeSet::new())
    }

    fn exit_scope(&mut self) {
        self.shadowed.pop_front();
    }

    fn var_decl(&mut self, var: &mut Var) {
        self.shadowed
            .front_mut()
            .expect("scoped")
            .insert(var.0.value);
    }

    fn infer_abilities(&mut self, ty: &mut Type) {
        self.inliner.infer_abilities(ty)
    }
}

impl<'l, 'r> SubstitutionVisitor<'l, 'r> {
    /// Called when a variable is used in call position. In this case, this must be call to
    /// a function parameter of an inline function, and a frame parameter is expected to
    /// represent a lambda. Inlining the lambda leads to an anonymous frame on the inlining stack.
    fn var_call(&mut self, var: &Var, args: &mut Exp) -> Option<UnannotatedExp_> {
        match self.bindings.get(&var.0.value).cloned() {
            Some(mut repl) if !self.is_shadowed(var.0.value) => {
                // Rename the lambda so it does not clash with variables in the context. For
                // more details why this is needed, see discussion for `Inliner::module_call`.
                let mut rename_visitor = RenamingVisitor {
                    inliner: self.inliner,
                    renamings: VecDeque::new(),
                };
                Dispatcher::new(&mut rename_visitor).exp_unannotated(repl.loc, &mut repl.value);
                // Inline the lambda's body
                match repl.value {
                    UnannotatedExp_::Lambda(decls, mut body) => {
                        let loc = args.exp.loc;
                        let (decls_for_let, bindings) = self.inliner.process_parameters(
                            loc,
                            get_params_from_decls(&decls)
                                .into_iter()
                                .zip(get_args_from_exp(args).into_iter())
                                .map(|(s, e)| ((Var(Name::new(e.exp.loc, s)), e.ty.clone()), e)),
                        );
                        // Process body in sub-visitor
                        let mut sub_visitor = SubstitutionVisitor {
                            inliner: self.inliner,
                            type_arguments: Default::default(),
                            bindings,
                            shadowed: Default::default(),
                        };
                        Dispatcher::new(&mut sub_visitor).exp(body.as_mut());
                        // Build `let (params) = (args); body` as result.
                        let mut items = VecDeque::from(decls_for_let);
                        items.push_back(sp(loc, SequenceItem_::Seq(body)));
                        Some(UnannotatedExp_::Block(items))
                    }
                    _ => panic!("ICE expected function parameter to be a lambda"),
                }
            }
            _ => None,
        }
    }

    fn is_shadowed(&self, name: Symbol) -> bool {
        self.shadowed.iter().any(|s| s.contains(&name))
    }
}

// =============================================================================================
// Renaming Visitor

/// A visitor which renames every bound variables into some new, globally unique names.
/// This is used for lambdas to "rename away" those variables from any variables in the context.
struct RenamingVisitor<'l, 'r> {
    inliner: &'l mut Inliner<'r>,
    renamings: VecDeque<BTreeMap<Symbol, Symbol>>,
}

impl<'l, 'r> Visitor for RenamingVisitor<'l, 'r> {
    fn enter_scope(&mut self) {
        self.renamings.push_front(BTreeMap::new())
    }

    fn exit_scope(&mut self) {
        self.renamings.pop_front();
    }

    fn var_decl(&mut self, var: &mut Var) {
        let new_name = Symbol::from(format!("{}#{}", var.0.value, self.inliner.rename_counter));
        self.inliner.rename_counter += 1;
        self.renamings
            .front_mut()
            .unwrap()
            .insert(var.0.value, new_name);
        var.0.value = new_name;
    }

    fn var_use(&mut self, var: &mut Var) {
        for mapping in &self.renamings {
            if let Some(new_name) = mapping.get(&var.0.value) {
                var.0.value = *new_name
            }
        }
    }
}

// =============================================================================================
// Expansion of a call

impl<'l> Inliner<'l> {
    /// Process a call and initiate inlining. This checks for potential cycles and launches
    /// a `SubstitutionVisitor` for inlined functions.
    fn module_call(&mut self, call_loc: Loc, mcall: &mut ModuleCall) -> Option<UnannotatedExp_> {
        let global_name = (mcall.module.value, mcall.name.0.value);
        if let Some(mut fdef) = self.inline_defs.get(&global_name).cloned() {
            // Function to inline: check for cycles
            if let Some(pos) = self.inline_stack.iter().position(|f| f == &global_name) {
                let cycle = self
                    .inline_stack
                    .iter()
                    .take(pos + 1)
                    .map(|f| f.1.to_string())
                    .fold(String::new(), |a, b| a + " -> " + &b);
                self.env.add_diag(diag!(
                    Inlining::Recursion,
                    (mcall.name.loc(), &format!("cyclic inlining: {}", cycle))
                ));
                return Some(UnannotatedExp_::UnresolvedError);
            }

            // Before we expand the body we need to rename
            // any bound variables so there are no clashes with variables in the context.
            // Consider an expression `let y; apply(v, |x| x + y)`. Notice that `y` binds to
            // to the outer let. If we expand `apply` and this function would internally
            // declare any `y`, we would shadow the `y` from the context when the lambda is
            // inserted. Therefore we rename any and all declarations in `apply`.
            let mut rename_visitor = RenamingVisitor {
                inliner: self,
                renamings: VecDeque::new(),
            };
            Dispatcher::new(&mut rename_visitor).function(&mut fdef);

            // Now build `{ let (p1, p2, ..) = (arg1, arg2, ...); body }`
            let mut seq = match &fdef.body.value {
                FunctionBody_::Defined(seq) => seq.clone(),
                _ => panic!(
                    "ICE missing body of inline function `{}`",
                    mcall.name.0.value
                ),
            };
            let type_arguments = fdef
                .signature
                .type_parameters
                .iter()
                .zip(mcall.type_arguments.iter())
                .map(|(p, t)| (p.id, t.clone()))
                .collect();
            let (decls_for_let, bindings) = self.process_parameters(
                call_loc,
                fdef.signature
                    .parameters
                    .iter()
                    .cloned()
                    .zip(get_args_from_exp(&mcall.arguments)),
            );

            // Expand the body in its own independent visitor
            self.inline_stack.push_front(global_name); // for cycle detection
            let mut sub_visitor = SubstitutionVisitor {
                inliner: self,
                type_arguments,
                bindings,
                shadowed: Default::default(),
            };
            Dispatcher::new(&mut sub_visitor).sequence(&mut seq);
            self.inline_stack.pop_front();
            // Construct the let
            for decl in decls_for_let.into_iter().rev() {
                seq.push_front(decl)
            }
            Some(UnannotatedExp_::Block(seq))
        } else {
            None
        }
    }

    /// Process parameters, splitting them in those which are eagerly bound as regular
    /// values and those which are lambdas which are going to be transitively inlined.
    fn process_parameters(
        &mut self,
        loc: Loc,
        params: impl Iterator<Item = ((Var, Type), Exp)>,
    ) -> (Vec<SequenceItem>, BTreeMap<Symbol, UnannotatedExp>) {
        let mut bindings = BTreeMap::new();

        let mut lvalues = vec![];
        let mut tys = vec![];
        let mut exps = vec![];

        for ((var, _), e) in params {
            let ty = e.ty.clone();
            if ty.value.is_fun() {
                bindings.insert(var.0.value, e.exp);
            } else {
                lvalues.push(sp(loc, LValue_::Var(var, Box::new(ty.clone()))));
                tys.push(ty);
                exps.push(e);
            }
        }

        let opt_tys = tys.iter().map(|t| Some(t.clone())).collect();

        let exp = match exps.len() {
            0 => Exp {
                ty: sp(loc, Type_::Unit),
                exp: sp(loc, UnannotatedExp_::Unit { trailing: false }),
            },
            1 => exps.pop().unwrap(),
            _ => {
                let mut ty = Type_::multiple(loc, tys);
                self.infer_abilities(&mut ty);

                Exp {
                    ty,
                    exp: sp(
                        loc,
                        UnannotatedExp_::ExpList(
                            exps.into_iter()
                                .map(|e| {
                                    let ty = e.ty.clone();
                                    ExpListItem::Single(e, Box::new(ty))
                                })
                                .collect(),
                        ),
                    ),
                }
            }
        };

        let decl = sp(
            loc,
            SequenceItem_::Bind(sp(loc, lvalues), opt_tys, Box::new(exp)),
        );
        (vec![decl], bindings)
    }
}

// =============================================================================================
// Ability Inference

impl<'l> InferAbilityContext for Inliner<'l> {
    fn struct_declared_abilities(&self, m: &ModuleIdent, n: &StructName) -> AbilitySet {
        self.struct_defs
            .get(&(m.value, n.0.value))
            .map(|s| s.abilities.clone())
            .unwrap_or_else(|| AbilitySet::all(self.current_function_loc.expect("loc")))
    }

    fn struct_tparams(&self, m: &ModuleIdent, n: &StructName) -> Vec<StructTypeParameter> {
        self.struct_defs
            .get(&(m.value, n.0.value))
            .map(|s| s.type_parameters.clone())
            .unwrap_or_default()
    }
}

impl<'l> Inliner<'l> {
    #[allow(clippy::single_match)]
    fn infer_abilities(&mut self, ty: &mut Type) {
        match &mut ty.value {
            Type_::Apply(abls, ..) => {
                *abls = None; // reset abilities
            }
            _ => {}
        }
        let abilities = infer_abilities(self, &Subst::empty(), ty.clone());
        match &mut ty.value {
            Type_::Apply(abls, ..) => *abls = Some(abilities),
            _ => {}
        }
    }
}

// =============================================================================================
// AST Helpers

fn get_args_from_exp(args: &Exp) -> Vec<Exp> {
    match &args.exp.value {
        UnannotatedExp_::ExpList(items) => items
            .iter()
            .map(|item| match item {
                ExpListItem::Single(ex, _) => ex.clone(),
                ExpListItem::Splat(_, ex, _) => ex.clone(),
            })
            .collect::<Vec<_>>(),
        _ => vec![args.clone()],
    }
}

fn get_params_from_decls(decls: &LValueList) -> Vec<Symbol> {
    decls
        .value
        .iter()
        .flat_map(|lv| match &lv.value {
            LValue_::Var(v, _) => vec![v.0.value],
            LValue_::Ignore => vec![],
            LValue_::Unpack(_, _, _, fields) | LValue_::BorrowUnpack(_, _, _, _, fields) => {
                fields.iter().map(|(_, x, _)| *x).collect()
            }
        })
        .collect()
}

fn visit_type(subs: &BTreeMap<TParamID, Type>, ty: &mut Type) -> VisitorContinuation {
    if let Type_::Param(p) = &ty.value {
        if let Some(rty) = subs.get(&p.id) {
            *ty = rty.clone();
            return VisitorContinuation::Stop;
        }
    }
    VisitorContinuation::Descend
}
