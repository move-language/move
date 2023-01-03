// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    diag,
    expansion::ast::ModuleIdent_,
    naming::ast::{TParamID, Type, Type_},
    parser::ast::Var,
    shared::{unique_map::UniqueMap, CompilationEnv, Identifier},
    typing::ast::{
        Exp, ExpListItem, Function, FunctionBody_, LValueList, LValue_, ModuleCall, Program,
        Sequence, SequenceItem_, UnannotatedExp_,
    },
};
use move_ir_types::location::{sp, Spanned};
use move_symbol_pool::Symbol;
use std::collections::{BTreeMap, BTreeSet, VecDeque};

/// A globally unique function name
type GlobalFunctionName = (ModuleIdent_, Symbol);

#[derive(Debug)]
struct Inliner<'l> {
    env: &'l mut CompilationEnv,
    current_module: Option<ModuleIdent_>,
    current_function: Symbol,
    inline_defs: BTreeMap<GlobalFunctionName, Function>,
    inline_stack: VecDeque<InlineFrame>,
}

#[derive(Debug)]
enum InlineFrame {
    FunctionExpansion {
        function: GlobalFunctionName,
        type_parameters: BTreeMap<TParamID, Type>,
        parameters: BTreeMap<Symbol, UnannotatedExp_>,
    },
    LambdaExpansion {
        parameters: BTreeMap<Symbol, UnannotatedExp_>,
    },
    Shadowing {
        symbols: BTreeSet<Symbol>,
    },
}

// ============================================================================================
// Entry point

pub fn run_inlining(env: &mut CompilationEnv, prog: &mut Program) {
    Inliner {
        env,
        current_module: None,
        current_function: Symbol::from(""),
        inline_defs: BTreeMap::new(),
        inline_stack: Default::default(),
    }
    .run(prog)
}

impl<'l> Inliner<'l> {
    fn run(&mut self, prog: &mut Program) {
        // First collect all definitions of inlined functions so we can expand them later in the AST.
        self.visit_functions(prog, VisitingMode::All, &mut |ctx, fdef| {
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

        // Next expand all inline function calls
        self.visit_functions(prog, VisitingMode::SourceOnly, &mut |ctx, fdef| {
            if !fdef.inline {
                ctx.function(fdef);
            }
        });

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

    /// A helper to visit all functions in the program.
    fn visit_functions<V>(&mut self, prog: &mut Program, mode: VisitingMode, visitor: &mut V)
    where
        V: FnMut(&mut Inliner<'_>, &mut Function),
    {
        for (_, mid_, mdef) in prog.modules.iter_mut() {
            self.current_module = Some(*mid_);
            if mode == VisitingMode::All || mdef.is_source_module {
                for (_, fname, fdef) in mdef.functions.iter_mut() {
                    self.current_function = *fname;
                    (*visitor)(self, fdef)
                }
            }
        }
        for (name, sdef) in prog.scripts.iter_mut() {
            self.current_module = None;
            self.current_function = *name;
            (*visitor)(self, &mut sdef.function)
        }
    }
}

#[derive(PartialEq)]
enum VisitingMode {
    All,
    SourceOnly,
}

// =============================================================================================
// Core Logic

impl<'l> Inliner<'l> {
    fn get_substitution(&self, name: Symbol) -> Option<UnannotatedExp_> {
        for frame in self.inline_stack.iter() {
            match frame {
                InlineFrame::Shadowing { symbols } => {
                    if symbols.contains(&name) {
                        return None;
                    }
                }
                InlineFrame::FunctionExpansion { parameters, .. }
                | InlineFrame::LambdaExpansion { parameters, .. } => {
                    // Notice we do not look into some outer frames since
                    // (a) all our declarations are lexically bound
                    // (b) if we substitute a lambda we do that _after_ walking its body and
                    //     substituting all references there, those the lambda is already
                    //     a closure
                    return parameters.get(&name).cloned();
                }
            }
        }
        None
    }

    fn get_type_substitution(&self, param: TParamID) -> Option<Type> {
        for frame in self.inline_stack.iter() {
            if let InlineFrame::FunctionExpansion {
                type_parameters, ..
            } = frame
            {
                // Notice we do not look into some outer frames since there are no nested
                // type quantifiers
                return type_parameters.get(&param).cloned();
            }
        }
        None
    }

    /// Called when a variable is used. If the variable is not shadowed and an argument of
    /// the current inline expansion, substitute it.
    fn var_use(&mut self, var: &Var) -> Option<UnannotatedExp_> {
        self.get_substitution(var.0.value)
    }

    /// Called when a variable is used in call position. In this case, this must be call to
    /// a function parameter of an inline function, and a frame parameter is expected to
    /// represent a lambda. Inlining the lambda leads to an anonymous frame on the inlining stack.
    fn var_call(&mut self, var: &Var, args: &mut Exp) -> Option<UnannotatedExp_> {
        let result;

        // First process the argument. (We are doing applicative order reduction.)
        self.exp(args);

        if let Some(repl) = self.get_substitution(var.0.value) {
            // Inline the lambda's body
            match repl {
                UnannotatedExp_::Lambda(decls, body) => {
                    let parameters = get_params_from_decls(&decls)
                        .into_iter()
                        .zip(get_args_from_exp(&args.exp.value).into_iter())
                        .collect();
                    self.inline_stack
                        .push_front(InlineFrame::LambdaExpansion { parameters });
                    let mut body = body.exp.value;
                    self.exp_unannotated(&mut body); // substitute lambda parameters
                    self.inline_stack.pop_front();
                    result = Some(body);
                }
                _ => panic!("ICE expected function parameter to be a lambda"),
            }
        } else {
            panic!("ICE unexpected non-bound variable call")
        }
        result
    }

    /// Process a call and initiate inlining. This checks for potential cycles in inlining and
    /// pushes a new inlining frame for an inlined function.
    fn module_call(&mut self, mcall: &mut ModuleCall) -> Option<UnannotatedExp_> {
        // First process arguments and types.
        self.exp(mcall.arguments.as_mut());
        mcall.type_arguments = std::mem::take(&mut mcall.type_arguments)
            .into_iter()
            .map(|ty| self.instantiate(ty))
            .collect();
        mcall.parameter_types = std::mem::take(&mut mcall.parameter_types)
            .into_iter()
            .map(|ty| self.instantiate(ty))
            .collect();

        let global_name = (mcall.module.value, mcall.name.0.value);
        let mut result = None;
        if let Some(fdef) = self.inline_defs.get(&global_name) {
            if let Some(pos) = self
                .inline_stack
                .iter()
                .position(|f| matches!(f, InlineFrame::FunctionExpansion { function, .. } if function == &global_name))
            {
                let cycle = self
                    .inline_stack
                    .iter()
                    .take(pos + 1)
                    .filter_map(|f| match f {
                        InlineFrame::FunctionExpansion {
                            function: (_, name),
                            ..
                        } => Some(name.to_string()),
                        _ => None,
                    })
                    .fold(String::new(), |a, b| a + " -> " + &b);
                self.env.add_diag(diag!(
                    Inlining::Recursion,
                    (mcall.name.loc(), &format!("cyclic inlining: {}", cycle))
                ));
                return None;
            }
            let parameters = fdef
                .signature
                .parameters
                .iter()
                .map(|(v, _)| v.0.value)
                .zip(get_args_from_exp(&mcall.arguments.exp.value))
                .collect::<BTreeMap<_, _>>();
            let type_parameters = fdef
                .signature
                .type_parameters
                .iter()
                .zip(mcall.type_arguments.iter())
                .map(|(p, t)| (p.id, t.clone()))
                .collect();
            self.inline_stack
                .push_front(InlineFrame::FunctionExpansion {
                    function: global_name,
                    type_parameters,
                    parameters,
                });
            match &fdef.body.value {
                FunctionBody_::Defined(seq) => {
                    let mut body = UnannotatedExp_::Block(seq.clone());
                    self.exp_unannotated(&mut body); // inline the body
                    result = Some(body);
                }
                _ => panic!("ICE unexpected body of inline function"),
            }
            self.inline_stack.pop_front();
        }
        result
    }

    /// Evaluate a lambda. The body of the lambda is processed for expansion of inline
    /// parameters in the current context, which is the one the lambda is written down. This is
    /// also referred to lexical scoping, and avoids any name clash problems. However, any names
    /// introduced by the lambda itself shadow names from the context, so we add those to the
    /// shadow list.
    fn lambda(&mut self, decls: &mut LValueList, body: &mut Exp) {
        self.inline_stack.push_front(InlineFrame::Shadowing {
            symbols: get_params_from_decls(decls).into_iter().collect(),
        });
        self.exp(body);
        self.inline_stack.pop_front();
    }

    /// Evaluate a sequence. Similar as with lambda, we need to add declarations in the sequence
    /// to the shadow list.
    fn sequence(&mut self, seq: &mut Sequence) {
        let mut shadow_cnt = 0;
        for item in seq.iter_mut() {
            match &mut item.value {
                SequenceItem_::Bind(decls, _, e) => {
                    self.exp(e.as_mut());
                    self.inline_stack.push_front(InlineFrame::Shadowing {
                        symbols: get_params_from_decls(decls).into_iter().collect(),
                    });
                    shadow_cnt += 1;
                }
                SequenceItem_::Declare(decls) => {
                    self.inline_stack.push_front(InlineFrame::Shadowing {
                        symbols: get_params_from_decls(decls).into_iter().collect(),
                    });
                    shadow_cnt += 1;
                }
                SequenceItem_::Seq(e) => self.exp(e.as_mut()),
            }
        }
        while shadow_cnt > 0 {
            self.inline_stack.pop_front();
            shadow_cnt -= 1
        }
    }

    fn instantiate(&self, ty: Type) -> Type {
        if self.inline_stack.is_empty() {
            return ty;
        }
        let Spanned { loc, value } = ty;
        sp(loc, self.instantiate_(value))
    }

    fn instantiate_(&self, ty: Type_) -> Type_ {
        match ty {
            Type_::Ref(m, t) => Type_::Ref(m, Box::new(self.instantiate(*t))),
            Type_::Param(p) => {
                if let Some(rty) = self.get_type_substitution(p.id) {
                    rty.value
                } else {
                    Type_::Param(p)
                }
            }
            Type_::Apply(abilities, name, args) => Type_::Apply(
                abilities,
                name,
                args.into_iter().map(|ty| self.instantiate(ty)).collect(),
            ),
            ty @ Type_::Unit
            | ty @ Type_::Var(_)
            | ty @ Type_::Anything
            | ty @ Type_::UnresolvedError => ty,
        }
    }
}

// =============================================================================================
// Visitor Boilerplate

impl<'l> Inliner<'l> {
    fn function(&mut self, fdef: &mut Function) {
        match &mut fdef.body.value {
            FunctionBody_::Native => {}
            FunctionBody_::Defined(seq) => self.sequence(seq),
        }
    }

    fn exp(&mut self, ex: &mut Exp) {
        ex.ty = self.instantiate(ex.ty.clone());
        self.exp_unannotated(&mut ex.exp.value)
    }

    fn exp_unannotated(&mut self, ex: &mut UnannotatedExp_) {
        match ex {
            UnannotatedExp_::ModuleCall(mcall) => {
                if let Some(new_ex) = self.module_call(mcall.as_mut()) {
                    *ex = new_ex
                }
            }
            UnannotatedExp_::Use(var) => {
                if let Some(new_ex) = self.var_use(var) {
                    *ex = new_ex
                }
            }
            UnannotatedExp_::Copy { from_user: _, var } => {
                if let Some(new_ex) = self.var_use(var) {
                    *ex = new_ex
                }
            }
            UnannotatedExp_::Move { from_user: _, var } => {
                if let Some(new_ex) = self.var_use(var) {
                    *ex = new_ex
                }
            }
            UnannotatedExp_::VarCall(var, exp) => {
                if let Some(new_ex) = self.var_call(var, exp.as_mut()) {
                    *ex = new_ex
                }
            }
            UnannotatedExp_::Lambda(decls, body) => self.lambda(decls, body.as_mut()),

            UnannotatedExp_::IfElse(cex, iex, eex) => {
                self.exp(cex.as_mut());
                self.exp(iex.as_mut());
                self.exp(eex.as_mut());
            }
            UnannotatedExp_::While(cex, bex) => {
                self.exp(cex.as_mut());
                self.exp(bex.as_mut());
            }
            UnannotatedExp_::Block(seq) => self.sequence(seq),
            UnannotatedExp_::Mutate(dex, sex) => {
                self.exp(dex.as_mut());
                self.exp(sex.as_mut());
            }
            UnannotatedExp_::BinopExp(lex, _, _, rex) => {
                self.exp(lex.as_mut());
                self.exp(rex.as_mut());
            }
            UnannotatedExp_::Pack(_, _, _, fields) => {
                for (_, _, (_, (_, ex))) in fields.iter_mut() {
                    self.exp(ex);
                }
            }
            UnannotatedExp_::ExpList(items) => {
                for item in items.iter_mut() {
                    match item {
                        ExpListItem::Single(ex, _) | ExpListItem::Splat(_, ex, _) => self.exp(ex),
                    }
                }
            }

            UnannotatedExp_::Loop { body: ex, .. }
            | UnannotatedExp_::Assign(_, _, ex)
            | UnannotatedExp_::Builtin(_, ex)
            | UnannotatedExp_::Vector(_, _, _, ex)
            | UnannotatedExp_::Return(ex)
            | UnannotatedExp_::Abort(ex)
            | UnannotatedExp_::Dereference(ex)
            | UnannotatedExp_::UnaryExp(_, ex)
            | UnannotatedExp_::Borrow(_, ex, _)
            | UnannotatedExp_::TempBorrow(_, ex)
            | UnannotatedExp_::Cast(ex, _)
            | UnannotatedExp_::Annotate(ex, _) => self.exp(ex.as_mut()),

            UnannotatedExp_::Unit { .. }
            | UnannotatedExp_::Value(_)
            | UnannotatedExp_::Constant(_, _)
            | UnannotatedExp_::Break
            | UnannotatedExp_::BorrowLocal(_, _)
            | UnannotatedExp_::Continue
            | UnannotatedExp_::Spec(_, _)
            | UnannotatedExp_::UnresolvedError => {}
        }
    }
}

// =============================================================================================
// AST Helpers

fn get_args_from_exp(args: &UnannotatedExp_) -> Vec<UnannotatedExp_> {
    match args {
        UnannotatedExp_::ExpList(items) => items
            .iter()
            .map(|item| match item {
                ExpListItem::Single(ex, _) => ex.exp.value.clone(),
                ExpListItem::Splat(_, ex, _) => ex.exp.value.clone(),
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
            //_ => panic!("ICE unexpected parameter {:?}", lv.value),
            LValue_::Ignore => vec![],
            LValue_::Unpack(_, _, _, fields) | LValue_::BorrowUnpack(_, _, _, _, fields) => {
                fields.iter().map(|(_, x, _)| *x).collect()
            }
        })
        .collect()
}
