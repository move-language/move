// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    ast::{Exp, ExpData, MemoryLabel, Operation, Pattern, TempIndex, Value},
    model::{GlobalEnv, ModuleId, NodeId, SpecVarId},
    symbol::Symbol,
    ty::Type,
};
use itertools::Itertools;
use std::collections::{BTreeSet, VecDeque};

/// Rewriter for expressions, allowing to substitute locals by expressions as well as instantiate
/// types.
pub struct ExpRewriter<'env, 'rewriter> {
    env: &'env GlobalEnv,
    replacer: &'rewriter mut dyn FnMut(NodeId, RewriteTarget) -> Option<Exp>,
    type_args: &'rewriter [Type],
    shadowed: VecDeque<BTreeSet<Symbol>>,
}

/// A target for expression rewrites of either an `Exp::LocalVar` or an `Exp::Temporary`.
/// This is used as a parameter to the `replacer` function which defines the behavior of
/// the rewriter. Notice we use a single function entry point for `replacer` to allow it
/// to be a function which mutates it's context.
pub enum RewriteTarget {
    LocalVar(Symbol),
    Temporary(TempIndex),
}

impl<'env, 'rewriter> ExpRewriter<'env, 'rewriter> {
    /// Creates a new rewriter with the given replacer map.
    pub fn new<F>(env: &'env GlobalEnv, replacer: &'rewriter mut F) -> Self
    where
        F: FnMut(NodeId, RewriteTarget) -> Option<Exp>,
    {
        ExpRewriter {
            env,
            replacer,
            type_args: &[],
            shadowed: VecDeque::new(),
        }
    }

    /// Adds a type argument list to this rewriter. Generic type parameters are replaced by
    /// the given types.
    pub fn set_type_args(mut self, type_args: &'rewriter [Type]) -> Self {
        self.type_args = type_args;
        self
    }
}

impl<'env, 'rewriter> ExpRewriterFunctions for ExpRewriter<'env, 'rewriter> {
    fn rewrite_local_var(&mut self, id: NodeId, sym: Symbol) -> Option<Exp> {
        for vars in &self.shadowed {
            if vars.contains(&sym) {
                return None;
            }
        }
        (*self.replacer)(id, RewriteTarget::LocalVar(sym))
    }

    fn rewrite_temporary(&mut self, id: NodeId, idx: TempIndex) -> Option<Exp> {
        (*self.replacer)(id, RewriteTarget::Temporary(idx))
    }

    fn rewrite_node_id(&mut self, id: NodeId) -> Option<NodeId> {
        ExpData::instantiate_node(self.env, id, self.type_args)
    }
}

// ======================================================================================
// Expression rewriting trait

/// A general trait for expression rewriting.
///
/// This allows customization by re-implementing any of the `rewrite_local_var`,
/// `rewrite_temporary`, etc. functions. Each expression node has an equivalent of such
/// a function.
///
/// This rewriter takes care of preserving sharing between expressions: only expression trees
/// which are actually modified are reconstructed.
///
/// For most rewriting problems, there are already specializations of this trait, like `ExpRewriter`
/// in this module, and `Exp::rewrite` in the AST module.
///
/// When custom implementing this trait, consider the semantics of the generic logic used.
/// When any of the `rewrite_<exp-variant>` functions is called, any arguments have been already
/// recursively rewritten, inclusive of the passed node id. To implement a pre-descent
/// transformation, you need to implement the `rewrite_exp` function and after pre-processing,
/// continue (or not) descent with `rewrite_exp_descent` for sub-expressions.
#[allow(unused)] // for trait default parameters
pub trait ExpRewriterFunctions {
    /// Top-level entry for rewriting an expression. Can be re-implemented to do some
    /// pre/post processing embedding a call to `do_rewrite`.
    fn rewrite_exp(&mut self, exp: Exp) -> Exp {
        self.rewrite_exp_descent(exp)
    }

    fn rewrite_vec(&mut self, exps: &[Exp]) -> Vec<Exp> {
        exps.iter().map(|e| self.rewrite_exp(e.clone())).collect()
    }

    // Functions to specialize for the rewriting problem
    // --------------------------------------------------

    fn rewrite_enter_scope<'a>(&mut self, vars: impl Iterator<Item = &'a (NodeId, Symbol)>) {}
    fn rewrite_exit_scope(&mut self) {}
    fn rewrite_node_id(&mut self, id: NodeId) -> Option<NodeId> {
        None
    }
    fn rewrite_local_var(&mut self, id: NodeId, sym: Symbol) -> Option<Exp> {
        None
    }
    fn rewrite_temporary(&mut self, id: NodeId, idx: TempIndex) -> Option<Exp> {
        None
    }
    fn rewrite_value(&mut self, id: NodeId, value: &Value) -> Option<Exp> {
        None
    }
    fn rewrite_spec_var(
        &mut self,
        id: NodeId,
        mid: ModuleId,
        vid: SpecVarId,
        label: &Option<MemoryLabel>,
    ) -> Option<Exp> {
        None
    }
    fn rewrite_call(&mut self, id: NodeId, oper: &Operation, args: &[Exp]) -> Option<Exp> {
        None
    }
    fn rewrite_invoke(&mut self, id: NodeId, target: &Exp, args: &[Exp]) -> Option<Exp> {
        None
    }
    fn rewrite_lambda(&mut self, id: NodeId, pat: &Pattern, body: &Exp) -> Option<Exp> {
        None
    }
    fn rewrite_block(
        &mut self,
        id: NodeId,
        pat: &Pattern,
        binding: &Option<Exp>,
        body: &Exp,
    ) -> Option<Exp> {
        None
    }
    fn rewrite_pattern(&mut self, pat: &Pattern) -> Option<Pattern> {
        None
    }
    fn rewrite_quant(
        &mut self,
        id: NodeId,
        ranges: &[(Pattern, Exp)],
        triggers: &[Vec<Exp>],
        cond: &Option<Exp>,
        body: &Exp,
    ) -> Option<Exp> {
        None
    }
    fn rewrite_if_else(&mut self, id: NodeId, cond: &Exp, then: &Exp, else_: &Exp) -> Option<Exp> {
        None
    }

    // Core traversal functions, not intended to be re-implemented
    // -----------------------------------------------------------

    fn rewrite_exp_descent(&mut self, exp: Exp) -> Exp {
        use ExpData::*;
        match exp.as_ref() {
            Value(id, value) => {
                let (id_changed, new_id) = self.internal_rewrite_id(id);
                if let Some(new_exp) = self.rewrite_value(new_id, value) {
                    new_exp
                } else if id_changed {
                    Value(new_id, value.clone()).into_exp()
                } else {
                    exp
                }
            },
            LocalVar(id, sym) => {
                let (id_changed, new_id) = self.internal_rewrite_id(id);
                if let Some(new_exp) = self.rewrite_local_var(new_id, *sym) {
                    new_exp
                } else if id_changed {
                    LocalVar(new_id, *sym).into_exp()
                } else {
                    exp
                }
            },
            Temporary(id, idx) => {
                let (id_changed, new_id) = self.internal_rewrite_id(id);
                if let Some(new_exp) = self.rewrite_temporary(new_id, *idx) {
                    new_exp
                } else if id_changed {
                    Temporary(new_id, *idx).into_exp()
                } else {
                    exp
                }
            },
            Call(id, oper, args) => {
                let (id_changed, new_id) = self.internal_rewrite_id(id);
                let new_args_opt = self.internal_rewrite_vec(args);
                let args_ref = if let Some(new_args) = &new_args_opt {
                    new_args.as_slice()
                } else {
                    args.as_slice()
                };
                if let Some(new_exp) = self.rewrite_call(new_id, oper, args_ref) {
                    new_exp
                } else if new_args_opt.is_some() || id_changed {
                    let args_owned = if let Some(new_args) = new_args_opt {
                        new_args
                    } else {
                        args.to_owned()
                    };
                    Call(new_id, oper.clone(), args_owned).into_exp()
                } else {
                    exp
                }
            },
            Invoke(id, target, args) => {
                let (id_changed, new_id) = self.internal_rewrite_id(id);
                let (target_changed, new_target) = self.internal_rewrite_exp(target);
                let new_args_opt = self.internal_rewrite_vec(args);
                let args_ref = if let Some(new_args) = &new_args_opt {
                    new_args.as_slice()
                } else {
                    args.as_slice()
                };
                if let Some(new_exp) = self.rewrite_invoke(new_id, &new_target, args_ref) {
                    new_exp
                } else if id_changed || target_changed || new_args_opt.is_some() {
                    let args_owned = if let Some(new_args) = new_args_opt {
                        new_args
                    } else {
                        args.to_owned()
                    };
                    Invoke(new_id, new_target, args_owned).into_exp()
                } else {
                    exp
                }
            },
            Lambda(id, pat, body) => {
                let (id_changed, new_id) = self.internal_rewrite_id(id);
                let (pat_changed, new_pat) = self.internal_rewrite_pattern(pat);
                self.rewrite_enter_scope(new_pat.vars().iter());
                let (body_changed, new_body) = self.internal_rewrite_exp(body);
                self.rewrite_exit_scope();
                if let Some(new_exp) = self.rewrite_lambda(new_id, &new_pat, &new_body) {
                    new_exp
                } else if id_changed || pat_changed || body_changed {
                    Lambda(new_id, new_pat, new_body).into_exp()
                } else {
                    exp
                }
            },
            Block(id, pat, binding, body) => {
                let (id_changed, new_id) = self.internal_rewrite_id(id);
                let (pat_changed, new_pat) = self.internal_rewrite_pattern(pat);
                let (binding_changed, new_binding) = if let Some(b) = binding {
                    let (changed, b) = self.internal_rewrite_exp(b);
                    (changed, Some(b))
                } else {
                    (false, None)
                };
                self.rewrite_enter_scope(new_pat.vars().iter());
                let (body_changed, new_body) = self.internal_rewrite_exp(body);
                self.rewrite_exit_scope();
                if let Some(new_exp) = self.rewrite_block(new_id, &new_pat, &new_binding, &new_body)
                {
                    new_exp
                } else if id_changed || pat_changed || binding_changed || body_changed {
                    Block(new_id, new_pat, new_binding, new_body).into_exp()
                } else {
                    exp
                }
            },
            Quant(id, kind, ranges, triggers, cond, body) => {
                let (id_changed, new_id) = self.internal_rewrite_id(id);
                let (ranges_changed, new_ranges) = self.internal_rewrite_quant_ranges(ranges);
                self.rewrite_enter_scope(
                    ranges
                        .iter()
                        .flat_map(|(pat, _)| pat.vars())
                        .collect::<Vec<_>>()
                        .iter(),
                );
                let mut triggers_changed = false;
                let new_triggers = triggers
                    .iter()
                    .map(|p| {
                        let (c, new_p) = self
                            .internal_rewrite_vec(p)
                            .map(|pr| (true, pr))
                            .unwrap_or_else(|| (false, p.clone()));
                        triggers_changed = triggers_changed || c;
                        new_p
                    })
                    .collect_vec();
                let mut cond_changed = false;
                let new_cond = cond.as_ref().map(|c| {
                    let (c, new_c) = self.internal_rewrite_exp(c);
                    cond_changed = c;
                    new_c
                });
                let (body_changed, new_body) = self.internal_rewrite_exp(body);
                self.rewrite_exit_scope();
                if let Some(new_exp) =
                    self.rewrite_quant(new_id, &new_ranges, &new_triggers, &new_cond, &new_body)
                {
                    new_exp
                } else if id_changed
                    || ranges_changed
                    || triggers_changed
                    || cond_changed
                    || body_changed
                {
                    Quant(new_id, *kind, new_ranges, new_triggers, new_cond, new_body).into_exp()
                } else {
                    exp
                }
            },
            IfElse(id, cond, then, else_) => {
                let (id_changed, new_id) = self.internal_rewrite_id(id);
                let (cond_changed, new_cond) = self.internal_rewrite_exp(cond);
                let (then_changed, new_then) = self.internal_rewrite_exp(then);
                let (else_changed, new_else) = self.internal_rewrite_exp(else_);
                if let Some(new_exp) = self.rewrite_if_else(new_id, &new_cond, &new_then, &new_else)
                {
                    new_exp
                } else if id_changed || cond_changed || then_changed || else_changed {
                    IfElse(new_id, new_cond, new_then, new_else).into_exp()
                } else {
                    exp
                }
            },
            Sequence(id, es) => {
                let (id_changed, new_id) = self.internal_rewrite_id(id);
                let changed_vec = self.internal_rewrite_vec(es);
                if id_changed || changed_vec.is_some() {
                    Sequence(new_id, changed_vec.unwrap_or_else(|| es.clone())).into_exp()
                } else {
                    exp
                }
            },
            Loop(id, body) => {
                let (id_changed, new_id) = self.internal_rewrite_id(id);
                let (body_changed, new_body) = self.internal_rewrite_exp(body);
                if id_changed || body_changed {
                    Loop(new_id, new_body).into_exp()
                } else {
                    exp
                }
            },
            LoopCont(id, do_cont) => {
                let (id_changed, new_id) = self.internal_rewrite_id(id);
                if id_changed {
                    LoopCont(new_id, *do_cont).into_exp()
                } else {
                    exp
                }
            },
            Return(id, val) => {
                let (id_changed, new_id) = self.internal_rewrite_id(id);
                let (val_changed, new_val) = self.internal_rewrite_exp(val);
                if id_changed || val_changed {
                    Return(new_id, new_val).into_exp()
                } else {
                    exp
                }
            },
            Assign(id, lhs, rhs) => {
                let (id_changed, new_id) = self.internal_rewrite_id(id);
                let (lhs_changed, new_lhs) = self.internal_rewrite_pattern(lhs);
                let (rhs_changed, new_rhs) = self.internal_rewrite_exp(rhs);
                if id_changed || lhs_changed || rhs_changed {
                    Assign(new_id, new_lhs, new_rhs).into_exp()
                } else {
                    exp
                }
            },
            // This can happen since we are calling the rewriter during type checking, and
            // we may have encountered an error which is represented as an Invalid expression.
            Invalid(id) => Invalid(*id).into_exp(),
        }
    }

    fn internal_rewrite_pattern(&mut self, pat: &Pattern) -> (bool, Pattern) {
        if let Some(new_pat) = self.rewrite_pattern(pat) {
            (true, new_pat)
        } else {
            (false, pat.clone())
        }
    }

    fn internal_rewrite_id(&mut self, id: &NodeId) -> (bool, NodeId) {
        if let Some(new_id) = self.rewrite_node_id(*id) {
            (true, new_id)
        } else {
            (false, *id)
        }
    }

    fn internal_rewrite_exp(&mut self, exp: &Exp) -> (bool, Exp) {
        let new_exp = self.rewrite_exp(exp.clone());
        (!ExpData::ptr_eq(exp, &new_exp), new_exp)
    }

    fn internal_rewrite_vec(&mut self, exps: &[Exp]) -> Option<Vec<Exp>> {
        // The vector rewrite works a bit different as we try to avoid constructing
        // new vectors if nothing changed, and optimize common cases of 0-3 arguments.
        match exps.len() {
            0 => None,
            1 => {
                let (c, e) = self.internal_rewrite_exp(&exps[0]);
                if c {
                    Some(vec![e])
                } else {
                    None
                }
            },
            2 => {
                let (c1, e1) = self.internal_rewrite_exp(&exps[0]);
                let (c2, e2) = self.internal_rewrite_exp(&exps[1]);
                if c1 || c2 {
                    Some(vec![e1, e2])
                } else {
                    None
                }
            },
            3 => {
                let (c1, e1) = self.internal_rewrite_exp(&exps[0]);
                let (c2, e2) = self.internal_rewrite_exp(&exps[1]);
                let (c3, e3) = self.internal_rewrite_exp(&exps[2]);
                if c1 || c2 || c3 {
                    Some(vec![e1, e2, e3])
                } else {
                    None
                }
            },
            _ => {
                // generic treatment
                let mut change = false;
                let mut res = vec![];
                for exp in exps {
                    let (c, new_exp) = self.internal_rewrite_exp(exp);
                    change = change || c;
                    res.push(new_exp)
                }
                if change {
                    Some(res)
                } else {
                    None
                }
            },
        }
    }

    fn internal_rewrite_quant_ranges(
        &mut self,
        ranges: &[(Pattern, Exp)],
    ) -> (bool, Vec<(Pattern, Exp)>) {
        let mut change = false;
        let new_ranges = ranges
            .iter()
            .map(|(pat, exp)| {
                let (pat_changed, new_pat) = self.internal_rewrite_pattern(pat);
                change = change || pat_changed;
                let (exp_changed, new_exp) = self.internal_rewrite_exp(exp);
                change = change || exp_changed;
                (new_pat, new_exp)
            })
            .collect();
        (change, new_ranges)
    }
}
