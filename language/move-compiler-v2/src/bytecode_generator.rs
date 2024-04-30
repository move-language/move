// Copyright © Aptos Foundation
// Parts of the project are originally copyright © Meta Platforms, Inc.
// SPDX-License-Identifier: Apache-2.0

use codespan_reporting::diagnostic::Severity;
use ethnum::U256;
use move_model::{
    ast::{Exp, ExpData, Operation, Pattern, TempIndex, Value},
    model::{
        FieldId, FunId, FunctionEnv, GlobalEnv, NodeId, Parameter, QualifiedId, QualifiedInstId,
        StructId,
    },
    symbol::Symbol,
    ty::{PrimitiveType, ReferenceKind, Type},
};
use move_stackless_bytecode::{
    function_target::FunctionData,
    stackless_bytecode::{
        AssignKind, AttrId, Bytecode, Constant, Label, Operation as BytecodeOperation,
    },
    stackless_bytecode_generator::BytecodeGeneratorContext,
};
use num::ToPrimitive;
use std::collections::BTreeMap;

// ======================================================================================
// Entry

/// Generate code for the given function from its AST representation in the env.
/// This returns `FunctionData` suitable for the bytecode processing pipeline.
pub fn generate_bytecode(env: &GlobalEnv, fid: QualifiedId<FunId>) -> FunctionData {
    let func_env = env.get_function(fid);
    let mut gen = Generator {
        func_env,
        context: Default::default(),
        temps: Default::default(),
        scopes: Default::default(),
        label_counter: 0,
        loops: vec![],
        reference_mode_counter: 0,
        reference_mode_kind: ReferenceKind::Immutable,
        results: vec![],
        code: vec![],
    };
    let mut scope = BTreeMap::new();
    for Parameter(name, ty) in gen.func_env.get_parameters() {
        let temp = gen.new_temp(ty);
        scope.insert(name, temp);
    }
    for ty in gen.func_env.get_result_type().flatten() {
        let temp = gen.new_temp(ty);
        gen.results.push(temp)
    }
    gen.scopes.push(scope);
    if let Some(def) = gen.func_env.get_def().cloned() {
        let results = gen.results.clone();
        // Need to clone expression if present because of sharing issues with `gen`. However, because
        // of interning, clone is cheap.
        gen.gen(results.clone(), &def);
        gen.emit_with(def.node_id(), |attr| Bytecode::Ret(attr, results))
    }
    let Generator {
        func_env,
        context,
        temps,
        scopes: _,
        label_counter: _,
        loops: _,
        reference_mode_counter: _,
        reference_mode_kind: _,
        results: _,
        code,
    } = gen;
    let BytecodeGeneratorContext {
        loop_unrolling,
        loop_invariants,
        location_table,
        ..
    } = context;
    FunctionData::new(
        &func_env,
        code,
        temps,
        func_env.get_result_type(),
        location_table,
        BTreeMap::default(),
        vec![],
        loop_unrolling,
        loop_invariants,
    )
}

// ======================================================================================
// Generator state and helpers

/// Internal state of the code generator
#[derive(Debug)]
struct Generator<'env> {
    /// Access to the function env and its parent.
    func_env: FunctionEnv<'env>,
    /// A general bytecode generator context, shared with the stackless bytecode generator. This
    /// maintains a location table as well as information about specification constructs.
    context: BytecodeGeneratorContext,
    /// The temporaries allocated so far.
    temps: Vec<Type>,
    /// A list of scopes, where each scope is a map from symbol to  assigned temporary.
    scopes: Vec<Scope>,
    /// A counter for branch labels.
    label_counter: u16,
    /// A stack of loop contexts
    loops: Vec<LoopContext>,
    /// Whether we are currently generating for a reference expression. In this mode, an expression
    /// which would denote a value, as `s.f.g`, will denote in fact a reference for this value.
    /// This mode is used at the left-hand side of Mutate expressions (`*s.f.g = ...`), and in
    /// a field selection sequence: e.g. in `s.f.g + 1` references will be used for the intermediate
    /// selection steps and only the last field's type need to by copyable.
    reference_mode_counter: usize,
    /// The kind of the reference mode.
    reference_mode_kind: ReferenceKind,
    /// The list of temporaries where to store function return result.
    results: Vec<TempIndex>,
    /// The bytecode, as generated so far.
    code: Vec<Bytecode>,
}

type Scope = BTreeMap<Symbol, TempIndex>;

#[derive(Debug)]
struct LoopContext {
    /// The label where to continue the loop.
    continue_label: Label,
    /// The label where to break the loop.
    break_label: Label,
}

impl<'env> Generator<'env> {
    /// Shortcut to access global env.
    fn env(&self) -> &GlobalEnv {
        self.func_env.module_env.env
    }

    /// Emit a bytecode.
    fn emit(&mut self, b: Bytecode) {
        self.code.push(b)
    }

    /// Emit bytecode with attribute derived from node_id.
    fn emit_with(&mut self, id: NodeId, mk: impl FnOnce(AttrId) -> Bytecode) {
        let bytecode = mk(self.new_loc_attr(id));
        self.emit(bytecode)
    }

    /// Shortcut to emit a Call instruction.
    fn emit_call(
        &mut self,
        id: NodeId,
        targets: Vec<TempIndex>,
        oper: BytecodeOperation,
        sources: Vec<TempIndex>,
    ) {
        self.emit_with(id, |attr| {
            Bytecode::Call(attr, targets, oper, sources, None)
        })
    }

    /// Perform some action in reference generation mode. The parameter to the action indicates
    /// whether we entering or exiting this mode (true means we are entering).
    fn with_reference_mode<T>(&mut self, action: impl FnOnce(&mut Self, bool) -> T) -> T {
        let enter = self.reference_mode_counter == 0;
        self.reference_mode_counter += 1;
        let r = action(self, enter);
        self.reference_mode_counter -= 1;
        r
    }

    /// Whether we run in reference mode.
    fn reference_mode(&self) -> bool {
        self.reference_mode_counter > 0
    }

    /// Create a new attribute id and populate location table.
    fn new_loc_attr(&mut self, id: NodeId) -> AttrId {
        let loc = self.env().get_node_loc(id);
        self.context.new_loc_attr(loc)
    }

    /// Create a new temporary of type.
    fn new_temp(&mut self, ty: Type) -> TempIndex {
        let next_idx = self.temps.len();
        self.temps.insert(next_idx, ty);
        next_idx
    }

    /// Create a new temporary and check whether it has a valid type.
    fn new_temp_with_valid_type(&mut self, id: NodeId, ty: Type) -> TempIndex {
        if matches!(ty, Type::Tuple(..)) {
            self.error(
                id,
                format!("cannot assign tuple type `{}` to single variable (use `(a, b, ..) = ..` instead)",
                    ty.display(&self.env().get_type_display_ctx()))
            )
        }
        let next_idx = self.temps.len();
        self.temps.insert(next_idx, ty);
        next_idx
    }

    /// Release a temporary.
    fn release_temp(&mut self, _temp: TempIndex) {
        // Nop for now
    }

    /// Release temporaries.
    fn release_temps(&mut self, temps: impl AsRef<[TempIndex]>) {
        for temp in temps.as_ref() {
            self.release_temp(*temp)
        }
    }

    /// Creates a new branching label.
    fn new_label(&mut self, id: NodeId) -> Label {
        if self.label_counter < u16::MAX {
            let n = self.label_counter;
            self.label_counter += 1;
            Label::new(n as usize)
        } else {
            self.internal_error(id, "too many labels");
            Label::new(0)
        }
    }

    /// Require unary target.
    fn require_unary_target(&mut self, id: NodeId, target: Vec<TempIndex>) -> TempIndex {
        if target.len() != 1 {
            self.internal_error(id, "inconsistent expression target arity");
            0
        } else {
            target[0]
        }
    }

    /// Require unary argument. This has to clone the arg but thats fine because of
    /// interning.
    fn require_unary_arg(&self, id: NodeId, args: &[Exp]) -> Exp {
        if args.len() != 1 {
            self.internal_error(id, "inconsistent expression argument arity");
            ExpData::Invalid(self.env().new_node_id()).into_exp()
        } else {
            args[0].to_owned()
        }
    }

    /// Finds the temporary index assigned to the local.
    fn find_local(&self, id: NodeId, sym: Symbol) -> TempIndex {
        for scope in &self.scopes {
            if let Some(idx) = scope.get(&sym) {
                return *idx;
            }
        }
        self.internal_error(id, "local not defined");
        0
    }

    /// Return type of temporary.
    fn temp_type(&self, temp: TempIndex) -> &Type {
        &self.temps[temp]
    }

    /// Report an error at the location associated with the node.
    fn error(&self, id: NodeId, msg: impl AsRef<str>) {
        self.diag(id, Severity::Error, msg)
    }

    /// Report an (internal) error at the location associated with the node.
    fn internal_error(&self, id: NodeId, msg: impl AsRef<str>) {
        self.diag(id, Severity::Bug, msg)
    }

    fn diag(&self, id: NodeId, severity: Severity, msg: impl AsRef<str>) {
        let env = self.env();
        let loc = env.get_node_loc(id);
        env.diag(severity, &loc, msg.as_ref())
    }
}

// ======================================================================================
// Dispatcher

impl<'env> Generator<'env> {
    fn gen(&mut self, targets: Vec<TempIndex>, exp: &Exp) {
        match exp.as_ref() {
            ExpData::Invalid(id) => self.internal_error(*id, "invalid expression"),
            ExpData::Temporary(id, temp) => self.gen_temporary(targets, *id, *temp),
            ExpData::Value(id, val) => self.gen_value(targets, *id, val),
            ExpData::LocalVar(id, name) => self.gen_local(targets, *id, *name),
            ExpData::Call(id, op, args) => self.gen_call(targets, *id, op, args),
            ExpData::Sequence(_, exps) => {
                for step in exps.iter().take(exps.len() - 1) {
                    // Result is thrown away, but for typing reasons, we need to introduce
                    // temps to construct the step target.
                    let step_targets = self
                        .env()
                        .get_node_type(step.node_id())
                        .flatten()
                        .into_iter()
                        .map(|ty| self.new_temp(ty))
                        .collect::<Vec<_>>();
                    self.gen(step_targets.clone(), step);
                    self.release_temps(step_targets)
                }
                if let Some(final_step) = exps.last() {
                    self.gen(targets, final_step)
                } else {
                    self.release_temps(targets)
                }
            },
            ExpData::Block(id, pat, opt_binding, body) => {
                // Declare all variables bound by the pattern
                let mut scope = BTreeMap::new();
                for (id, sym) in pat.vars() {
                    let ty = self.env().get_node_type(id);
                    let temp = self.new_temp_with_valid_type(id, ty);
                    scope.insert(sym, temp);
                }
                // If there is a binding, assign the pattern
                if let Some(binding) = opt_binding {
                    self.gen_assign(*id, pat, binding, Some(&scope));
                }
                // Compile the body
                self.scopes.push(scope);
                self.gen(targets, body);
                self.scopes.pop();
            },
            ExpData::Mutate(id, lhs, rhs) => {
                let rhs_temp = self.gen_arg(rhs);
                let lhs_temp = self.gen_auto_ref_arg(lhs, ReferenceKind::Mutable);
                if !self.temp_type(lhs_temp).is_mutable_reference() {
                    self.error(
                        lhs.node_id(),
                        format!(
                            "expected `&mut` but found `{}`",
                            self.temp_type(lhs_temp)
                                .display(&self.env().get_type_display_ctx()),
                        ),
                    );
                }
                self.emit_call(*id, targets, BytecodeOperation::WriteRef, vec![
                    lhs_temp, rhs_temp,
                ])
            },
            ExpData::Assign(id, lhs, rhs) => self.gen_assign(*id, lhs, rhs, None),
            ExpData::Return(id, exp) => {
                let results = self.results.clone();
                self.gen(results.clone(), exp);
                self.emit_with(*id, |attr| Bytecode::Ret(attr, results))
            },
            ExpData::IfElse(id, cond, then_exp, else_exp) => {
                let cond_temp = self.gen_arg(cond);
                let then_label = self.new_label(*id);
                let else_label = self.new_label(*id);
                let end_label = self.new_label(*id);
                self.emit_with(*id, |attr| {
                    Bytecode::Branch(attr, then_label, else_label, cond_temp)
                });
                let then_id = then_exp.node_id();
                self.emit_with(then_id, |attr| Bytecode::Label(attr, then_label));
                self.gen(targets.clone(), then_exp);
                self.emit_with(then_id, |attr| Bytecode::Jump(attr, end_label));
                let else_id = else_exp.node_id();
                self.emit_with(else_id, |attr| Bytecode::Label(attr, else_label));
                self.gen(targets, else_exp);
                self.emit_with(else_id, |attr| Bytecode::Label(attr, end_label));
            },
            ExpData::Loop(id, body) => {
                let continue_label = self.new_label(*id);
                let break_label = self.new_label(*id);
                self.loops.push(LoopContext {
                    continue_label,
                    break_label,
                });
                self.emit_with(*id, |attr| Bytecode::Label(attr, continue_label));
                self.gen(vec![], body);
                self.loops.pop();
                self.emit_with(*id, |attr| Bytecode::Jump(attr, continue_label));
                self.emit_with(*id, |attr| Bytecode::Label(attr, break_label));
            },
            ExpData::LoopCont(id, do_continue) => {
                if let Some(LoopContext {
                    continue_label,
                    break_label,
                }) = self.loops.last()
                {
                    let target = if *do_continue {
                        *continue_label
                    } else {
                        *break_label
                    };
                    self.emit_with(*id, |attr| Bytecode::Jump(attr, target))
                } else {
                    self.error(*id, "missing enclosing loop statement")
                }
            },
            ExpData::SpecBlock(_, spec) => {
                let (mut code, mut update_map) = self.context.generate_spec(&self.func_env, spec);
                self.code.append(&mut code);
                self.func_env
                    .get_mut_spec()
                    .update_map
                    .append(&mut update_map)
            },
            ExpData::Invoke(id, _, _) | ExpData::Lambda(id, _, _) => {
                self.internal_error(*id, format!("not yet implemented: {:?}", exp))
            },
            ExpData::Quant(id, _, _, _, _, _) => {
                self.internal_error(*id, "unsupported specification construct")
            },
        }
    }
}

// ======================================================================================
// Values

impl<'env> Generator<'env> {
    fn gen_value(&mut self, target: Vec<TempIndex>, id: NodeId, val: &Value) {
        let target = self.require_unary_target(id, target);
        let cons = self.to_constant(id, val);
        self.emit_with(id, |attr| Bytecode::Load(attr, target, cons))
    }

    /// Convert a value from AST world into a constant as expected in bytecode.
    fn to_constant(&self, id: NodeId, val: &Value) -> Constant {
        let ty = self.env().get_node_type(id);
        match val {
            Value::Address(x) => Constant::Address(x.clone()),
            Value::Number(x) => match ty {
                // In the AST, all numbers are uniquely represent by `BigInt`. The bytecode
                // distinguishes representations, we need to do a type based conversion.
                Type::Primitive(PrimitiveType::U8) => Constant::U8(x.to_u8().unwrap_or_default()),
                Type::Primitive(PrimitiveType::U16) => {
                    Constant::U16(x.to_u16().unwrap_or_default())
                },
                Type::Primitive(PrimitiveType::U32) => {
                    Constant::U32(x.to_u32().unwrap_or_default())
                },
                Type::Primitive(PrimitiveType::U64) => {
                    Constant::U64(x.to_u64().unwrap_or_default())
                },
                Type::Primitive(PrimitiveType::U128) => {
                    Constant::U128(x.to_u128().unwrap_or_default())
                },
                Type::Primitive(PrimitiveType::U256) => {
                    // No direct way to go from BigInt to ethnum::U256...
                    let x = U256::from_str_radix(&x.to_str_radix(16), 16).unwrap();
                    Constant::U256(x)
                },
                ty => {
                    self.internal_error(id, format!("inconsistent numeric constant: {:?}", ty));
                    Constant::Bool(false)
                },
            },
            Value::Bool(x) => Constant::Bool(*x),
            Value::ByteArray(x) => Constant::ByteArray(x.clone()),
            Value::AddressArray(x) => Constant::AddressArray(x.clone()),
            Value::Vector(x) => {
                Constant::Vector(x.iter().map(|v| self.to_constant(id, v)).collect())
            },
        }
    }
}

// ======================================================================================
// Locals

impl<'env> Generator<'env> {
    fn gen_local(&mut self, targets: Vec<TempIndex>, id: NodeId, name: Symbol) {
        let target = self.require_unary_target(id, targets);
        let attr = self.new_loc_attr(id);
        for scope in &self.scopes {
            if let Some(temp) = scope.get(&name) {
                self.emit(Bytecode::Assign(attr, target, *temp, AssignKind::Move));
                return;
            }
        }
        self.internal_error(
            id,
            format!(
                "unbound symbol `{}`",
                name.display(self.env().symbol_pool())
            ),
        )
    }

    fn gen_temporary(&mut self, targets: Vec<TempIndex>, id: NodeId, temp: TempIndex) {
        let target = self.require_unary_target(id, targets);
        self.emit_with(id, |attr| {
            Bytecode::Assign(attr, target, temp, AssignKind::Move)
        })
    }
}

// ======================================================================================
// Calls

impl<'env> Generator<'env> {
    fn gen_call(&mut self, targets: Vec<TempIndex>, id: NodeId, op: &Operation, args: &[Exp]) {
        match op {
            Operation::Vector => self.gen_op_call(targets, id, BytecodeOperation::Vector, args),
            Operation::Freeze => self.gen_op_call(targets, id, BytecodeOperation::FreezeRef, args),
            Operation::Tuple => {
                if targets.len() != args.len() {
                    self.internal_error(id, "inconsistent tuple arity")
                } else {
                    for (target, arg) in targets.into_iter().zip(args.iter()) {
                        self.gen(vec![target], arg)
                    }
                }
            },
            Operation::Pack(mid, sid) => {
                let inst = self.env().get_node_instantiation(id);
                self.gen_op_call(targets, id, BytecodeOperation::Pack(*mid, *sid, inst), args)
            },
            Operation::Select(mid, sid, fid) => {
                let inst = self.env().get_node_instantiation(id);
                let target = self.require_unary_target(id, targets);
                let arg = self.require_unary_arg(id, args);
                self.gen_select(target, id, mid.qualified_inst(*sid, inst), *fid, &arg)
            },
            Operation::Exists(None) => {
                let inst = self.env().get_node_instantiation(id);
                let (mid, sid, inst) = inst[0].require_struct();
                self.gen_op_call(
                    targets,
                    id,
                    BytecodeOperation::Exists(mid, sid, inst.to_owned()),
                    args,
                )
            },
            Operation::BorrowGlobal(_) => {
                let inst = self.env().get_node_instantiation(id);
                let (mid, sid, inst) = inst[0].require_struct();
                self.gen_op_call(
                    targets,
                    id,
                    BytecodeOperation::BorrowGlobal(mid, sid, inst.to_owned()),
                    args,
                )
            },
            Operation::MoveTo => {
                let inst = self.env().get_node_instantiation(id);
                let (mid, sid, inst) = inst[0].require_struct();
                self.gen_op_call(
                    targets,
                    id,
                    BytecodeOperation::MoveTo(mid, sid, inst.to_owned()),
                    args,
                )
            },
            Operation::MoveFrom => {
                let inst = self.env().get_node_instantiation(id);
                let (mid, sid, inst) = inst[0].require_struct();
                self.gen_op_call(
                    targets,
                    id,
                    BytecodeOperation::MoveFrom(mid, sid, inst.to_owned()),
                    args,
                )
            },
            Operation::Borrow(kind) => {
                let target = self.require_unary_target(id, targets);
                let arg = self.require_unary_arg(id, args);
                self.gen_borrow(target, id, *kind, &arg)
            },
            Operation::Abort => {
                let arg = self.require_unary_arg(id, args);
                let temp = self.gen_arg(&arg);
                self.emit_with(id, |attr| Bytecode::Abort(attr, temp))
            },
            Operation::Deref => self.gen_op_call(targets, id, BytecodeOperation::ReadRef, args),
            Operation::MoveFunction(m, f) => {
                self.gen_function_call(targets, id, m.qualified(*f), args)
            },
            Operation::Cast => self.gen_cast_call(targets, id, args),
            Operation::Add => self.gen_op_call(targets, id, BytecodeOperation::Add, args),
            Operation::Sub => self.gen_op_call(targets, id, BytecodeOperation::Sub, args),
            Operation::Mul => self.gen_op_call(targets, id, BytecodeOperation::Mul, args),
            Operation::Mod => self.gen_op_call(targets, id, BytecodeOperation::Mod, args),
            Operation::Div => self.gen_op_call(targets, id, BytecodeOperation::Div, args),
            Operation::BitOr => self.gen_op_call(targets, id, BytecodeOperation::BitOr, args),
            Operation::BitAnd => self.gen_op_call(targets, id, BytecodeOperation::BitAnd, args),
            Operation::Xor => self.gen_op_call(targets, id, BytecodeOperation::Xor, args),
            Operation::Shl => self.gen_op_call(targets, id, BytecodeOperation::Shl, args),
            Operation::Shr => self.gen_op_call(targets, id, BytecodeOperation::Shr, args),
            Operation::And => self.gen_logical_shortcut(true, targets, id, args),
            Operation::Or => self.gen_logical_shortcut(false, targets, id, args),
            Operation::Eq => self.gen_op_call(targets, id, BytecodeOperation::Eq, args),
            Operation::Neq => self.gen_op_call(targets, id, BytecodeOperation::Neq, args),
            Operation::Lt => self.gen_op_call(targets, id, BytecodeOperation::Lt, args),
            Operation::Gt => self.gen_op_call(targets, id, BytecodeOperation::Gt, args),
            Operation::Le => self.gen_op_call(targets, id, BytecodeOperation::Le, args),
            Operation::Ge => self.gen_op_call(targets, id, BytecodeOperation::Ge, args),
            Operation::Not => self.gen_op_call(targets, id, BytecodeOperation::Not, args),

            // Non-supported specification related operations
            Operation::Exists(Some(_))
            | Operation::SpecFunction(_, _, _)
            | Operation::Implies
            | Operation::Iff
            | Operation::UpdateField(_, _, _)
            | Operation::Index
            | Operation::Slice
            | Operation::Range
            | Operation::Result(_)
            | Operation::Len
            | Operation::TypeValue
            | Operation::TypeDomain
            | Operation::ResourceDomain
            | Operation::Global(_)
            | Operation::CanModify
            | Operation::Old
            | Operation::Trace(_)
            | Operation::Identical
            | Operation::EmptyVec
            | Operation::SingleVec
            | Operation::UpdateVec
            | Operation::ConcatVec
            | Operation::IndexOfVec
            | Operation::ContainsVec
            | Operation::InRangeRange
            | Operation::InRangeVec
            | Operation::RangeVec
            | Operation::MaxU8
            | Operation::MaxU16
            | Operation::MaxU32
            | Operation::MaxU64
            | Operation::MaxU128
            | Operation::MaxU256
            | Operation::Bv2Int
            | Operation::Int2Bv
            | Operation::AbortFlag
            | Operation::AbortCode
            | Operation::WellFormed
            | Operation::BoxValue
            | Operation::UnboxValue
            | Operation::EmptyEventStore
            | Operation::ExtendEventStore
            | Operation::EventStoreIncludes
            | Operation::EventStoreIncludedIn
            | Operation::NoOp => self.internal_error(
                id,
                format!("unsupported specification construct: `{:?}`", op),
            ),
        }
    }

    fn gen_cast_call(&mut self, targets: Vec<TempIndex>, id: NodeId, args: &[Exp]) {
        let ty = self.env().get_node_type(id);
        let bytecode_op = match ty {
            Type::Primitive(PrimitiveType::U8) => BytecodeOperation::CastU8,
            Type::Primitive(PrimitiveType::U16) => BytecodeOperation::CastU16,
            Type::Primitive(PrimitiveType::U32) => BytecodeOperation::CastU32,
            Type::Primitive(PrimitiveType::U64) => BytecodeOperation::CastU64,
            Type::Primitive(PrimitiveType::U128) => BytecodeOperation::CastU128,
            Type::Primitive(PrimitiveType::U256) => BytecodeOperation::CastU256,
            _ => {
                self.internal_error(id, "inconsistent type");
                return;
            },
        };
        self.gen_op_call(targets, id, bytecode_op, args)
    }

    fn gen_op_call(
        &mut self,
        targets: Vec<TempIndex>,
        id: NodeId,
        op: BytecodeOperation,
        args: &[Exp],
    ) {
        let arg_temps = self.gen_arg_list(args);
        self.emit_with(id, |attr| {
            Bytecode::Call(attr, targets, op, arg_temps, None)
        })
    }

    fn gen_logical_shortcut(
        &mut self,
        is_and: bool,
        targets: Vec<TempIndex>,
        id: NodeId,
        args: &[Exp],
    ) {
        let target = self.require_unary_target(id, targets);
        let arg1 = self.gen_arg(&args[0]);
        let true_label = self.new_label(id);
        let false_label = self.new_label(id);
        let done_label = self.new_label(id);
        self.emit_with(id, |attr| {
            Bytecode::Branch(attr, true_label, false_label, arg1)
        });
        self.emit_with(id, |attr| Bytecode::Label(attr, true_label));
        if is_and {
            self.gen(vec![target], &args[1]);
        } else {
            self.emit_with(id, |attr| {
                Bytecode::Load(attr, target, Constant::Bool(true))
            })
        }
        self.emit_with(id, |attr| Bytecode::Jump(attr, done_label));
        self.emit_with(id, |attr| Bytecode::Label(attr, false_label));
        if is_and {
            self.emit_with(id, |attr| {
                Bytecode::Load(attr, target, Constant::Bool(false))
            })
        } else {
            self.gen(vec![target], &args[1]);
        }
        self.emit_with(id, |attr| Bytecode::Label(attr, done_label));
    }

    fn gen_function_call(
        &mut self,
        targets: Vec<TempIndex>,
        id: NodeId,
        fun: QualifiedId<FunId>,
        args: &[Exp],
    ) {
        let type_args = self
            .env()
            .get_node_instantiation_opt(id)
            .unwrap_or_default();
        // Function calls can have implicit conversion of &mut to &, need to compute implicit
        // conversions.
        let param_types: Vec<Type> = self
            .env()
            .get_function(fun)
            .get_parameters()
            .into_iter()
            .map(|Parameter(_, ty)| ty.instantiate(&type_args))
            .collect();
        if args.len() != param_types.len() {
            self.internal_error(id, "inconsistent type arity");
            return;
        }
        let args = args
            .iter()
            .zip(param_types)
            .map(|(e, t)| self.maybe_convert(e, &t))
            .collect::<Vec<_>>();
        let args = self.gen_arg_list(&args);
        self.emit_with(id, |attr| {
            Bytecode::Call(
                attr,
                targets,
                BytecodeOperation::Function(fun.module_id, fun.id, type_args),
                args,
                None,
            )
        })
    }

    /// Convert the expression so it matches the expected type. This is currently only needed
    /// for `&mut` to `&` conversion, in which case we need to to introduce a Freeze operation.
    fn maybe_convert(&self, exp: &Exp, expected_ty: &Type) -> Exp {
        let id = exp.node_id();
        let exp_ty = self.env().get_node_type(id);
        if let (
            Type::Reference(ReferenceKind::Mutable, _),
            Type::Reference(ReferenceKind::Immutable, et),
        ) = (exp_ty, expected_ty)
        {
            let freeze_id = self
                .env()
                .new_node(self.env().get_node_loc(id), expected_ty.clone());
            self.env()
                .set_node_instantiation(freeze_id, vec![et.as_ref().clone()]);
            ExpData::Call(freeze_id, Operation::Freeze, vec![exp.clone()]).into_exp()
        } else {
            exp.clone()
        }
    }

    fn gen_arg_list(&mut self, exps: &[Exp]) -> Vec<TempIndex> {
        exps.iter().map(|exp| self.gen_arg(exp)).collect()
    }

    fn gen_arg(&mut self, exp: &Exp) -> TempIndex {
        match exp.as_ref() {
            ExpData::Temporary(_, temp) => *temp,
            ExpData::LocalVar(id, sym) => self.find_local(*id, *sym),
            ExpData::Call(_, Operation::Select(..), _) if self.reference_mode() => {
                // In reference mode, a selection is interpreted as selecting a reference to the
                // field.
                let id = exp.node_id();
                let ty = Type::Reference(
                    self.reference_mode_kind,
                    Box::new(self.env().get_node_type(id)),
                );
                let temp = self.new_temp(ty);
                self.gen(vec![temp], exp);
                temp
            },
            _ => {
                // Otherwise, introduce a temporary
                let id = exp.node_id();
                let ty = self.env().get_node_type(id);
                let temp = self.new_temp(ty);
                self.gen(vec![temp], exp);
                temp
            },
        }
    }

    fn gen_auto_ref_arg(&mut self, exp: &Exp, default_ref_kind: ReferenceKind) -> TempIndex {
        let temp = self.with_reference_mode(|s, entering| {
            if entering {
                s.reference_mode_kind = default_ref_kind
            }
            s.gen_arg(exp)
        });
        let ty = self.temp_type(temp);
        if ty.is_reference() {
            temp
        } else {
            // Need to introduce a reference for the temp.
            let temp_ref = self.new_temp(Type::Reference(
                self.reference_mode_kind,
                Box::new(ty.to_owned()),
            ));
            self.emit_call(
                exp.node_id(),
                vec![temp_ref],
                BytecodeOperation::BorrowLoc,
                vec![temp],
            );
            temp_ref
        }
    }
}

// ======================================================================================
// References

impl<'env> Generator<'env> {
    fn gen_borrow(&mut self, target: TempIndex, id: NodeId, kind: ReferenceKind, arg: &Exp) {
        match arg.as_ref() {
            ExpData::Call(_arg_id, Operation::Select(mid, sid, fid), args) => {
                return self.gen_borrow_field(
                    target,
                    id,
                    kind,
                    mid.qualified(*sid),
                    *fid,
                    &self.require_unary_arg(id, args),
                )
            },
            ExpData::LocalVar(_arg_id, sym) => return self.gen_borrow_local(target, id, *sym),
            ExpData::Temporary(_arg_id, temp) => return self.gen_borrow_temp(target, id, *temp),
            _ => {},
        }
        if kind == ReferenceKind::Mutable {
            // Operand is neither field selection nor local. We can take an immutable reference
            // of such anonymous (stack) locations, but cannot mutate them.
            self.error(
                arg.node_id(),
                "operand to `&mut` must be a field selection (`&mut s.f`) or a local (`&mut name`)",
            );
        } else {
            // Borrow the temporary, allowing to do e.g. `&(1+2)`. Note to match
            // this capability in the stack machine, we need to keep those temps in locals
            // and can't manage them on the stack during stackification.
            let temp = self.gen_arg(arg);
            self.gen_borrow_temp(target, id, temp)
        }
    }

    fn gen_borrow_local(&mut self, target: TempIndex, id: NodeId, name: Symbol) {
        self.gen_borrow_temp(target, id, self.find_local(id, name))
    }

    fn gen_borrow_temp(&mut self, target: TempIndex, id: NodeId, temp: TempIndex) {
        self.emit_call(id, vec![target], BytecodeOperation::BorrowLoc, vec![temp]);
    }

    fn gen_borrow_field(
        &mut self,
        target: TempIndex,
        id: NodeId,
        kind: ReferenceKind,
        struct_id: QualifiedId<StructId>,
        field_id: FieldId,
        oper: &Exp,
    ) {
        let (field_name, field_offset) = {
            let struct_env = self.env().get_struct(struct_id);
            let field_env = struct_env.get_field(field_id);
            (field_env.get_name(), field_env.get_offset())
        };
        let temp = self.gen_arg(oper);
        match kind {
            ReferenceKind::Mutable => {
                // Either the operand is a &mut, or a declared location
                // which is not a reference
                let ty = self.temp_type(temp);
                if !(ty.is_mutable_reference()
                    || matches!(
                        oper.as_ref(),
                        ExpData::LocalVar(..) | ExpData::Temporary(..)
                    ) && !ty.is_reference())
                {
                    let struct_name = self.env().get_struct(struct_id).get_full_name_str();
                    self.error(
                        oper.node_id(),
                        format!(
                            "operand to `&mut _.{}` must have type `&mut {}` or be a local of type `{}`",
                            field_name.display(self.env().symbol_pool()),
                            struct_name,
                            struct_name
                        ),
                    )
                }
            },
            ReferenceKind::Immutable => {
                // Currently no conditions for immutable, so we do allow `&fun().field`.
            },
        }
        let inst = self.env().get_node_instantiation(id);
        self.emit_call(
            id,
            vec![target],
            BytecodeOperation::BorrowField(struct_id.module_id, struct_id.id, inst, field_offset),
            vec![temp],
        );
    }
}

// ======================================================================================
// Structs

impl<'env> Generator<'env> {
    /// Generate code for a field selection. This needs to deal with the combination of the
    /// following cases which the type checker allows:
    /// (1) the operand is a reference or is not.
    /// (2) the select is used as an lvalue or it is not
    fn gen_select(
        &mut self,
        target: TempIndex,
        id: NodeId,
        str: QualifiedInstId<StructId>,
        field: FieldId,
        oper: &Exp,
    ) {
        let struct_env = self.env().get_struct(str.to_qualified_id());
        let field_offset = struct_env.get_field(field).get_offset();

        // Compile operand in reference mode, defaulting to immutable mpde.
        let oper_temp = self.gen_auto_ref_arg(oper, ReferenceKind::Immutable);

        // If we are in reference mode and a &mut is requested, the operand also needs to be
        // &mut.
        if self.reference_mode()
            && self.reference_mode_kind == ReferenceKind::Mutable
            && !self.temp_type(oper_temp).is_mutable_reference()
        {
            self.error(
                oper.node_id(),
                format!(
                    "expected `&mut` but found `{}`",
                    self.temp_type(oper_temp)
                        .display(&self.env().get_type_display_ctx())
                ),
            )
        }

        // Borrow the field, resulting a reference. A reference is what we want
        // if (a) the target is a reference (b) or we are compiling in reference mode. If
        // none of those cases apply, we want to do a ReadRef at the end of the selection
        // to get the actual value from the field selection.
        let target_type = self.temp_type(target).to_owned();
        let need_read_ref = !(target_type.is_reference() || self.reference_mode());
        let borrow_dest = if need_read_ref {
            let ref_ty = Type::Reference(ReferenceKind::Immutable, Box::new(target_type));
            self.new_temp(ref_ty)
        } else {
            target
        };
        self.emit_call(
            id,
            vec![borrow_dest],
            BytecodeOperation::BorrowField(str.module_id, str.id, str.inst, field_offset),
            vec![oper_temp],
        );
        if need_read_ref {
            self.emit_call(id, vec![target], BytecodeOperation::ReadRef, vec![
                borrow_dest,
            ])
        }
    }
}

// ======================================================================================
// Pattern matching

impl<'env> Generator<'env> {
    /// Generate code for assignment of an expression to a pattern. This involves
    /// flattening nested patterns as needed. The optional `next_scope` is a
    /// scope to enter after the rhs exp has been compiled.
    fn gen_assign(&mut self, id: NodeId, pat: &Pattern, exp: &Exp, next_scope: Option<&Scope>) {
        if let Pattern::Tuple(_, pat_args) = pat {
            self.gen_tuple_assign(id, pat_args, exp, next_scope)
        } else {
            let arg = self.gen_arg(exp);
            self.gen_assign_from_temp(id, pat, arg, next_scope)
        }
    }

    /// Generate assignment for tuples. Move has a weird semantics for tuples: they aren't first
    /// class citizens, so there is no runtime value for tuples. They are only allowed in
    // `(a, b, ...) = fun_call` or `(a, b, ...) = (x, y, ...)`
    fn gen_tuple_assign(
        &mut self,
        id: NodeId,
        pats: &[Pattern],
        exp: &Exp,
        next_scope: Option<&Scope>,
    ) {
        match exp.as_ref() {
            ExpData::Call(_, Operation::Tuple, args) => {
                if args.len() != pats.len() {
                    // Type checker should have complained already
                    self.internal_error(id, "inconsistent tuple arity")
                } else {
                    // Map this to point-wise assignment
                    for (pat, exp) in pats.iter().zip(args.iter()) {
                        self.gen_assign(id, pat, exp, next_scope)
                    }
                }
            },
            ExpData::Call(id, Operation::MoveFunction(mid, fid), args) => {
                // The type checker has ensured that this function returns a tuple
                let (temps, cont_assigns) = self.flatten_patterns(pats, next_scope);
                self.gen_function_call(temps, *id, mid.qualified(*fid), args);
                for (cont_id, cont_pat, cont_temp) in cont_assigns {
                    self.gen_assign_from_temp(cont_id, &cont_pat, cont_temp, next_scope)
                }
            },
            _ => self.error(
                id,
                "assignment to tuple must be tuple itself or a function call",
            ),
        }
    }

    fn gen_assign_from_temp(
        &mut self,
        id: NodeId,
        pat: &Pattern,
        arg: TempIndex,
        next_scope: Option<&Scope>,
    ) {
        match pat {
            Pattern::Wildcard(_) => {
                // Nothing to do
            },
            Pattern::Var(var_id, sym) => {
                let local = self.find_local_for_pattern(*var_id, *sym, next_scope);
                self.emit_with(id, |attr| {
                    Bytecode::Assign(attr, local, arg, AssignKind::Move)
                })
            },
            Pattern::Struct(id, str, args) => {
                let (temps, cont_assigns) = self.flatten_patterns(args, next_scope);
                self.emit_call(
                    *id,
                    temps,
                    BytecodeOperation::Unpack(str.module_id, str.id, str.inst.to_owned()),
                    vec![arg],
                );
                for (cont_id, cont_pat, cont_temp) in cont_assigns {
                    self.gen_assign_from_temp(cont_id, &cont_pat, cont_temp, next_scope)
                }
            },
            Pattern::Tuple(id, _) => self.error(*id, "tuple not allowed here"),
            Pattern::Error(_) => self.internal_error(id, "unexpected error pattern"),
        }
    }

    /// Flatten a pattern, returning a temporary to receive the value being matched against,
    /// and an optional continuation assignment to match the sub-pattern. The optional
    /// `next_scope` is used to lookup locals which are introduced by this binding
    /// but are not yet in scope; this is needed to deal with assignments of the form
    /// `let x = f(x)` where the 2nd x refers to a variable in the current scope, but the first
    /// to one which we introduce right now.
    fn flatten_pattern(
        &mut self,
        pat: &Pattern,
        next_scope: Option<&Scope>,
    ) -> (TempIndex, Option<(NodeId, Pattern, TempIndex)>) {
        match pat {
            Pattern::Wildcard(id) => {
                // Wildcard pattern: we need to create a temporary to receive the value, even
                // if its dropped afterwards.
                let temp = self.new_temp(self.env().get_node_type(*id));
                (temp, None)
            },
            Pattern::Var(id, sym) => {
                // Variable pattern: no continuation assignment needed as it is already in
                // the expected form.
                (self.find_local_for_pattern(*id, *sym, next_scope), None)
            },
            _ => {
                // Pattern is not flat: create a new temporary and an Assignment of this
                // temporary to the pattern.
                let id = pat.node_id();
                let ty = self.env().get_node_type(id);
                let temp = self.new_temp_with_valid_type(id, ty);
                (temp, Some((id, pat.clone(), temp)))
            },
        }
    }

    fn flatten_patterns(
        &mut self,
        pats: &[Pattern],
        next_scope: Option<&Scope>,
    ) -> (Vec<TempIndex>, Vec<(NodeId, Pattern, TempIndex)>) {
        let mut temps = vec![];
        let mut cont_assigns = vec![];
        for pat in pats {
            let (temp, opt_cont) = self.flatten_pattern(pat, next_scope);
            temps.push(temp);
            if let Some(cont) = opt_cont {
                cont_assigns.push(cont)
            }
        }
        (temps, cont_assigns)
    }

    fn find_local_for_pattern(
        &mut self,
        id: NodeId,
        sym: Symbol,
        next_scope: Option<&Scope>,
    ) -> TempIndex {
        if let Some(temp) = next_scope.and_then(|s| s.get(&sym)) {
            *temp
        } else {
            self.find_local(id, sym)
        }
    }
}
