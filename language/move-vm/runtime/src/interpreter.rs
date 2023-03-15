// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    config::VMRuntimeLimitsConfig,
    loader::{Function, Loader, Resolver},
    native_functions::NativeContext,
    trace,
};
use fail::fail_point;
use move_binary_format::{
    errors::*,
    file_format::{Ability, AbilitySet, Bytecode, FunctionHandleIndex, FunctionInstantiationIndex},
};
use move_core_types::{
    account_address::AccountAddress,
    gas_algebra::{NumArgs, NumBytes},
    language_storage::TypeTag,
    vm_status::{StatusCode, StatusType},
};
use move_vm_types::{
    data_store::DataStore,
    gas::{GasMeter, SimpleInstruction},
    loaded_data::runtime_types::Type,
    values::{
        self, GlobalValue, IntegerValue, Locals, Reference, Struct, StructRef, VMValueCast, Value,
        Vector, VectorRef,
    },
    views::TypeView,
};

use crate::{loader::Scope, native_extensions::NativeContextExtensions};
use move_binary_format::file_format::StructDefInstantiationIndex;
use std::{
    cmp::min,
    collections::{HashMap, VecDeque},
    fmt::Write,
    sync::Arc,
};
use tracing::error;

macro_rules! debug_write {
    ($($toks: tt)*) => {
        write!($($toks)*).map_err(|_|
            PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                .with_message("failed to write to buffer".to_string())
        )
    };
}

macro_rules! debug_writeln {
    ($($toks: tt)*) => {
        writeln!($($toks)*).map_err(|_|
            PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                .with_message("failed to write to buffer".to_string())
        )
    };
}

macro_rules! set_err_info {
    ($frame:ident, $e:expr) => {{
        $e.at_code_offset($frame.function.index(), $frame.pc)
            .finish($frame.location())
    }};
}

/// `Interpreter` instances can execute Move functions.
///
/// An `Interpreter` instance is a stand alone execution context for a function.
/// It mimics execution on a single thread, with an call stack and an operand stack.
pub(crate) struct Interpreter {
    /// Operand stack, where Move `Value`s are stored for stack operations.
    operand_stack: Stack,
    /// The stack of active functions.
    call_stack: CallStack,
    /// Whether to perform a paranoid type safety checks at runtime.
    paranoid_type_checks: bool,
    /// Limits imposed at runtime
    runtime_limits_config: VMRuntimeLimitsConfig,
    /// Type to instantiation cache
    instantiation_cache: HashMap<Type, HashMap<Vec<Type>, usize>>,
    instantiations: Vec<Type>,
    /// Type instantiations from instantiation index
    type_instantiation_cache:
        HashMap<(Scope, StructDefInstantiationIndex), HashMap<Vec<Type>, usize>>,
    type_instantiations: Vec<Type>,
    /// Function instantiations from instantiation index
    function_instantiation_cache:
        HashMap<(Scope, FunctionInstantiationIndex), HashMap<Vec<Type>, usize>>,
    function_instantiations: Vec<Vec<Type>>,
}

struct TypeWithLoader<'a, 'b> {
    ty: &'a Type,
    loader: &'b Loader,
}

impl<'a, 'b> TypeView for TypeWithLoader<'a, 'b> {
    fn to_type_tag(&self) -> TypeTag {
        self.loader.type_to_type_tag(self.ty).unwrap()
    }
}

impl Interpreter {
    /// Limits imposed at runtime
    pub fn runtime_limits_config(&self) -> &VMRuntimeLimitsConfig {
        &self.runtime_limits_config
    }
    /// Entrypoint into the interpreter. All external calls need to be routed through this
    /// function.
    pub(crate) fn entrypoint(
        function: Arc<Function>,
        ty_args: Vec<Type>,
        args: Vec<Value>,
        data_store: &mut impl DataStore,
        gas_meter: &mut impl GasMeter,
        extensions: &mut NativeContextExtensions,
        loader: &Loader,
    ) -> VMResult<Vec<Value>> {
        let mut interpreter = Interpreter {
            operand_stack: Stack::new(),
            call_stack: CallStack::new(),
            paranoid_type_checks: loader.vm_config().paranoid_type_checks,
            runtime_limits_config: loader.vm_config().runtime_limits_config.clone(),
            instantiation_cache: HashMap::new(),
            instantiations: vec![],
            type_instantiation_cache: HashMap::new(),
            type_instantiations: vec![],
            function_instantiation_cache: HashMap::new(),
            function_instantiations: if ty_args.is_empty() {
                vec![ty_args]
            } else {
                vec![vec![], ty_args]
            },
        };
        interpreter.execute_main(loader, data_store, gas_meter, extensions, function, args)?;
        Ok(interpreter.operand_stack.value)
    }

    /// Main loop for the execution of a function.
    ///
    /// This function sets up a `Frame` and calls `execute_code_unit` to execute code of the
    /// function represented by the frame. Control comes back to this function on return or
    /// on call. When that happens the frame is changes to a new one (call) or to the one
    /// at the top of the stack (return). If the call stack is empty execution is completed.
    fn execute_main(
        &mut self,
        loader: &Loader,
        data_store: &mut impl DataStore,
        gas_meter: &mut impl GasMeter,
        extensions: &mut NativeContextExtensions,
        function: Arc<Function>,
        args: Vec<Value>,
    ) -> VMResult<()> {
        let mut locals = Locals::new(function.local_count());
        for (i, value) in args.into_iter().enumerate() {
            locals
                .store_loc(i, value)
                .map_err(|e| self.set_location(e))?;
        }

        let mut current_frame = self
            .make_new_frame(function, self.function_instantiations.len() - 1, locals)
            .map_err(|err| self.set_location(err))?;
        loop {
            let resolver = current_frame.resolver(loader);
            let exit_code = self
                .execute_code(&resolver, &mut current_frame, data_store, gas_meter)
                .map_err(|err| self.maybe_core_dump(err, &current_frame))?;
            match exit_code {
                ExitCode::Return => {
                    let non_ref_vals = current_frame
                        .locals
                        .drop_all_values()
                        .map(|(_idx, val)| val);

                    // TODO: Check if the error location is set correctly.
                    gas_meter
                        .charge_drop_frame(non_ref_vals.into_iter())
                        .map_err(|e| self.set_location(e))?;

                    if let Some(frame) = self.call_stack.pop() {
                        // Note: the caller will find the callee's return values at the top of the shared operand stack
                        current_frame = frame;
                        current_frame.pc += 1; // advance past the Call instruction in the caller
                    } else {
                        // end of execution. `self` should no longer be used afterward
                        return Ok(());
                    }
                }
                ExitCode::Call(fh_idx) => {
                    let func = resolver.function_from_handle(fh_idx);

                    if self.paranoid_type_checks {
                        self.check_friend_or_private_call(&current_frame.function, &func)?;
                    }

                    // Charge gas
                    let module_id = func
                        .module_id()
                        .ok_or_else(|| {
                            PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                                .with_message("Failed to get native function module id".to_string())
                        })
                        .map_err(|e| set_err_info!(current_frame, e))?;
                    gas_meter
                        .charge_call(
                            module_id,
                            func.name(),
                            self.operand_stack
                                .last_n(func.arg_count())
                                .map_err(|e| set_err_info!(current_frame, e))?,
                            (func.local_count() as u64).into(),
                        )
                        .map_err(|e| set_err_info!(current_frame, e))?;

                    if func.is_native() {
                        self.call_native(&resolver, data_store, gas_meter, extensions, func, 0)?;
                        current_frame.pc += 1; // advance past the Call instruction in the caller
                        continue;
                    }
                    let frame = self
                        .make_call_frame(func, 0)
                        .map_err(|e| self.set_location(e))
                        .map_err(|err| self.maybe_core_dump(err, &current_frame))?;
                    self.call_stack.push(current_frame).map_err(|frame| {
                        let err = PartialVMError::new(StatusCode::CALL_STACK_OVERFLOW);
                        let err = set_err_info!(frame, err);
                        self.maybe_core_dump(err, &frame)
                    })?;
                    // Note: the caller will find the the callee's return values at the top of the shared operand stack
                    current_frame = frame;
                }
                ExitCode::CallGeneric(idx) => {
                    let ty_args_idx = self
                        .function_instantiation(&resolver, &mut current_frame, idx)
                        .map_err(|e| set_err_info!(current_frame, e))?;
                    let func = resolver.function_from_instantiation(idx);

                    if self.paranoid_type_checks {
                        self.check_friend_or_private_call(&current_frame.function, &func)?;
                    }

                    // Charge gas
                    let module_id = func
                        .module_id()
                        .ok_or_else(|| {
                            PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                                .with_message("Failed to get native function module id".to_string())
                        })
                        .map_err(|e| set_err_info!(current_frame, e))?;
                    gas_meter
                        .charge_call_generic(
                            module_id,
                            func.name(),
                            self.function_instantiations[ty_args_idx]
                                .iter()
                                .map(|ty| TypeWithLoader { ty, loader }),
                            self.operand_stack
                                .last_n(func.arg_count())
                                .map_err(|e| set_err_info!(current_frame, e))?,
                            (func.local_count() as u64).into(),
                        )
                        .map_err(|e| set_err_info!(current_frame, e))?;

                    if func.is_native() {
                        self.call_native(
                            &resolver,
                            data_store,
                            gas_meter,
                            extensions,
                            func,
                            ty_args_idx,
                        )?;
                        current_frame.pc += 1; // advance past the Call instruction in the caller
                        continue;
                    }
                    let frame = self
                        .make_call_frame(func, ty_args_idx)
                        .map_err(|e| self.set_location(e))
                        .map_err(|err| self.maybe_core_dump(err, &current_frame))?;
                    self.call_stack.push(current_frame).map_err(|frame| {
                        let err = PartialVMError::new(StatusCode::CALL_STACK_OVERFLOW);
                        let err = set_err_info!(frame, err);
                        self.maybe_core_dump(err, &frame)
                    })?;
                    current_frame = frame;
                }
            }
        }
    }

    /// Returns a `Frame` if the call is to a Move function. Calls to native functions are
    /// "inlined" and this returns `None`.
    ///
    /// Native functions do not push a frame at the moment and as such errors from a native
    /// function are incorrectly attributed to the caller.
    fn make_call_frame(
        &mut self,
        func: Arc<Function>,
        ty_args_idx: usize,
    ) -> PartialVMResult<Frame> {
        let mut locals = Locals::new(func.local_count());
        let arg_count = func.arg_count();
        let is_generic = ty_args_idx != 0;
        for i in 0..arg_count {
            locals.store_loc(arg_count - i - 1, self.operand_stack.pop()?)?;

            if self.paranoid_type_checks {
                let ty = self.operand_stack.pop_ty()?;
                if is_generic {
                    ty.check_eq(
                        &func.local_types()[arg_count - i - 1]
                            .subst(&self.function_instantiations[ty_args_idx])?,
                    )?;
                } else {
                    // Directly check against the expected type to save a clone here.
                    ty.check_eq(&func.local_types()[arg_count - i - 1])?;
                }
            }
        }
        self.make_new_frame(func, ty_args_idx, locals)
    }

    /// Create a new `Frame` given a `Function` and the function `Locals`.
    ///
    /// The locals must be loaded before calling this.
    fn make_new_frame(
        &self,
        function: Arc<Function>,
        ty_args_idx: usize,
        locals: Locals,
    ) -> PartialVMResult<Frame> {
        let local_tys = if self.paranoid_type_checks {
            if ty_args_idx == 0 {
                function.local_types().to_vec()
            } else {
                function
                    .local_types()
                    .iter()
                    .map(|ty| ty.subst(&self.function_instantiations[ty_args_idx]))
                    .collect::<PartialVMResult<Vec<_>>>()?
            }
        } else {
            vec![]
        };

        Ok(Frame {
            pc: 0,
            locals,
            function,
            ty_args_idx,
            local_tys,
            instantiations: HashMap::new(),
            type_instantiations: HashMap::new(),
            function_instantiations: HashMap::new(),
        })
    }

    /// Call a native functions.
    fn call_native(
        &mut self,
        resolver: &Resolver,
        data_store: &mut dyn DataStore,
        gas_meter: &mut impl GasMeter,
        extensions: &mut NativeContextExtensions,
        function: Arc<Function>,
        ty_args_idx: usize,
    ) -> VMResult<()> {
        // Note: refactor if native functions push a frame on the stack
        self.call_native_impl(
            resolver,
            data_store,
            gas_meter,
            extensions,
            function.clone(),
            ty_args_idx,
        )
        .map_err(|e| match function.module_id() {
            Some(id) => e
                .at_code_offset(function.index(), 0)
                .finish(Location::Module(id.clone())),
            None => {
                let err = PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                    .with_message("Unexpected native function not located in a module".to_owned());
                self.set_location(err)
            }
        })
    }

    fn call_native_impl(
        &mut self,
        resolver: &Resolver,
        data_store: &mut dyn DataStore,
        gas_meter: &mut impl GasMeter,
        extensions: &mut NativeContextExtensions,
        function: Arc<Function>,
        ty_args_idx: usize,
    ) -> PartialVMResult<()> {
        let return_type_count = function.return_type_count();
        let mut args = VecDeque::new();
        let expected_args = function.arg_count();
        for _ in 0..expected_args {
            args.push_front(self.operand_stack.pop()?);
        }

        let ty_args = self.function_instantiations[ty_args_idx].clone();

        if self.paranoid_type_checks {
            for i in 0..expected_args {
                let expected_ty =
                    resolver.subst(&function.parameter_types()[expected_args - i - 1], &ty_args)?;
                let ty = self.operand_stack.pop_ty()?;
                ty.check_eq(&expected_ty)?;
            }
        }

        let mut native_context = NativeContext::new(
            self,
            data_store,
            resolver,
            extensions,
            gas_meter.remaining_gas(),
        );
        let native_function = function.get_native()?;

        gas_meter.charge_native_function_before_execution(
            ty_args.iter().map(|ty| TypeWithLoader {
                ty,
                loader: resolver.loader(),
            }),
            args.iter(),
        )?;

        let result = native_function(&mut native_context, ty_args, args)?;

        // Note(Gas): The order by which gas is charged / error gets returned MUST NOT be modified
        //            here or otherwise it becomes an incompatible change!!!
        let return_values = match result.result {
            Ok(vals) => {
                gas_meter.charge_native_function(result.cost, Some(vals.iter()))?;
                vals
            }
            Err(code) => {
                gas_meter.charge_native_function(
                    result.cost,
                    Option::<std::iter::Empty<&Value>>::None,
                )?;
                return Err(PartialVMError::new(StatusCode::ABORTED).with_sub_status(code));
            }
        };

        // Paranoid check to protect us against incorrect native function implementations. A native function that
        // returns a different number of values than its declared types will trigger this check
        if return_values.len() != return_type_count {
            return Err(
                PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR).with_message(
                    "Arity mismatch: return value count does not match return type count"
                        .to_string(),
                ),
            );
        }
        // Put return values on the top of the operand stack, where the caller will find them.
        // This is one of only two times the operand stack is shared across call stack frames; the other is in handling
        // the Return instruction for normal calls
        for value in return_values {
            self.operand_stack.push(value)?;
        }

        if self.paranoid_type_checks {
            for ty in function.return_types() {
                self.operand_stack
                    .push_ty(ty.subst(&self.function_instantiations[ty_args_idx])?)?;
            }
        }
        Ok(())
    }

    /// Make sure only private/friend function can only be invoked by modules under the same address.
    fn check_friend_or_private_call(
        &self,
        caller: &Arc<Function>,
        callee: &Arc<Function>,
    ) -> VMResult<()> {
        if callee.is_friend_or_private() {
            match (caller.module_id(), callee.module_id()) {
                (Some(caller_id), Some(callee_id)) => {
                    if caller_id.address() == callee_id.address() {
                        Ok(())
                    } else {
                        Err(self.set_location(PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                                .with_message(
                                    format!("Private/Friend function invokation error, caller: {:?}::{:?}, callee: {:?}::{:?}", caller_id, caller.name(), callee_id, callee.name()),
                                )))
                    }
                }
                _ => Err(self.set_location(
                    PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                        .with_message(format!(
                            "Private/Friend function invokation error caller: {:?}, callee {:?}",
                            caller.name(),
                            callee.name()
                        )),
                )),
            }
        } else {
            Ok(())
        }
    }

    /// Perform a binary operation to two values at the top of the stack.
    fn binop<F, T>(&mut self, f: F) -> PartialVMResult<()>
    where
        Value: VMValueCast<T>,
        F: FnOnce(T, T) -> PartialVMResult<Value>,
    {
        let rhs = self.operand_stack.pop_as::<T>()?;
        let lhs = self.operand_stack.pop_as::<T>()?;
        let result = f(lhs, rhs)?;
        self.operand_stack.push(result)
    }

    /// Perform a binary operation for integer values.
    fn binop_int<F>(&mut self, f: F) -> PartialVMResult<()>
    where
        F: FnOnce(IntegerValue, IntegerValue) -> PartialVMResult<IntegerValue>,
    {
        self.binop(|lhs, rhs| {
            Ok(match f(lhs, rhs)? {
                IntegerValue::U8(x) => Value::u8(x),
                IntegerValue::U16(x) => Value::u16(x),
                IntegerValue::U32(x) => Value::u32(x),
                IntegerValue::U64(x) => Value::u64(x),
                IntegerValue::U128(x) => Value::u128(x),
                IntegerValue::U256(x) => Value::u256(x),
            })
        })
    }

    /// Perform a binary operation for boolean values.
    fn binop_bool<F, T>(&mut self, f: F) -> PartialVMResult<()>
    where
        Value: VMValueCast<T>,
        F: FnOnce(T, T) -> PartialVMResult<bool>,
    {
        self.binop(|lhs, rhs| Ok(Value::bool(f(lhs, rhs)?)))
    }

    /// Loads a resource from the data store and return the number of bytes read from the storage.
    fn load_resource<'b>(
        gas_meter: &mut impl GasMeter,
        data_store: &'b mut impl DataStore,
        addr: AccountAddress,
        ty: &Type,
    ) -> PartialVMResult<&'b mut GlobalValue> {
        match data_store.load_resource(addr, ty) {
            Ok((gv, load_res)) => {
                if let Some(loaded) = load_res {
                    let opt = match loaded {
                        Some(num_bytes) => {
                            let view = gv.view().ok_or_else(|| {
                                PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                                    .with_message(
                                        "Failed to create view for global value".to_owned(),
                                    )
                            })?;

                            Some((num_bytes, view))
                        }
                        None => None,
                    };
                    gas_meter.charge_load_resource(opt)?;
                }
                Ok(gv)
            }
            Err(e) => {
                error!(
                    "[VM] error loading resource at ({}, {:?}): {:?} from data store",
                    addr, ty, e
                );
                Err(e)
            }
        }
    }

    /// BorrowGlobal (mutable and not) opcode.
    fn borrow_global(
        is_mut: bool,
        is_generic: bool,
        loader: &Loader,
        gas_meter: &mut impl GasMeter,
        data_store: &mut impl DataStore,
        addr: AccountAddress,
        ty: &Type,
    ) -> PartialVMResult<Value> {
        let res = Self::load_resource(gas_meter, data_store, addr, ty)?.borrow_global();
        gas_meter.charge_borrow_global(
            is_mut,
            is_generic,
            TypeWithLoader { ty, loader },
            res.is_ok(),
        )?;
        res
    }

    /// Exists opcode.
    fn exists(
        is_generic: bool,
        loader: &Loader,
        gas_meter: &mut impl GasMeter,
        data_store: &mut impl DataStore,
        addr: AccountAddress,
        ty: &Type,
    ) -> PartialVMResult<bool> {
        let gv = Self::load_resource(gas_meter, data_store, addr, ty)?;
        let exists = gv.exists()?;
        gas_meter.charge_exists(is_generic, TypeWithLoader { ty, loader }, exists)?;
        Ok(exists)
    }

    /// MoveFrom opcode.
    fn move_from(
        is_generic: bool,
        loader: &Loader,
        gas_meter: &mut impl GasMeter,
        data_store: &mut impl DataStore,
        addr: AccountAddress,
        ty: &Type,
    ) -> PartialVMResult<Value> {
        let resource = match Self::load_resource(gas_meter, data_store, addr, ty)?.move_from() {
            Ok(resource) => {
                gas_meter.charge_move_from(
                    is_generic,
                    TypeWithLoader { ty, loader },
                    Some(&resource),
                )?;
                resource
            }
            Err(err) => {
                let val: Option<&Value> = None;
                gas_meter.charge_move_from(is_generic, TypeWithLoader { ty, loader }, val)?;
                return Err(err);
            }
        };
        Ok(resource)
    }

    /// MoveTo opcode.
    fn move_to(
        is_generic: bool,
        loader: &Loader,
        gas_meter: &mut impl GasMeter,
        data_store: &mut impl DataStore,
        addr: AccountAddress,
        ty: &Type,
        resource: Value,
    ) -> PartialVMResult<()> {
        let gv = Self::load_resource(gas_meter, data_store, addr, ty)?;
        // NOTE(Gas): To maintain backward compatibility, we need to charge gas after attempting
        //            the move_to operation.
        match gv.move_to(resource) {
            Ok(()) => {
                gas_meter.charge_move_to(
                    is_generic,
                    TypeWithLoader { ty, loader },
                    gv.view().unwrap(),
                    true,
                )?;
                Ok(())
            }
            Err((err, resource)) => {
                gas_meter.charge_move_to(
                    is_generic,
                    TypeWithLoader { ty, loader },
                    &resource,
                    false,
                )?;
                Err(err)
            }
        }
    }

    //
    // Debugging and logging helpers.
    //

    /// Given an `VMStatus` generate a core dump if the error is an `InvariantViolation`.
    fn maybe_core_dump(&self, mut err: VMError, current_frame: &Frame) -> VMError {
        // a verification error cannot happen at runtime so change it into an invariant violation.
        if err.status_type() == StatusType::Verification {
            error!("Verification error during runtime: {:?}", err);
            let new_err = PartialVMError::new(StatusCode::VERIFICATION_ERROR);
            let new_err = match err.message() {
                None => new_err,
                Some(msg) => new_err.with_message(msg.to_owned()),
            };
            err = new_err.finish(err.location().clone())
        }
        if err.status_type() == StatusType::InvariantViolation {
            let state = self.internal_state_str(current_frame);

            error!(
                "Error: {:?}\nCORE DUMP: >>>>>>>>>>>>\n{}\n<<<<<<<<<<<<\n",
                err, state,
            );
        }
        err
    }

    #[allow(dead_code)]
    fn debug_print_frame<B: Write>(
        &self,
        buf: &mut B,
        loader: &Loader,
        idx: usize,
        frame: &Frame,
    ) -> PartialVMResult<()> {
        // Print out the function name with type arguments.
        let func = &frame.function;

        debug_write!(buf, "    [{}] ", idx)?;
        if let Some(module) = func.module_id() {
            debug_write!(buf, "{}::{}::", module.address(), module.name(),)?;
        }
        debug_write!(buf, "{}", func.name())?;
        let ty_args = &self.function_instantiations[frame.ty_args_idx];
        let mut ty_tags = vec![];
        for ty in ty_args {
            ty_tags.push(loader.type_to_type_tag(ty)?);
        }
        if !ty_tags.is_empty() {
            debug_write!(buf, "<")?;
            let mut it = ty_tags.iter();
            if let Some(tag) = it.next() {
                debug_write!(buf, "{}", tag)?;
                for tag in it {
                    debug_write!(buf, ", ")?;
                    debug_write!(buf, "{}", tag)?;
                }
            }
            debug_write!(buf, ">")?;
        }
        debug_writeln!(buf)?;

        // Print out the current instruction.
        debug_writeln!(buf)?;
        debug_writeln!(buf, "        Code:")?;
        let pc = frame.pc as usize;
        let code = func.code();
        let before = if pc > 3 { pc - 3 } else { 0 };
        let after = min(code.len(), pc + 4);
        for (idx, instr) in code.iter().enumerate().take(pc).skip(before) {
            debug_writeln!(buf, "            [{}] {:?}", idx, instr)?;
        }
        debug_writeln!(buf, "          > [{}] {:?}", pc, &code[pc])?;
        for (idx, instr) in code.iter().enumerate().take(after).skip(pc + 1) {
            debug_writeln!(buf, "            [{}] {:?}", idx, instr)?;
        }

        // Print out the locals.
        debug_writeln!(buf)?;
        debug_writeln!(buf, "        Locals:")?;
        if func.local_count() > 0 {
            values::debug::print_locals(buf, &frame.locals)?;
            debug_writeln!(buf)?;
        } else {
            debug_writeln!(buf, "            (none)")?;
        }

        debug_writeln!(buf)?;
        Ok(())
    }

    #[allow(dead_code)]
    pub(crate) fn debug_print_stack_trace<B: Write>(
        &self,
        buf: &mut B,
        loader: &Loader,
    ) -> PartialVMResult<()> {
        debug_writeln!(buf, "Call Stack:")?;
        for (i, frame) in self.call_stack.0.iter().enumerate() {
            self.debug_print_frame(buf, loader, i, frame)?;
        }
        debug_writeln!(buf, "Operand Stack:")?;
        for (idx, val) in self.operand_stack.value.iter().enumerate() {
            // TODO: Currently we do not know the types of the values on the operand stack.
            // Revisit.
            debug_write!(buf, "    [{}] ", idx)?;
            values::debug::print_value(buf, val)?;
            debug_writeln!(buf)?;
        }
        Ok(())
    }

    /// Generate a string which is the status of the interpreter: call stack, current bytecode
    /// stream, locals and operand stack.
    ///
    /// It is used when generating a core dump but can be used for debugging of the interpreter.
    /// It will be exposed via a debug module to give developers a way to print the internals
    /// of an execution.
    fn internal_state_str(&self, current_frame: &Frame) -> String {
        let mut internal_state = "Call stack:\n".to_string();
        for (i, frame) in self.call_stack.0.iter().enumerate() {
            internal_state.push_str(
                format!(
                    " frame #{}: {} [pc = {}]\n",
                    i,
                    frame.function.pretty_string(),
                    frame.pc,
                )
                .as_str(),
            );
        }
        internal_state.push_str(
            format!(
                "*frame #{}: {} [pc = {}]:\n",
                self.call_stack.0.len(),
                current_frame.function.pretty_string(),
                current_frame.pc,
            )
            .as_str(),
        );
        let code = current_frame.function.code();
        let pc = current_frame.pc as usize;
        if pc < code.len() {
            let mut i = 0;
            for bytecode in &code[..pc] {
                internal_state.push_str(format!("{}> {:?}\n", i, bytecode).as_str());
                i += 1;
            }
            internal_state.push_str(format!("{}* {:?}\n", i, code[pc]).as_str());
        }
        internal_state.push_str(
            format!(
                "Locals ({:x}):\n{}\n",
                current_frame.locals.raw_address(),
                current_frame.locals
            )
            .as_str(),
        );
        internal_state.push_str("Operand Stack:\n");
        for value in &self.operand_stack.value {
            internal_state.push_str(format!("{}\n", value).as_str());
        }
        internal_state
    }

    fn set_location(&self, err: PartialVMError) -> VMError {
        err.finish(self.call_stack.current_location())
    }

    fn get_internal_state(&self) -> ExecutionState {
        self.get_stack_frames(usize::MAX)
    }

    /// Get count stack frames starting from the top of the stack.
    pub(crate) fn get_stack_frames(&self, count: usize) -> ExecutionState {
        // collect frames in the reverse order as this is what is
        // normally expected from the stack trace (outermost frame
        // is the last one)
        let stack_trace = self
            .call_stack
            .0
            .iter()
            .rev()
            .take(count)
            .map(|frame| {
                (
                    frame.function.module_id().cloned(),
                    frame.function.index(),
                    frame.pc,
                )
            })
            .collect();
        ExecutionState::new(stack_trace)
    }
}

// TODO Determine stack size limits based on gas limit
const OPERAND_STACK_SIZE_LIMIT: usize = 1024;
const CALL_STACK_SIZE_LIMIT: usize = 1024;

/// The operand stack.
struct Stack {
    value: Vec<Value>,
    types: Vec<Type>,
}

impl Stack {
    /// Create a new empty operand stack.
    fn new() -> Self {
        Stack {
            value: vec![],
            types: vec![],
        }
    }

    /// Push a `Value` on the stack if the max stack size has not been reached. Abort execution
    /// otherwise.
    fn push(&mut self, value: Value) -> PartialVMResult<()> {
        if self.value.len() < OPERAND_STACK_SIZE_LIMIT {
            self.value.push(value);
            Ok(())
        } else {
            Err(PartialVMError::new(StatusCode::EXECUTION_STACK_OVERFLOW))
        }
    }

    /// Pop a `Value` off the stack or abort execution if the stack is empty.
    fn pop(&mut self) -> PartialVMResult<Value> {
        self.value
            .pop()
            .ok_or_else(|| PartialVMError::new(StatusCode::EMPTY_VALUE_STACK))
    }

    /// Pop a `Value` of a given type off the stack. Abort if the value is not of the given
    /// type or if the stack is empty.
    fn pop_as<T>(&mut self) -> PartialVMResult<T>
    where
        Value: VMValueCast<T>,
    {
        self.pop()?.value_as()
    }

    /// Pop n values off the stack.
    fn popn(&mut self, n: u16) -> PartialVMResult<Vec<Value>> {
        let remaining_stack_size = self
            .value
            .len()
            .checked_sub(n as usize)
            .ok_or_else(|| PartialVMError::new(StatusCode::EMPTY_VALUE_STACK))?;
        let args = self.value.split_off(remaining_stack_size);
        Ok(args)
    }

    fn last_n(&self, n: usize) -> PartialVMResult<impl ExactSizeIterator<Item = &Value>> {
        if self.value.len() < n {
            return Err(PartialVMError::new(StatusCode::EMPTY_VALUE_STACK)
                .with_message("Failed to get last n arguments on the argument stack".to_string()));
        }
        Ok(self.value[(self.value.len() - n)..].iter())
    }

    /// Push a `Value` on the stack if the max stack size has not been reached. Abort execution
    /// otherwise.
    fn push_ty(&mut self, ty: Type) -> PartialVMResult<()> {
        if self.types.len() < OPERAND_STACK_SIZE_LIMIT {
            self.types.push(ty);
            Ok(())
        } else {
            Err(PartialVMError::new(StatusCode::EXECUTION_STACK_OVERFLOW))
        }
    }

    /// Pop a `Value` off the stack or abort execution if the stack is empty.
    fn pop_ty(&mut self) -> PartialVMResult<Type> {
        self.types
            .pop()
            .ok_or_else(|| PartialVMError::new(StatusCode::EMPTY_VALUE_STACK))
    }

    /// Pop n values off the stack.
    fn popn_tys(&mut self, n: u16) -> PartialVMResult<Vec<Type>> {
        let remaining_stack_size = self
            .types
            .len()
            .checked_sub(n as usize)
            .ok_or_else(|| PartialVMError::new(StatusCode::EMPTY_VALUE_STACK))?;
        let args = self.types.split_off(remaining_stack_size);
        Ok(args)
    }

    fn check_balance(&self) -> PartialVMResult<()> {
        if self.types.len() != self.value.len() {
            return Err(
                PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR).with_message(
                    "Paranoid Mode: Type and value stack need to be balanced".to_string(),
                ),
            );
        }
        Ok(())
    }
}

/// A call stack.
// #[derive(Debug)]
struct CallStack(Vec<Frame>);

impl CallStack {
    /// Create a new empty call stack.
    fn new() -> Self {
        CallStack(vec![])
    }

    /// Push a `Frame` on the call stack.
    fn push(&mut self, frame: Frame) -> ::std::result::Result<(), Frame> {
        if self.0.len() < CALL_STACK_SIZE_LIMIT {
            self.0.push(frame);
            Ok(())
        } else {
            Err(frame)
        }
    }

    /// Pop a `Frame` off the call stack.
    fn pop(&mut self) -> Option<Frame> {
        self.0.pop()
    }

    fn current_location(&self) -> Location {
        let location_opt = self.0.last().map(|frame| frame.location());
        location_opt.unwrap_or(Location::Undefined)
    }
}

/// A `Frame` is the execution context for a function. It holds the locals of the function and
/// the function itself.
// #[derive(Debug)]
struct Frame {
    pc: u16,
    locals: Locals,
    function: Arc<Function>,
    ty_args_idx: usize,
    local_tys: Vec<Type>,
    instantiations: HashMap<u16, usize>,
    type_instantiations: HashMap<u16, usize>,
    function_instantiations: HashMap<u16, usize>,
}

/// An `ExitCode` from `execute_code_unit`.
#[derive(Debug)]
enum ExitCode {
    Return,
    Call(FunctionHandleIndex),
    CallGeneric(FunctionInstantiationIndex),
}

fn check_ability(has_ability: bool) -> PartialVMResult<()> {
    if has_ability {
        Ok(())
    } else {
        Err(
            PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                .with_message("Paranoid Mode: Expected ability mismatch".to_string()),
        )
    }
}

impl Interpreter {
    /// Execute a Move function until a return or a call opcode is found.
    fn execute_code(
        &mut self,
        resolver: &Resolver,
        frame: &mut Frame,
        data_store: &mut impl DataStore,
        gas_meter: &mut impl GasMeter,
    ) -> VMResult<ExitCode> {
        self.execute_code_impl(resolver, frame, data_store, gas_meter)
            .map_err(|e| {
                let e = if cfg!(feature = "testing") || cfg!(feature = "stacktrace") {
                    e.with_exec_state(self.get_internal_state())
                } else {
                    e
                };
                e.at_code_offset(frame.function.index(), frame.pc)
                    .finish(frame.location())
            })
    }

    /// Paranoid type checks to perform before instruction execution.
    ///
    /// Note that most of the checks should happen after instruction execution, because gas charging will happen during
    /// instruction execution and we want to avoid running code without charging proper gas as much as possible.
    fn pre_execution_type_stack_transition(
        &mut self,
        local_tys: &[Type],
        locals: &Locals,
        resolver: &Resolver,
        instruction: &Bytecode,
    ) -> PartialVMResult<()> {
        match instruction {
            // Call instruction will be checked at execute_main.
            Bytecode::Call(_) | Bytecode::CallGeneric(_) => (),
            Bytecode::BrFalse(_) | Bytecode::BrTrue(_) => {
                self.operand_stack.pop_ty()?;
            }
            Bytecode::Branch(_) => (),
            Bytecode::Ret => {
                for (idx, ty) in local_tys.iter().enumerate() {
                    if !locals.is_invalid(idx)? {
                        check_ability(resolver.loader().abilities(ty)?.has_drop())?;
                    }
                }
            }
            Bytecode::Abort => {
                self.operand_stack.pop_ty()?;
            }
            // StLoc needs to check before execution as we need to check the drop ability of values.
            Bytecode::StLoc(idx) => {
                let ty = local_tys[*idx as usize].clone();
                let val_ty = self.operand_stack.pop_ty()?;
                ty.check_eq(&val_ty)?;
                if !locals.is_invalid(*idx as usize)? {
                    check_ability(resolver.loader().abilities(&ty)?.has_drop())?;
                }
            }
            // We will check the rest of the instructions after execution phase.
            Bytecode::Pop
            | Bytecode::LdU8(_)
            | Bytecode::LdU16(_)
            | Bytecode::LdU32(_)
            | Bytecode::LdU64(_)
            | Bytecode::LdU128(_)
            | Bytecode::LdU256(_)
            | Bytecode::LdTrue
            | Bytecode::LdFalse
            | Bytecode::LdConst(_)
            | Bytecode::CopyLoc(_)
            | Bytecode::MoveLoc(_)
            | Bytecode::MutBorrowLoc(_)
            | Bytecode::ImmBorrowLoc(_)
            | Bytecode::ImmBorrowField(_)
            | Bytecode::MutBorrowField(_)
            | Bytecode::ImmBorrowFieldGeneric(_)
            | Bytecode::MutBorrowFieldGeneric(_)
            | Bytecode::Pack(_)
            | Bytecode::PackGeneric(_)
            | Bytecode::Unpack(_)
            | Bytecode::UnpackGeneric(_)
            | Bytecode::ReadRef
            | Bytecode::WriteRef
            | Bytecode::CastU8
            | Bytecode::CastU16
            | Bytecode::CastU32
            | Bytecode::CastU64
            | Bytecode::CastU128
            | Bytecode::CastU256
            | Bytecode::Add
            | Bytecode::Sub
            | Bytecode::Mul
            | Bytecode::Mod
            | Bytecode::Div
            | Bytecode::BitOr
            | Bytecode::BitAnd
            | Bytecode::Xor
            | Bytecode::Or
            | Bytecode::And
            | Bytecode::Shl
            | Bytecode::Shr
            | Bytecode::Lt
            | Bytecode::Le
            | Bytecode::Gt
            | Bytecode::Ge
            | Bytecode::Eq
            | Bytecode::Neq
            | Bytecode::MutBorrowGlobal(_)
            | Bytecode::ImmBorrowGlobal(_)
            | Bytecode::MutBorrowGlobalGeneric(_)
            | Bytecode::ImmBorrowGlobalGeneric(_)
            | Bytecode::Exists(_)
            | Bytecode::ExistsGeneric(_)
            | Bytecode::MoveTo(_)
            | Bytecode::MoveToGeneric(_)
            | Bytecode::MoveFrom(_)
            | Bytecode::MoveFromGeneric(_)
            | Bytecode::FreezeRef
            | Bytecode::Nop
            | Bytecode::Not
            | Bytecode::VecPack(_, _)
            | Bytecode::VecLen(_)
            | Bytecode::VecImmBorrow(_)
            | Bytecode::VecMutBorrow(_)
            | Bytecode::VecPushBack(_)
            | Bytecode::VecPopBack(_)
            | Bytecode::VecUnpack(_, _)
            | Bytecode::VecSwap(_) => (),
        };
        Ok(())
    }

    /// Paranoid type checks to perform after instruction execution.
    ///
    /// This function and `pre_execution_type_stack_transition` should constitute the full type stack transition for the paranoid mode.
    fn post_execution_type_stack_transition(
        &mut self,
        frame: &mut Frame,
        resolver: &Resolver,
        instruction: &Bytecode,
    ) -> PartialVMResult<()> {
        match instruction {
            Bytecode::BrTrue(_) | Bytecode::BrFalse(_) => (),
            Bytecode::Branch(_)
            | Bytecode::Ret
            | Bytecode::Call(_)
            | Bytecode::CallGeneric(_)
            | Bytecode::Abort => {
                // Invariants hold because all of the instructions above will force VM to break from the interpreter loop and thus not hit this code path.
                unreachable!("control flow instruction encountered during type check")
            }
            Bytecode::Pop => {
                let ty = self.operand_stack.pop_ty()?;
                check_ability(resolver.loader().abilities(&ty)?.has_drop())?;
            }
            Bytecode::LdU8(_) => self.operand_stack.push_ty(Type::U8)?,
            Bytecode::LdU16(_) => self.operand_stack.push_ty(Type::U16)?,
            Bytecode::LdU32(_) => self.operand_stack.push_ty(Type::U32)?,
            Bytecode::LdU64(_) => self.operand_stack.push_ty(Type::U64)?,
            Bytecode::LdU128(_) => self.operand_stack.push_ty(Type::U128)?,
            Bytecode::LdU256(_) => self.operand_stack.push_ty(Type::U256)?,
            Bytecode::LdTrue | Bytecode::LdFalse => self.operand_stack.push_ty(Type::Bool)?,
            Bytecode::LdConst(i) => {
                let constant = resolver.constant_at(*i);
                self.operand_stack
                    .push_ty(Type::from_const_signature(&constant.type_)?)?;
            }
            Bytecode::CopyLoc(idx) => {
                let ty = frame.local_tys[*idx as usize].clone();
                check_ability(resolver.loader().abilities(&ty)?.has_copy())?;
                self.operand_stack.push_ty(ty)?;
            }
            Bytecode::MoveLoc(idx) => {
                let ty = frame.local_tys[*idx as usize].clone();
                self.operand_stack.push_ty(ty)?;
            }
            Bytecode::StLoc(_) => (),
            Bytecode::MutBorrowLoc(idx) => {
                let ty = frame.local_tys[*idx as usize].clone();
                self.operand_stack
                    .push_ty(Type::MutableReference(Box::new(ty)))?;
            }
            Bytecode::ImmBorrowLoc(idx) => {
                let ty = frame.local_tys[*idx as usize].clone();
                self.operand_stack.push_ty(Type::Reference(Box::new(ty)))?;
            }
            Bytecode::ImmBorrowField(fh_idx) => {
                let expected_ty = resolver.field_handle_to_struct(*fh_idx);
                let top_ty = self.operand_stack.pop_ty()?;
                top_ty.check_ref_eq(&expected_ty)?;
                self.operand_stack
                    .push_ty(Type::Reference(Box::new(resolver.get_field_type(*fh_idx)?)))?;
            }
            Bytecode::MutBorrowField(fh_idx) => {
                let expected_ty = resolver.field_handle_to_struct(*fh_idx);
                let top_ty = self.operand_stack.pop_ty()?;
                top_ty.check_eq(&Type::MutableReference(Box::new(expected_ty)))?;
                self.operand_stack.push_ty(Type::MutableReference(Box::new(
                    resolver.get_field_type(*fh_idx)?,
                )))?;
            }
            Bytecode::ImmBorrowFieldGeneric(idx) => {
                let expected_ty = resolver.field_instantiation_to_struct(
                    *idx,
                    &self.function_instantiations[frame.ty_args_idx],
                )?;
                let top_ty = self.operand_stack.pop_ty()?;
                top_ty.check_ref_eq(&expected_ty)?;
                self.operand_stack.push_ty(Type::Reference(Box::new(
                    resolver.instantiate_generic_field(
                        *idx,
                        &self.function_instantiations[frame.ty_args_idx],
                    )?,
                )))?;
            }
            Bytecode::MutBorrowFieldGeneric(idx) => {
                let expected_ty = resolver.field_instantiation_to_struct(
                    *idx,
                    &self.function_instantiations[frame.ty_args_idx],
                )?;
                let top_ty = self.operand_stack.pop_ty()?;
                top_ty.check_eq(&Type::MutableReference(Box::new(expected_ty)))?;
                self.operand_stack.push_ty(Type::MutableReference(Box::new(
                    resolver.instantiate_generic_field(
                        *idx,
                        &self.function_instantiations[frame.ty_args_idx],
                    )?,
                )))?;
            }
            Bytecode::Pack(idx) => {
                let field_count = resolver.field_count(*idx);
                let args_ty = resolver.get_struct_fields(*idx)?;
                let output_ty = resolver.get_struct_type(*idx);
                let ability = resolver.loader().abilities(&output_ty)?;

                // If the struct has a key ability, we expects all of its field to have store ability but not key ability.
                let field_expected_abilities = if ability.has_key() {
                    ability
                        .remove(Ability::Key)
                        .union(AbilitySet::singleton(Ability::Store))
                } else {
                    ability
                };

                if field_count as usize != args_ty.fields.len() {
                    return Err(
                        PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                            .with_message("Args count mismatch".to_string()),
                    );
                }

                for (ty, expected_ty) in self
                    .operand_stack
                    .popn_tys(field_count)?
                    .into_iter()
                    .zip(args_ty.fields.iter())
                {
                    // Fields ability should be a subset of the struct ability because abilities can be weakened but not the other direction.
                    // For example, it is ok to have a struct that doesn't have a copy capability where its field is a struct that has copy capability but not vice versa.
                    check_ability(
                        field_expected_abilities.is_subset(resolver.loader().abilities(&ty)?),
                    )?;
                    ty.check_eq(expected_ty)?;
                }

                self.operand_stack.push_ty(output_ty)?;
            }
            Bytecode::PackGeneric(idx) => {
                let field_count = resolver.field_instantiation_count(*idx);
                let args_ty = resolver.instantiate_generic_struct_fields(
                    *idx,
                    &self.function_instantiations[frame.ty_args_idx],
                )?;
                let output_ty = resolver.instantiate_generic_type(
                    *idx,
                    &self.function_instantiations[frame.ty_args_idx],
                )?;
                let ability = resolver.loader().abilities(&output_ty)?;

                // If the struct has a key ability, we expects all of its field to have store ability but not key ability.
                let field_expected_abilities = if ability.has_key() {
                    ability
                        .remove(Ability::Key)
                        .union(AbilitySet::singleton(Ability::Store))
                } else {
                    ability
                };

                if field_count as usize != args_ty.len() {
                    return Err(
                        PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                            .with_message("Args count mismatch".to_string()),
                    );
                }

                for (ty, expected_ty) in self
                    .operand_stack
                    .popn_tys(field_count)?
                    .into_iter()
                    .zip(args_ty.iter())
                {
                    // Fields ability should be a subset of the struct ability because abilities can be weakened but not the other direction.
                    // For example, it is ok to have a struct that doesn't have a copy capability where its field is a struct that has copy capability but not vice versa.
                    check_ability(
                        field_expected_abilities.is_subset(resolver.loader().abilities(&ty)?),
                    )?;
                    ty.check_eq(expected_ty)?;
                }

                self.operand_stack.push_ty(output_ty)?;
            }
            Bytecode::Unpack(idx) => {
                let struct_ty = self.operand_stack.pop_ty()?;
                struct_ty.check_eq(&resolver.get_struct_type(*idx))?;
                let struct_decl = resolver.get_struct_fields(*idx)?;
                for ty in struct_decl.fields.iter() {
                    self.operand_stack.push_ty(ty.clone())?;
                }
            }
            Bytecode::UnpackGeneric(idx) => {
                let struct_ty = self.operand_stack.pop_ty()?;
                struct_ty.check_eq(&resolver.instantiate_generic_type(
                    *idx,
                    &self.function_instantiations[frame.ty_args_idx],
                )?)?;

                let struct_decl = resolver.instantiate_generic_struct_fields(
                    *idx,
                    &self.function_instantiations[frame.ty_args_idx],
                )?;
                for ty in struct_decl.into_iter() {
                    self.operand_stack.push_ty(ty.clone())?;
                }
            }
            Bytecode::ReadRef => {
                let ref_ty = self.operand_stack.pop_ty()?;
                match ref_ty {
                    Type::Reference(inner) | Type::MutableReference(inner) => {
                        check_ability(resolver.loader().abilities(&inner)?.has_copy())?;
                        self.operand_stack.push_ty(inner.as_ref().clone())?;
                    }
                    _ => {
                        return Err(PartialVMError::new(
                            StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR,
                        )
                        .with_message("ReadRef expecting a value of reference type".to_string()))
                    }
                }
            }
            Bytecode::WriteRef => {
                let ref_ty = self.operand_stack.pop_ty()?;
                let val_ty = self.operand_stack.pop_ty()?;
                match ref_ty {
                    Type::MutableReference(inner) => {
                        if *inner == val_ty {
                            check_ability(resolver.loader().abilities(&inner)?.has_drop())?;
                        } else {
                            return Err(PartialVMError::new(
                                StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR,
                            )
                            .with_message(
                                "WriteRef tried to write references of different types".to_string(),
                            ));
                        }
                    }
                    _ => {
                        return Err(PartialVMError::new(
                            StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR,
                        )
                        .with_message(
                            "WriteRef expecting a value of mutable reference type".to_string(),
                        ))
                    }
                }
            }
            Bytecode::CastU8 => {
                self.operand_stack.pop_ty()?;
                self.operand_stack.push_ty(Type::U8)?;
            }
            Bytecode::CastU16 => {
                self.operand_stack.pop_ty()?;
                self.operand_stack.push_ty(Type::U16)?;
            }
            Bytecode::CastU32 => {
                self.operand_stack.pop_ty()?;
                self.operand_stack.push_ty(Type::U32)?;
            }
            Bytecode::CastU64 => {
                self.operand_stack.pop_ty()?;
                self.operand_stack.push_ty(Type::U64)?;
            }
            Bytecode::CastU128 => {
                self.operand_stack.pop_ty()?;
                self.operand_stack.push_ty(Type::U128)?;
            }
            Bytecode::CastU256 => {
                self.operand_stack.pop_ty()?;
                self.operand_stack.push_ty(Type::U256)?;
            }
            Bytecode::Add
            | Bytecode::Sub
            | Bytecode::Mul
            | Bytecode::Mod
            | Bytecode::Div
            | Bytecode::BitOr
            | Bytecode::BitAnd
            | Bytecode::Xor
            | Bytecode::Or
            | Bytecode::And => {
                let lhs = self.operand_stack.pop_ty()?;
                let rhs = self.operand_stack.pop_ty()?;
                lhs.check_eq(&rhs)?;
                self.operand_stack.push_ty(lhs)?;
            }
            Bytecode::Shl | Bytecode::Shr => {
                self.operand_stack.pop_ty()?;
                let rhs = self.operand_stack.pop_ty()?;
                self.operand_stack.push_ty(rhs)?;
            }
            Bytecode::Lt | Bytecode::Le | Bytecode::Gt | Bytecode::Ge => {
                let lhs = self.operand_stack.pop_ty()?;
                let rhs = self.operand_stack.pop_ty()?;
                lhs.check_eq(&rhs)?;
                self.operand_stack.push_ty(Type::Bool)?;
            }
            Bytecode::Eq | Bytecode::Neq => {
                let lhs = self.operand_stack.pop_ty()?;
                let rhs = self.operand_stack.pop_ty()?;
                if lhs != rhs {
                    return Err(
                        PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                            .with_message(
                                "Integer binary operation expecting values of same type"
                                    .to_string(),
                            ),
                    );
                }
                check_ability(resolver.loader().abilities(&lhs)?.has_drop())?;
                self.operand_stack.push_ty(Type::Bool)?;
            }
            Bytecode::MutBorrowGlobal(idx) => {
                self.operand_stack.pop_ty()?.check_eq(&Type::Address)?;
                let ty = resolver.get_struct_type(*idx);
                check_ability(resolver.loader().abilities(&ty)?.has_key())?;
                self.operand_stack
                    .push_ty(Type::MutableReference(Box::new(ty)))?;
            }
            Bytecode::ImmBorrowGlobal(idx) => {
                self.operand_stack.pop_ty()?.check_eq(&Type::Address)?;
                let ty = resolver.get_struct_type(*idx);
                check_ability(resolver.loader().abilities(&ty)?.has_key())?;
                self.operand_stack.push_ty(Type::Reference(Box::new(ty)))?;
            }
            Bytecode::MutBorrowGlobalGeneric(idx) => {
                self.operand_stack.pop_ty()?.check_eq(&Type::Address)?;
                let ty = resolver.instantiate_generic_type(
                    *idx,
                    &self.function_instantiations[frame.ty_args_idx],
                )?;
                check_ability(resolver.loader().abilities(&ty)?.has_key())?;
                self.operand_stack
                    .push_ty(Type::MutableReference(Box::new(ty)))?;
            }
            Bytecode::ImmBorrowGlobalGeneric(idx) => {
                self.operand_stack.pop_ty()?.check_eq(&Type::Address)?;
                let ty = resolver.instantiate_generic_type(
                    *idx,
                    &self.function_instantiations[frame.ty_args_idx],
                )?;
                check_ability(resolver.loader().abilities(&ty)?.has_key())?;
                self.operand_stack.push_ty(Type::Reference(Box::new(ty)))?;
            }
            Bytecode::Exists(_) | Bytecode::ExistsGeneric(_) => {
                self.operand_stack.pop_ty()?.check_eq(&Type::Address)?;
                self.operand_stack.push_ty(Type::Bool)?;
            }
            Bytecode::MoveTo(idx) => {
                let ty = self.operand_stack.pop_ty()?;
                self.operand_stack
                    .pop_ty()?
                    .check_eq(&Type::Reference(Box::new(Type::Signer)))?;
                ty.check_eq(&resolver.get_struct_type(*idx))?;
                check_ability(resolver.loader().abilities(&ty)?.has_key())?;
            }
            Bytecode::MoveToGeneric(idx) => {
                let ty = self.operand_stack.pop_ty()?;
                self.operand_stack
                    .pop_ty()?
                    .check_eq(&Type::Reference(Box::new(Type::Signer)))?;
                ty.check_eq(&resolver.instantiate_generic_type(
                    *idx,
                    &self.function_instantiations[frame.ty_args_idx],
                )?)?;
                check_ability(resolver.loader().abilities(&ty)?.has_key())?;
            }
            Bytecode::MoveFrom(idx) => {
                self.operand_stack.pop_ty()?.check_eq(&Type::Address)?;
                let ty = resolver.get_struct_type(*idx);
                check_ability(resolver.loader().abilities(&ty)?.has_key())?;
                self.operand_stack.push_ty(ty)?;
            }
            Bytecode::MoveFromGeneric(idx) => {
                self.operand_stack.pop_ty()?.check_eq(&Type::Address)?;
                let ty = resolver.instantiate_generic_type(
                    *idx,
                    &self.function_instantiations[frame.ty_args_idx],
                )?;
                check_ability(resolver.loader().abilities(&ty)?.has_key())?;
                self.operand_stack.push_ty(ty)?;
            }
            Bytecode::FreezeRef => {
                match self.operand_stack.pop_ty()? {
                    Type::MutableReference(ty) => {
                        self.operand_stack.push_ty(Type::Reference(ty))?
                    }
                    _ => {
                        return Err(PartialVMError::new(
                            StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR,
                        )
                        .with_message("FreezeRef expects a mutable reference".to_string()))
                    }
                };
            }
            Bytecode::Nop => (),
            Bytecode::Not => {
                self.operand_stack.pop_ty()?.check_eq(&Type::Bool)?;
                self.operand_stack.push_ty(Type::Bool)?;
            }
            Bytecode::VecPack(si, num) => {
                let elem_tys = self.operand_stack.popn_tys(*num as u16)?;
                let ty = resolver.single_type_at(*si);
                let ty = self.get_instantiation(frame, ty)?;
                for elem_ty in elem_tys.iter() {
                    elem_ty.check_eq(ty)?;
                }
                let ty = ty.clone();
                self.operand_stack.push_ty(Type::Vector(Box::new(ty)))?;
            }
            Bytecode::VecLen(si) => {
                let top_ty = self.operand_stack.pop_ty()?;
                let ty = resolver.single_type_at(*si);
                let ty = self.get_instantiation(frame, ty)?;
                top_ty.check_vec_ref(ty, false)?;
                self.operand_stack.push_ty(Type::U64)?;
            }
            Bytecode::VecImmBorrow(si) => {
                self.operand_stack.pop_ty()?.check_eq(&Type::U64)?;
                let top_ty = self.operand_stack.pop_ty()?;
                let ty = resolver.single_type_at(*si);
                let ty = self.get_instantiation(frame, ty)?;
                let inner_ty = top_ty.check_vec_ref(ty, false)?;
                self.operand_stack
                    .push_ty(Type::Reference(Box::new(inner_ty)))?;
            }
            Bytecode::VecMutBorrow(si) => {
                self.operand_stack.pop_ty()?.check_eq(&Type::U64)?;
                let top_ty = self.operand_stack.pop_ty()?;
                let ty = resolver.single_type_at(*si);
                let ty = self.get_instantiation(frame, ty)?;
                let inner_ty = top_ty.check_vec_ref(ty, true)?;
                self.operand_stack
                    .push_ty(Type::MutableReference(Box::new(inner_ty)))?;
            }
            Bytecode::VecPushBack(si) => {
                let elem_ty = self.operand_stack.pop_ty()?;
                let vec_ty = self.operand_stack.pop_ty()?;
                let ty = resolver.single_type_at(*si);
                let ty = self.get_instantiation(frame, ty)?;
                elem_ty.check_eq(ty)?;
                vec_ty.check_vec_ref(ty, true)?;
            }
            Bytecode::VecPopBack(si) => {
                let top_ty = self.operand_stack.pop_ty()?;
                let ty = resolver.single_type_at(*si);
                let ty = self.get_instantiation(frame, ty)?;
                let inner_ty = top_ty.check_vec_ref(ty, true)?;
                self.operand_stack.push_ty(inner_ty)?;
            }
            Bytecode::VecUnpack(si, num) => {
                let vec_ty = self.operand_stack.pop_ty()?;
                let ty = resolver.single_type_at(*si);
                let ty = self.get_instantiation(frame, ty)?;
                match vec_ty {
                    Type::Vector(v) => {
                        v.check_eq(ty)?;
                        for _ in 0..*num {
                            self.operand_stack.push_ty(v.as_ref().clone())?;
                        }
                    }
                    _ => {
                        return Err(PartialVMError::new(
                            StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR,
                        )
                        .with_message("VecUnpack expect a vector type".to_string()))
                    }
                };
            }
            Bytecode::VecSwap(si) => {
                self.operand_stack.pop_ty()?.check_eq(&Type::U64)?;
                self.operand_stack.pop_ty()?.check_eq(&Type::U64)?;
                let top_ty = self.operand_stack.pop_ty()?;
                let ty = resolver.single_type_at(*si);
                let ty = self.get_instantiation(frame, ty)?;
                top_ty.check_vec_ref(ty, true)?;
            }
        }
        Ok(())
    }

    fn execute_code_impl(
        &mut self,
        resolver: &Resolver,
        frame: &mut Frame,
        data_store: &mut impl DataStore,
        gas_meter: &mut impl GasMeter,
    ) -> PartialVMResult<ExitCode> {
        use SimpleInstruction as S;

        macro_rules! make_ty {
            ($ty: expr) => {
                TypeWithLoader {
                    ty: $ty,
                    loader: resolver.loader(),
                }
            };
        }

        // this is disgusting, to fool the borrow checker we make a copy
        // of the Arc and acces code that way.
        // Doing so, allows us to keep the Frame borrow mutablw and operate on that
        let function = Arc::clone(&frame.function);
        let code = function.code();
        loop {
            for instruction in &code[frame.pc as usize..] {
                trace!(
                    &frame.function,
                    &frame.locals,
                    frame.pc,
                    instruction,
                    resolver,
                    self
                );

                fail_point!("move_vm::interpreter_loop", |_| {
                    Err(
                        PartialVMError::new(StatusCode::VERIFIER_INVARIANT_VIOLATION).with_message(
                            "Injected move_vm::interpreter verifier failure".to_owned(),
                        ),
                    )
                });

                // Paranoid Mode: Perform the type stack transition check to make sure all type safety requirements has been met.
                //
                // We will run the checks for only the control flow instructions and StLoc here. The majority of checks will be
                // performed after the instruction execution, i.e: the big match block below.
                //
                // The reason for this design is we charge gas during instruction execution and we want to perform checks only after
                // proper gas has been charged for each instruction.

                if self.paranoid_type_checks {
                    self.operand_stack.check_balance()?;
                    self.pre_execution_type_stack_transition(
                        &frame.local_tys,
                        &frame.locals,
                        resolver,
                        instruction,
                    )?;
                }

                match instruction {
                    Bytecode::Pop => {
                        let popped_val = self.operand_stack.pop()?;
                        gas_meter.charge_pop(popped_val)?;
                    }
                    Bytecode::Ret => {
                        gas_meter.charge_simple_instr(S::Ret)?;
                        return Ok(ExitCode::Return);
                    }
                    Bytecode::BrTrue(offset) => {
                        gas_meter.charge_simple_instr(S::BrTrue)?;
                        if self.operand_stack.pop_as::<bool>()? {
                            frame.pc = *offset;
                            break;
                        }
                    }
                    Bytecode::BrFalse(offset) => {
                        gas_meter.charge_simple_instr(S::BrFalse)?;
                        if !self.operand_stack.pop_as::<bool>()? {
                            frame.pc = *offset;
                            break;
                        }
                    }
                    Bytecode::Branch(offset) => {
                        gas_meter.charge_simple_instr(S::Branch)?;
                        frame.pc = *offset;
                        break;
                    }
                    Bytecode::LdU8(int_const) => {
                        gas_meter.charge_simple_instr(S::LdU8)?;
                        self.operand_stack.push(Value::u8(*int_const))?;
                    }
                    Bytecode::LdU16(int_const) => {
                        gas_meter.charge_simple_instr(S::LdU16)?;
                        self.operand_stack.push(Value::u16(*int_const))?;
                    }
                    Bytecode::LdU32(int_const) => {
                        gas_meter.charge_simple_instr(S::LdU32)?;
                        self.operand_stack.push(Value::u32(*int_const))?;
                    }
                    Bytecode::LdU64(int_const) => {
                        gas_meter.charge_simple_instr(S::LdU64)?;
                        self.operand_stack.push(Value::u64(*int_const))?;
                    }
                    Bytecode::LdU128(int_const) => {
                        gas_meter.charge_simple_instr(S::LdU128)?;
                        self.operand_stack.push(Value::u128(*int_const))?;
                    }
                    Bytecode::LdU256(int_const) => {
                        gas_meter.charge_simple_instr(S::LdU256)?;
                        self.operand_stack.push(Value::u256(*int_const))?;
                    }
                    Bytecode::LdConst(idx) => {
                        let constant = resolver.constant_at(*idx);
                        gas_meter.charge_ld_const(NumBytes::new(constant.data.len() as u64))?;

                        let val = Value::deserialize_constant(constant).ok_or_else(|| {
                            PartialVMError::new(StatusCode::VERIFIER_INVARIANT_VIOLATION)
                                .with_message(
                                    "Verifier failed to verify the deserialization of constants"
                                        .to_owned(),
                                )
                        })?;

                        gas_meter.charge_ld_const_after_deserialization(&val)?;

                        self.operand_stack.push(val)?
                    }
                    Bytecode::LdTrue => {
                        gas_meter.charge_simple_instr(S::LdTrue)?;
                        self.operand_stack.push(Value::bool(true))?;
                    }
                    Bytecode::LdFalse => {
                        gas_meter.charge_simple_instr(S::LdFalse)?;
                        self.operand_stack.push(Value::bool(false))?;
                    }
                    Bytecode::CopyLoc(idx) => {
                        // TODO(Gas): We should charge gas before copying the value.
                        let local = frame.locals.copy_loc(*idx as usize)?;
                        gas_meter.charge_copy_loc(&local)?;
                        self.operand_stack.push(local)?;
                    }
                    Bytecode::MoveLoc(idx) => {
                        let local = frame.locals.move_loc(*idx as usize)?;
                        gas_meter.charge_move_loc(&local)?;

                        self.operand_stack.push(local)?;
                    }
                    Bytecode::StLoc(idx) => {
                        let value_to_store = self.operand_stack.pop()?;
                        gas_meter.charge_store_loc(&value_to_store)?;
                        frame.locals.store_loc(*idx as usize, value_to_store)?;
                    }
                    Bytecode::Call(idx) => {
                        return Ok(ExitCode::Call(*idx));
                    }
                    Bytecode::CallGeneric(idx) => {
                        return Ok(ExitCode::CallGeneric(*idx));
                    }
                    Bytecode::MutBorrowLoc(idx) | Bytecode::ImmBorrowLoc(idx) => {
                        let instr = match instruction {
                            Bytecode::MutBorrowLoc(_) => S::MutBorrowLoc,
                            _ => S::ImmBorrowLoc,
                        };
                        gas_meter.charge_simple_instr(instr)?;
                        self.operand_stack
                            .push(frame.locals.borrow_loc(*idx as usize)?)?;
                    }
                    Bytecode::ImmBorrowField(fh_idx) | Bytecode::MutBorrowField(fh_idx) => {
                        let instr = match instruction {
                            Bytecode::MutBorrowField(_) => S::MutBorrowField,
                            _ => S::ImmBorrowField,
                        };
                        gas_meter.charge_simple_instr(instr)?;

                        let reference = self.operand_stack.pop_as::<StructRef>()?;

                        let offset = resolver.field_offset(*fh_idx);
                        let field_ref = reference.borrow_field(offset)?;
                        self.operand_stack.push(field_ref)?;
                    }
                    Bytecode::ImmBorrowFieldGeneric(fi_idx)
                    | Bytecode::MutBorrowFieldGeneric(fi_idx) => {
                        let instr = match instruction {
                            Bytecode::MutBorrowField(_) => S::MutBorrowFieldGeneric,
                            _ => S::ImmBorrowFieldGeneric,
                        };
                        gas_meter.charge_simple_instr(instr)?;

                        let reference = self.operand_stack.pop_as::<StructRef>()?;

                        let offset = resolver.field_instantiation_offset(*fi_idx);
                        let field_ref = reference.borrow_field(offset)?;
                        self.operand_stack.push(field_ref)?;
                    }
                    Bytecode::Pack(sd_idx) => {
                        let field_count = resolver.field_count(*sd_idx);
                        gas_meter
                            .charge_pack(false, self.operand_stack.last_n(field_count as usize)?)?;
                        let args = self.operand_stack.popn(field_count)?;
                        self.operand_stack
                            .push(Value::struct_(Struct::pack(args)))?;
                    }
                    Bytecode::PackGeneric(si_idx) => {
                        let field_count = resolver.field_instantiation_count(*si_idx);
                        gas_meter
                            .charge_pack(true, self.operand_stack.last_n(field_count as usize)?)?;
                        let args = self.operand_stack.popn(field_count)?;
                        self.operand_stack
                            .push(Value::struct_(Struct::pack(args)))?;
                    }
                    Bytecode::Unpack(_sd_idx) => {
                        let struct_ = self.operand_stack.pop_as::<Struct>()?;

                        gas_meter.charge_unpack(false, struct_.field_views())?;

                        for value in struct_.unpack()? {
                            self.operand_stack.push(value)?;
                        }
                    }
                    Bytecode::UnpackGeneric(_si_idx) => {
                        let struct_ = self.operand_stack.pop_as::<Struct>()?;

                        gas_meter.charge_unpack(true, struct_.field_views())?;

                        // TODO: Whether or not we want this gas metering in the loop is
                        // questionable.  However, if we don't have it in the loop we could wind up
                        // doing a fair bit of work before charging for it.
                        for value in struct_.unpack()? {
                            self.operand_stack.push(value)?;
                        }
                    }
                    Bytecode::ReadRef => {
                        let reference = self.operand_stack.pop_as::<Reference>()?;
                        gas_meter.charge_read_ref(reference.value_view())?;
                        let value = reference.read_ref()?;
                        self.operand_stack.push(value)?;
                    }
                    Bytecode::WriteRef => {
                        let reference = self.operand_stack.pop_as::<Reference>()?;
                        let value = self.operand_stack.pop()?;
                        gas_meter.charge_write_ref(&value, reference.value_view())?;
                        reference.write_ref(value)?;
                    }
                    Bytecode::CastU8 => {
                        gas_meter.charge_simple_instr(S::CastU8)?;
                        let integer_value = self.operand_stack.pop_as::<IntegerValue>()?;
                        self.operand_stack
                            .push(Value::u8(integer_value.cast_u8()?))?;
                    }
                    Bytecode::CastU16 => {
                        gas_meter.charge_simple_instr(S::CastU16)?;
                        let integer_value = self.operand_stack.pop_as::<IntegerValue>()?;
                        self.operand_stack
                            .push(Value::u16(integer_value.cast_u16()?))?;
                    }
                    Bytecode::CastU32 => {
                        gas_meter.charge_simple_instr(S::CastU16)?;
                        let integer_value = self.operand_stack.pop_as::<IntegerValue>()?;
                        self.operand_stack
                            .push(Value::u32(integer_value.cast_u32()?))?;
                    }
                    Bytecode::CastU64 => {
                        gas_meter.charge_simple_instr(S::CastU64)?;
                        let integer_value = self.operand_stack.pop_as::<IntegerValue>()?;
                        self.operand_stack
                            .push(Value::u64(integer_value.cast_u64()?))?;
                    }
                    Bytecode::CastU128 => {
                        gas_meter.charge_simple_instr(S::CastU128)?;
                        let integer_value = self.operand_stack.pop_as::<IntegerValue>()?;
                        self.operand_stack
                            .push(Value::u128(integer_value.cast_u128()?))?;
                    }
                    Bytecode::CastU256 => {
                        gas_meter.charge_simple_instr(S::CastU16)?;
                        let integer_value = self.operand_stack.pop_as::<IntegerValue>()?;
                        self.operand_stack
                            .push(Value::u256(integer_value.cast_u256()?))?;
                    }
                    // Arithmetic Operations
                    Bytecode::Add => {
                        gas_meter.charge_simple_instr(S::Add)?;
                        self.binop_int(IntegerValue::add_checked)?
                    }
                    Bytecode::Sub => {
                        gas_meter.charge_simple_instr(S::Sub)?;
                        self.binop_int(IntegerValue::sub_checked)?
                    }
                    Bytecode::Mul => {
                        gas_meter.charge_simple_instr(S::Mul)?;
                        self.binop_int(IntegerValue::mul_checked)?
                    }
                    Bytecode::Mod => {
                        gas_meter.charge_simple_instr(S::Mod)?;
                        self.binop_int(IntegerValue::rem_checked)?
                    }
                    Bytecode::Div => {
                        gas_meter.charge_simple_instr(S::Div)?;
                        self.binop_int(IntegerValue::div_checked)?
                    }
                    Bytecode::BitOr => {
                        gas_meter.charge_simple_instr(S::BitOr)?;
                        self.binop_int(IntegerValue::bit_or)?
                    }
                    Bytecode::BitAnd => {
                        gas_meter.charge_simple_instr(S::BitAnd)?;
                        self.binop_int(IntegerValue::bit_and)?
                    }
                    Bytecode::Xor => {
                        gas_meter.charge_simple_instr(S::Xor)?;
                        self.binop_int(IntegerValue::bit_xor)?
                    }
                    Bytecode::Shl => {
                        gas_meter.charge_simple_instr(S::Shl)?;
                        let rhs = self.operand_stack.pop_as::<u8>()?;
                        let lhs = self.operand_stack.pop_as::<IntegerValue>()?;
                        self.operand_stack
                            .push(lhs.shl_checked(rhs)?.into_value())?;
                    }
                    Bytecode::Shr => {
                        gas_meter.charge_simple_instr(S::Shr)?;
                        let rhs = self.operand_stack.pop_as::<u8>()?;
                        let lhs = self.operand_stack.pop_as::<IntegerValue>()?;
                        self.operand_stack
                            .push(lhs.shr_checked(rhs)?.into_value())?;
                    }
                    Bytecode::Or => {
                        gas_meter.charge_simple_instr(S::Or)?;
                        self.binop_bool(|l, r| Ok(l || r))?
                    }
                    Bytecode::And => {
                        gas_meter.charge_simple_instr(S::And)?;
                        self.binop_bool(|l, r| Ok(l && r))?
                    }
                    Bytecode::Lt => {
                        gas_meter.charge_simple_instr(S::Lt)?;
                        self.binop_bool(IntegerValue::lt)?
                    }
                    Bytecode::Gt => {
                        gas_meter.charge_simple_instr(S::Gt)?;
                        self.binop_bool(IntegerValue::gt)?
                    }
                    Bytecode::Le => {
                        gas_meter.charge_simple_instr(S::Le)?;
                        self.binop_bool(IntegerValue::le)?
                    }
                    Bytecode::Ge => {
                        gas_meter.charge_simple_instr(S::Ge)?;
                        self.binop_bool(IntegerValue::ge)?
                    }
                    Bytecode::Abort => {
                        gas_meter.charge_simple_instr(S::Abort)?;
                        let error_code = self.operand_stack.pop_as::<u64>()?;
                        let error = PartialVMError::new(StatusCode::ABORTED)
                            .with_sub_status(error_code)
                            .with_message(format!(
                                "{} at offset {}",
                                frame.function.pretty_string(),
                                frame.pc,
                            ));
                        return Err(error);
                    }
                    Bytecode::Eq => {
                        let lhs = self.operand_stack.pop()?;
                        let rhs = self.operand_stack.pop()?;
                        gas_meter.charge_eq(&lhs, &rhs)?;
                        self.operand_stack.push(Value::bool(lhs.equals(&rhs)?))?;
                    }
                    Bytecode::Neq => {
                        let lhs = self.operand_stack.pop()?;
                        let rhs = self.operand_stack.pop()?;
                        gas_meter.charge_neq(&lhs, &rhs)?;
                        self.operand_stack.push(Value::bool(!lhs.equals(&rhs)?))?;
                    }
                    Bytecode::MutBorrowGlobal(sd_idx) | Bytecode::ImmBorrowGlobal(sd_idx) => {
                        let is_mut = matches!(instruction, Bytecode::MutBorrowGlobal(_));
                        let addr = self.operand_stack.pop_as::<AccountAddress>()?;
                        let ty = resolver.get_struct_type(*sd_idx);
                        let value = Self::borrow_global(
                            is_mut,
                            false,
                            resolver.loader(),
                            gas_meter,
                            data_store,
                            addr,
                            &ty,
                        )?;
                        self.operand_stack.push(value)?;
                    }
                    Bytecode::MutBorrowGlobalGeneric(si_idx)
                    | Bytecode::ImmBorrowGlobalGeneric(si_idx) => {
                        let is_mut = matches!(instruction, Bytecode::MutBorrowGlobalGeneric(_));
                        let addr = self.operand_stack.pop_as::<AccountAddress>()?;
                        let ty = self.instantiate_generic_type(resolver, frame, si_idx)?;
                        let value = Self::borrow_global(
                            is_mut,
                            true,
                            resolver.loader(),
                            gas_meter,
                            data_store,
                            addr,
                            ty,
                        )?;
                        self.operand_stack.push(value)?;
                    }
                    Bytecode::Exists(sd_idx) => {
                        let addr = self.operand_stack.pop_as::<AccountAddress>()?;
                        let ty = resolver.get_struct_type(*sd_idx);
                        let exists = Self::exists(
                            false,
                            resolver.loader(),
                            gas_meter,
                            data_store,
                            addr,
                            &ty,
                        )?;
                        self.operand_stack.push(Value::bool(exists))?;
                    }
                    Bytecode::ExistsGeneric(si_idx) => {
                        let addr = self.operand_stack.pop_as::<AccountAddress>()?;
                        let ty = self.instantiate_generic_type(resolver, frame, si_idx)?;
                        let exists =
                            Self::exists(true, resolver.loader(), gas_meter, data_store, addr, ty)?;
                        self.operand_stack.push(Value::bool(exists))?;
                    }
                    Bytecode::MoveFrom(sd_idx) => {
                        let addr = self.operand_stack.pop_as::<AccountAddress>()?;
                        let ty = resolver.get_struct_type(*sd_idx);
                        let resource = Self::move_from(
                            false,
                            resolver.loader(),
                            gas_meter,
                            data_store,
                            addr,
                            &ty,
                        )?;
                        self.operand_stack.push(resource)?;
                    }
                    Bytecode::MoveFromGeneric(si_idx) => {
                        let addr = self.operand_stack.pop_as::<AccountAddress>()?;
                        let ty = self.instantiate_generic_type(resolver, frame, si_idx)?;
                        let resource = Self::move_from(
                            true,
                            resolver.loader(),
                            gas_meter,
                            data_store,
                            addr,
                            ty,
                        )?;
                        self.operand_stack.push(resource)?;
                    }
                    Bytecode::MoveTo(sd_idx) => {
                        let resource = self.operand_stack.pop()?;
                        let signer_reference = self.operand_stack.pop_as::<StructRef>()?;
                        let addr = signer_reference
                            .borrow_field(0)?
                            .value_as::<Reference>()?
                            .read_ref()?
                            .value_as::<AccountAddress>()?;
                        let ty = resolver.get_struct_type(*sd_idx);
                        // REVIEW: Can we simplify Interpreter::move_to?
                        Self::move_to(
                            false,
                            resolver.loader(),
                            gas_meter,
                            data_store,
                            addr,
                            &ty,
                            resource,
                        )?;
                    }
                    Bytecode::MoveToGeneric(si_idx) => {
                        let resource = self.operand_stack.pop()?;
                        let signer_reference = self.operand_stack.pop_as::<StructRef>()?;
                        let addr = signer_reference
                            .borrow_field(0)?
                            .value_as::<Reference>()?
                            .read_ref()?
                            .value_as::<AccountAddress>()?;
                        let ty = self.instantiate_generic_type(resolver, frame, si_idx)?;
                        Self::move_to(
                            true,
                            resolver.loader(),
                            gas_meter,
                            data_store,
                            addr,
                            ty,
                            resource,
                        )?;
                    }
                    Bytecode::FreezeRef => {
                        gas_meter.charge_simple_instr(S::FreezeRef)?;
                        // FreezeRef should just be a null op as we don't distinguish between mut
                        // and immut ref at runtime.
                    }
                    Bytecode::Not => {
                        gas_meter.charge_simple_instr(S::Not)?;
                        let value = !self.operand_stack.pop_as::<bool>()?;
                        self.operand_stack.push(Value::bool(value))?;
                    }
                    Bytecode::Nop => {
                        gas_meter.charge_simple_instr(S::Nop)?;
                    }
                    Bytecode::VecPack(si, num) => {
                        let elements = self.operand_stack.popn(*num as u16)?;
                        let ty = resolver.single_type_at(*si);
                        let ty = self.get_instantiation(frame, ty)?;
                        gas_meter.charge_vec_pack(make_ty!(ty), elements.iter())?;
                        let value = Vector::pack(ty, elements)?;
                        self.operand_stack.push(value)?;
                    }
                    Bytecode::VecLen(si) => {
                        let vec_ref = self.operand_stack.pop_as::<VectorRef>()?;
                        let ty = resolver.single_type_at(*si);
                        let ty = self.get_instantiation(frame, ty)?;
                        gas_meter.charge_vec_len(TypeWithLoader {
                            ty,
                            loader: resolver.loader(),
                        })?;
                        let value = vec_ref.len(ty)?;
                        self.operand_stack.push(value)?;
                    }
                    Bytecode::VecImmBorrow(si) => {
                        let idx = self.operand_stack.pop_as::<u64>()? as usize;
                        let vec_ref = self.operand_stack.pop_as::<VectorRef>()?;
                        let ty = resolver.single_type_at(*si);
                        let ty = self.get_instantiation(frame, ty)?;
                        let res = vec_ref.borrow_elem(idx, ty);
                        gas_meter.charge_vec_borrow(false, make_ty!(ty), res.is_ok())?;
                        self.operand_stack.push(res?)?;
                    }
                    Bytecode::VecMutBorrow(si) => {
                        let idx = self.operand_stack.pop_as::<u64>()? as usize;
                        let vec_ref = self.operand_stack.pop_as::<VectorRef>()?;
                        let ty = resolver.single_type_at(*si);
                        let ty = self.get_instantiation(frame, ty)?;
                        let res = vec_ref.borrow_elem(idx, ty);
                        gas_meter.charge_vec_borrow(true, make_ty!(ty), res.is_ok())?;
                        self.operand_stack.push(res?)?;
                    }
                    Bytecode::VecPushBack(si) => {
                        let max_len = self.runtime_limits_config.vector_len_max;
                        let elem = self.operand_stack.pop()?;
                        let vec_ref = self.operand_stack.pop_as::<VectorRef>()?;
                        let ty = resolver.single_type_at(*si);
                        let ty = self.get_instantiation(frame, ty)?;
                        gas_meter.charge_vec_push_back(make_ty!(ty), &elem)?;
                        vec_ref.push_back(elem, ty, max_len)?;
                    }
                    Bytecode::VecPopBack(si) => {
                        let vec_ref = self.operand_stack.pop_as::<VectorRef>()?;
                        let ty = resolver.single_type_at(*si);
                        let ty = self.get_instantiation(frame, ty)?;
                        let res = vec_ref.pop(ty);
                        gas_meter.charge_vec_pop_back(make_ty!(ty), res.as_ref().ok())?;
                        self.operand_stack.push(res?)?;
                    }
                    Bytecode::VecUnpack(si, num) => {
                        let vec_val = self.operand_stack.pop_as::<Vector>()?;
                        let ty = resolver.single_type_at(*si);
                        let ty = self.get_instantiation(frame, ty)?;
                        gas_meter.charge_vec_unpack(
                            make_ty!(ty),
                            NumArgs::new(*num),
                            vec_val.elem_views(),
                        )?;
                        let elements = vec_val.unpack(ty, *num)?;
                        for value in elements {
                            self.operand_stack.push(value)?;
                        }
                    }
                    Bytecode::VecSwap(si) => {
                        let idx2 = self.operand_stack.pop_as::<u64>()? as usize;
                        let idx1 = self.operand_stack.pop_as::<u64>()? as usize;
                        let vec_ref = self.operand_stack.pop_as::<VectorRef>()?;
                        let ty = resolver.single_type_at(*si);
                        let ty = self.get_instantiation(frame, ty)?;
                        gas_meter.charge_vec_swap(make_ty!(ty))?;
                        vec_ref.swap(idx1, idx2, ty)?;
                    }
                }
                if self.paranoid_type_checks {
                    self.post_execution_type_stack_transition(frame, resolver, instruction)?;

                    self.operand_stack.check_balance()?;
                }

                // invariant: advance to pc +1 is iff instruction at pc executed without aborting
                frame.pc += 1;
            }
            // ok we are out, it's a branch, check the pc for good luck
            // TODO: re-work the logic here. Tests should have a more
            // natural way to plug in
            if frame.pc as usize >= code.len() {
                if cfg!(test) {
                    // In order to test the behavior of an instruction stream, hitting end of the
                    // code should report no error so that we can check the
                    // locals.
                    return Ok(ExitCode::Return);
                } else {
                    return Err(PartialVMError::new(StatusCode::PC_OVERFLOW));
                }
            }
        }
    }

    fn get_instantiation<'a: 'b, 'b>(
        &'a mut self,
        frame: &mut Frame,
        ty: &'b Type,
    ) -> PartialVMResult<&'b Type> {
        if self.function_instantiations[frame.ty_args_idx].is_empty() {
            return Ok(ty);
        }
        if let Some(idx) = frame.instantiations.get(&frame.pc) {
            Ok(self.instantiations.get(*idx).unwrap())
        } else {
            if let Some(inst_map) = self.instantiation_cache.get(ty) {
                if let Some(idx) = inst_map.get(&self.function_instantiations[frame.ty_args_idx]) {
                    frame.instantiations.insert(frame.pc, *idx);
                    return Ok(self.instantiations.get(*idx).unwrap());
                }
            }
            let type_ = ty.subst(&self.function_instantiations[frame.ty_args_idx])?;
            self.instantiations.push(type_);
            let idx = self.instantiations.len() - 1;
            let inst_map = self
                .instantiation_cache
                .entry(ty.clone())
                .or_insert_with(HashMap::new);
            inst_map.insert(self.function_instantiations[frame.ty_args_idx].clone(), idx);
            frame.instantiations.insert(frame.pc, idx);
            Ok(self.instantiations.get(idx).unwrap())
        }
    }

    fn instantiate_generic_type<'a>(
        &'a mut self,
        resolver: &Resolver,
        frame: &mut Frame,
        sdi_idx: &StructDefInstantiationIndex,
    ) -> PartialVMResult<&'a Type> {
        if let Some(idx) = frame.type_instantiations.get(&frame.pc) {
            Ok(self.type_instantiations.get(*idx).unwrap())
        } else {
            let sdi_key = (resolver.get_scope(), *sdi_idx);
            if let Some(inst_map) = self.type_instantiation_cache.get(&sdi_key) {
                if let Some(idx) = inst_map.get(&self.function_instantiations[frame.ty_args_idx]) {
                    frame.type_instantiations.insert(frame.pc, *idx);
                    return Ok(self.type_instantiations.get(*idx).unwrap());
                }
            }
            let type_ = resolver.instantiate_generic_type(
                *sdi_idx,
                &self.function_instantiations[frame.ty_args_idx],
            )?;
            self.type_instantiations.push(type_);
            let idx = self.type_instantiations.len() - 1;
            let inst_map = self
                .type_instantiation_cache
                .entry(sdi_key)
                .or_insert_with(HashMap::new);
            inst_map.insert(self.function_instantiations[frame.ty_args_idx].clone(), idx);
            frame.type_instantiations.insert(frame.pc, idx);
            Ok(self.type_instantiations.get(idx).unwrap())
        }
    }

    fn function_instantiation<'a>(
        &'a mut self,
        resolver: &Resolver,
        frame: &mut Frame,
        fdi_idx: FunctionInstantiationIndex,
    ) -> PartialVMResult<usize> {
        if let Some(idx) = frame.function_instantiations.get(&frame.pc) {
            Ok(*idx)
        } else {
            let fdi_key = (resolver.get_scope(), fdi_idx);
            if let Some(inst_map) = self.function_instantiation_cache.get(&fdi_key) {
                if let Some(idx) = inst_map.get(&self.function_instantiations[frame.ty_args_idx]) {
                    frame.function_instantiations.insert(frame.pc, *idx);
                    return Ok(*idx);
                }
            }
            let mut instantiation = vec![];
            for ty in resolver.function_instantiation(fdi_idx) {
                let type_ = ty.subst(&self.function_instantiations[frame.ty_args_idx])?;
                instantiation.push(type_);
            }
            self.function_instantiations.push(instantiation);
            let idx = self.function_instantiations.len() - 1;
            let inst_map = self
                .function_instantiation_cache
                .entry(fdi_key)
                .or_insert_with(HashMap::new);
            inst_map.insert(self.function_instantiations[frame.ty_args_idx].clone(), idx);
            frame.function_instantiations.insert(frame.pc, idx);
            Ok(idx)
        }
    }
}

impl Frame {
    fn resolver<'a>(&self, loader: &'a Loader) -> Resolver<'a> {
        self.function.get_resolver(loader)
    }

    fn location(&self) -> Location {
        match self.function.module_id() {
            None => Location::Script,
            Some(id) => Location::Module(id.clone()),
        }
    }
}
