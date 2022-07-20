// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    data_cache::TransactionDataCache,
    interpreter::Interpreter,
    loader::{Function, Loader},
    native_extensions::NativeContextExtensions,
    native_functions::{NativeFunction, NativeFunctions},
    session::{LoadedFunctionInstantiation, SerializedReturnValues, Session},
};
use move_binary_format::{
    errors::{Location, PartialVMError, PartialVMResult, VMResult},
    file_format::LocalIndex,
};
use move_bytecode_verifier::script_signature;
use move_core_types::{
    account_address::AccountAddress,
    identifier::{IdentStr, Identifier},
    language_storage::{ModuleId, TypeTag},
    resolver::MoveResolver,
    value::MoveTypeLayout,
    vm_status::StatusCode,
};
use move_vm_types::{
    data_store::DataStore,
    gas_schedule::GasStatus,
    loaded_data::runtime_types::Type,
    values::{Locals, Reference, VMValueCast, Value},
};
use std::{borrow::Borrow, sync::Arc};
use tracing::warn;

/// An instantiation of the MoveVM.
pub(crate) struct VMRuntime {
    loader: Loader,
}

impl VMRuntime {
    pub(crate) fn new(
        natives: impl IntoIterator<Item = (AccountAddress, Identifier, Identifier, NativeFunction)>,
    ) -> PartialVMResult<Self> {
        Ok(VMRuntime {
            loader: Loader::new(NativeFunctions::new(natives)?),
        })
    }

    pub fn new_session<'r, S: MoveResolver>(&self, remote: &'r S) -> Session<'r, '_, S> {
        Session {
            runtime: self,
            data_cache: TransactionDataCache::new(remote, &self.loader),
            native_extensions: NativeContextExtensions::default(),
        }
    }

    pub fn new_session_with_extensions<'r, S: MoveResolver>(
        &self,
        remote: &'r S,
        native_extensions: NativeContextExtensions<'r>,
    ) -> Session<'r, '_, S> {
        Session {
            runtime: self,
            data_cache: TransactionDataCache::new(remote, &self.loader),
            native_extensions,
        }
    }

    pub(crate) fn publish_module_bundle(
        &self,
        modules: Vec<Vec<u8>>,
        sender: AccountAddress,
        data_store: &mut impl DataStore,
        _gas_status: &mut GasStatus,
    ) -> VMResult<()> {
        let compiled_modules = crate::module_validator::validate_and_compile_modules(
            &modules,
            sender,
            data_store,
            &self.loader,
        )?;

        // All modules verified, publish them to data cache
        for (module, blob) in compiled_modules.into_iter().zip(modules.into_iter()) {
            data_store.publish_module(&module.self_id(), blob)?;
        }
        Ok(())
    }

    fn deserialize_value(&self, ty: &Type, arg: impl Borrow<[u8]>) -> PartialVMResult<Value> {
        let layout = match self.loader.type_to_type_layout(ty) {
            Ok(layout) => layout,
            Err(_err) => {
                warn!("[VM] failed to get layout from type");
                return Err(PartialVMError::new(
                    StatusCode::INVALID_PARAM_TYPE_FOR_DESERIALIZATION,
                ));
            }
        };

        match Value::simple_deserialize(arg.borrow(), &layout) {
            Some(val) => Ok(val),
            None => {
                warn!("[VM] failed to deserialize argument");
                Err(PartialVMError::new(
                    StatusCode::FAILED_TO_DESERIALIZE_ARGUMENT,
                ))
            }
        }
    }

    fn deserialize_args(
        &self,
        arg_tys: Vec<Type>,
        serialized_args: Vec<impl Borrow<[u8]>>,
    ) -> PartialVMResult<(Locals, Vec<Value>)> {
        if arg_tys.len() != serialized_args.len() {
            return Err(
                PartialVMError::new(StatusCode::NUMBER_OF_ARGUMENTS_MISMATCH).with_message(
                    format!(
                        "argument length mismatch: expected {} got {}",
                        arg_tys.len(),
                        serialized_args.len()
                    ),
                ),
            );
        }

        // Create a list of dummy locals. Each value stored will be used be borrowed and passed
        // by reference to the invoked function
        let mut dummy_locals = Locals::new(arg_tys.len());
        // Arguments for the invoked function. These can be owned values or references
        let deserialized_args = arg_tys
            .into_iter()
            .zip(serialized_args)
            .enumerate()
            .map(|(idx, (arg_ty, arg_bytes))| match &arg_ty {
                Type::MutableReference(inner_t) | Type::Reference(inner_t) => {
                    dummy_locals.store_loc(idx, self.deserialize_value(inner_t, arg_bytes)?)?;
                    dummy_locals.borrow_loc(idx)
                }
                _ => self.deserialize_value(&arg_ty, arg_bytes),
            })
            .collect::<PartialVMResult<Vec<_>>>()?;
        Ok((dummy_locals, deserialized_args))
    }

    fn serialize_return_value(
        &self,
        ty: &Type,
        value: Value,
    ) -> PartialVMResult<(Vec<u8>, MoveTypeLayout)> {
        let (ty, value) = match ty {
            Type::Reference(inner) | Type::MutableReference(inner) => {
                let ref_value: Reference = value.cast().map_err(|_err| {
                    PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR).with_message(
                        "non reference value given for a reference typed return value".to_string(),
                    )
                })?;
                let inner_value = ref_value.read_ref()?;
                (&**inner, inner_value)
            }
            _ => (ty, value),
        };

        let layout = self.loader.type_to_type_layout(ty).map_err(|_err| {
            PartialVMError::new(StatusCode::VERIFICATION_ERROR).with_message(
                "entry point functions cannot have non-serializable return types".to_string(),
            )
        })?;
        let bytes = value.simple_serialize(&layout).ok_or_else(|| {
            PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                .with_message("failed to serialize return values".to_string())
        })?;
        Ok((bytes, layout))
    }

    fn serialize_return_values(
        &self,
        return_types: &[Type],
        return_values: Vec<Value>,
    ) -> PartialVMResult<Vec<(Vec<u8>, MoveTypeLayout)>> {
        if return_types.len() != return_values.len() {
            return Err(
                PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR).with_message(
                    format!(
                        "declared {} return types, but got {} return values",
                        return_types.len(),
                        return_values.len()
                    ),
                ),
            );
        }

        return_types
            .iter()
            .zip(return_values)
            .map(|(ty, value)| self.serialize_return_value(ty, value))
            .collect()
    }

    fn execute_function_impl(
        &self,
        func: Arc<Function>,
        ty_args: Vec<Type>,
        param_types: Vec<Type>,
        return_types: Vec<Type>,
        serialized_args: Vec<impl Borrow<[u8]>>,
        data_store: &mut impl DataStore,
        gas_status: &mut GasStatus,
        extensions: &mut NativeContextExtensions,
    ) -> VMResult<SerializedReturnValues> {
        let arg_types = param_types
            .into_iter()
            .map(|ty| ty.subst(&ty_args))
            .collect::<PartialVMResult<Vec<_>>>()
            .map_err(|err| err.finish(Location::Undefined))?;
        let mut_ref_args = arg_types
            .iter()
            .enumerate()
            .filter_map(|(idx, ty)| match ty {
                Type::MutableReference(inner) => Some((idx, inner.clone())),
                _ => None,
            })
            .collect::<Vec<_>>();
        let (mut dummy_locals, deserialized_args) = self
            .deserialize_args(arg_types, serialized_args)
            .map_err(|e| e.finish(Location::Undefined))?;
        let return_types = return_types
            .into_iter()
            .map(|ty| ty.subst(&ty_args))
            .collect::<PartialVMResult<Vec<_>>>()
            .map_err(|err| err.finish(Location::Undefined))?;

        let return_values = Interpreter::entrypoint(
            func,
            ty_args,
            deserialized_args,
            data_store,
            gas_status,
            extensions,
            &self.loader,
        )?;

        let serialized_return_values = self
            .serialize_return_values(&return_types, return_values)
            .map_err(|e| e.finish(Location::Undefined))?;
        let serialized_mut_ref_outputs = mut_ref_args
            .into_iter()
            .map(|(idx, ty)| {
                // serialize return values first in the case that a value points into this local
                let local_val = dummy_locals.move_loc(idx)?;
                let (bytes, layout) = self.serialize_return_value(&ty, local_val)?;
                Ok((idx as LocalIndex, bytes, layout))
            })
            .collect::<PartialVMResult<_>>()
            .map_err(|e| e.finish(Location::Undefined))?;

        // locals should not be dropped until all return values are serialized
        std::mem::drop(dummy_locals);

        Ok(SerializedReturnValues {
            mutable_reference_outputs: serialized_mut_ref_outputs,
            return_values: serialized_return_values,
        })
    }

    pub(crate) fn execute_function(
        &self,
        module: &ModuleId,
        function_name: &IdentStr,
        ty_args: Vec<TypeTag>,
        serialized_args: Vec<impl Borrow<[u8]>>,
        data_store: &mut impl DataStore,
        gas_status: &mut GasStatus,
        extensions: &mut NativeContextExtensions,
        bypass_declared_entry_check: bool,
    ) -> VMResult<SerializedReturnValues> {
        use move_binary_format::{binary_views::BinaryIndexedView, file_format::SignatureIndex};
        fn check_is_entry(
            _resolver: &BinaryIndexedView,
            is_entry: bool,
            _parameters_idx: SignatureIndex,
            _return_idx: Option<SignatureIndex>,
        ) -> PartialVMResult<()> {
            if is_entry {
                Ok(())
            } else {
                Err(PartialVMError::new(
                    StatusCode::EXECUTE_ENTRY_FUNCTION_CALLED_ON_NON_ENTRY_FUNCTION,
                ))
            }
        }

        let additional_signature_checks = if bypass_declared_entry_check {
            move_bytecode_verifier::no_additional_script_signature_checks
        } else {
            check_is_entry
        };
        // load the function
        let (
            module,
            func,
            LoadedFunctionInstantiation {
                type_arguments,
                parameters,
                return_,
            },
        ) = self
            .loader
            .load_function(module, function_name, &ty_args, data_store)?;

        script_signature::verify_module_function_signature_by_name(
            module.module(),
            function_name,
            additional_signature_checks,
        )?;

        // execute the function
        self.execute_function_impl(
            func,
            type_arguments,
            parameters,
            return_,
            serialized_args,
            data_store,
            gas_status,
            extensions,
        )
    }

    // See Session::execute_script for what contracts to follow.
    pub(crate) fn execute_script(
        &self,
        script: impl Borrow<[u8]>,
        ty_args: Vec<TypeTag>,
        serialized_args: Vec<impl Borrow<[u8]>>,
        data_store: &mut impl DataStore,
        gas_status: &mut GasStatus,
        extensions: &mut NativeContextExtensions,
    ) -> VMResult<SerializedReturnValues> {
        // load the script, perform verification
        let (
            func,
            LoadedFunctionInstantiation {
                type_arguments,
                parameters,
                return_,
            },
        ) = self
            .loader
            .load_script(script.borrow(), &ty_args, data_store)?;
        // execute the function
        self.execute_function_impl(
            func,
            type_arguments,
            parameters,
            return_,
            serialized_args,
            data_store,
            gas_status,
            extensions,
        )
    }

    pub(crate) fn loader(&self) -> &Loader {
        &self.loader
    }
}
