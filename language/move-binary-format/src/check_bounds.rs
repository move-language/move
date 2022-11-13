// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    binary_views::BinaryIndexedView,
    errors::{
        bounds_error, offset_out_of_bounds as offset_out_of_bounds_error, verification_error,
        PartialVMError, PartialVMResult,
    },
    file_format::{
        AbilitySet, Bytecode, CodeOffset, CodeUnit, CompiledModule, CompiledScript, Constant,
        FieldHandle, FieldInstantiation, FunctionDefinition, FunctionDefinitionIndex,
        FunctionHandle, FunctionInstantiation, LocalIndex, ModuleHandle, Signature, SignatureToken,
        StructDefInstantiation, StructDefinition, StructFieldInformation, StructHandle, TableIndex,
    },
    internals::ModuleIndex,
    IndexKind,
};
use move_core_types::vm_status::StatusCode;

enum BoundsCheckingContext {
    Module,
    ModuleFunction(FunctionDefinitionIndex),
    Script,
}
pub struct BoundsChecker<'a> {
    view: BinaryIndexedView<'a>,
    context: BoundsCheckingContext,
}

impl<'a> BoundsChecker<'a> {
    pub fn verify_script(script: &'a CompiledScript) -> PartialVMResult<()> {
        let mut bounds_check = Self {
            view: BinaryIndexedView::Script(script),
            context: BoundsCheckingContext::Script,
        };
        bounds_check.verify_impl()?;

        let type_param_count = script.type_parameters.len();

        check_bounds_impl(bounds_check.view.signatures(), script.parameters)?;
        if let Some(sig) = bounds_check
            .view
            .signatures()
            .get(script.parameters.into_index())
        {
            for ty in &sig.0 {
                bounds_check.check_type_parameter(ty, type_param_count)?
            }
        }

        // The bounds checker has already checked each function definition's code, but a
        // script's code exists outside of any function definition. It gets checked here.
        bounds_check.check_code(
            &script.code,
            &script.type_parameters,
            bounds_check
                .view
                .signatures()
                .get(script.parameters.into_index())
                .unwrap(),
            CompiledScript::MAIN_INDEX.into_index(),
        )
    }

    pub fn verify_module(module: &'a CompiledModule) -> PartialVMResult<()> {
        let mut bounds_check = Self {
            view: BinaryIndexedView::Module(module),
            context: BoundsCheckingContext::Module,
        };
        if bounds_check.view.module_handles().is_empty() {
            let status =
                verification_error(StatusCode::NO_MODULE_HANDLES, IndexKind::ModuleHandle, 0);
            return Err(status);
        }
        bounds_check.verify_impl()
    }

    fn verify_impl(&mut self) -> PartialVMResult<()> {
        self.check_signatures()?;
        self.check_constants()?;
        self.check_module_handles()?;
        self.check_self_module_handle()?;
        self.check_struct_handles()?;
        self.check_function_handles()?;
        self.check_field_handles()?;
        self.check_friend_decls()?;
        self.check_struct_instantiations()?;
        self.check_function_instantiations()?;
        self.check_field_instantiations()?;
        self.check_struct_defs()?;
        self.check_function_defs()
    }

    fn check_signatures(&self) -> PartialVMResult<()> {
        for signature in self.view.signatures() {
            self.check_signature(signature)?
        }
        Ok(())
    }

    fn check_constants(&self) -> PartialVMResult<()> {
        for constant in self.view.constant_pool() {
            self.check_constant(constant)?
        }
        Ok(())
    }

    fn check_module_handles(&self) -> PartialVMResult<()> {
        for script_handle in self.view.module_handles() {
            self.check_module_handle(script_handle)?
        }
        Ok(())
    }

    fn check_struct_handles(&self) -> PartialVMResult<()> {
        for struct_handle in self.view.struct_handles() {
            self.check_struct_handle(struct_handle)?
        }
        Ok(())
    }

    fn check_function_handles(&self) -> PartialVMResult<()> {
        for function_handle in self.view.function_handles() {
            self.check_function_handle(function_handle)?
        }
        Ok(())
    }

    fn check_field_handles(&self) -> PartialVMResult<()> {
        for field_handle in self.view.field_handles().into_iter().flatten() {
            self.check_field_handle(field_handle)?
        }
        Ok(())
    }

    fn check_friend_decls(&self) -> PartialVMResult<()> {
        for friend_decl in self.view.friend_decls().into_iter().flatten() {
            self.check_module_handle(friend_decl)?
        }
        Ok(())
    }

    fn check_struct_instantiations(&self) -> PartialVMResult<()> {
        for struct_instantiation in self.view.struct_instantiations().into_iter().flatten() {
            self.check_struct_instantiation(struct_instantiation)?
        }
        Ok(())
    }

    fn check_function_instantiations(&self) -> PartialVMResult<()> {
        for function_instantiation in self.view.function_instantiations() {
            self.check_function_instantiation(function_instantiation)?
        }
        Ok(())
    }

    fn check_field_instantiations(&self) -> PartialVMResult<()> {
        for field_instantiation in self.view.field_instantiations().into_iter().flatten() {
            self.check_field_instantiation(field_instantiation)?
        }
        Ok(())
    }

    fn check_struct_defs(&self) -> PartialVMResult<()> {
        for struct_def in self.view.struct_defs().into_iter().flatten() {
            self.check_struct_def(struct_def)?
        }
        Ok(())
    }

    fn check_function_defs(&mut self) -> PartialVMResult<()> {
        let view = self.view;
        for (function_def_idx, function_def) in
            view.function_defs().into_iter().flatten().enumerate()
        {
            self.check_function_def(function_def_idx, function_def)?
        }
        Ok(())
    }

    fn check_module_handle(&self, module_handle: &ModuleHandle) -> PartialVMResult<()> {
        check_bounds_impl(self.view.address_identifiers(), module_handle.address)?;
        check_bounds_impl(self.view.identifiers(), module_handle.name)
    }

    fn check_self_module_handle(&self) -> PartialVMResult<()> {
        match self.view.self_handle_idx() {
            Some(idx) => check_bounds_impl(self.view.module_handles(), idx),
            None => Ok(()),
        }
    }

    fn check_struct_handle(&self, struct_handle: &StructHandle) -> PartialVMResult<()> {
        check_bounds_impl(self.view.module_handles(), struct_handle.module)?;
        check_bounds_impl(self.view.identifiers(), struct_handle.name)
    }

    fn check_function_handle(&self, function_handle: &FunctionHandle) -> PartialVMResult<()> {
        check_bounds_impl(self.view.module_handles(), function_handle.module)?;
        check_bounds_impl(self.view.identifiers(), function_handle.name)?;
        check_bounds_impl(self.view.signatures(), function_handle.parameters)?;
        check_bounds_impl(self.view.signatures(), function_handle.return_)?;
        // function signature type paramters must be in bounds to the function type parameters
        let type_param_count = function_handle.type_parameters.len();
        if let Some(sig) = self
            .view
            .signatures()
            .get(function_handle.parameters.into_index())
        {
            for ty in &sig.0 {
                self.check_type_parameter(ty, type_param_count)?
            }
        }
        if let Some(sig) = self
            .view
            .signatures()
            .get(function_handle.return_.into_index())
        {
            for ty in &sig.0 {
                self.check_type_parameter(ty, type_param_count)?
            }
        }
        Ok(())
    }

    fn check_field_handle(&self, field_handle: &FieldHandle) -> PartialVMResult<()> {
        check_bounds_impl_opt(&self.view.struct_defs(), field_handle.owner)?;
        // field offset must be in bounds, struct def just checked above must exist
        if let Some(struct_def) = &self
            .view
            .struct_defs()
            .and_then(|d| d.get(field_handle.owner.into_index()))
        {
            let fields_count = match &struct_def.field_information {
                StructFieldInformation::Native => 0,
                StructFieldInformation::Declared(fields) => fields.len(),
            };
            if field_handle.field as usize >= fields_count {
                return Err(bounds_error(
                    StatusCode::INDEX_OUT_OF_BOUNDS,
                    IndexKind::MemberCount,
                    field_handle.field,
                    fields_count,
                ));
            }
        }
        Ok(())
    }

    fn check_struct_instantiation(
        &self,
        struct_instantiation: &StructDefInstantiation,
    ) -> PartialVMResult<()> {
        check_bounds_impl_opt(&self.view.struct_defs(), struct_instantiation.def)?;
        check_bounds_impl(self.view.signatures(), struct_instantiation.type_parameters)
    }

    fn check_function_instantiation(
        &self,
        function_instantiation: &FunctionInstantiation,
    ) -> PartialVMResult<()> {
        check_bounds_impl(self.view.function_handles(), function_instantiation.handle)?;
        check_bounds_impl(
            self.view.signatures(),
            function_instantiation.type_parameters,
        )
    }

    fn check_field_instantiation(
        &self,
        field_instantiation: &FieldInstantiation,
    ) -> PartialVMResult<()> {
        check_bounds_impl_opt(&self.view.field_handles(), field_instantiation.handle)?;
        check_bounds_impl(self.view.signatures(), field_instantiation.type_parameters)
    }

    fn check_signature(&self, signature: &Signature) -> PartialVMResult<()> {
        for ty in &signature.0 {
            self.check_type(ty)?
        }
        Ok(())
    }

    fn check_constant(&self, constant: &Constant) -> PartialVMResult<()> {
        self.check_type(&constant.type_)
    }

    fn check_struct_def(&self, struct_def: &StructDefinition) -> PartialVMResult<()> {
        check_bounds_impl(self.view.struct_handles(), struct_def.struct_handle)?;
        // check signature (type) and type parameter for the field type
        if let StructFieldInformation::Declared(fields) = &struct_def.field_information {
            let type_param_count = self
                .view
                .struct_handles()
                .get(struct_def.struct_handle.into_index())
                .map_or(0, |sh| sh.type_parameters.len());
            // field signatures are inlined
            for field in fields {
                check_bounds_impl(self.view.identifiers(), field.name)?;
                self.check_type(&field.signature.0)?;
                self.check_type_parameter(&field.signature.0, type_param_count)?;
            }
        }
        Ok(())
    }

    fn check_function_def(
        &mut self,
        function_def_idx: usize,
        function_def: &FunctionDefinition,
    ) -> PartialVMResult<()> {
        self.context = BoundsCheckingContext::ModuleFunction(FunctionDefinitionIndex(
            function_def_idx as TableIndex,
        ));
        check_bounds_impl(self.view.function_handles(), function_def.function)?;
        for ty in &function_def.acquires_global_resources {
            check_bounds_impl_opt(&self.view.struct_defs(), *ty)?;
        }

        let code_unit = match &function_def.code {
            Some(code) => code,
            None => return Ok(()),
        };

        if function_def.function.into_index() >= self.view.function_handles().len() {
            return Err(verification_error(
                StatusCode::INDEX_OUT_OF_BOUNDS,
                IndexKind::FunctionDefinition,
                function_def_idx as TableIndex,
            ));
        }
        let function_handle = &self.view.function_handles()[function_def.function.into_index()];
        if function_handle.parameters.into_index() >= self.view.signatures().len() {
            return Err(verification_error(
                StatusCode::INDEX_OUT_OF_BOUNDS,
                IndexKind::FunctionDefinition,
                function_def_idx as TableIndex,
            ));
        }
        let parameters = &self.view.signatures()[function_handle.parameters.into_index()];

        self.check_code(
            code_unit,
            &function_handle.type_parameters,
            parameters,
            function_def_idx,
        )
    }

    fn check_code(
        &self,
        code_unit: &CodeUnit,
        type_parameters: &[AbilitySet],
        parameters: &Signature,
        index: usize,
    ) -> PartialVMResult<()> {
        check_bounds_impl(self.view.signatures(), code_unit.locals)?;

        let locals = self.get_locals(code_unit)?;
        // Use saturating add for stability
        let locals_count = locals.len().saturating_add(parameters.len());

        if locals_count > LocalIndex::MAX as usize {
            return Err(verification_error(
                StatusCode::TOO_MANY_LOCALS,
                IndexKind::FunctionDefinition,
                index as TableIndex,
            ));
        }

        // if there are locals check that the type parameters in local signature are in bounds.
        let type_param_count = type_parameters.len();
        for local in locals {
            self.check_type_parameter(local, type_param_count)?
        }

        // check bytecodes
        let code_len = code_unit.code.len();
        for (bytecode_offset, bytecode) in code_unit.code.iter().enumerate() {
            use self::Bytecode::*;

            match bytecode {
                LdConst(idx) => self.check_code_unit_bounds_impl(
                    self.view.constant_pool(),
                    *idx,
                    bytecode_offset,
                )?,
                MutBorrowField(idx) | ImmBorrowField(idx) => self.check_code_unit_bounds_impl_opt(
                    &self.view.field_handles(),
                    *idx,
                    bytecode_offset,
                )?,
                MutBorrowFieldGeneric(idx) | ImmBorrowFieldGeneric(idx) => {
                    self.check_code_unit_bounds_impl_opt(
                        &self.view.field_instantiations(),
                        *idx,
                        bytecode_offset,
                    )?;
                    // check type parameters in borrow are bound to the function type parameters
                    if let Some(field_inst) = self
                        .view
                        .field_instantiations()
                        .and_then(|f| f.get(idx.into_index()))
                    {
                        if let Some(sig) = self
                            .view
                            .signatures()
                            .get(field_inst.type_parameters.into_index())
                        {
                            for ty in &sig.0 {
                                self.check_type_parameter(ty, type_param_count)?
                            }
                        }
                    }
                }
                Call(idx) => self.check_code_unit_bounds_impl(
                    self.view.function_handles(),
                    *idx,
                    bytecode_offset,
                )?,
                CallGeneric(idx) => {
                    self.check_code_unit_bounds_impl(
                        self.view.function_instantiations(),
                        *idx,
                        bytecode_offset,
                    )?;
                    // check type parameters in call are bound to the function type parameters
                    if let Some(func_inst) =
                        self.view.function_instantiations().get(idx.into_index())
                    {
                        if let Some(sig) = self
                            .view
                            .signatures()
                            .get(func_inst.type_parameters.into_index())
                        {
                            for ty in &sig.0 {
                                self.check_type_parameter(ty, type_param_count)?
                            }
                        }
                    }
                }
                Pack(idx) | Unpack(idx) | Exists(idx) | ImmBorrowGlobal(idx)
                | MutBorrowGlobal(idx) | MoveFrom(idx) | MoveTo(idx) => self
                    .check_code_unit_bounds_impl_opt(
                        &self.view.struct_defs(),
                        *idx,
                        bytecode_offset,
                    )?,
                PackGeneric(idx)
                | UnpackGeneric(idx)
                | ExistsGeneric(idx)
                | ImmBorrowGlobalGeneric(idx)
                | MutBorrowGlobalGeneric(idx)
                | MoveFromGeneric(idx)
                | MoveToGeneric(idx) => {
                    self.check_code_unit_bounds_impl_opt(
                        &self.view.struct_instantiations(),
                        *idx,
                        bytecode_offset,
                    )?;
                    // check type parameters in type operations are bound to the function type parameters
                    if let Some(struct_inst) = self
                        .view
                        .struct_instantiations()
                        .and_then(|s| s.get(idx.into_index()))
                    {
                        if let Some(sig) = self
                            .view
                            .signatures()
                            .get(struct_inst.type_parameters.into_index())
                        {
                            for ty in &sig.0 {
                                self.check_type_parameter(ty, type_param_count)?
                            }
                        }
                    }
                }
                // Instructions that refer to this code block.
                BrTrue(offset) | BrFalse(offset) | Branch(offset) => {
                    let offset = *offset as usize;
                    if offset >= code_len {
                        return Err(self.offset_out_of_bounds(
                            StatusCode::INDEX_OUT_OF_BOUNDS,
                            IndexKind::CodeDefinition,
                            offset,
                            code_len,
                            bytecode_offset as CodeOffset,
                        ));
                    }
                }
                // Instructions that refer to the locals.
                CopyLoc(idx) | MoveLoc(idx) | StLoc(idx) | MutBorrowLoc(idx)
                | ImmBorrowLoc(idx) => {
                    let idx = *idx as usize;
                    if idx >= locals_count {
                        return Err(self.offset_out_of_bounds(
                            StatusCode::INDEX_OUT_OF_BOUNDS,
                            IndexKind::LocalPool,
                            idx,
                            locals_count,
                            bytecode_offset as CodeOffset,
                        ));
                    }
                }

                // Instructions that refer to a signature
                VecPack(idx, _)
                | VecLen(idx)
                | VecImmBorrow(idx)
                | VecMutBorrow(idx)
                | VecPushBack(idx)
                | VecPopBack(idx)
                | VecUnpack(idx, _)
                | VecSwap(idx) => {
                    self.check_code_unit_bounds_impl(
                        self.view.signatures(),
                        *idx,
                        bytecode_offset,
                    )?;
                    if let Some(sig) = self.view.signatures().get(idx.into_index()) {
                        for ty in &sig.0 {
                            self.check_type_parameter(ty, type_param_count)?;
                        }
                    }
                }

                // List out the other options explicitly so there's a compile error if a new
                // bytecode gets added.
                FreezeRef | Pop | Ret | LdU8(_) | LdU16(_) | LdU32(_) | LdU64(_) | LdU256(_)
                | LdU128(_) | CastU8 | CastU16 | CastU32 | CastU64 | CastU128 | CastU256
                | LdTrue | LdFalse | ReadRef | WriteRef | Add | Sub | Mul | Mod | Div | BitOr
                | BitAnd | Xor | Shl | Shr | Or | And | Not | Eq | Neq | Lt | Gt | Le | Ge
                | Abort | Nop => (),
            }
        }
        Ok(())
    }

    fn check_type(&self, ty: &SignatureToken) -> PartialVMResult<()> {
        use self::SignatureToken::*;

        for ty in ty.preorder_traversal() {
            match ty {
                Bool | U8 | U16 | U32 | U64 | U128 | U256 | Address | Signer | TypeParameter(_)
                | Reference(_) | MutableReference(_) | Vector(_) => (),
                Struct(idx) => {
                    check_bounds_impl(self.view.struct_handles(), *idx)?;
                    if let Some(sh) = self.view.struct_handles().get(idx.into_index()) {
                        if !sh.type_parameters.is_empty() {
                            return Err(PartialVMError::new(
                                StatusCode::NUMBER_OF_TYPE_ARGUMENTS_MISMATCH,
                            )
                            .with_message(format!(
                                "expected {} type parameters got 0 (Struct)",
                                sh.type_parameters.len(),
                            )));
                        }
                    }
                }
                StructInstantiation(idx, type_params) => {
                    check_bounds_impl(self.view.struct_handles(), *idx)?;
                    if let Some(sh) = self.view.struct_handles().get(idx.into_index()) {
                        if sh.type_parameters.len() != type_params.len() {
                            return Err(PartialVMError::new(
                                StatusCode::NUMBER_OF_TYPE_ARGUMENTS_MISMATCH,
                            )
                            .with_message(format!(
                                "expected {} type parameters got {}",
                                sh.type_parameters.len(),
                                type_params.len(),
                            )));
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn check_type_parameter(
        &self,
        ty: &SignatureToken,
        type_param_count: usize,
    ) -> PartialVMResult<()> {
        use self::SignatureToken::*;

        for ty in ty.preorder_traversal() {
            match ty {
                SignatureToken::TypeParameter(idx) => {
                    if *idx as usize >= type_param_count {
                        return Err(bounds_error(
                            StatusCode::INDEX_OUT_OF_BOUNDS,
                            IndexKind::TypeParameter,
                            *idx,
                            type_param_count,
                        ));
                    }
                }

                Bool
                | U8
                | U16
                | U32
                | U64
                | U128
                | U256
                | Address
                | Signer
                | Struct(_)
                | Reference(_)
                | MutableReference(_)
                | Vector(_)
                | StructInstantiation(_, _) => (),
            }
        }
        Ok(())
    }

    fn check_code_unit_bounds_impl_opt<T, I>(
        &self,
        pool: &Option<&[T]>,
        idx: I,
        bytecode_offset: usize,
    ) -> PartialVMResult<()>
    where
        I: ModuleIndex,
    {
        pool.map_or(Ok(()), |p| {
            self.check_code_unit_bounds_impl(p, idx, bytecode_offset)
        })
    }

    fn check_code_unit_bounds_impl<T, I>(
        &self,
        pool: &[T],
        idx: I,
        bytecode_offset: usize,
    ) -> PartialVMResult<()>
    where
        I: ModuleIndex,
    {
        let idx = idx.into_index();
        let len = pool.len();
        if idx >= len {
            Err(self.offset_out_of_bounds(
                StatusCode::INDEX_OUT_OF_BOUNDS,
                I::KIND,
                idx,
                len,
                bytecode_offset as CodeOffset,
            ))
        } else {
            Ok(())
        }
    }

    fn get_locals(&self, code_unit: &CodeUnit) -> PartialVMResult<&[SignatureToken]> {
        match self.view.signatures().get(code_unit.locals.into_index()) {
            Some(signature) => Ok(&signature.0),
            None => Err(bounds_error(
                StatusCode::INDEX_OUT_OF_BOUNDS,
                IndexKind::Signature,
                code_unit.locals.into_index() as u16,
                self.view.signatures().len(),
            )),
        }
    }

    fn offset_out_of_bounds(
        &self,
        status: StatusCode,
        kind: IndexKind,
        target_offset: usize,
        target_pool_len: usize,
        cur_bytecode_offset: CodeOffset,
    ) -> PartialVMError {
        match self.context {
            BoundsCheckingContext::Module => {
                let msg = format!("Indexing into bytecode {} during bounds checking but 'current_function' was not set", cur_bytecode_offset);
                PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR).with_message(msg)
            }
            BoundsCheckingContext::ModuleFunction(current_function_index) => {
                offset_out_of_bounds_error(
                    status,
                    kind,
                    target_offset,
                    target_pool_len,
                    current_function_index,
                    cur_bytecode_offset,
                )
            }
            BoundsCheckingContext::Script => {
                let msg = format!(
        "Index {} out of bounds for {} at bytecode offset {} in script while indexing {}",
        target_offset, target_pool_len, cur_bytecode_offset, kind);
                PartialVMError::new(status).with_message(msg)
            }
        }
    }
}

fn check_bounds_impl_opt<T, I>(pool: &Option<&[T]>, idx: I) -> PartialVMResult<()>
where
    I: ModuleIndex,
{
    pool.map_or(Ok(()), |p| check_bounds_impl(p, idx))
}

fn check_bounds_impl<T, I>(pool: &[T], idx: I) -> PartialVMResult<()>
where
    I: ModuleIndex,
{
    let idx = idx.into_index();
    let len = pool.len();
    if idx >= len {
        Err(bounds_error(
            StatusCode::INDEX_OUT_OF_BOUNDS,
            I::KIND,
            idx as TableIndex,
            len,
        ))
    } else {
        Ok(())
    }
}
