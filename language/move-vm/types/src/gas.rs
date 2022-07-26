// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use move_binary_format::{errors::PartialVMResult, file_format_common::Opcodes};
use move_core_types::gas_schedule::{AbstractMemorySize, GasCarrier};

pub trait GasMeter {
    /// Charge an instruction and fail if not enough gas units are left.
    fn charge_instr(&mut self, opcode: Opcodes) -> PartialVMResult<()>;

    /// Charge an instruction over data with a given size and fail if not enough gas units are left.
    fn charge_instr_with_size(
        &mut self,
        opcode: Opcodes,
        size: AbstractMemorySize<GasCarrier>,
    ) -> PartialVMResult<()>;

    /// Charge a given amount in the unit of measurement native to the GasMeter implementation.
    /// Should fail if not enough gas units are left.
    ///
    /// This is used for metering native functions currently.
    /// However, in the future, we may want to remove this and directly pass a reference to the GasMeter
    /// instance to the native functions to allow gas to be deducted during computation.
    fn charge_in_native_unit(&mut self, amount: u64) -> PartialVMResult<()>;
}

/// A dummy gas meter that does not meter anything.
/// Charge operations will always succeed.
pub struct UnmeteredGasMeter;

impl GasMeter for UnmeteredGasMeter {
    fn charge_instr(&mut self, _opcode: Opcodes) -> PartialVMResult<()> {
        Ok(())
    }

    fn charge_instr_with_size(
        &mut self,
        _opcode: Opcodes,
        _size: AbstractMemorySize<GasCarrier>,
    ) -> PartialVMResult<()> {
        Ok(())
    }

    fn charge_in_native_unit(&mut self, _amount: u64) -> PartialVMResult<()> {
        Ok(())
    }
}
