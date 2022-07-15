// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! This module lays out the basic abstract costing schedule for bytecode instructions.
//!
//! It is important to note that the cost schedule defined in this file does not track hashing
//! operations or other native operations; the cost of each native operation will be returned by the
//! native function itself.
use move_binary_format::{
    errors::{Location, PartialVMError, PartialVMResult, VMResult},
    file_format::{
        Bytecode, ConstantPoolIndex, FieldHandleIndex, FieldInstantiationIndex,
        FunctionHandleIndex, FunctionInstantiationIndex, SignatureIndex,
        StructDefInstantiationIndex, StructDefinitionIndex,
    },
    file_format_common::{instruction_key, Opcodes},
};
use move_core_types::{
    gas_schedule::{
        AbstractMemorySize, GasAlgebra, GasCarrier, GasPrice, GasUnits, InternalGasUnits,
    },
    vm_status::StatusCode,
};
use move_vm_types::gas::GasMeter;
use once_cell::sync::Lazy;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, PartialEq, Deserialize)]
pub struct GasConstants {
    /// The cost per-byte read from global storage.
    pub global_memory_per_byte_cost: InternalGasUnits<GasCarrier>,

    /// The cost per-byte written to storage.
    pub global_memory_per_byte_write_cost: InternalGasUnits<GasCarrier>,

    /// The flat minimum amount of gas required for any transaction.
    /// Charged at the start of execution.
    pub min_transaction_gas_units: InternalGasUnits<GasCarrier>,

    /// Any transaction over this size will be charged an additional amount per byte.
    pub large_transaction_cutoff: AbstractMemorySize<GasCarrier>,

    /// The units of gas that to be charged per byte over the `large_transaction_cutoff` in addition to
    /// `min_transaction_gas_units` for transactions whose size exceeds `large_transaction_cutoff`.
    pub intrinsic_gas_per_byte: InternalGasUnits<GasCarrier>,

    /// ~5 microseconds should equal one unit of computational gas. We bound the maximum
    /// computational time of any given transaction at roughly 20 seconds. We want this number and
    /// `MAX_PRICE_PER_GAS_UNIT` to always satisfy the inequality that
    /// MAXIMUM_NUMBER_OF_GAS_UNITS * MAX_PRICE_PER_GAS_UNIT < min(u64::MAX, GasUnits<GasCarrier>::MAX)
    /// NB: The bound is set quite high since custom scripts aren't allowed except from predefined
    /// and vetted senders.
    pub maximum_number_of_gas_units: GasUnits<GasCarrier>,

    /// The minimum gas price that a transaction can be submitted with.
    pub min_price_per_gas_unit: GasPrice<GasCarrier>,

    /// The maximum gas unit price that a transaction can be submitted with.
    pub max_price_per_gas_unit: GasPrice<GasCarrier>,

    pub max_transaction_size_in_bytes: GasCarrier,

    pub gas_unit_scaling_factor: GasCarrier,
    pub default_account_size: AbstractMemorySize<GasCarrier>,
}

impl GasConstants {
    pub fn to_internal_units(&self, units: GasUnits<GasCarrier>) -> InternalGasUnits<GasCarrier> {
        InternalGasUnits::new(units.get() * self.gas_unit_scaling_factor)
    }

    pub fn to_external_units(&self, units: InternalGasUnits<GasCarrier>) -> GasUnits<GasCarrier> {
        GasUnits::new(units.get() / self.gas_unit_scaling_factor)
    }
}

impl Default for GasConstants {
    fn default() -> Self {
        Self {
            global_memory_per_byte_cost: InternalGasUnits::new(4),
            global_memory_per_byte_write_cost: InternalGasUnits::new(9),
            min_transaction_gas_units: InternalGasUnits::new(600),
            large_transaction_cutoff: AbstractMemorySize::new(600),
            intrinsic_gas_per_byte: InternalGasUnits::new(8),
            maximum_number_of_gas_units: GasUnits::new(4_000_000),
            min_price_per_gas_unit: GasPrice::new(0),
            max_price_per_gas_unit: GasPrice::new(10_000),
            max_transaction_size_in_bytes: 4096,
            gas_unit_scaling_factor: 1000,
            default_account_size: AbstractMemorySize::new(800),
        }
    }
}
/// The cost tables, keyed by the serialized form of the bytecode instruction.  We use the
/// serialized form as opposed to the instruction enum itself as the key since this will be the
/// on-chain representation of bytecode instructions in the future.
#[derive(Clone, Debug, Serialize, PartialEq, Deserialize)]
pub struct CostTable {
    pub instruction_table: Vec<GasCost>,
    pub gas_constants: GasConstants,
}

impl CostTable {
    #[inline]
    pub fn instruction_cost(&self, instr_index: u8) -> &GasCost {
        debug_assert!(instr_index > 0 && instr_index <= (self.instruction_table.len() as u8));
        &self.instruction_table[(instr_index - 1) as usize]
    }
}

/// The  `GasCost` tracks:
/// - instruction cost: how much time/computational power is needed to perform the instruction
/// - memory cost: how much memory is required for the instruction, and storage overhead
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct GasCost {
    pub instruction_gas: InternalGasUnits<GasCarrier>,
    pub memory_gas: InternalGasUnits<GasCarrier>,
}

impl GasCost {
    pub fn new(instr_gas: GasCarrier, mem_gas: GasCarrier) -> Self {
        Self {
            instruction_gas: InternalGasUnits::new(instr_gas),
            memory_gas: InternalGasUnits::new(mem_gas),
        }
    }

    /// Convert a GasCost to a total gas charge in `InternalGasUnits`.
    #[inline]
    pub fn total(&self) -> InternalGasUnits<GasCarrier> {
        self.instruction_gas.add(self.memory_gas)
    }
}

static ZERO_COST_SCHEDULE: Lazy<CostTable> = Lazy::new(|| zero_cost_schedule());

/// The Move VM implementation of state for gas metering.
///
/// Initialize with a `CostTable` and the gas provided to the transaction.
/// Provide all the proper guarantees about gas metering in the Move VM.
///
/// Every client must use an instance of this type to interact with the Move VM.
pub struct GasStatus<'a> {
    cost_table: &'a CostTable,
    gas_left: InternalGasUnits<GasCarrier>,
    charge: bool,
}

impl<'a> GasStatus<'a> {
    /// Initialize the gas state with metering enabled.
    ///
    /// Charge for every operation and fail when there is no more gas to pay for operations.
    /// This is the instantiation that must be used when executing a user script.
    pub fn new(cost_table: &'a CostTable, gas_left: GasUnits<GasCarrier>) -> Self {
        Self {
            gas_left: cost_table.gas_constants.to_internal_units(gas_left),
            cost_table,
            charge: true,
        }
    }

    /// Initialize the gas state with metering disabled.
    ///
    /// It should be used by clients in very specific cases and when executing system
    /// code that does not have to charge the user.
    pub fn new_unmetered() -> Self {
        Self {
            gas_left: InternalGasUnits::new(0),
            cost_table: &ZERO_COST_SCHEDULE,
            charge: false,
        }
    }

    /// Return the `CostTable` behind this `GasStatus`.
    pub fn cost_table(&self) -> &CostTable {
        self.cost_table
    }

    /// Return the gas left.
    pub fn remaining_gas(&self) -> GasUnits<GasCarrier> {
        self.cost_table
            .gas_constants
            .to_external_units(self.gas_left)
    }

    /// Charge a given amount of gas and fail if not enough gas units are left.
    pub fn deduct_gas(&mut self, amount: InternalGasUnits<GasCarrier>) -> PartialVMResult<()> {
        if !self.charge {
            return Ok(());
        }
        if self
            .gas_left
            .app(&amount, |curr_gas, gas_amt| curr_gas >= gas_amt)
        {
            self.gas_left = self.gas_left.sub(amount);
            Ok(())
        } else {
            // Zero out the internal gas state
            self.gas_left = InternalGasUnits::new(0);
            Err(PartialVMError::new(StatusCode::OUT_OF_GAS))
        }
    }

    /// Charge gas related to the overall size of a transaction and fail if not enough
    /// gas units are left.
    pub fn charge_intrinsic_gas(
        &mut self,
        intrinsic_cost: AbstractMemorySize<GasCarrier>,
    ) -> VMResult<()> {
        let cost = calculate_intrinsic_gas(intrinsic_cost, &self.cost_table.gas_constants);
        self.deduct_gas(cost)
            .map_err(|e| e.finish(Location::Undefined))
    }

    pub fn set_metering(&mut self, enabled: bool) {
        self.charge = enabled
    }
}

impl<'a> GasMeter for GasStatus<'a> {
    /// Charge an instruction over data with a given size and fail if not enough gas units are left.
    fn charge_instr_with_size(
        &mut self,
        opcode: Opcodes,
        size: AbstractMemorySize<GasCarrier>,
    ) -> PartialVMResult<()> {
        // Make sure that the size is always non-zero
        let size = size.map(|x| std::cmp::max(1, x));
        debug_assert!(size.get() > 0);
        self.deduct_gas(
            self.cost_table
                .instruction_cost(opcode as u8)
                .total()
                .mul(size),
        )
    }

    /// Charge an instruction and fail if not enough gas units are left.
    fn charge_instr(&mut self, opcode: Opcodes) -> PartialVMResult<()> {
        self.deduct_gas(self.cost_table.instruction_cost(opcode as u8).total())
    }

    fn charge_in_native_unit(&mut self, amount: u64) -> PartialVMResult<()> {
        self.deduct_gas(InternalGasUnits::new(amount))
    }
}

pub fn new_from_instructions(mut instrs: Vec<(Bytecode, GasCost)>) -> CostTable {
    instrs.sort_by_key(|cost| instruction_key(&cost.0));

    if cfg!(debug_assertions) {
        let mut instructions_covered = 0;
        for (index, (instr, _)) in instrs.iter().enumerate() {
            let key = instruction_key(instr);
            if index == (key - 1) as usize {
                instructions_covered += 1;
            }
        }
        debug_assert!(
            instructions_covered == Bytecode::VARIANT_COUNT,
            "all instructions must be in the cost table"
        );
    }
    let instruction_table = instrs
        .into_iter()
        .map(|(_, cost)| cost)
        .collect::<Vec<GasCost>>();
    CostTable {
        instruction_table,
        gas_constants: GasConstants::default(),
    }
}

pub fn zero_cost_instruction_table() -> Vec<(Bytecode, GasCost)> {
    use Bytecode::*;

    vec![
        (MoveTo(StructDefinitionIndex::new(0)), GasCost::new(0, 0)),
        (
            MoveToGeneric(StructDefInstantiationIndex::new(0)),
            GasCost::new(0, 0),
        ),
        (MoveFrom(StructDefinitionIndex::new(0)), GasCost::new(0, 0)),
        (
            MoveFromGeneric(StructDefInstantiationIndex::new(0)),
            GasCost::new(0, 0),
        ),
        (BrTrue(0), GasCost::new(0, 0)),
        (WriteRef, GasCost::new(0, 0)),
        (Mul, GasCost::new(0, 0)),
        (MoveLoc(0), GasCost::new(0, 0)),
        (And, GasCost::new(0, 0)),
        (Pop, GasCost::new(0, 0)),
        (BitAnd, GasCost::new(0, 0)),
        (ReadRef, GasCost::new(0, 0)),
        (Sub, GasCost::new(0, 0)),
        (MutBorrowField(FieldHandleIndex::new(0)), GasCost::new(0, 0)),
        (
            MutBorrowFieldGeneric(FieldInstantiationIndex::new(0)),
            GasCost::new(0, 0),
        ),
        (ImmBorrowField(FieldHandleIndex::new(0)), GasCost::new(0, 0)),
        (
            ImmBorrowFieldGeneric(FieldInstantiationIndex::new(0)),
            GasCost::new(0, 0),
        ),
        (Add, GasCost::new(0, 0)),
        (CopyLoc(0), GasCost::new(0, 0)),
        (StLoc(0), GasCost::new(0, 0)),
        (Ret, GasCost::new(0, 0)),
        (Lt, GasCost::new(0, 0)),
        (LdU8(0), GasCost::new(0, 0)),
        (LdU64(0), GasCost::new(0, 0)),
        (LdU128(0), GasCost::new(0, 0)),
        (CastU8, GasCost::new(0, 0)),
        (CastU64, GasCost::new(0, 0)),
        (CastU128, GasCost::new(0, 0)),
        (Abort, GasCost::new(0, 0)),
        (MutBorrowLoc(0), GasCost::new(0, 0)),
        (ImmBorrowLoc(0), GasCost::new(0, 0)),
        (LdConst(ConstantPoolIndex::new(0)), GasCost::new(0, 0)),
        (Ge, GasCost::new(0, 0)),
        (Xor, GasCost::new(0, 0)),
        (Shl, GasCost::new(0, 0)),
        (Shr, GasCost::new(0, 0)),
        (Neq, GasCost::new(0, 0)),
        (Not, GasCost::new(0, 0)),
        (Call(FunctionHandleIndex::new(0)), GasCost::new(0, 0)),
        (
            CallGeneric(FunctionInstantiationIndex::new(0)),
            GasCost::new(0, 0),
        ),
        (Le, GasCost::new(0, 0)),
        (Branch(0), GasCost::new(0, 0)),
        (Unpack(StructDefinitionIndex::new(0)), GasCost::new(0, 0)),
        (
            UnpackGeneric(StructDefInstantiationIndex::new(0)),
            GasCost::new(0, 0),
        ),
        (Or, GasCost::new(0, 0)),
        (LdFalse, GasCost::new(0, 0)),
        (LdTrue, GasCost::new(0, 0)),
        (Mod, GasCost::new(0, 0)),
        (BrFalse(0), GasCost::new(0, 0)),
        (Exists(StructDefinitionIndex::new(0)), GasCost::new(0, 0)),
        (
            ExistsGeneric(StructDefInstantiationIndex::new(0)),
            GasCost::new(0, 0),
        ),
        (BitOr, GasCost::new(0, 0)),
        (FreezeRef, GasCost::new(0, 0)),
        (
            MutBorrowGlobal(StructDefinitionIndex::new(0)),
            GasCost::new(0, 0),
        ),
        (
            MutBorrowGlobalGeneric(StructDefInstantiationIndex::new(0)),
            GasCost::new(0, 0),
        ),
        (
            ImmBorrowGlobal(StructDefinitionIndex::new(0)),
            GasCost::new(0, 0),
        ),
        (
            ImmBorrowGlobalGeneric(StructDefInstantiationIndex::new(0)),
            GasCost::new(0, 0),
        ),
        (Div, GasCost::new(0, 0)),
        (Eq, GasCost::new(0, 0)),
        (Gt, GasCost::new(0, 0)),
        (Pack(StructDefinitionIndex::new(0)), GasCost::new(0, 0)),
        (
            PackGeneric(StructDefInstantiationIndex::new(0)),
            GasCost::new(0, 0),
        ),
        (Nop, GasCost::new(0, 0)),
        (VecPack(SignatureIndex::new(0), 0), GasCost::new(0, 0)),
        (VecLen(SignatureIndex::new(0)), GasCost::new(0, 0)),
        (VecImmBorrow(SignatureIndex::new(0)), GasCost::new(0, 0)),
        (VecMutBorrow(SignatureIndex::new(0)), GasCost::new(0, 0)),
        (VecPushBack(SignatureIndex::new(0)), GasCost::new(0, 0)),
        (VecPopBack(SignatureIndex::new(0)), GasCost::new(0, 0)),
        (VecUnpack(SignatureIndex::new(0), 0), GasCost::new(0, 0)),
        (VecSwap(SignatureIndex::new(0)), GasCost::new(0, 0)),
    ]
}

// Only used for genesis and for tests where we need a cost table and
// don't have a genesis storage state.
pub fn zero_cost_schedule() -> CostTable {
    // The actual costs for the instructions in this table _DO NOT MATTER_. This is only used
    // for genesis and testing, and for these cases we don't need to worry
    // about the actual gas for instructions.  The only thing we care about is having an entry
    // in the gas schedule for each instruction.
    let instrs = zero_cost_instruction_table();
    new_from_instructions(instrs)
}

pub fn bytecode_instruction_costs() -> Vec<(Bytecode, GasCost)> {
    use Bytecode::*;
    return vec![
        (MoveTo(StructDefinitionIndex::new(0)), GasCost::new(13, 1)),
        (
            MoveToGeneric(StructDefInstantiationIndex::new(0)),
            GasCost::new(27, 1),
        ),
        (
            MoveFrom(StructDefinitionIndex::new(0)),
            GasCost::new(459, 1),
        ),
        (
            MoveFromGeneric(StructDefInstantiationIndex::new(0)),
            GasCost::new(13, 1),
        ),
        (BrTrue(0), GasCost::new(1, 1)),
        (WriteRef, GasCost::new(1, 1)),
        (Mul, GasCost::new(1, 1)),
        (MoveLoc(0), GasCost::new(1, 1)),
        (And, GasCost::new(1, 1)),
        (Pop, GasCost::new(1, 1)),
        (BitAnd, GasCost::new(2, 1)),
        (ReadRef, GasCost::new(1, 1)),
        (Sub, GasCost::new(1, 1)),
        (MutBorrowField(FieldHandleIndex::new(0)), GasCost::new(1, 1)),
        (
            MutBorrowFieldGeneric(FieldInstantiationIndex::new(0)),
            GasCost::new(1, 1),
        ),
        (ImmBorrowField(FieldHandleIndex::new(0)), GasCost::new(1, 1)),
        (
            ImmBorrowFieldGeneric(FieldInstantiationIndex::new(0)),
            GasCost::new(1, 1),
        ),
        (Add, GasCost::new(1, 1)),
        (CopyLoc(0), GasCost::new(1, 1)),
        (StLoc(0), GasCost::new(1, 1)),
        (Ret, GasCost::new(638, 1)),
        (Lt, GasCost::new(1, 1)),
        (LdU8(0), GasCost::new(1, 1)),
        (LdU64(0), GasCost::new(1, 1)),
        (LdU128(0), GasCost::new(1, 1)),
        (CastU8, GasCost::new(2, 1)),
        (CastU64, GasCost::new(1, 1)),
        (CastU128, GasCost::new(1, 1)),
        (Abort, GasCost::new(1, 1)),
        (MutBorrowLoc(0), GasCost::new(2, 1)),
        (ImmBorrowLoc(0), GasCost::new(1, 1)),
        (LdConst(ConstantPoolIndex::new(0)), GasCost::new(1, 1)),
        (Ge, GasCost::new(1, 1)),
        (Xor, GasCost::new(1, 1)),
        (Shl, GasCost::new(2, 1)),
        (Shr, GasCost::new(1, 1)),
        (Neq, GasCost::new(1, 1)),
        (Not, GasCost::new(1, 1)),
        (Call(FunctionHandleIndex::new(0)), GasCost::new(1132, 1)),
        (
            CallGeneric(FunctionInstantiationIndex::new(0)),
            GasCost::new(582, 1),
        ),
        (Le, GasCost::new(2, 1)),
        (Branch(0), GasCost::new(1, 1)),
        (Unpack(StructDefinitionIndex::new(0)), GasCost::new(2, 1)),
        (
            UnpackGeneric(StructDefInstantiationIndex::new(0)),
            GasCost::new(2, 1),
        ),
        (Or, GasCost::new(2, 1)),
        (LdFalse, GasCost::new(1, 1)),
        (LdTrue, GasCost::new(1, 1)),
        (Mod, GasCost::new(1, 1)),
        (BrFalse(0), GasCost::new(1, 1)),
        (Exists(StructDefinitionIndex::new(0)), GasCost::new(41, 1)),
        (
            ExistsGeneric(StructDefInstantiationIndex::new(0)),
            GasCost::new(34, 1),
        ),
        (BitOr, GasCost::new(2, 1)),
        (FreezeRef, GasCost::new(1, 1)),
        (
            MutBorrowGlobal(StructDefinitionIndex::new(0)),
            GasCost::new(21, 1),
        ),
        (
            MutBorrowGlobalGeneric(StructDefInstantiationIndex::new(0)),
            GasCost::new(15, 1),
        ),
        (
            ImmBorrowGlobal(StructDefinitionIndex::new(0)),
            GasCost::new(23, 1),
        ),
        (
            ImmBorrowGlobalGeneric(StructDefInstantiationIndex::new(0)),
            GasCost::new(14, 1),
        ),
        (Div, GasCost::new(3, 1)),
        (Eq, GasCost::new(1, 1)),
        (Gt, GasCost::new(1, 1)),
        (Pack(StructDefinitionIndex::new(0)), GasCost::new(2, 1)),
        (
            PackGeneric(StructDefInstantiationIndex::new(0)),
            GasCost::new(2, 1),
        ),
        (Nop, GasCost::new(1, 1)),
        (VecPack(SignatureIndex::new(0), 0), GasCost::new(84, 1)),
        (VecLen(SignatureIndex::new(0)), GasCost::new(98, 1)),
        (VecImmBorrow(SignatureIndex::new(0)), GasCost::new(1334, 1)),
        (VecMutBorrow(SignatureIndex::new(0)), GasCost::new(1902, 1)),
        (VecPushBack(SignatureIndex::new(0)), GasCost::new(53, 1)),
        (VecPopBack(SignatureIndex::new(0)), GasCost::new(227, 1)),
        (VecUnpack(SignatureIndex::new(0), 0), GasCost::new(572, 1)),
        (VecSwap(SignatureIndex::new(0)), GasCost::new(1436, 1)),
    ];
}

pub static INITIAL_COST_SCHEDULE: Lazy<CostTable> = Lazy::new(|| {
    let mut instrs = bytecode_instruction_costs();
    // Note that the DiemVM is expecting the table sorted by instruction order.
    instrs.sort_by_key(|cost| instruction_key(&cost.0));

    new_from_instructions(instrs)
});

/// Calculate the intrinsic gas for the transaction based upon its size in bytes/words.
pub fn calculate_intrinsic_gas(
    transaction_size: AbstractMemorySize<GasCarrier>,
    gas_constants: &GasConstants,
) -> InternalGasUnits<GasCarrier> {
    let min_transaction_fee = gas_constants.min_transaction_gas_units;

    if transaction_size.get() > gas_constants.large_transaction_cutoff.get() {
        let excess = transaction_size.sub(gas_constants.large_transaction_cutoff);
        min_transaction_fee.add(gas_constants.intrinsic_gas_per_byte.mul(excess))
    } else {
        min_transaction_fee.unitary_cast()
    }
}
