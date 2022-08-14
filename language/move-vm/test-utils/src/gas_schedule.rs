// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! This module lays out the basic abstract costing schedule for bytecode instructions.
//!
//! It is important to note that the cost schedule defined in this file does not track hashing
//! operations or other native operations; the cost of each native operation will be returned by the
//! native function itself.
use move_binary_format::{
    errors::{PartialVMError, PartialVMResult},
    file_format::{
        Bytecode, ConstantPoolIndex, FieldHandleIndex, FieldInstantiationIndex,
        FunctionHandleIndex, FunctionInstantiationIndex, SignatureIndex,
        StructDefInstantiationIndex, StructDefinitionIndex,
    },
    file_format_common::{instruction_key, Opcodes},
};
use move_core_types::{
    gas_algebra::{
        AbstractMemorySize, GasQuantity, InternalGas, InternalGasPerAbstractMemoryUnit,
        InternalGasUnit, ToUnit, ToUnitFractional,
    },
    vm_status::StatusCode,
};
use move_vm_types::gas::GasMeter;
use once_cell::sync::Lazy;
use serde::{Deserialize, Serialize};
use std::{
    ops::{Add, Mul},
    u64,
};

pub enum GasUnit {}

pub type Gas = GasQuantity<GasUnit>;

impl ToUnit<InternalGasUnit> for GasUnit {
    const MULTIPLIER: u64 = 1000;
}

impl ToUnitFractional<GasUnit> for InternalGasUnit {
    const NOMINATOR: u64 = 1;
    const DENOMINATOR: u64 = 1000;
}

/// The cost tables, keyed by the serialized form of the bytecode instruction.  We use the
/// serialized form as opposed to the instruction enum itself as the key since this will be the
/// on-chain representation of bytecode instructions in the future.
#[derive(Clone, Debug, Serialize, PartialEq, Deserialize)]
pub struct CostTable {
    pub instruction_table: Vec<GasCost>,
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
    pub instruction_gas: u64,
    pub memory_gas: u64,
}

impl GasCost {
    pub fn new(instruction_gas: u64, memory_gas: u64) -> Self {
        Self {
            instruction_gas,
            memory_gas,
        }
    }

    /// Convert a GasCost to a total gas charge in `InternalGas`.
    #[inline]
    pub fn total(&self) -> u64 {
        self.instruction_gas.add(self.memory_gas)
    }
}

static ZERO_COST_SCHEDULE: Lazy<CostTable> = Lazy::new(zero_cost_schedule);

/// The Move VM implementation of state for gas metering.
///
/// Initialize with a `CostTable` and the gas provided to the transaction.
/// Provide all the proper guarantees about gas metering in the Move VM.
///
/// Every client must use an instance of this type to interact with the Move VM.
pub struct GasStatus<'a> {
    cost_table: &'a CostTable,
    gas_left: InternalGas,
    charge: bool,
}

impl<'a> GasStatus<'a> {
    /// Initialize the gas state with metering enabled.
    ///
    /// Charge for every operation and fail when there is no more gas to pay for operations.
    /// This is the instantiation that must be used when executing a user script.
    pub fn new(cost_table: &'a CostTable, gas_left: Gas) -> Self {
        Self {
            gas_left: gas_left.to_unit(),
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
            gas_left: InternalGas::new(0),
            cost_table: &ZERO_COST_SCHEDULE,
            charge: false,
        }
    }

    /// Return the `CostTable` behind this `GasStatus`.
    pub fn cost_table(&self) -> &CostTable {
        self.cost_table
    }

    /// Return the gas left.
    pub fn remaining_gas(&self) -> Gas {
        self.gas_left.to_unit_round_down()
    }

    /// Charge a given amount of gas and fail if not enough gas units are left.
    pub fn deduct_gas(&mut self, amount: InternalGas) -> PartialVMResult<()> {
        if !self.charge {
            return Ok(());
        }

        match self.gas_left.checked_sub(amount) {
            Some(gas_left) => {
                self.gas_left = gas_left;
                Ok(())
            }
            None => {
                self.gas_left = InternalGas::new(0);
                Err(PartialVMError::new(StatusCode::OUT_OF_GAS))
            }
        }
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
        size: AbstractMemorySize,
    ) -> PartialVMResult<()> {
        // Make sure that the size is always non-zero
        let size = std::cmp::max(1.into(), size);
        debug_assert!(size > 0.into());
        self.deduct_gas(
            InternalGasPerAbstractMemoryUnit::new(
                self.cost_table.instruction_cost(opcode as u8).total(),
            )
            .mul(size),
        )
    }

    /// Charge an instruction and fail if not enough gas units are left.
    fn charge_instr(&mut self, opcode: Opcodes) -> PartialVMResult<()> {
        self.deduct_gas(
            self.cost_table
                .instruction_cost(opcode as u8)
                .total()
                .into(),
        )
    }

    fn charge_in_native_unit(&mut self, amount: InternalGas) -> PartialVMResult<()> {
        self.deduct_gas(amount)
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
    CostTable { instruction_table }
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
