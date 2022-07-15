// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::Result;
use move_core_types::{account_address::AccountAddress, errmap::ErrorMapping};

fn main() -> Result<()> {
    let error_descriptions: ErrorMapping = bcs::from_bytes(move_stdlib::error_descriptions())?;
    let cost_table = &move_vm_test_utils::gas_schedule::INITIAL_COST_SCHEDULE;
    move_cli::move_cli(
        move_stdlib::natives::all_natives(
            AccountAddress::from_hex_literal("0x1").unwrap(),
            // TODO: is this correct?
            move_stdlib::natives::GasParameters::zeros(),
        ),
        cost_table,
        &error_descriptions,
    )
}
