// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::Result;
use clap::Parser;
use move_cli::{Command, Move};
use move_core_types::{
    errmap::ErrorMapping,
    gas_schedule::{CostTable, GasCost},
    language_storage::CORE_CODE_ADDRESS,
};
use move_vm_types::gas_schedule::{new_from_instructions, zero_cost_instruction_table};

#[derive(Parser)]
pub struct DfCli {
    #[clap(flatten)]
    move_args: Move,

    #[clap(subcommand)]
    cmd: DfCommands,
}

#[derive(Parser)]
pub enum DfCommands {
    #[clap(flatten)]
    Command(Command),
    // extra commands available only in df-cli can be added below
}

fn cost_table(num_natives: usize) -> CostTable {
    let instruction_table = zero_cost_instruction_table();
    let native_table = std::iter::repeat_with(|| GasCost::new(0, 0))
        .take(num_natives)
        .collect();

    new_from_instructions(instruction_table, native_table)
}

fn main() -> Result<()> {
    // let error_descriptions: ErrorMapping =
    //     bcs::from_bytes(diem_framework_releases::current_error_descriptions())?;

    let natives = move_stdlib::natives::all_natives(CORE_CODE_ADDRESS)
        .into_iter()
        .chain(diem_framework_natives::all_natives(CORE_CODE_ADDRESS))
        .collect::<Vec<_>>();

    let num_natives = natives.len();

    let args = DfCli::parse();
    match &args.cmd {
        DfCommands::Command(cmd) => move_cli::run_cli(
            natives,
            &cost_table(num_natives),
            // TODO: implement this
            &ErrorMapping::default(),
            &args.move_args,
            cmd,
        ),
    }
}
