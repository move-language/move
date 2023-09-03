// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use log::*;
use serde::{Deserialize, Serialize};
use solana_bpf_loader_program::{
    create_vm, load_program_from_bytes,
    serialization::serialize_parameters,
    syscalls::{create_program_runtime_environment, SyscallError},
};
use solana_program_runtime::{
    invoke_context::InvokeContext,
    loaded_programs::{LoadProgramMetrics, LoadedProgramType},
    with_mock_invoke_context,
};
use solana_rbpf::{elf::Executable, static_analysis::Analysis, verifier::RequisiteVerifier};
use solana_sdk::{
    account::AccountSharedData,
    bpf_loader_upgradeable,
    pubkey::Pubkey,
    slot_history::Slot,
    transaction_context::{IndexOfAccount, InstructionAccount},
};
use std::{
    fs,
    path::{Path, PathBuf},
    sync::Arc,
    time::{Duration, Instant},
};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct AccountInfo {
    key: String,
    owner: Option<String>,
    is_signer: Option<bool>,
    is_writable: Option<bool>,
    lamports: Option<u64>,
    data: Option<Vec<u8>>,
}
#[derive(Serialize, Deserialize, Clone)]
pub struct Input {
    pub program_id: String,
    pub accounts: Vec<AccountInfo>,
    pub instruction_data: Vec<u8>,
}

#[derive(Debug, Default)]
pub enum ExitReason {
    Abort,
    Failure,
    #[default]
    Success,
}

#[derive(Debug, Default)]
pub struct ExecuteResult {
    pub exit_reason: ExitReason,
    pub return_value: u64,
    pub compute_units: u64,
    pub log: String,
}

fn load_input(path: PathBuf) -> serde_json::Result<Input> {
    debug!("Reading input file {path:?}");
    let file = fs::File::open(path).unwrap();
    let input: Input = serde_json::from_reader(file)?;
    debug!("Program input:");
    debug!("program_id: {}", &input.program_id);
    debug!("accounts {:?}", &input.accounts);
    debug!("instruction_data {:?}", &input.instruction_data);
    Ok(input)
}

fn load_program<'a>(
    filename: &Path,
    program_id: Pubkey,
    invoke_context: &InvokeContext<'a>,
) -> Executable<RequisiteVerifier, InvokeContext<'a>> {
    debug!("Load program {filename:?}, id {program_id}");
    let contents = &std::fs::read(filename).unwrap_or_else(|e| {
        eprintln!("Can't read the executable {:?}, error: {}", filename, e);
        std::process::exit(1);
    });
    let slot = Slot::default();
    let log_collector = invoke_context.get_log_collector();
    let loader_key = bpf_loader_upgradeable::id();
    let mut load_program_metrics = LoadProgramMetrics {
        program_id: program_id.to_string(),
        ..LoadProgramMetrics::default()
    };
    let account_size = contents.len();
    let program_runtime_environment = create_program_runtime_environment(
        &invoke_context.feature_set,
        invoke_context.get_compute_budget(),
        false, /* deployment */
        true,  /* debugging_features */
    )
    .unwrap();
    let result = load_program_from_bytes(
        &invoke_context.feature_set,
        log_collector,
        &mut load_program_metrics,
        contents,
        &loader_key,
        account_size,
        slot,
        Arc::new(program_runtime_environment),
    );
    match result {
        Ok(loaded_program) => match loaded_program.program {
            LoadedProgramType::LegacyV1(program) => Ok(unsafe { std::mem::transmute(program) }),
            _ => unreachable!(),
        },
        Err(err) => Err(format!("Loading executable failed: {err:?}")),
    }
    .unwrap()
}

fn get_abort_code(message: String) -> u64 {
    let codes = message
        .split(", ")
        .collect::<Vec<&str>>()
        .iter()
        .map(|x| {
            let y = x.trim_start_matches("0x");
            u64::from_str_radix(y, 16).unwrap()
        })
        .collect::<Vec<u64>>();
    assert!(codes.iter().all(|c| *c == codes[0]), "all abort codes same");
    codes[0]
}

struct LazyAnalysis<'a, 'b> {
    analysis: Option<Analysis<'a>>,
    executable: &'a Executable<RequisiteVerifier, InvokeContext<'b>>,
}

impl<'a, 'b> LazyAnalysis<'a, 'b> {
    fn new(executable: &'a Executable<RequisiteVerifier, InvokeContext<'b>>) -> Self {
        Self {
            analysis: None,
            executable,
        }
    }

    fn analyze(&mut self) -> &Analysis {
        if let Some(ref analysis) = self.analysis {
            return analysis;
        }
        self.analysis
            .insert(Analysis::from_executable(self.executable).unwrap())
    }
}

fn output_trace(filename: &str, trace: &[[u64; 12]], frame: usize, analysis: &mut LazyAnalysis) {
    use std::{fs::File, io::Write};
    if filename.is_empty() || filename == "stdout" {
        writeln!(&mut std::io::stdout(), "Frame {frame}").unwrap();
        analysis
            .analyze()
            .disassemble_trace_log(&mut std::io::stdout(), trace)
            .unwrap();
    } else {
        let mut fd = File::create(filename).unwrap();
        writeln!(&fd, "Frame {frame}").unwrap();
        analysis
            .analyze()
            .disassemble_trace_log(&mut fd, trace)
            .unwrap();
    }
}

pub fn run_solana_vm(exe: String) -> (ExecuteResult, Duration) {
    let exe = Path::new(&exe);
    let mut transaction_accounts = Vec::new();
    let mut instruction_accounts = Vec::new();
    let loader_id = bpf_loader_upgradeable::id();
    let input = load_input(Path::new("input.json").to_path_buf()).unwrap();
    let instruction_data = input.instruction_data.clone();
    let program_id = input.program_id.parse::<Pubkey>().unwrap_or_else(|err| {
        debug!(
            "Invalid program ID in input {}, error {}",
            input.program_id, err,
        );
        Pubkey::new_unique()
    });
    let accounts = input.accounts;
    for (index, account_info) in accounts.into_iter().enumerate() {
        let pubkey = account_info.key.parse::<Pubkey>().unwrap_or_else(|err| {
            debug!("Invalid key in input {}, error {}", account_info.key, err);
            Pubkey::new_unique()
        });
        let data = account_info.data.unwrap_or_default();
        let space = data.len();
        let owner = account_info
            .owner
            .unwrap_or_else(|| Pubkey::new_unique().to_string());
        let owner = owner.parse::<Pubkey>().unwrap_or_else(|err| {
            eprintln!("Invalid owner key in input {owner}, error {err}");
            Pubkey::new_unique()
        });
        let lamports = account_info.lamports.unwrap_or(0);
        let mut account = AccountSharedData::new(lamports, space, &owner);
        account.set_data(data);
        transaction_accounts.push((pubkey, account));
        instruction_accounts.push(InstructionAccount {
            index_in_transaction: index as IndexOfAccount,
            index_in_caller: index as IndexOfAccount,
            index_in_callee: index as IndexOfAccount,
            is_signer: account_info.is_signer.unwrap_or(false),
            is_writable: account_info.is_writable.unwrap_or(false),
        });
    }
    transaction_accounts.push((
        loader_id,
        AccountSharedData::new(0, 0, &solana_sdk::native_loader::id()),
    ));
    transaction_accounts.push((
        program_id, // ID of the loaded program. It can modify accounts with the same owner key
        AccountSharedData::new(0, 0, &loader_id),
    ));
    with_mock_invoke_context!(invoke_context, transaction_context, transaction_accounts);
    let program_index: u16 = instruction_accounts.len().try_into().unwrap();
    invoke_context
        .transaction_context
        .get_next_instruction_context()
        .unwrap()
        .configure(
            &[program_index, program_index.saturating_add(1)],
            &instruction_accounts,
            &instruction_data,
        );
    invoke_context.push().unwrap();
    #[allow(unused_mut)]
    let mut verified_executable = load_program(exe, program_id, &invoke_context);
    let (_parameter_bytes, regions, account_lengths) = serialize_parameters(
        invoke_context.transaction_context,
        invoke_context
            .transaction_context
            .get_current_instruction_context()
            .unwrap(),
        true, // should_cap_ix_accounts
        true, // copy_account_data
    )
    .unwrap();

    let mut analysis = LazyAnalysis::new(&verified_executable);

    create_vm!(
        vm,
        &verified_executable,
        regions,
        account_lengths,
        &mut invoke_context,
    );
    let mut vm = vm.unwrap();

    let now = Instant::now();
    let (instruction_count, result) = vm.execute_program(&verified_executable, true);
    let elapsed = now.elapsed();

    let result = Result::from(result);

    let trace_var = std::env::var("TRACE");
    if let Ok(trace_filename) = trace_var {
        if let Some(Some(syscall_context)) = vm.context_object_pointer.syscall_context.last() {
            let trace = syscall_context.trace_log.as_slice();
            output_trace(&trace_filename, trace, 0, &mut analysis);

            // The remaining traces are saved in InvokeContext when
            // corresponding syscall_contexts are popped.
            let traces = vm.context_object_pointer.get_traces();
            for (frame, trace) in traces.iter().filter(|t| !t.is_empty()).enumerate() {
                output_trace(&trace_filename, trace, frame + 1, &mut analysis);
            }
        }
    }

    drop(vm);

    let mut all_logs = invoke_context
        .get_log_collector()
        .unwrap()
        .borrow()
        .get_recorded_content()
        .to_vec()
        .iter()
        .map(|x| {
            if x.starts_with("Program log: ") {
                x.strip_prefix("Program log: ").unwrap().to_string()
            } else {
                x.clone()
            }
        })
        .collect::<Vec<_>>();

    if std::env::var("DUMP").is_ok() {
        for (i, event) in all_logs.iter().enumerate() {
            eprintln!("event {i}: {event:?}");
        }
    }

    let vm_log = invoke_context
        .get_log_collector()
        .unwrap()
        .borrow()
        .get_recorded_content()
        .join("\n");
    (
        match result {
            Ok(x) => ExecuteResult {
                exit_reason: ExitReason::Success,
                return_value: x,
                compute_units: instruction_count,
                log: vm_log,
            },
            Err(e) if e.is::<SyscallError>() => {
                let syscall_error = *(e.downcast::<SyscallError>().unwrap());
                match syscall_error {
                    SyscallError::Abort => {
                        let abort_code = get_abort_code(all_logs.pop().unwrap());
                        debug!("Solana VM abort with code {abort_code}");
                        ExecuteResult {
                            exit_reason: ExitReason::Abort,
                            return_value: abort_code,
                            compute_units: instruction_count,
                            log: vm_log,
                        }
                    }
                    _ => {
                        debug!("Solana VM Syscall Error {syscall_error:?}");
                        ExecuteResult {
                            exit_reason: ExitReason::Failure,
                            return_value: u64::MAX,
                            compute_units: instruction_count,
                            log: vm_log + format!("\n{syscall_error:?}").as_str(),
                        }
                    }
                }
            }
            e => {
                debug!("Solana VM Error {e:?}");
                ExecuteResult {
                    exit_reason: ExitReason::Failure,
                    return_value: u64::MAX,
                    compute_units: instruction_count,
                    log: vm_log + format!("\n{e:?}").as_str(),
                }
            }
        },
        elapsed,
    )
}
