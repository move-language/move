// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::rt_types::{AnyValue, MoveType, MoveUntypedVector, SolanaAccountInfo, SolanaPubkey};

#[export_name = "move_rt_abort"]
extern "C" fn abort(code: u64) -> ! {
    crate::target_defs::abort(code);
}

#[export_name = "move_rt_vec_destroy"]
unsafe extern "C" fn vec_destroy(type_ve: &MoveType, v: MoveUntypedVector) {
    assert_eq!(0, v.length, "can't destroy vectors with elements yet");
    crate::vector::destroy_empty(type_ve, v);
}

#[export_name = "move_rt_vec_empty"]
unsafe extern "C" fn vec_empty(type_ve: &MoveType) -> MoveUntypedVector {
    crate::vector::empty(type_ve)
}

#[export_name = "move_rt_vec_copy"]
unsafe extern "C" fn vec_copy(
    type_ve: &MoveType,
    dstv: &mut MoveUntypedVector,
    srcv: &MoveUntypedVector,
) {
    crate::vector::copy(type_ve, dstv, srcv)
}

#[export_name = "move_rt_vec_cmp_eq"]
unsafe extern "C" fn vec_cmp_eq(
    type_ve: &MoveType,
    v1: &MoveUntypedVector,
    v2: &MoveUntypedVector,
) -> bool {
    crate::vector::cmp_eq(type_ve, v1, v2)
}

#[export_name = "move_rt_str_cmp_eq"]
unsafe fn str_cmp_eq(s1: &str, s2: &str) -> bool {
    *s1 == *s2
}

#[export_name = "move_rt_struct_cmp_eq"]
unsafe extern "C" fn struct_cmp_eq(type_ve: &MoveType, s1: &AnyValue, s2: &AnyValue) -> bool {
    crate::structs::cmp_eq(type_ve, s1, s2)
}

/// Maximum number of bytes a program may add to an account during a single realloc
pub const MAX_PERMITTED_DATA_INCREASE: usize = 1_024 * 10;

/// `assert_eq(std::mem::align_of::<u128>(), 8)` is true for BPF but not for some host machines
pub const BPF_ALIGN_OF_U128: usize = 8;

/// Deserialize the input arguments in encoded in borsh
/// https://github.com/solana-labs/solana/blob/master/sdk/program/src/lib.rs
/// https://github.com/solana-labs/solana/blob/master/sdk/program/src/instruction.rs: new_with_borsh
///
/// Input arguments consist of three items
/// - program_id -- a 32 byte Pubkey of the deployed module in Solana ledger,
/// - accounts -- a vector of AccountInfo items, meta data of Solana
///               accounts available to and used by the program,
/// - instruction_data -- a byte array of arbitrary instruction
///                       specific data. We use it to pass a name of the entry function that
///                       the Instruction requests to invoke.
/// accounts in move compiler are represented by SolanaAccountinfo structure in rt_types,
/// and program_id is represeted by SolanaPubkey structure.
/// # Safety
#[allow(clippy::integer_arithmetic)]
#[allow(clippy::type_complexity)]
#[export_name = "move_rt_deserialize"]
pub unsafe fn deserialize<'a>(input: *mut u8) -> (&'a [u8], &'a SolanaPubkey, MoveUntypedVector) {
    use crate::conv::*;
    use alloc::vec::Vec;
    use core::mem::size_of;
    let mut offset: usize = 0;

    // Number of accounts present

    #[allow(clippy::cast_ptr_alignment)]
    let num_accounts = *(input.add(offset) as *const u64) as usize;
    offset += size_of::<u64>();

    // Account Infos

    let mut accounts = Vec::with_capacity(num_accounts);

    for _ in 0..num_accounts {
        let _dup_info = *(input.add(offset) as *const u8);
        offset += size_of::<u8>();
        #[allow(clippy::cast_ptr_alignment)]
        let is_signer = *(input.add(offset) as *const u8) != 0;
        offset += size_of::<u8>();

        #[allow(clippy::cast_ptr_alignment)]
        let is_writable = *(input.add(offset) as *const u8) != 0;
        offset += size_of::<u8>();

        #[allow(clippy::cast_ptr_alignment)]
        let executable = *(input.add(offset) as *const u8) != 0;
        offset += size_of::<u8>();

        // The original data length is stored here because these 4 bytes were
        // originally only used for padding and served as a good location to
        // track the original size of the account data in a compatible way.
        let original_data_len_offset = offset;
        offset += size_of::<u32>();

        let key: &SolanaPubkey = &*(input.add(offset) as *const SolanaPubkey);
        offset += size_of::<SolanaPubkey>();

        let owner: &SolanaPubkey = &*(input.add(offset) as *const SolanaPubkey);
        offset += size_of::<SolanaPubkey>();

        #[allow(clippy::cast_ptr_alignment)]
        let lamports = *(input.add(offset) as *mut u64);
        offset += size_of::<u64>();

        #[allow(clippy::cast_ptr_alignment)]
        let data_len = *(input.add(offset) as *const u64) as usize;
        offset += size_of::<u64>();

        // Store the original data length for detecting invalid reallocations and
        // requires that MAX_PERMITTED_DATA_LENGTH fits in a u32
        *(input.add(original_data_len_offset) as *mut u32) = data_len as u32;

        let data = core::slice::from_raw_parts(input.add(offset), data_len);
        offset += data_len + MAX_PERMITTED_DATA_INCREASE;
        offset += (offset as *const u8).align_offset(BPF_ALIGN_OF_U128); // padding

        #[allow(clippy::cast_ptr_alignment)]
        let rent_epoch = *(input.add(offset) as *const u64);
        offset += size_of::<u64>();

        accounts.push(SolanaAccountInfo {
            key,
            is_signer,
            is_writable,
            lamports,
            data,
            owner,
            executable,
            rent_epoch,
        });
    }

    // Instruction data

    #[allow(clippy::cast_ptr_alignment)]
    let instruction_data_len = *(input.add(offset) as *const u64) as usize;
    offset += size_of::<u64>();

    let instruction_data = { core::slice::from_raw_parts(input.add(offset), instruction_data_len) };
    offset += instruction_data_len;

    // Program Id

    let program_id: &SolanaPubkey = &*(input.add(offset) as *const SolanaPubkey);

    (
        instruction_data,
        program_id,
        rust_vec_to_move_vec::<SolanaAccountInfo>(accounts),
    )
}
