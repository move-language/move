// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::rt_types::{AnyValue, MoveType, MoveUntypedVector};

#[export_name = "move_rt_abort"]
fn abort(code: u64) -> ! {
    crate::target_defs::abort(code);
}

#[export_name = "move_rt_vec_destroy"]
unsafe fn vec_destroy(type_ve: &MoveType, v: MoveUntypedVector) {
    assert_eq!(0, v.length, "can't destroy vectors with elements yet");
    crate::vector::destroy_empty(type_ve, v);
}

#[export_name = "move_rt_vec_empty"]
unsafe fn vec_empty(type_ve: &MoveType) -> MoveUntypedVector {
    crate::vector::empty(type_ve)
}

#[export_name = "move_rt_vec_copy"]
unsafe fn vec_copy(type_ve: &MoveType, dstv: &mut MoveUntypedVector, srcv: &MoveUntypedVector) {
    crate::vector::copy(type_ve, dstv, srcv)
}

#[export_name = "move_rt_vec_cmp_eq"]
unsafe fn vec_cmp_eq(type_ve: &MoveType, v1: &MoveUntypedVector, v2: &MoveUntypedVector) -> bool {
    crate::vector::cmp_eq(type_ve, v1, v2)
}

#[export_name = "move_rt_struct_cmp_eq"]
unsafe fn struct_cmp_eq(type_ve: &MoveType, s1: &AnyValue, s2: &AnyValue) -> bool {
    crate::structs::cmp_eq(type_ve, s1, s2)
}
