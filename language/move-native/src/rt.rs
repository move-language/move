// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::rt_types::{AnyValue, MoveType, MoveUntypedVector, MoveAddress, MoveSigner};

#[export_name = "move_rt_abort"]
fn abort(code: u64) -> ! {
    crate::target_defs::abort(code);
}

#[export_name = "move_rt_vec_destroy"]
unsafe fn vec_destroy(type_ve: &MoveType, v: MoveUntypedVector) {
    assert_eq!(0, v.length, "can't destroy vectors with elements yet");
    crate::std::vector::destroy_empty(type_ve, v);
}

#[export_name = "move_rt_vec_empty"]
unsafe fn vec_empty(type_ve: &MoveType) -> MoveUntypedVector {
    crate::std::vector::empty(type_ve)
}

#[export_name = "move_rt_vec_copy"]
unsafe fn vec_copy(type_ve: &MoveType, dstv: &mut MoveUntypedVector, srcv: &MoveUntypedVector) {
    use crate::std::vector as V;
    let src_len = V::length(type_ve, srcv);
    let dst_len = V::length(type_ve, dstv);

    // Drain the destination first.
    for i in 0..dst_len {
        crate::rt::pop_back_discard(type_ve, dstv);
    }

    // Now copy.
    for i in 0..src_len {
        let se = V::borrow(type_ve, srcv, i);
        let septr = se as *const AnyValue as *mut AnyValue;
        V::push_back(type_ve, dstv, septr);
    }
}

unsafe fn pop_back_discard(
    type_ve: &MoveType,
    v: &mut MoveUntypedVector,
) {
    use crate::conv::{TypedMoveBorrowedRustVecMut, borrow_typed_move_vec_as_rust_vec_mut};
    let mut rust_vec = borrow_typed_move_vec_as_rust_vec_mut(type_ve, v);

    let msg = "popping from empty vec";
    match rust_vec {
        TypedMoveBorrowedRustVecMut::Bool(mut v) => { v.pop().expect(msg); }
        TypedMoveBorrowedRustVecMut::U8(mut v) => { v.pop().expect(msg); }
        TypedMoveBorrowedRustVecMut::U16(mut v) => { v.pop().expect(msg); }
        TypedMoveBorrowedRustVecMut::U32(mut v) => { v.pop().expect(msg); }
        TypedMoveBorrowedRustVecMut::U64(mut v) => { v.pop().expect(msg); }
        TypedMoveBorrowedRustVecMut::U128(mut v) => { v.pop().expect(msg); }
        TypedMoveBorrowedRustVecMut::U256(mut v) => { v.pop().expect(msg); }
        TypedMoveBorrowedRustVecMut::Address(mut v) => { v.pop().expect(msg);}
        TypedMoveBorrowedRustVecMut::Signer(mut v) => { v.pop().expect(msg); }
        TypedMoveBorrowedRustVecMut::Vector(_t, mut v) => { v.pop().expect(msg); }
        TypedMoveBorrowedRustVecMut::Struct(mut v) => { todo!(); }
        TypedMoveBorrowedRustVecMut::Reference(_t, mut v) => { v.pop().expect(msg); }
    };
}

#[export_name = "move_rt_vec_cmp_eq"]
unsafe fn vec_cmp_eq(type_ve: &MoveType, v1: &MoveUntypedVector, v2: &MoveUntypedVector) -> bool {
    use crate::conv::borrow_move_vec_as_rust_vec;
    use ethnum::U256;
    use crate::rt_types::TypeDesc;
    use crate::std::vector as V;
    use core::ops::Deref;

    let v1_len = V::length(type_ve, v1);
    let v2_len = V::length(type_ve, v2);

    if v1_len != v2_len {
        return false;
    }

    let is_eq = match type_ve.type_desc {
        TypeDesc::Bool => {
            let mut rv1 = borrow_move_vec_as_rust_vec::<bool>(v1);
            let mut rv2 = borrow_move_vec_as_rust_vec::<bool>(v2);
            rv1.deref().eq(rv2.deref())
        }
        TypeDesc::U8 => {
            let mut rv1 = borrow_move_vec_as_rust_vec::<u8>(v1);
            let mut rv2 = borrow_move_vec_as_rust_vec::<u8>(v2);
            rv1.deref().eq(rv2.deref())
        }
        TypeDesc::U16 => {
            let mut rv1 = borrow_move_vec_as_rust_vec::<u16>(v1);
            let mut rv2 = borrow_move_vec_as_rust_vec::<u16>(v2);
            rv1.deref().eq(rv2.deref())
        }
        TypeDesc::U32 => {
            let mut rv1 = borrow_move_vec_as_rust_vec::<u32>(v1);
            let mut rv2 = borrow_move_vec_as_rust_vec::<u32>(v2);
            rv1.deref().eq(rv2.deref())
        }
        TypeDesc::U64 => {
            let mut rv1 = borrow_move_vec_as_rust_vec::<u64>(v1);
            let mut rv2 = borrow_move_vec_as_rust_vec::<u64>(v2);
            rv1.deref().eq(rv2.deref())
        }
        TypeDesc::U128 => {
            let mut rv1 = borrow_move_vec_as_rust_vec::<u128>(v1);
            let mut rv2 = borrow_move_vec_as_rust_vec::<u128>(v2);
            rv1.deref().eq(rv2.deref())
        }
        TypeDesc::U256 => {
            let mut rv1 = borrow_move_vec_as_rust_vec::<U256>(v1);
            let mut rv2 = borrow_move_vec_as_rust_vec::<U256>(v2);
            rv1.deref().eq(rv2.deref())
        }
        TypeDesc::Address => {
            let mut rv1 = borrow_move_vec_as_rust_vec::<MoveAddress>(v1);
            let mut rv2 = borrow_move_vec_as_rust_vec::<MoveAddress>(v2);
            rv1.deref().eq(rv2.deref())
        }
        TypeDesc::Signer => {
            let mut rv1 = borrow_move_vec_as_rust_vec::<MoveSigner>(v1);
            let mut rv2 = borrow_move_vec_as_rust_vec::<MoveSigner>(v2);
            rv1.deref().eq(rv2.deref())
        }
        TypeDesc::Vector => {
            assert!(v1_len == v2_len, "unexpected vec cmp lengths");
            let inner_element_type = *(*type_ve.type_info).vector.element_type;
            let mut tmp_result = true;
            for i in 0..v1_len {
                let anyval_ref1 = V::borrow(type_ve, v1, i);
                let anyval_ref2 = V::borrow(type_ve, v2, i);
                let mv_ut_vec1 = &*(anyval_ref1 as *const AnyValue as *const MoveUntypedVector);
                let mv_ut_vec2 = &*(anyval_ref2 as *const AnyValue as *const MoveUntypedVector);
                tmp_result = vec_cmp_eq(&inner_element_type, mv_ut_vec1, mv_ut_vec2);
                if !tmp_result { break; }
            }
            tmp_result
        }
        TypeDesc::Struct => {
            assert!(v1_len == v2_len, "unexpected vec cmp lengths");
            let mut tmp_result = true;
            for i in 0..v1_len {
                let anyval_ref1 = V::borrow(type_ve, v1, i);
                let anyval_ref2 = V::borrow(type_ve, v2, i);
                tmp_result = struct_cmp_eq(type_ve, anyval_ref1, anyval_ref2);
                if !tmp_result { break; }
            }
            tmp_result
        }
        _ => todo!("vec_cmp_eq: unhandled element type: {:?}", type_ve.type_desc)
    };
    is_eq
}

#[export_name = "move_rt_struct_cmp_eq"]
unsafe fn struct_cmp_eq(type_ve: &MoveType, s1: &AnyValue, s2: &AnyValue) -> bool {
    use crate::conv::walk_struct_fields;
    use crate::conv::{BorrowedTypedMoveValue as BTMV, borrow_move_value_as_rust_value};

    let st_info = (*(type_ve.type_info)).struct_;
    let fields1 = walk_struct_fields(&st_info, s1);
    let fields2 = walk_struct_fields(&st_info, s2);
    for ((fld_ty1, fld_ref1, fld_name1), (fld_ty2, fld_ref2, fld_name2)) in Iterator::zip(fields1, fields2) {
        let rv1 = borrow_move_value_as_rust_value(fld_ty1, fld_ref1);
        let rv2 = borrow_move_value_as_rust_value(fld_ty2, fld_ref2);

        let is_eq = match (rv1, rv2) {
            (BTMV::Bool(val1), BTMV::Bool(val2)) => { val1 == val2  }
            (BTMV::U8(val1), BTMV::U8(val2)) => { val1 == val2  }
            (BTMV::U16(val1), BTMV::U16(val2)) => { val1 == val2  }
            (BTMV::U32(val1), BTMV::U32(val2)) => { val1 == val2  }
            (BTMV::U64(val1), BTMV::U64(val2)) => { val1 == val2  }
            (BTMV::U128(val1), BTMV::U128(val2)) => { val1 == val2  }
            (BTMV::U256(val1), BTMV::U256(val2)) => { val1 == val2  }
            (BTMV::Address(val1), BTMV::Address(val2)) => { val1 == val2  }
            (BTMV::Signer(val1), BTMV::Signer(val2)) => { val1 == val2  }
            (BTMV::Vector(t1, utv1), BTMV::Vector(_t2, utv2)) => {
                vec_cmp_eq(&t1, utv1, utv2)
            }
            (BTMV::Struct(t1, anyv1), BTMV::Struct(_t2, anyv2)) => {
                struct_cmp_eq(&t1, anyv1, anyv2)
            }
            (BTMV::Reference(_, _), BTMV::Reference(_, _)) => {
                unreachable!("reference in struct field impossible")
            }
            _ => { unreachable!("struct_cmp_eq unexpected value combination") }
        };

        if !is_eq { return false }
    }
    true
}
