// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{rt_types::*, vector::TypedMoveBorrowedRustVec};
use core::slice;

pub unsafe fn walk_fields<'mv>(
    info: &'mv StructTypeInfo,
    struct_ref: &'mv AnyValue,
) -> impl DoubleEndedIterator<Item = (&'mv MoveType, &'mv AnyValue, &'mv StaticName)> {
    let field_len = usize::try_from(info.field_array_len).expect("overflow");
    let fields: &'mv [StructFieldInfo] = slice::from_raw_parts(info.field_array_ptr, field_len);

    fields.iter().map(|field| {
        let struct_base_ptr: *const AnyValue = struct_ref as _;
        let field_offset = isize::try_from(field.offset).expect("overflow");
        let field_ptr = struct_base_ptr.offset(field_offset);
        let field_ref: &'mv AnyValue = &*field_ptr;
        (&field.type_, field_ref, &field.name)
    })
}

pub unsafe fn walk_fields_mut<'mv>(
    info: &'mv StructTypeInfo,
    struct_ref: *mut AnyValue,
) -> impl DoubleEndedIterator<Item = (&'mv MoveType, *mut AnyValue, &'mv StaticName)> {
    let field_len = usize::try_from(info.field_array_len).expect("overflow");
    let fields: &'mv [StructFieldInfo] = slice::from_raw_parts(info.field_array_ptr, field_len);

    fields.iter().map(move |field| {
        let struct_base_ptr: *mut AnyValue = struct_ref as _;
        let field_offset = isize::try_from(field.offset).expect("overflow");
        let field_ptr = struct_base_ptr.offset(field_offset);
        (&field.type_, field_ptr, &field.name)
    })
}

pub unsafe fn destroy(info: &StructTypeInfo, struct_ref: *mut AnyValue) {
    // nb: destroying from back to front. Move doesn't
    // have side-effecting dtors so drop order probably doesn't matter.
    // Safety: This may not be panic-safe if destroying an element fails.
    // This module should be compiled with panic=abort.
    for (ty, ptr, _name) in walk_fields_mut(info, struct_ref).rev() {
        match ty.type_desc {
            TypeDesc::Bool
            | TypeDesc::U8
            | TypeDesc::U16
            | TypeDesc::U32
            | TypeDesc::U64
            | TypeDesc::U128
            | TypeDesc::U256
            | TypeDesc::Address
            | TypeDesc::Signer
            | TypeDesc::Reference => { /* nop */ }
            TypeDesc::Vector => {
                let elt_type = (*ty.type_info).vector.element_type;
                let ptr = ptr as *mut MoveUntypedVector;
                // Awkward: MoveUntypedVector::destroy takes by-value self,
                // which make sense in most cases, but which we don't have here.
                // MoveUntypedVector doesn't otherwise need to clonable,
                // and cloning it could be error-prone by making ownership unclear,
                // so this clone is just open-coded.
                let clone = MoveUntypedVector {
                    ptr: (*ptr).ptr,
                    capacity: (*ptr).capacity,
                    length: (*ptr).length,
                };
                // nb: indirect recursive call, possible stack overflow.
                clone.destroy(elt_type);
            }
            TypeDesc::Struct => {
                let struct_type = &(*ty.type_info).struct_;
                // nb: recursive call, possible stack overflow.
                destroy(struct_type, ptr);
            }
        }
    }
}

pub unsafe fn cmp_eq(type_ve: &MoveType, s1: &AnyValue, s2: &AnyValue) -> bool {
    use crate::conv::{borrow_move_value_as_rust_value, BorrowedTypedMoveValue as BTMV};

    let st_info = (*(type_ve.type_info)).struct_;
    let fields1 = walk_fields(&st_info, s1);
    let fields2 = walk_fields(&st_info, s2);
    for ((fld_ty1, fld_ref1, _fld_name1), (fld_ty2, fld_ref2, _fld_name2)) in
        Iterator::zip(fields1, fields2)
    {
        let rv1 = borrow_move_value_as_rust_value(fld_ty1, fld_ref1);
        let rv2 = borrow_move_value_as_rust_value(fld_ty2, fld_ref2);

        let is_eq = match (rv1, rv2) {
            (BTMV::Bool(val1), BTMV::Bool(val2)) => val1 == val2,
            (BTMV::U8(val1), BTMV::U8(val2)) => val1 == val2,
            (BTMV::U16(val1), BTMV::U16(val2)) => val1 == val2,
            (BTMV::U32(val1), BTMV::U32(val2)) => val1 == val2,
            (BTMV::U64(val1), BTMV::U64(val2)) => val1 == val2,
            (BTMV::U128(val1), BTMV::U128(val2)) => val1 == val2,
            (BTMV::U256(val1), BTMV::U256(val2)) => val1 == val2,
            (BTMV::Address(val1), BTMV::Address(val2)) => val1 == val2,
            (BTMV::Signer(val1), BTMV::Signer(val2)) => val1 == val2,
            (BTMV::Vector(t1, utv1), BTMV::Vector(t2, utv2)) => {
                let v1 = TypedMoveBorrowedRustVec::new(&t1, utv1);
                let v2 = TypedMoveBorrowedRustVec::new(&t2, utv2);
                v1.cmp_eq(&v2)
            }
            (BTMV::Struct(t1, anyv1), BTMV::Struct(_t2, anyv2)) => cmp_eq(&t1, anyv1, anyv2),
            (BTMV::Reference(_, _), BTMV::Reference(_, _)) => {
                unreachable!("reference in struct field impossible")
            }
            _ => {
                unreachable!("struct_cmp_eq unexpected value combination")
            }
        };

        if !is_eq {
            return false;
        }
    }
    true
}
