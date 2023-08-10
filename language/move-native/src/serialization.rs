// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{conv::*, rt_types::*, vector::*};
use alloc::vec::Vec;
use borsh::{BorshDeserialize, BorshSerialize};
use core::ptr;

fn borsh_to_buf<T: BorshSerialize>(v: &T, buf: &mut Vec<u8>) {
    borsh::to_writer(buf, v).expect("serialization failure")
}

fn borsh_from_slice<T: BorshDeserialize>(buf: &mut &[u8]) -> T {
    BorshDeserialize::deserialize(buf).expect("deserialization failure")
}

pub unsafe fn serialize(type_v: &MoveType, v: &AnyValue) -> MoveByteVector {
    let mut buf = Vec::new();
    serialize_to_buf(type_v, v, &mut buf);
    MoveByteVector::from_rust_vec(buf)
}

unsafe fn serialize_to_buf(type_v: &MoveType, v: &AnyValue, buf: &mut Vec<u8>) {
    let v = borrow_move_value_as_rust_value(type_v, v);
    match v {
        BorrowedTypedMoveValue::Bool(v) => {
            borsh_to_buf(v, buf);
        }
        BorrowedTypedMoveValue::U8(v) => {
            borsh_to_buf(v, buf);
        }
        BorrowedTypedMoveValue::U16(v) => {
            borsh_to_buf(v, buf);
        }
        BorrowedTypedMoveValue::U32(v) => {
            borsh_to_buf(v, buf);
        }
        BorrowedTypedMoveValue::U64(v) => {
            borsh_to_buf(v, buf);
        }
        BorrowedTypedMoveValue::U128(v) => {
            borsh_to_buf(v, buf);
        }
        BorrowedTypedMoveValue::U256(v) => {
            borsh_to_buf(v, buf);
        }
        BorrowedTypedMoveValue::Address(v) => {
            borsh_to_buf(v, buf);
        }
        BorrowedTypedMoveValue::Signer(v) => {
            borsh_to_buf(v, buf);
        }
        BorrowedTypedMoveValue::Vector(t, v) => {
            serialize_vector(&t, v, buf);
        }
        BorrowedTypedMoveValue::Struct(t, v) => {
            serialize_struct(&t, v, buf);
        }
        BorrowedTypedMoveValue::Reference(_, _) => {
            todo!("impossible case?");
        }
    };
}

pub unsafe fn deserialize(type_v: &MoveType, bytes: &MoveByteVector, v: *mut AnyValue) {
    let bytes = bytes.as_rust_vec();
    let bytes = &mut &bytes[..];
    deserialize_from_slice(type_v, bytes, v);
    assert!(bytes.is_empty());
}

unsafe fn deserialize_from_slice(type_v: &MoveType, bytes: &mut &[u8], v: *mut AnyValue) {
    // These writes are to uninitialized memory.
    // Using `ptr::write` guarantees that the destination is never read,
    // which can happen if the type has destructors.
    let v = raw_borrow_move_value_as_rust_value(type_v, v);
    match v {
        RawBorrowedTypedMoveValue::Bool(vptr) => {
            let v = borsh_from_slice(bytes);
            ptr::write(vptr, v);
        }
        RawBorrowedTypedMoveValue::U8(vptr) => {
            let v = borsh_from_slice(bytes);
            ptr::write(vptr, v);
        }
        RawBorrowedTypedMoveValue::U16(vptr) => {
            let v = borsh_from_slice(bytes);
            ptr::write(vptr, v);
        }
        RawBorrowedTypedMoveValue::U32(vptr) => {
            let v = borsh_from_slice(bytes);
            ptr::write(vptr, v);
        }
        RawBorrowedTypedMoveValue::U64(vptr) => {
            let v = borsh_from_slice(bytes);
            ptr::write(vptr, v);
        }
        RawBorrowedTypedMoveValue::U128(vptr) => {
            let v = borsh_from_slice(bytes);
            ptr::write(vptr, v);
        }
        RawBorrowedTypedMoveValue::U256(vptr) => {
            let v = borsh_from_slice(bytes);
            ptr::write(vptr, v);
        }
        RawBorrowedTypedMoveValue::Address(vptr) => {
            let v = borsh_from_slice(bytes);
            ptr::write(vptr, v);
        }
        RawBorrowedTypedMoveValue::Signer(vptr) => {
            let v = borsh_from_slice(bytes);
            ptr::write(vptr, v);
        }
        RawBorrowedTypedMoveValue::Vector(t, vptr) => {
            let v = deserialize_vector(&t, bytes);
            ptr::write(vptr, v);
        }
        RawBorrowedTypedMoveValue::Struct(t, vptr) => {
            deserialize_struct(&t, bytes, vptr);
        }
        RawBorrowedTypedMoveValue::Reference(_, _) => {
            todo!("impossible case?");
        }
    }
}

unsafe fn serialize_vector(type_elt: &MoveType, v: &MoveUntypedVector, buf: &mut Vec<u8>) {
    let v = TypedMoveBorrowedRustVec::new(type_elt, v);
    match v {
        TypedMoveBorrowedRustVec::Bool(v) => borsh_to_buf(&*v, buf),
        TypedMoveBorrowedRustVec::U8(v) => borsh_to_buf(&*v, buf),
        TypedMoveBorrowedRustVec::U16(v) => borsh_to_buf(&*v, buf),
        TypedMoveBorrowedRustVec::U32(v) => borsh_to_buf(&*v, buf),
        TypedMoveBorrowedRustVec::U64(v) => borsh_to_buf(&*v, buf),
        TypedMoveBorrowedRustVec::U128(v) => borsh_to_buf(&*v, buf),
        TypedMoveBorrowedRustVec::U256(v) => borsh_to_buf(&*v, buf),
        TypedMoveBorrowedRustVec::Address(v) => borsh_to_buf(&*v, buf),
        TypedMoveBorrowedRustVec::Signer(v) => borsh_to_buf(&*v, buf),
        TypedMoveBorrowedRustVec::Vector(t, v) => {
            let len: u32 = v.len().try_into().expect("overlong vector");
            borsh_to_buf(&len, buf);
            for elt in v.iter() {
                serialize_vector(&t, elt, buf);
            }
        }
        TypedMoveBorrowedRustVec::Struct(v) => {
            let len: u32 = v.len().try_into().expect("overlong vector");
            borsh_to_buf(&len, buf);
            for elt in v.iter() {
                serialize_struct(v.type_(), elt, buf);
            }
        }
        TypedMoveBorrowedRustVec::Reference(_, _) => {
            todo!("impossible case?");
        }
    }
}

unsafe fn deserialize_vector(type_elt: &MoveType, bytes: &mut &[u8]) -> MoveUntypedVector {
    let mut mv = MoveUntypedVector::empty(type_elt);
    let mut rv = TypedMoveBorrowedRustVecMut::new(type_elt, &mut mv);
    match &mut rv {
        TypedMoveBorrowedRustVecMut::Bool(v) => {
            **v = borsh_from_slice(bytes);
        }
        TypedMoveBorrowedRustVecMut::U8(v) => {
            **v = borsh_from_slice(bytes);
        }
        TypedMoveBorrowedRustVecMut::U16(v) => {
            **v = borsh_from_slice(bytes);
        }
        TypedMoveBorrowedRustVecMut::U32(v) => {
            **v = borsh_from_slice(bytes);
        }
        TypedMoveBorrowedRustVecMut::U64(v) => {
            **v = borsh_from_slice(bytes);
        }
        TypedMoveBorrowedRustVecMut::U128(v) => {
            **v = borsh_from_slice(bytes);
        }
        TypedMoveBorrowedRustVecMut::U256(v) => {
            **v = borsh_from_slice(bytes);
        }
        TypedMoveBorrowedRustVecMut::Address(v) => {
            **v = borsh_from_slice(bytes);
        }
        TypedMoveBorrowedRustVecMut::Signer(v) => {
            **v = borsh_from_slice(bytes);
        }
        TypedMoveBorrowedRustVecMut::Vector(inner_elt_type, v) => {
            let len: u32 = borsh_from_slice(bytes);
            let len: usize = len as usize;
            v.reserve_exact(len);
            for _ in 0..len {
                let eltv = deserialize_vector(inner_elt_type, bytes);
                v.push(eltv);
            }
        }
        TypedMoveBorrowedRustVecMut::Struct(vs) => {
            let len: u32 = borsh_from_slice(bytes);
            let len: usize = len as usize;
            vs.reserve_exact(len);
            for i in 0..len {
                let eltptr = vs.get_mut_unchecked_raw(i);
                deserialize_struct(type_elt, bytes, eltptr);
            }
            vs.set_length(len);
        }
        TypedMoveBorrowedRustVecMut::Reference(..) => {
            todo!("impossible case?");
        }
    }
    drop(rv);
    mv
}

unsafe fn serialize_struct(t: &MoveType, v: &AnyValue, buf: &mut Vec<u8>) {
    assert_eq!(t.type_desc, TypeDesc::Struct);
    let structinfo = &(*(t.type_info)).struct_;
    for (ft, fv, _) in crate::structs::walk_fields(structinfo, v) {
        serialize_to_buf(ft, fv, buf);
    }
}

unsafe fn deserialize_struct(t: &MoveType, bytes: &mut &[u8], v: *mut AnyValue) {
    assert_eq!(t.type_desc, TypeDesc::Struct);
    let structinfo = &(*(t.type_info)).struct_;
    for (ft, fv, _) in crate::structs::walk_fields_mut(structinfo, v) {
        deserialize_from_slice(ft, bytes, fv);
    }
}
