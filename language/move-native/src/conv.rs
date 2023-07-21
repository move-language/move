// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::rt_types::*;
use alloc::vec::Vec;
use core::{
    marker::PhantomData,
    mem,
    ops::{Deref, DerefMut},
};

/// This is a placeholder for the unstable `ptr::invalid_mut`.
///
/// It is a potential future way to create invalid pointers, which is
/// required for correctly initializing empty vectors.
///
/// This crate initializes empty vectors knowing only the alignment of their
/// elements, but not the full type.
#[allow(clippy::useless_transmute)]
pub const fn invalid_mut<T>(addr: usize) -> *mut T {
    // FIXME(strict_provenance_magic): I am magic and should be a compiler intrinsic.
    // We use transmute rather than a cast so tools like Miri can tell that this
    // is *not* the same as from_exposed_addr.
    // SAFETY: every valid integer is also a valid pointer (as long as you don't dereference that
    // pointer).
    unsafe { mem::transmute(addr) }
}

pub unsafe fn move_byte_vec_to_rust_vec(mv: MoveByteVector) -> Vec<u8> {
    let ret = MoveUntypedVector {
        ptr: mv.ptr,
        capacity: mv.capacity,
        length: mv.length,
    };
    move_vec_to_rust_vec(ret)
}

pub fn rust_vec_to_move_byte_vec(rv: Vec<u8>) -> MoveByteVector {
    let mv = rust_vec_to_move_vec(rv);
    MoveByteVector {
        ptr: mv.ptr,
        capacity: mv.capacity,
        length: mv.length,
    }
}

pub fn borrow_move_byte_vec_as_rust_vec<'mv>(
    mv: &'mv MoveByteVector,
) -> MoveBorrowedRustVec<'mv, u8> {
    assert_eq!(
        mem::size_of::<MoveByteVector>(),
        mem::size_of::<MoveUntypedVector>()
    );
    assert_eq!(
        mem::align_of::<MoveByteVector>(),
        mem::align_of::<MoveUntypedVector>()
    );
    // Safety: both repr(c) with same layout, probably ok
    let mv: &'mv MoveUntypedVector = unsafe { mem::transmute(mv) };
    unsafe { borrow_move_vec_as_rust_vec(mv) }
}

pub unsafe fn move_vec_to_rust_vec<T>(mv: MoveUntypedVector) -> Vec<T> {
    Vec::from_raw_parts(
        mv.ptr as *mut T,
        usize::try_from(mv.length).expect("overflow"),
        usize::try_from(mv.capacity).expect("overflow"),
    )
}

pub fn rust_vec_to_move_vec<T>(mut rv: Vec<T>) -> MoveUntypedVector {
    let mv = MoveUntypedVector {
        ptr: rv.as_mut_ptr() as *mut u8,
        capacity: u64::try_from(rv.capacity()).expect("overflow"),
        length: u64::try_from(rv.len()).expect("overflow"),
    };
    mem::forget(rv);
    mv
}

pub unsafe fn borrow_move_vec_as_rust_vec<T>(mv: &MoveUntypedVector) -> MoveBorrowedRustVec<'_, T> {
    let rv = Vec::from_raw_parts(
        mv.ptr as *mut T,
        usize::try_from(mv.length).expect("overflow"),
        usize::try_from(mv.capacity).expect("overflow"),
    );
    MoveBorrowedRustVec {
        inner: rv,
        _lifetime: PhantomData,
    }
}

pub unsafe fn borrow_move_vec_as_rust_vec_mut<T>(
    mv: &mut MoveUntypedVector,
) -> MoveBorrowedRustVecMut<'_, T> {
    let rv = Vec::from_raw_parts(
        mv.ptr as *mut T,
        usize::try_from(mv.length).expect("overflow"),
        usize::try_from(mv.capacity).expect("overflow"),
    );
    MoveBorrowedRustVecMut {
        inner: rv,
        original: mv,
    }
}

pub struct MoveBorrowedRustVec<'mv, T> {
    inner: Vec<T>,
    _lifetime: PhantomData<&'mv ()>,
}

#[derive(Debug)]
pub struct MoveBorrowedRustVecMut<'mv, T> {
    inner: Vec<T>,
    original: &'mv mut MoveUntypedVector,
}

impl<'mv, T> Drop for MoveBorrowedRustVec<'mv, T> {
    fn drop(&mut self) {
        let rv = mem::take(&mut self.inner);
        mem::forget(rv);
    }
}

impl<'mv, T> Drop for MoveBorrowedRustVecMut<'mv, T> {
    fn drop(&mut self) {
        let mut rv = mem::take(&mut self.inner);

        self.original.length = u64::try_from(rv.len()).expect("overflow");
        self.original.capacity = u64::try_from(rv.capacity()).expect("overflow");
        self.original.ptr = rv.as_mut_ptr() as *mut u8;

        mem::forget(rv);
    }
}

impl<'mv, T> Deref for MoveBorrowedRustVec<'mv, T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'mv, T> Deref for MoveBorrowedRustVecMut<'mv, T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'mv, T> DerefMut for MoveBorrowedRustVecMut<'mv, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

/// A vector of Move structs.
///
/// Since we can't instantiate Move structs as Rust structs, this is a
/// container that unsafely implements exactly the ops needed to deal with
/// Move's `vector<T>`.
#[derive(Debug)]
pub struct MoveBorrowedRustVecOfStruct<'mv> {
    pub inner: &'mv MoveUntypedVector,
    pub name: StaticTypeName,
    pub type_: &'mv StructTypeInfo,
}

#[derive(Debug)]
pub struct MoveBorrowedRustVecOfStructMut<'mv> {
    pub inner: &'mv mut MoveUntypedVector,
    pub name: StaticTypeName,
    pub type_: &'mv StructTypeInfo,
}

pub enum BorrowedTypedMoveValue<'mv> {
    Bool(&'mv bool),
    U8(&'mv u8),
    U16(&'mv u16),
    U32(&'mv u32),
    U64(&'mv u64),
    U128(&'mv u128),
    U256(&'mv U256),
    Address(&'mv MoveAddress),
    Signer(&'mv MoveSigner),
    Vector(MoveType, &'mv MoveUntypedVector),
    Struct(MoveType, &'mv AnyValue),
    Reference(MoveType, &'mv MoveUntypedReference),
    // todo
}

pub unsafe fn borrow_move_value_as_rust_value<'mv>(
    type_: &MoveType,
    value: &'mv AnyValue,
) -> BorrowedTypedMoveValue<'mv> {
    // todo need to think about the soundness of this transmute
    match type_.type_desc {
        TypeDesc::Bool => BorrowedTypedMoveValue::Bool(mem::transmute(value)),
        TypeDesc::U8 => BorrowedTypedMoveValue::U8(mem::transmute(value)),
        TypeDesc::U16 => BorrowedTypedMoveValue::U16(mem::transmute(value)),
        TypeDesc::U32 => BorrowedTypedMoveValue::U32(mem::transmute(value)),
        TypeDesc::U64 => BorrowedTypedMoveValue::U64(mem::transmute(value)),
        TypeDesc::U128 => BorrowedTypedMoveValue::U128(mem::transmute(value)),
        TypeDesc::U256 => BorrowedTypedMoveValue::U256(mem::transmute(value)),
        TypeDesc::Address => BorrowedTypedMoveValue::Address(mem::transmute(value)),
        TypeDesc::Signer => BorrowedTypedMoveValue::Signer(mem::transmute(value)),
        TypeDesc::Vector => {
            let element_type = *(*type_.type_info).vector.element_type;
            let move_ref = mem::transmute(value);
            BorrowedTypedMoveValue::Vector(element_type, move_ref)
        }
        TypeDesc::Struct => {
            // Previously we stored the StructTypeInfo here. But passing the enclosing
            // MoveType instead gives routines access to the struct name (i.e., more
            // context). Otherwise we would need an uplevel pointer in StructTypeInfo or
            // to redundantly store the name there.
            BorrowedTypedMoveValue::Struct(*type_, value)
        }
        TypeDesc::Reference => {
            let element_type = *(*type_.type_info).reference.element_type;
            let move_ref = mem::transmute(value);
            BorrowedTypedMoveValue::Reference(element_type, move_ref)
        }
    }
}

/// The same as `BorrowedTypedMoveValue` but with raw pointers.
///
/// Allows for uninitialized values.
pub enum RawBorrowedTypedMoveValue {
    Bool(*mut bool),
    U8(*mut u8),
    U16(*mut u16),
    U32(*mut u32),
    U64(*mut u64),
    U128(*mut u128),
    U256(*mut U256),
    Address(*mut MoveAddress),
    Signer(*mut MoveSigner),
    Vector(MoveType, *mut MoveUntypedVector),
    Struct(MoveType, *mut AnyValue),
    Reference(MoveType, *mut MoveUntypedReference),
}

pub unsafe fn raw_borrow_move_value_as_rust_value(
    type_: &MoveType,
    value: *mut AnyValue,
) -> RawBorrowedTypedMoveValue {
    match type_.type_desc {
        TypeDesc::Bool => RawBorrowedTypedMoveValue::Bool(mem::transmute(value)),
        TypeDesc::U8 => RawBorrowedTypedMoveValue::U8(mem::transmute(value)),
        TypeDesc::U16 => RawBorrowedTypedMoveValue::U16(mem::transmute(value)),
        TypeDesc::U32 => RawBorrowedTypedMoveValue::U32(mem::transmute(value)),
        TypeDesc::U64 => RawBorrowedTypedMoveValue::U64(mem::transmute(value)),
        TypeDesc::U128 => RawBorrowedTypedMoveValue::U128(mem::transmute(value)),
        TypeDesc::U256 => RawBorrowedTypedMoveValue::U256(mem::transmute(value)),
        TypeDesc::Address => RawBorrowedTypedMoveValue::Address(mem::transmute(value)),
        TypeDesc::Signer => RawBorrowedTypedMoveValue::Signer(mem::transmute(value)),
        TypeDesc::Vector => {
            let element_type = *(*type_.type_info).vector.element_type;
            let move_ref = mem::transmute(value);
            RawBorrowedTypedMoveValue::Vector(element_type, move_ref)
        }
        TypeDesc::Struct => RawBorrowedTypedMoveValue::Struct(*type_, value),
        TypeDesc::Reference => {
            let element_type = *(*type_.type_info).reference.element_type;
            let move_ref = mem::transmute(value);
            RawBorrowedTypedMoveValue::Reference(element_type, move_ref)
        }
    }
}

pub enum TypedMoveBorrowedRustVec<'mv> {
    Bool(MoveBorrowedRustVec<'mv, bool>),
    U8(MoveBorrowedRustVec<'mv, u8>),
    U16(MoveBorrowedRustVec<'mv, u16>),
    U32(MoveBorrowedRustVec<'mv, u32>),
    U64(MoveBorrowedRustVec<'mv, u64>),
    U128(MoveBorrowedRustVec<'mv, u128>),
    U256(MoveBorrowedRustVec<'mv, U256>),
    Address(MoveBorrowedRustVec<'mv, MoveAddress>),
    Signer(MoveBorrowedRustVec<'mv, MoveSigner>),
    Vector(MoveType, MoveBorrowedRustVec<'mv, MoveUntypedVector>),
    Struct(MoveBorrowedRustVecOfStruct<'mv>),
    Reference(MoveType, MoveBorrowedRustVec<'mv, MoveUntypedReference>),
    // todo
}

#[derive(Debug)]
pub enum TypedMoveBorrowedRustVecMut<'mv> {
    Bool(MoveBorrowedRustVecMut<'mv, bool>),
    U8(MoveBorrowedRustVecMut<'mv, u8>),
    U16(MoveBorrowedRustVecMut<'mv, u16>),
    U32(MoveBorrowedRustVecMut<'mv, u32>),
    U64(MoveBorrowedRustVecMut<'mv, u64>),
    U128(MoveBorrowedRustVecMut<'mv, u128>),
    U256(MoveBorrowedRustVecMut<'mv, U256>),
    Address(MoveBorrowedRustVecMut<'mv, MoveAddress>),
    Signer(MoveBorrowedRustVecMut<'mv, MoveSigner>),
    Vector(MoveType, MoveBorrowedRustVecMut<'mv, MoveUntypedVector>),
    Struct(MoveBorrowedRustVecOfStructMut<'mv>),
    Reference(MoveType, MoveBorrowedRustVecMut<'mv, MoveUntypedReference>),
    // todo
}

pub unsafe fn borrow_typed_move_vec_as_rust_vec<'mv>(
    type_: &'mv MoveType,
    mv: &'mv MoveUntypedVector,
) -> TypedMoveBorrowedRustVec<'mv> {
    match type_.type_desc {
        TypeDesc::Bool => TypedMoveBorrowedRustVec::Bool(borrow_move_vec_as_rust_vec::<bool>(mv)),
        TypeDesc::U8 => TypedMoveBorrowedRustVec::U8(borrow_move_vec_as_rust_vec::<u8>(mv)),
        TypeDesc::U16 => TypedMoveBorrowedRustVec::U16(borrow_move_vec_as_rust_vec::<u16>(mv)),
        TypeDesc::U32 => TypedMoveBorrowedRustVec::U32(borrow_move_vec_as_rust_vec::<u32>(mv)),
        TypeDesc::U64 => TypedMoveBorrowedRustVec::U64(borrow_move_vec_as_rust_vec::<u64>(mv)),
        TypeDesc::U128 => TypedMoveBorrowedRustVec::U128(borrow_move_vec_as_rust_vec::<u128>(mv)),
        TypeDesc::U256 => TypedMoveBorrowedRustVec::U256(borrow_move_vec_as_rust_vec::<U256>(mv)),
        TypeDesc::Address => {
            TypedMoveBorrowedRustVec::Address(borrow_move_vec_as_rust_vec::<MoveAddress>(mv))
        }
        TypeDesc::Signer => {
            TypedMoveBorrowedRustVec::Signer(borrow_move_vec_as_rust_vec::<MoveSigner>(mv))
        }
        TypeDesc::Vector => TypedMoveBorrowedRustVec::Vector(
            *(*type_.type_info).vector.element_type,
            borrow_move_vec_as_rust_vec::<MoveUntypedVector>(mv),
        ),
        TypeDesc::Struct => TypedMoveBorrowedRustVec::Struct(MoveBorrowedRustVecOfStruct {
            inner: mv,
            name: type_.name,
            type_: &(*type_.type_info).struct_,
        }),
        TypeDesc::Reference => TypedMoveBorrowedRustVec::Reference(
            *(*type_.type_info).reference.element_type,
            borrow_move_vec_as_rust_vec::<MoveUntypedReference>(mv),
        ),
    }
}

pub unsafe fn borrow_typed_move_vec_as_rust_vec_mut<'mv>(
    type_: &'mv MoveType,
    mv: &'mv mut MoveUntypedVector,
) -> TypedMoveBorrowedRustVecMut<'mv> {
    match type_.type_desc {
        TypeDesc::Bool => {
            TypedMoveBorrowedRustVecMut::Bool(borrow_move_vec_as_rust_vec_mut::<bool>(mv))
        }
        TypeDesc::U8 => TypedMoveBorrowedRustVecMut::U8(borrow_move_vec_as_rust_vec_mut::<u8>(mv)),
        TypeDesc::U16 => {
            TypedMoveBorrowedRustVecMut::U16(borrow_move_vec_as_rust_vec_mut::<u16>(mv))
        }
        TypeDesc::U32 => {
            TypedMoveBorrowedRustVecMut::U32(borrow_move_vec_as_rust_vec_mut::<u32>(mv))
        }
        TypeDesc::U64 => {
            TypedMoveBorrowedRustVecMut::U64(borrow_move_vec_as_rust_vec_mut::<u64>(mv))
        }
        TypeDesc::U128 => {
            TypedMoveBorrowedRustVecMut::U128(borrow_move_vec_as_rust_vec_mut::<u128>(mv))
        }
        TypeDesc::U256 => {
            TypedMoveBorrowedRustVecMut::U256(borrow_move_vec_as_rust_vec_mut::<U256>(mv))
        }
        TypeDesc::Address => {
            TypedMoveBorrowedRustVecMut::Address(borrow_move_vec_as_rust_vec_mut::<MoveAddress>(mv))
        }
        TypeDesc::Signer => {
            TypedMoveBorrowedRustVecMut::Signer(borrow_move_vec_as_rust_vec_mut::<MoveSigner>(mv))
        }
        TypeDesc::Vector => TypedMoveBorrowedRustVecMut::Vector(
            *(*type_.type_info).vector.element_type,
            borrow_move_vec_as_rust_vec_mut::<MoveUntypedVector>(mv),
        ),
        TypeDesc::Struct => TypedMoveBorrowedRustVecMut::Struct(MoveBorrowedRustVecOfStructMut {
            inner: mv,
            name: type_.name,
            type_: &(*type_.type_info).struct_,
        }),
        TypeDesc::Reference => TypedMoveBorrowedRustVecMut::Reference(
            *(*type_.type_info).reference.element_type,
            borrow_move_vec_as_rust_vec_mut::<MoveUntypedReference>(mv),
        ),
    }
}

impl<'mv> core::fmt::Debug for BorrowedTypedMoveValue<'mv> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            BorrowedTypedMoveValue::Bool(v) => v.fmt(f),
            BorrowedTypedMoveValue::U8(v) => v.fmt(f),
            BorrowedTypedMoveValue::U16(v) => v.fmt(f),
            BorrowedTypedMoveValue::U32(v) => v.fmt(f),
            BorrowedTypedMoveValue::U64(v) => v.fmt(f),
            BorrowedTypedMoveValue::U128(v) => v.fmt(f),
            BorrowedTypedMoveValue::U256(v) => v.fmt(f),
            BorrowedTypedMoveValue::Address(v) => v.fmt(f),
            BorrowedTypedMoveValue::Signer(v) => v.fmt(f),
            BorrowedTypedMoveValue::Vector(t, v) => unsafe {
                let rv = borrow_typed_move_vec_as_rust_vec(t, v);
                rv.fmt(f)
            },
            BorrowedTypedMoveValue::Struct(t, v) => unsafe {
                let st = (*(t.type_info)).struct_;
                write!(f, "{} {{ ", t.name.as_ascii_str())?;
                let fields = crate::structs::walk_fields(&st, v);
                for (type_, ref_, fld_name) in fields {
                    let rv = borrow_move_value_as_rust_value(type_, ref_);
                    write!(f, "{}: ", fld_name.as_ascii_str())?;
                    rv.fmt(f)?;
                    f.write_str(", ")?;
                }
                f.write_str("}")?;
                Ok(())
            },
            BorrowedTypedMoveValue::Reference(t, v) => unsafe {
                let rv = borrow_move_value_as_rust_value(t, &*v.0);
                rv.fmt(f)
            },
        }
    }
}

impl<'mv> core::fmt::Debug for TypedMoveBorrowedRustVec<'mv> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            TypedMoveBorrowedRustVec::Bool(v) => v.fmt(f),
            TypedMoveBorrowedRustVec::U8(v) => v.fmt(f),
            TypedMoveBorrowedRustVec::U16(v) => v.fmt(f),
            TypedMoveBorrowedRustVec::U32(v) => v.fmt(f),
            TypedMoveBorrowedRustVec::U64(v) => v.fmt(f),
            TypedMoveBorrowedRustVec::U128(v) => v.fmt(f),
            TypedMoveBorrowedRustVec::U256(v) => v.fmt(f),
            TypedMoveBorrowedRustVec::Address(v) => v.fmt(f),
            TypedMoveBorrowedRustVec::Signer(v) => v.fmt(f),
            TypedMoveBorrowedRustVec::Vector(t, v) => {
                let mut dbg = f.debug_list();
                for e in v.iter() {
                    unsafe {
                        let e = borrow_typed_move_vec_as_rust_vec(t, e);
                        dbg.entry(&e);
                    }
                }
                dbg.finish()
            }
            TypedMoveBorrowedRustVec::Struct(s) => {
                f.write_str("[")?;
                unsafe {
                    for vref in s.iter() {
                        let type_ = MoveType {
                            name: s.name,
                            type_desc: TypeDesc::Struct,
                            type_info: &TypeInfo { struct_: *s.type_ },
                        };
                        let e = borrow_move_value_as_rust_value(&type_, vref);
                        e.fmt(f)?;
                        f.write_str(", ")?;
                    }
                }
                f.write_str("]")?;
                Ok(())
            }
            TypedMoveBorrowedRustVec::Reference(t, v) => {
                let mut dbg = f.debug_list();
                for e in v.iter() {
                    unsafe {
                        let e = borrow_move_value_as_rust_value(t, &*e.0);
                        dbg.entry(&e);
                    }
                }
                dbg.finish()
            }
        }
    }
}
