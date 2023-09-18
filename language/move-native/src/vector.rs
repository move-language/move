// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! Operations on Move vectors.
//!
//! Move vectors are temporarily converted to Rust `Vec`s,
//! then operated on by calling the standard `Vec` methods.
//!
//! This module provides the types and encapsulation to do
//! such operations as ergonomically and safely as reasonable.
//!
//!
//! # Vector support types
//!
//! [`MoveUntypedVector`] is the main the vector
//! type exposed to the compiler, with known layout.
//! It is always paired with a [`MoveType`] that describes its elements.
//! It is defined in the [`rt_types`] module, with public fields,
//! and so all operations on it are unsafe.
//!
//! It is created with [`MoveUntypedVector::empty`] and
//! destroyed with [`MoveUntypedVector::destroy_empty`].
//! It is almost always immediately converted to one of the types below.
//!
//! [`MoveByteVector`] is the same as `MoveUntypedVector` but is understood
//! to always contain bytes.
//!
//! This module defines additional types that encapsulate the previous,
//! adding some additional type safety:
//!
//! - [`MoveBorrowedRustVec`] / [`MoveBorrowedRustVecMut`] - these temporarily
//!   treat a `MoveUntypedVector` or `MoveByteVector` as a Rust `Vec`. They
//!   are created with `new` constructors.
//! - [`TypedMoveBorrowedRustVec`] / [`TypedMoveBorrowedRustVecMut`] - enums
//!   that pair a `MoveUntypedVector` with `MoveType`, handling all possible
//!   cases of Move types, sometimes safely. Most uses of Move vectors in the runtime
//!   operate on these types.
//! - [`MoveBorrowedRustVecOfStruct`] / [`MoveBorrowedRustVecOfStructMut`] - types
//!   that unsafely implement vector operations for vectors
//!   of Move structs. These are further encapsulated in the `TypedMoveBorrowedRustVec`
//!   struct case and don't typically need to be used directly.
//!
//!
//! # Safety
//!
//! This module is the safety boundary for all vector types except for `MoveUntypedVector`
//! and `MoveByteVector`. Once constructed unsafely, methods on types like
//! `TypedMoveBorrowedRustVec` can sometimes be declared safe, though doing so requires
//! careful understanding of what they are doing. Making these methods safe is an
//! ongoing project.
//!
//!
//! # Module organization
//!
//! This module is organized thusly:
//!
//! - type definitions
//! - constructors, destructors, conversions, and deref impls
//! - additional operations
//! - Debug impls

use crate::{conv::*, rt_types::*};
use alloc::vec::Vec;
use core::{
    marker::PhantomData,
    mem,
    ops::{Deref, DerefMut},
    ptr,
};

pub struct MoveBorrowedRustVec<'mv, T> {
    inner: Vec<T>,
    _lifetime: PhantomData<&'mv ()>,
}

#[derive(Debug)]
pub struct MoveBorrowedRustVecMut<'mv, T> {
    inner: Vec<T>,
    original: &'mv mut MoveUntypedVector,
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
}

/// A vector of Move structs.
///
/// Since we can't instantiate Move structs as Rust structs, this is a
/// container that unsafely implements exactly the ops needed to deal with
/// Move's `vector<T>`.
#[derive(Debug)]
pub struct MoveBorrowedRustVecOfStruct<'mv> {
    inner: &'mv MoveUntypedVector,
    type_: &'mv StructTypeInfo,
    full_type: &'mv MoveType,
}

#[derive(Debug)]
pub struct MoveBorrowedRustVecOfStructMut<'mv> {
    inner: &'mv mut MoveUntypedVector,
    type_: &'mv StructTypeInfo,
}

impl MoveUntypedVector {
    /// # Safety
    ///
    /// Unsafe because `MoveType`'s fields are public.
    pub unsafe fn empty(type_r: &MoveType) -> MoveUntypedVector {
        match type_r.type_desc {
            TypeDesc::Bool => MoveUntypedVector::from_rust_vec::<bool>(Vec::new()),
            TypeDesc::U8 => MoveUntypedVector::from_rust_vec::<u8>(Vec::new()),
            TypeDesc::U16 => MoveUntypedVector::from_rust_vec::<u16>(Vec::new()),
            TypeDesc::U32 => MoveUntypedVector::from_rust_vec::<u32>(Vec::new()),
            TypeDesc::U64 => MoveUntypedVector::from_rust_vec::<u64>(Vec::new()),
            TypeDesc::U128 => MoveUntypedVector::from_rust_vec::<u128>(Vec::new()),
            TypeDesc::U256 => MoveUntypedVector::from_rust_vec::<U256>(Vec::new()),
            TypeDesc::Address => MoveUntypedVector::from_rust_vec::<MoveAddress>(Vec::new()),
            TypeDesc::Signer => MoveUntypedVector::from_rust_vec::<MoveSigner>(Vec::new()),
            TypeDesc::Vector => {
                // Safety: need correct alignment for the internal vector
                // pointer of the outer vector, which is non-null even for
                // an unallocated vector. `MoveUntypedVector` has the same
                // size and alignment regardless of the type it contains, so
                // no need to interpret the vector type.
                MoveUntypedVector::from_rust_vec::<MoveUntypedVector>(Vec::new())
            }
            TypeDesc::Struct => unsafe {
                // Safety: this gets pretty sketchy, and relies on internal
                // Vec details that probably are not guaranteed. The most
                // _correct_ way to initialize a Vec is to call its
                // constructor.
                //
                // That is pretty tough with a type of any dynamically sized
                // layout, so we're going to munge the pointers ourselves.
                //
                // The critical thing to know about Vec's pointers is:
                //
                // - They must always be aligned correctly
                // - They are _never_ 0, even for empty Vec's, to allow null
                //   pointer optimizations.
                //
                // Vec uses `NonNull::dangling` to create invalid non-null
                // pointers, but that requires a concrete type of the
                // correct alignment. We dig even deeper and use
                // `ptr::invalid_mut`, which is an unstable function from
                // the pointer provenance project. As it is unstable we just
                // duplicate it in our `conv` module until it becomes
                // stable.
                //
                // This should be the only location in this crate where we
                // need to fabricate a pointer from an integer.
                let size = (*type_r.type_info).struct_.size;
                let size = usize::try_from(size).expect("overflow");
                let alignment = (*type_r.type_info).struct_.alignment;
                let alignment = usize::try_from(alignment).expect("overflow");

                assert!(size != 0); // can't handle ZSTs
                assert!(alignment != 0); // must have alignment
                assert!(alignment.is_power_of_two());

                let ptr = invalid_mut::<u8>(alignment);
                MoveUntypedVector {
                    ptr,
                    capacity: 0,
                    length: 0,
                }
            },
            TypeDesc::Reference => {
                MoveUntypedVector::from_rust_vec::<MoveUntypedReference>(Vec::new())
            }
        }
    }

    /// # Safety
    ///
    /// Unsafe because the provided type must be correct.
    pub unsafe fn destroy_empty(self, type_ve: &MoveType) {
        assert_eq!(self.length, 0);
        self.destroy(type_ve);
    }

    /// # Safety
    ///
    /// Unsafe because the provided type must be correct.
    //
    // todo: We should probably just leak the vector instead of
    // destroying it, since the program will terminate shortly anyway,
    // and Move destructors do not have side effects,
    // but for now we are writing the destructor "correctly"
    // so that we are sure all the cases are covered and understood.
    pub unsafe fn destroy(self, type_ve: &MoveType) {
        let v = self;
        match type_ve.type_desc {
            TypeDesc::Bool => drop(v.into_rust_vec::<bool>()),
            TypeDesc::U8 => drop(v.into_rust_vec::<u8>()),
            TypeDesc::U16 => drop(v.into_rust_vec::<u16>()),
            TypeDesc::U32 => drop(v.into_rust_vec::<u32>()),
            TypeDesc::U64 => drop(v.into_rust_vec::<u64>()),
            TypeDesc::U128 => drop(v.into_rust_vec::<u128>()),
            TypeDesc::U256 => drop(v.into_rust_vec::<U256>()),
            TypeDesc::Address => drop(v.into_rust_vec::<MoveAddress>()),
            TypeDesc::Signer => drop(v.into_rust_vec::<MoveSigner>()),
            TypeDesc::Vector => {
                // Safety: As in `empty`, the MoveUntypedVector element should have the
                // same size/alignment regardless of the contained type.
                let mut v = v.into_rust_vec::<MoveUntypedVector>();
                // nb: destroying from back to front. Move doesn't
                // have side-effecting dtors so drop order probably doesn't matter.
                while let Some(v_inner) = v.pop() {
                    let type_inner_elt = (*type_ve.type_info).vector.element_type;
                    // nb: recursive call, possible stack overflow.
                    v_inner.destroy(type_inner_elt);
                }
                drop(v);
            }
            TypeDesc::Struct => {
                // Safety: like in `empty` we want to deallocate here without
                // creating a `Vec` of a concrete type, since handling the
                // alignment would requiring enumerating many types.
                //
                // So here we're just going to free the pointer ourselves,
                // constructing a correct `Layout` value to pass to the
                // allocator.

                let size = (*type_ve.type_info).struct_.size;
                let size = usize::try_from(size).expect("overflow");
                let alignment = (*type_ve.type_info).struct_.alignment;
                let alignment = usize::try_from(alignment).expect("overflow");
                let capacity = usize::try_from(v.capacity).expect("overflow");
                let length = usize::try_from(v.length).expect("overflow");

                assert!(size != 0); // can't handle ZSTs

                // A reverse iterator over the elements.
                // nb: like the vector case above, destroying back to front.
                let elt_ptr_rev_iter = {
                    let start_ptr = v.ptr;
                    let size = isize::try_from(size).expect("overflow");
                    let length = isize::try_from(length).expect("overflow");
                    let end_ptr = start_ptr.offset(size.checked_mul(length).expect("overflow"));
                    let mut ptr = end_ptr;
                    core::iter::from_fn(move || {
                        if ptr > start_ptr {
                            ptr = ptr.offset(-size);
                            Some(ptr)
                        } else {
                            None
                        }
                    })
                };

                // Safety: This may not be panic-safe if destroying an element fails.
                // This module should be compiled with panic=abort.
                for elt_ptr in elt_ptr_rev_iter {
                    let type_inner_elt = &(*type_ve.type_info).struct_;
                    // nb: indirect recursive call, possible stack overflow.
                    crate::structs::destroy(type_inner_elt, elt_ptr as *mut AnyValue);
                }

                if capacity != 0 {
                    let vec_byte_size = capacity.checked_mul(size).expect("overflow");
                    let layout = alloc::alloc::Layout::from_size_align(vec_byte_size, alignment)
                        .expect("bad size or alignment");
                    alloc::alloc::dealloc(v.ptr, layout);
                }
            }
            TypeDesc::Reference => drop(v.into_rust_vec::<MoveUntypedReference>()),
        }
    }

    pub unsafe fn into_rust_vec<T>(self) -> Vec<T> {
        Vec::from_raw_parts(
            self.ptr as *mut T,
            usize::try_from(self.length).expect("overflow"),
            usize::try_from(self.capacity).expect("overflow"),
        )
    }

    pub fn from_rust_vec<T>(mut rv: Vec<T>) -> MoveUntypedVector {
        let mv = MoveUntypedVector {
            ptr: rv.as_mut_ptr() as *mut u8,
            capacity: u64::try_from(rv.capacity()).expect("overflow"),
            length: u64::try_from(rv.len()).expect("overflow"),
        };
        mem::forget(rv);
        mv
    }
}

impl MoveByteVector {
    pub unsafe fn as_rust_vec<'mv>(&'mv self) -> MoveBorrowedRustVec<'mv, u8> {
        assert_eq!(
            mem::size_of::<MoveByteVector>(),
            mem::size_of::<MoveUntypedVector>()
        );
        assert_eq!(
            mem::align_of::<MoveByteVector>(),
            mem::align_of::<MoveUntypedVector>()
        );
        // Safety: both repr(c) with same layout, probably ok
        let mv: &'mv MoveUntypedVector = mem::transmute(self);
        MoveBorrowedRustVec::new(mv)
    }

    pub unsafe fn into_rust_vec(self) -> Vec<u8> {
        let ret = MoveUntypedVector {
            ptr: self.ptr,
            capacity: self.capacity,
            length: self.length,
        };
        ret.into_rust_vec()
    }

    pub fn from_rust_vec(rv: Vec<u8>) -> MoveByteVector {
        let mv = MoveUntypedVector::from_rust_vec(rv);
        MoveByteVector {
            ptr: mv.ptr,
            capacity: mv.capacity,
            length: mv.length,
        }
    }
}

impl<'mv, T> MoveBorrowedRustVec<'mv, T> {
    pub unsafe fn new(mv: &MoveUntypedVector) -> MoveBorrowedRustVec<'_, T> {
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
}

impl<'mv, T> MoveBorrowedRustVecMut<'mv, T> {
    pub unsafe fn new(mv: &mut MoveUntypedVector) -> MoveBorrowedRustVecMut<'_, T> {
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
}

impl<'mv, T> Drop for MoveBorrowedRustVec<'mv, T> {
    fn drop(&mut self) {
        let rv = mem::take(&mut self.inner);

        /* mem::forget takes rv by value so it is no longer in scope to be
        passed to any other function.  We call forget here because
        rv is type Vec, which owns its interior buffer pointer, but
        this Vec was temporarily fabricated from a MoveUntypedVector,
        which actually owns that buffer. So forgetting the Vec quietly
        destroys it without running the Vec destructor - so that the
        original MoveUntypedVector can continue owning that buffer.
        The only reason to call mem::forget is to not run a
        destructor. */

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

impl<'mv> TypedMoveBorrowedRustVec<'mv> {
    // Forced inlining dramatically reduces instruction counts on tests ¯\_(ツ)_/¯
    #[inline(always)]
    pub unsafe fn new(
        type_: &'mv MoveType,
        mv: &'mv MoveUntypedVector,
    ) -> TypedMoveBorrowedRustVec<'mv> {
        match type_.type_desc {
            TypeDesc::Bool => TypedMoveBorrowedRustVec::Bool(MoveBorrowedRustVec::new(mv)),
            TypeDesc::U8 => TypedMoveBorrowedRustVec::U8(MoveBorrowedRustVec::new(mv)),
            TypeDesc::U16 => TypedMoveBorrowedRustVec::U16(MoveBorrowedRustVec::new(mv)),
            TypeDesc::U32 => TypedMoveBorrowedRustVec::U32(MoveBorrowedRustVec::new(mv)),
            TypeDesc::U64 => TypedMoveBorrowedRustVec::U64(MoveBorrowedRustVec::new(mv)),
            TypeDesc::U128 => TypedMoveBorrowedRustVec::U128(MoveBorrowedRustVec::new(mv)),
            TypeDesc::U256 => TypedMoveBorrowedRustVec::U256(MoveBorrowedRustVec::new(mv)),
            TypeDesc::Address => TypedMoveBorrowedRustVec::Address(MoveBorrowedRustVec::new(mv)),
            TypeDesc::Signer => TypedMoveBorrowedRustVec::Signer(MoveBorrowedRustVec::new(mv)),
            TypeDesc::Vector => TypedMoveBorrowedRustVec::Vector(
                *(*type_.type_info).vector.element_type,
                MoveBorrowedRustVec::new(mv),
            ),
            TypeDesc::Struct => {
                TypedMoveBorrowedRustVec::Struct(MoveBorrowedRustVecOfStruct::new(type_, mv))
            }
            TypeDesc::Reference => TypedMoveBorrowedRustVec::Reference(
                *(*type_.type_info).reference.element_type,
                MoveBorrowedRustVec::new(mv),
            ),
        }
    }
}

impl<'mv> TypedMoveBorrowedRustVecMut<'mv> {
    // Forced inlining dramatically reduces instruction counts on tests ¯\_(ツ)_/¯
    #[inline(always)]
    pub unsafe fn new(
        type_: &'mv MoveType,
        mv: &'mv mut MoveUntypedVector,
    ) -> TypedMoveBorrowedRustVecMut<'mv> {
        match type_.type_desc {
            TypeDesc::Bool => TypedMoveBorrowedRustVecMut::Bool(MoveBorrowedRustVecMut::new(mv)),
            TypeDesc::U8 => TypedMoveBorrowedRustVecMut::U8(MoveBorrowedRustVecMut::new(mv)),
            TypeDesc::U16 => TypedMoveBorrowedRustVecMut::U16(MoveBorrowedRustVecMut::new(mv)),
            TypeDesc::U32 => TypedMoveBorrowedRustVecMut::U32(MoveBorrowedRustVecMut::new(mv)),
            TypeDesc::U64 => TypedMoveBorrowedRustVecMut::U64(MoveBorrowedRustVecMut::new(mv)),
            TypeDesc::U128 => TypedMoveBorrowedRustVecMut::U128(MoveBorrowedRustVecMut::new(mv)),
            TypeDesc::U256 => TypedMoveBorrowedRustVecMut::U256(MoveBorrowedRustVecMut::new(mv)),
            TypeDesc::Address => {
                TypedMoveBorrowedRustVecMut::Address(MoveBorrowedRustVecMut::new(mv))
            }
            TypeDesc::Signer => {
                TypedMoveBorrowedRustVecMut::Signer(MoveBorrowedRustVecMut::new(mv))
            }
            TypeDesc::Vector => TypedMoveBorrowedRustVecMut::Vector(
                *(*type_.type_info).vector.element_type,
                MoveBorrowedRustVecMut::new(mv),
            ),
            TypeDesc::Struct => {
                TypedMoveBorrowedRustVecMut::Struct(MoveBorrowedRustVecOfStructMut::new(type_, mv))
            }
            TypeDesc::Reference => TypedMoveBorrowedRustVecMut::Reference(
                *(*type_.type_info).reference.element_type,
                MoveBorrowedRustVecMut::new(mv),
            ),
        }
    }
}

impl<'mv> MoveBorrowedRustVecOfStruct<'mv> {
    unsafe fn new(
        ty: &'mv MoveType,
        mv: &'mv MoveUntypedVector,
    ) -> MoveBorrowedRustVecOfStruct<'mv> {
        assert_eq!(ty.type_desc, TypeDesc::Struct);
        MoveBorrowedRustVecOfStruct {
            inner: mv,
            type_: &(*ty.type_info).struct_,
            full_type: ty,
        }
    }
}

impl<'mv> MoveBorrowedRustVecOfStructMut<'mv> {
    unsafe fn new(
        ty: &'mv MoveType,
        mv: &'mv mut MoveUntypedVector,
    ) -> MoveBorrowedRustVecOfStructMut<'mv> {
        assert_eq!(ty.type_desc, TypeDesc::Struct);
        MoveBorrowedRustVecOfStructMut {
            inner: mv,
            type_: &(*ty.type_info).struct_,
        }
    }
}

impl<'mv> TypedMoveBorrowedRustVec<'mv> {
    pub fn len(&self) -> u64 {
        let len = match self {
            TypedMoveBorrowedRustVec::Bool(v) => v.len(),
            TypedMoveBorrowedRustVec::U8(v) => v.len(),
            TypedMoveBorrowedRustVec::U16(v) => v.len(),
            TypedMoveBorrowedRustVec::U32(v) => v.len(),
            TypedMoveBorrowedRustVec::U64(v) => v.len(),
            TypedMoveBorrowedRustVec::U128(v) => v.len(),
            TypedMoveBorrowedRustVec::U256(v) => v.len(),
            TypedMoveBorrowedRustVec::Address(v) => v.len(),
            TypedMoveBorrowedRustVec::Signer(v) => v.len(),
            TypedMoveBorrowedRustVec::Vector(_t, v) => v.len(),
            TypedMoveBorrowedRustVec::Struct(s) => {
                usize::try_from(s.inner.length).expect("overflow")
            }
            TypedMoveBorrowedRustVec::Reference(_t, v) => v.len(),
        };

        u64::try_from(len).expect("u64")
    }

    pub fn borrow(&self, i: u64) -> &'mv AnyValue {
        unsafe {
            let i = usize::try_from(i).expect("usize");
            let value = match self {
                TypedMoveBorrowedRustVec::Bool(v) => mem::transmute(&v[i]),
                TypedMoveBorrowedRustVec::U8(v) => mem::transmute(&v[i]),
                TypedMoveBorrowedRustVec::U16(v) => mem::transmute(&v[i]),
                TypedMoveBorrowedRustVec::U32(v) => mem::transmute(&v[i]),
                TypedMoveBorrowedRustVec::U64(v) => mem::transmute(&v[i]),
                TypedMoveBorrowedRustVec::U128(v) => mem::transmute(&v[i]),
                TypedMoveBorrowedRustVec::U256(v) => mem::transmute(&v[i]),
                TypedMoveBorrowedRustVec::Address(v) => mem::transmute(&v[i]),
                TypedMoveBorrowedRustVec::Signer(v) => mem::transmute(&v[i]),
                TypedMoveBorrowedRustVec::Vector(_t, v) => mem::transmute(&v[i]),
                TypedMoveBorrowedRustVec::Struct(s) => s.get(i),
                TypedMoveBorrowedRustVec::Reference(_t, v) => mem::transmute(&v[i]),
            };

            value
        }
    }

    /// # Safety
    ///
    /// Unsafe because the struct path doesn't do necessary assertions on field types.
    pub unsafe fn cmp_eq(&self, v2: &TypedMoveBorrowedRustVec) -> bool {
        let v1t = self;
        let v2t = v2;
        let v1_len = v1t.len();
        let v2_len = v2t.len();

        if v1_len != v2_len {
            return false;
        }

        use TypedMoveBorrowedRustVec as V;
        let is_eq = match (v1t, v2t) {
            (V::Bool(rv1), V::Bool(rv2)) => rv1.deref().eq(rv2.deref()),
            (V::U8(rv1), V::U8(rv2)) => rv1.deref().eq(rv2.deref()),
            (V::U16(rv1), V::U16(rv2)) => rv1.deref().eq(rv2.deref()),
            (V::U32(rv1), V::U32(rv2)) => rv1.deref().eq(rv2.deref()),
            (V::U64(rv1), V::U64(rv2)) => rv1.deref().eq(rv2.deref()),
            (V::U128(rv1), V::U128(rv2)) => rv1.deref().eq(rv2.deref()),
            (V::U256(rv1), V::U256(rv2)) => rv1.deref().eq(rv2.deref()),
            (V::Address(rv1), V::Address(rv2)) => rv1.deref().eq(rv2.deref()),
            (V::Signer(rv1), V::Signer(rv2)) => rv1.deref().eq(rv2.deref()),
            (v1t @ V::Vector(elt_t1, _mv1), v2t @ V::Vector(elt_t2, _mv2)) => {
                assert_eq!(elt_t1.type_desc, elt_t2.type_desc);
                assert!(v1_len == v2_len, "unexpected vec cmp lengths");
                let inner_element_type = elt_t1;
                let mut tmp_result = true;
                for i in 0..v1_len {
                    let anyval_ref1 = v1t.borrow(i);
                    let anyval_ref2 = v2t.borrow(i);
                    let mv_ut_vec1 = &*(anyval_ref1 as *const AnyValue as *const MoveUntypedVector);
                    let mv_ut_vec2 = &*(anyval_ref2 as *const AnyValue as *const MoveUntypedVector);
                    let mv_vec1 = TypedMoveBorrowedRustVec::new(inner_element_type, mv_ut_vec1);
                    let mv_vec2 = TypedMoveBorrowedRustVec::new(inner_element_type, mv_ut_vec2);
                    tmp_result = mv_vec1.cmp_eq(&mv_vec2);
                    if !tmp_result {
                        break;
                    }
                }
                tmp_result
            }
            (V::Struct(v1t), V::Struct(v2t)) => {
                assert!(v1_len == v2_len, "unexpected vec cmp lengths");
                let mut tmp_result = true;
                for i in 0..v1_len {
                    let anyval_ref1 = v1t.get(i as usize);
                    let anyval_ref2 = v2t.get(i as usize);
                    tmp_result = crate::structs::cmp_eq(v1t.full_type, anyval_ref1, anyval_ref2);
                    if !tmp_result {
                        break;
                    }
                }
                tmp_result
            }
            _ => todo!("vec_cmp_eq: unhandled element type"),
        };
        is_eq
    }
}

impl<'mv> TypedMoveBorrowedRustVecMut<'mv> {
    pub fn len(&self) -> u64 {
        let len = match self {
            TypedMoveBorrowedRustVecMut::Bool(v) => v.len(),
            TypedMoveBorrowedRustVecMut::U8(v) => v.len(),
            TypedMoveBorrowedRustVecMut::U16(v) => v.len(),
            TypedMoveBorrowedRustVecMut::U32(v) => v.len(),
            TypedMoveBorrowedRustVecMut::U64(v) => v.len(),
            TypedMoveBorrowedRustVecMut::U128(v) => v.len(),
            TypedMoveBorrowedRustVecMut::U256(v) => v.len(),
            TypedMoveBorrowedRustVecMut::Address(v) => v.len(),
            TypedMoveBorrowedRustVecMut::Signer(v) => v.len(),
            TypedMoveBorrowedRustVecMut::Vector(_t, v) => v.len(),
            TypedMoveBorrowedRustVecMut::Struct(s) => {
                usize::try_from(s.inner.length).expect("overflow")
            }
            TypedMoveBorrowedRustVecMut::Reference(_t, v) => v.len(),
        };

        u64::try_from(len).expect("u64")
    }

    pub unsafe fn push_back(&mut self, e: *mut AnyValue) {
        match self {
            TypedMoveBorrowedRustVecMut::Bool(ref mut v) => v.push(ptr::read(e as *const bool)),
            TypedMoveBorrowedRustVecMut::U8(ref mut v) => v.push(ptr::read(e as *const u8)),
            TypedMoveBorrowedRustVecMut::U16(ref mut v) => v.push(ptr::read(e as *const u16)),
            TypedMoveBorrowedRustVecMut::U32(ref mut v) => v.push(ptr::read(e as *const u32)),
            TypedMoveBorrowedRustVecMut::U64(ref mut v) => v.push(ptr::read(e as *const u64)),
            TypedMoveBorrowedRustVecMut::U128(ref mut v) => v.push(ptr::read(e as *const u128)),
            TypedMoveBorrowedRustVecMut::U256(ref mut v) => v.push(ptr::read(e as *const U256)),
            TypedMoveBorrowedRustVecMut::Address(ref mut v) => {
                v.push(ptr::read(e as *const MoveAddress))
            }
            TypedMoveBorrowedRustVecMut::Signer(ref mut v) => {
                v.push(ptr::read(e as *const MoveSigner))
            }
            TypedMoveBorrowedRustVecMut::Vector(_t, ref mut v) => {
                v.push(ptr::read(e as *const MoveUntypedVector))
            }
            TypedMoveBorrowedRustVecMut::Struct(ref mut s) => s.push(e),
            TypedMoveBorrowedRustVecMut::Reference(_t, ref mut v) => {
                v.push(ptr::read(e as *const MoveUntypedReference))
            }
        }
    }

    pub fn borrow_mut(&mut self, i: u64) -> *mut AnyValue {
        unsafe {
            let i = usize::try_from(i).expect("usize");
            match self {
                TypedMoveBorrowedRustVecMut::Bool(ref mut v) => {
                    &mut v[i] as *mut bool as *mut AnyValue
                }
                TypedMoveBorrowedRustVecMut::U8(ref mut v) => &mut v[i] as *mut u8 as *mut AnyValue,
                TypedMoveBorrowedRustVecMut::U16(ref mut v) => {
                    &mut v[i] as *mut u16 as *mut AnyValue
                }
                TypedMoveBorrowedRustVecMut::U32(ref mut v) => {
                    &mut v[i] as *mut u32 as *mut AnyValue
                }
                TypedMoveBorrowedRustVecMut::U64(ref mut v) => {
                    &mut v[i] as *mut u64 as *mut AnyValue
                }
                TypedMoveBorrowedRustVecMut::U128(ref mut v) => {
                    &mut v[i] as *mut u128 as *mut AnyValue
                }
                TypedMoveBorrowedRustVecMut::U256(ref mut v) => {
                    &mut v[i] as *mut U256 as *mut AnyValue
                }
                TypedMoveBorrowedRustVecMut::Address(ref mut v) => {
                    &mut v[i] as *mut MoveAddress as *mut AnyValue
                }
                TypedMoveBorrowedRustVecMut::Signer(ref mut v) => {
                    &mut v[i] as *mut MoveSigner as *mut AnyValue
                }
                TypedMoveBorrowedRustVecMut::Vector(_t, ref mut v) => {
                    &mut v[i] as *mut MoveUntypedVector as *mut AnyValue
                }
                TypedMoveBorrowedRustVecMut::Struct(ref mut s) => s.get_mut(i),
                TypedMoveBorrowedRustVecMut::Reference(_t, ref mut v) => {
                    &mut v[i] as *mut MoveUntypedReference as *mut AnyValue
                }
            }
        }
    }

    pub unsafe fn pop_back(&mut self, r: *mut AnyValue) {
        let msg = "popping from empty vec";
        match self {
            TypedMoveBorrowedRustVecMut::Bool(ref mut v) => {
                ptr::write(r as *mut bool, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::U8(ref mut v) => {
                ptr::write(r as *mut u8, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::U16(ref mut v) => {
                ptr::write(r as *mut u16, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::U32(ref mut v) => {
                ptr::write(r as *mut u32, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::U64(ref mut v) => {
                ptr::write(r as *mut u64, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::U128(ref mut v) => {
                ptr::write(r as *mut u128, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::U256(ref mut v) => {
                ptr::write(r as *mut U256, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::Address(ref mut v) => {
                ptr::write(r as *mut MoveAddress, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::Signer(ref mut v) => {
                ptr::write(r as *mut MoveSigner, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::Vector(_t, ref mut v) => {
                ptr::write(r as *mut MoveUntypedVector, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::Struct(ref mut s) => s.pop_into(r),
            TypedMoveBorrowedRustVecMut::Reference(_t, ref mut v) => {
                ptr::write(r as *mut MoveUntypedReference, v.pop().expect(msg));
            }
        }
    }

    pub fn swap(&mut self, i: u64, j: u64) {
        let i = usize::try_from(i).expect("usize");
        let j = usize::try_from(j).expect("usize");

        match self {
            TypedMoveBorrowedRustVecMut::Bool(ref mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::U8(ref mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::U16(ref mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::U32(ref mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::U64(ref mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::U128(ref mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::U256(ref mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::Address(ref mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::Signer(ref mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::Vector(_t, ref mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::Struct(ref mut v) => unsafe { v.swap(i, j) },
            TypedMoveBorrowedRustVecMut::Reference(_t, ref mut v) => v.swap(i, j),
        }
    }

    fn pop_back_discard(&mut self) {
        let msg = "popping from empty vec";
        match self {
            TypedMoveBorrowedRustVecMut::Bool(ref mut v) => {
                v.pop().expect(msg);
            }
            TypedMoveBorrowedRustVecMut::U8(ref mut v) => {
                v.pop().expect(msg);
            }
            TypedMoveBorrowedRustVecMut::U16(ref mut v) => {
                v.pop().expect(msg);
            }
            TypedMoveBorrowedRustVecMut::U32(ref mut v) => {
                v.pop().expect(msg);
            }
            TypedMoveBorrowedRustVecMut::U64(ref mut v) => {
                v.pop().expect(msg);
            }
            TypedMoveBorrowedRustVecMut::U128(ref mut v) => {
                v.pop().expect(msg);
            }
            TypedMoveBorrowedRustVecMut::U256(ref mut v) => {
                v.pop().expect(msg);
            }
            TypedMoveBorrowedRustVecMut::Address(ref mut v) => {
                v.pop().expect(msg);
            }
            TypedMoveBorrowedRustVecMut::Signer(ref mut v) => {
                v.pop().expect(msg);
            }
            TypedMoveBorrowedRustVecMut::Vector(_t, ref mut v) => {
                // fixme this looks like it leaks the vector elements
                v.pop().expect(msg);
            }
            TypedMoveBorrowedRustVecMut::Struct(ref mut _v) => {
                todo!();
            }
            TypedMoveBorrowedRustVecMut::Reference(_t, ref mut v) => {
                v.pop().expect(msg);
            }
        };
    }

    // Safety: src must have same type.
    pub unsafe fn copy_from(&mut self, srcv: &TypedMoveBorrowedRustVec) {
        let src_len = srcv.len();
        let dst_len = self.len();

        // Drain the destination first.
        for _ in 0..dst_len {
            self.pop_back_discard();
        }

        // Now copy.
        for i in 0..src_len {
            let se = srcv.borrow(i);
            // fixme this is incorrect for vectors and structs containing vectors
            let septr = se as *const AnyValue as *mut AnyValue;
            self.push_back(septr);
        }
    }
}

impl<'mv> MoveBorrowedRustVecOfStruct<'mv> {
    pub fn len(&self) -> usize {
        self.inner.length.try_into().expect("overflow")
    }

    pub fn type_(&self) -> &MoveType {
        self.full_type
    }

    pub unsafe fn iter(&self) -> impl Iterator<Item = &AnyValue> {
        let struct_size = usize::try_from(self.type_.size).expect("overflow");
        let vec_len = usize::try_from(self.inner.length).expect("overflow");
        (0..vec_len).map(move |i| {
            let base_ptr = self.inner.ptr;
            let offset = i.checked_mul(struct_size).expect("overflow");
            let offset = isize::try_from(offset).expect("overflow");
            let element_ptr = base_ptr.offset(offset);
            &*(element_ptr as *const AnyValue)
        })
    }

    pub unsafe fn get(&self, i: usize) -> &'mv AnyValue {
        let struct_size = usize::try_from(self.type_.size).expect("overflow");
        let vec_len = usize::try_from(self.inner.length).expect("overflow");

        if i >= vec_len {
            panic!("index out of bounds");
        }

        let base_ptr = self.inner.ptr;
        let offset = i.checked_mul(struct_size).expect("overflow");
        let offset = isize::try_from(offset).expect("overflow");
        let element_ptr = base_ptr.offset(offset);
        &*(element_ptr as *const AnyValue)
    }
}

impl<'mv> MoveBorrowedRustVecOfStructMut<'mv> {
    pub unsafe fn get_mut(&mut self, i: usize) -> *mut AnyValue {
        let struct_size = usize::try_from(self.type_.size).expect("overflow");
        let vec_len = usize::try_from(self.inner.length).expect("overflow");

        if i >= vec_len {
            panic!("index out of bounds");
        }

        let base_ptr = self.inner.ptr;
        let offset = i.checked_mul(struct_size).expect("overflow");
        let offset = isize::try_from(offset).expect("overflow");
        let element_ptr = base_ptr.offset(offset);
        element_ptr as *mut AnyValue
    }

    /// Get a pointer to a possibly-uninitialized element.
    pub unsafe fn get_mut_unchecked_raw(&mut self, i: usize) -> *mut AnyValue {
        let struct_size = usize::try_from(self.type_.size).expect("overflow");
        let vec_capacity = usize::try_from(self.inner.capacity).expect("overflow");

        if i >= vec_capacity {
            panic!("index out of bounds");
        }

        let base_ptr = self.inner.ptr;
        let offset = i.checked_mul(struct_size).expect("overflow");
        let offset = isize::try_from(offset).expect("overflow");
        let element_ptr = base_ptr.offset(offset);
        element_ptr as *mut AnyValue
    }

    pub unsafe fn set_length(&mut self, len: usize) {
        let vec_capacity = usize::try_from(self.inner.capacity).expect("overflow");

        if len > vec_capacity {
            panic!("index greater than capacity");
        }

        let len = u64::try_from(len).expect("overflow");
        self.inner.length = len;
    }

    pub unsafe fn push(&mut self, ptr: *mut AnyValue) {
        self.maybe_grow();

        let struct_size = usize::try_from(self.type_.size).expect("overflow");
        let vec_len = usize::try_from(self.inner.length).expect("overflow");
        let vec_cap = usize::try_from(self.inner.capacity).expect("overflow");

        assert!(vec_len < vec_cap);

        let i = vec_len;

        let base_ptr = self.inner.ptr;
        let offset = i.checked_mul(struct_size).expect("overflow");
        let offset = isize::try_from(offset).expect("overflow");
        let element_ptr = base_ptr.offset(offset);

        let src_ptr = ptr as *mut u8;
        ptr::copy_nonoverlapping(src_ptr, element_ptr, struct_size);

        self.inner.length = self.inner.length.checked_add(1).expect("overflow");
    }

    pub unsafe fn maybe_grow(&mut self) {
        let vec_len = usize::try_from(self.inner.length).expect("overflow");
        let vec_cap = usize::try_from(self.inner.capacity).expect("overflow");

        if vec_len < vec_cap {
            return;
        }

        assert_eq!(vec_len, vec_cap);

        self.grow_amortized();
    }

    /// This is approximately like `RawVec::grow_amortized`.
    ///
    /// It always produces a power-of-two capacity.
    #[cold]
    pub unsafe fn grow_amortized(&mut self) {
        let struct_size = usize::try_from(self.type_.size).expect("overflow");
        let vec_len = usize::try_from(self.inner.length).expect("overflow");
        let vec_cap = usize::try_from(self.inner.capacity).expect("overflow");

        assert_eq!(vec_len, vec_cap);

        // Same as RawVec
        let min_non_zero_cap = if struct_size == 1 {
            8
        } else if struct_size <= 1024 {
            4
        } else {
            1
        };

        let new_cap = vec_cap.checked_mul(2).expect("overflow");
        let new_cap = core::cmp::max(new_cap, min_non_zero_cap);

        self.reserve_exact(new_cap);
    }

    pub unsafe fn reserve_exact(&mut self, new_cap: usize) {
        let struct_size = usize::try_from(self.type_.size).expect("overflow");
        let struct_align = usize::try_from(self.type_.alignment).expect("overflow");
        let vec_cap = usize::try_from(self.inner.capacity).expect("overflow");
        let new_cap_u64 = u64::try_from(new_cap).expect("overflow");

        assert!(struct_size != 0); // can't handle ZSTs
        assert!(new_cap >= vec_cap);

        let old_vec_byte_size = vec_cap.checked_mul(struct_size).expect("overflow");
        let new_vec_byte_size = new_cap.checked_mul(struct_size).expect("overflow");
        let new_layout = alloc::alloc::Layout::from_size_align(new_vec_byte_size, struct_align)
            .expect("bad size or alignment");

        if vec_cap == 0 {
            let new_ptr = alloc::alloc::alloc(new_layout);
            if new_ptr.is_null() {
                alloc::alloc::handle_alloc_error(new_layout);
            }
            self.inner.ptr = new_ptr;
            self.inner.capacity = new_cap_u64;
        } else {
            let old_layout = alloc::alloc::Layout::from_size_align(old_vec_byte_size, struct_align)
                .expect("bad size or alignment");

            let new_ptr = alloc::alloc::realloc(self.inner.ptr, old_layout, new_vec_byte_size);
            if new_ptr.is_null() {
                alloc::alloc::handle_alloc_error(new_layout);
            }
            self.inner.ptr = new_ptr;
            self.inner.capacity = new_cap_u64;
        }
    }

    pub unsafe fn pop_into(&mut self, ptr: *mut AnyValue) {
        let struct_size = usize::try_from(self.type_.size).expect("overflow");
        let vec_len = usize::try_from(self.inner.length).expect("overflow");

        let i = vec_len.checked_sub(1).expect("popping empty vector");

        let base_ptr = self.inner.ptr;
        let offset = i.checked_mul(struct_size).expect("overflow");
        let offset = isize::try_from(offset).expect("overflow");
        let element_ptr = base_ptr.offset(offset);

        let dest_ptr = ptr as *mut u8;
        ptr::copy_nonoverlapping(element_ptr, dest_ptr, struct_size);

        self.inner.length = self.inner.length.checked_sub(1).expect("overflow");
    }

    pub unsafe fn swap(&mut self, i: usize, j: usize) {
        let struct_size = usize::try_from(self.type_.size).expect("overflow");
        let vec_len = usize::try_from(self.inner.length).expect("overflow");

        if i >= vec_len || j >= vec_len {
            panic!("index out of bounds");
        }

        // Safety: must avoid overlapping pointers in swap_nonoverlapping
        // below.
        if i == j {
            return;
        }

        let base_ptr = self.inner.ptr;

        let i_offset = i.checked_mul(struct_size).expect("overflow");
        let i_offset = isize::try_from(i_offset).expect("overflow");
        let i_element_ptr = base_ptr.offset(i_offset);
        let j_offset = j.checked_mul(struct_size).expect("overflow");
        let j_offset = isize::try_from(j_offset).expect("overflow");
        let j_element_ptr = base_ptr.offset(j_offset);

        // Safety: because of the presense of uninitialized padding bytes,
        // we must (I think) do this swap with raw pointers, not slices.
        ptr::swap_nonoverlapping(i_element_ptr, j_element_ptr, struct_size);
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
                        let e = TypedMoveBorrowedRustVec::new(t, e);
                        dbg.entry(&e);
                    }
                }
                dbg.finish()
            }
            TypedMoveBorrowedRustVec::Struct(s) => {
                f.write_str("[")?;
                unsafe {
                    for vref in s.iter() {
                        let e = borrow_move_value_as_rust_value(s.type_(), vref);
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
