// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

mod bcs {
    use crate::conv::*;
    use crate::rt_types::*;

    /// Serialize any value.
    ///
    /// This does not necessarily produce the same serializations that
    /// the Move VM does, as the Rust types are not the same, and
    /// the serialization format may not actually be bcs.
    ///
    /// # References
    ///
    /// - `move-vm-types::values::Value`
    /// - `move-core-types::value`
    #[export_name = "move_native_bcs_to_bytes"]
    unsafe extern "C" fn to_bytes(type_v: &MoveType, v: &AnyValue) -> MoveByteVector {
        crate::serialization::serialize(type_v, v)
    }

    /// Deserialize any value.
    ///
    /// This is not actually in move std, but is used for testing.
    #[export_name = "move_native_bcs_test_from_bytes"]
    unsafe extern "C" fn test_from_bytes(type_v: &MoveType, bytes: &MoveByteVector, v: *mut AnyValue) {
        crate::serialization::deserialize(type_v, bytes, v)
    }
}

// nursery
mod debug {
    use crate::conv::*;
    use crate::rt_types::*;
    use crate::target_defs;
    use alloc::format;
    use alloc::string::String;
    use core::fmt::Write;

    #[export_name = "move_native_debug_print"]
    unsafe extern "C" fn print(type_x: &MoveType, x: &AnyValue) {
        let v = borrow_move_value_as_rust_value(type_x, x);
        target_defs::print_string(&format!("{:?}", v));
    }

    #[export_name = "move_native_print_stack_trace"]
    extern "C" fn print_stack_trace() {
        target_defs::print_stack_trace();
    }
}

// nursery
mod event {
    use crate::rt_types::*;

    #[export_name = "move_native_event_write_to_event_store"]
    unsafe extern "C" fn write_to_event_store(
        type_msg: &MoveType,
        guid: MoveByteVector,
        count: u64,
        msg: *mut AnyValue,
    ) {
        todo!()
    }
}

mod hash {
    use crate::conv::{move_byte_vec_to_rust_vec, rust_vec_to_move_byte_vec};
    use crate::rt_types::*;
    use sha2::{Digest, Sha256};
    use sha3::Sha3_256;

    #[export_name = "move_native_hash_sha2_256"]
    unsafe extern "C" fn sha2_256(ptr: MoveByteVector) -> MoveByteVector {
        let rust_vec = move_byte_vec_to_rust_vec(ptr);

        let hash_vec = Sha256::digest(rust_vec.as_slice()).to_vec();
        let move_vec = rust_vec_to_move_byte_vec(hash_vec);

        move_vec
    }

    #[export_name = "move_native_hash_sha3_256"]
    unsafe extern "C" fn sha3_256(ptr: MoveByteVector) -> MoveByteVector {
        let rust_vec = move_byte_vec_to_rust_vec(ptr);

        let hash_vec = Sha3_256::digest(rust_vec.as_slice()).to_vec();
        let move_vec = rust_vec_to_move_byte_vec(hash_vec);

        move_vec
    }
}

mod signer {
    use crate::rt_types::*;

    #[export_name = "move_native_signer_borrow_address"]
    extern "C" fn borrow_address(s: &MoveSigner) -> &MoveAddress {
        &s.0
    }
}

pub(crate) mod string {
    use crate::conv::*;
    use crate::rt_types::*;
    use alloc::vec::Vec;
    use core::str;

    #[export_name = "move_native_string_internal_check_utf8"]
    pub unsafe extern "C" fn internal_check_utf8(v: &MoveByteVector) -> bool {
        let rust_vec = borrow_move_byte_vec_as_rust_vec(v);
        let res = str::from_utf8(&rust_vec);

        match res {
            Ok(_) => true,
            Err(_) => false,
        }
    }

    #[export_name = "move_native_string_internal_is_char_boundary"]
    pub unsafe extern "C" fn internal_is_char_boundary(v: &MoveByteVector, i: u64) -> bool {
        let rust_vec = borrow_move_byte_vec_as_rust_vec(v);
        let i = usize::try_from(i).expect("usize");

        let rust_str = str::from_utf8(&rust_vec).expect("invalid utf8");
        rust_str.is_char_boundary(i)
    }

    #[export_name = "move_native_string_internal_sub_string"]
    pub unsafe extern "C" fn internal_sub_string(
        v: &MoveByteVector,
        i: u64,
        j: u64,
    ) -> MoveByteVector {
        let rust_vec = borrow_move_byte_vec_as_rust_vec(v);
        let i = usize::try_from(i).expect("usize");
        let j = usize::try_from(j).expect("usize");

        let rust_str = str::from_utf8(&rust_vec).expect("invalid utf8");

        let sub_rust_vec = rust_str[i..j].as_bytes().to_vec();
        rust_vec_to_move_byte_vec(sub_rust_vec)
    }

    #[export_name = "move_native_string_internal_index_of"]
    pub unsafe extern "C" fn internal_index_of(s: &MoveByteVector, r: &MoveByteVector) -> u64 {
        let s_rust_vec = borrow_move_byte_vec_as_rust_vec(s);
        let s_rust_str = str::from_utf8(&s_rust_vec).expect("invalid utf8");
        let r_rust_vec = borrow_move_byte_vec_as_rust_vec(r);
        let r_rust_str = str::from_utf8(&r_rust_vec).expect("invalid utf8");

        let res = s_rust_str.find(r_rust_str);

        u64::try_from(match res {
            Some(i) => i,
            None => s_rust_str.len(),
        })
            .expect("u64")
    }
}

mod type_name {
    use crate::conv::*;
    use crate::rt_types::*;

    #[export_name = "move_native_type_name_get"]
    unsafe extern "C" fn get(type_: &MoveType) -> TypeName {
        let name_slice = type_.name.as_ascii_str();
        let byte_type = MoveType {
            name: DUMMY_TYPE_NAME,
            type_desc: TypeDesc::U8,
            type_info: &TypeInfo { nothing: 0 },
        };
        let mut byte_vector = super::vector::empty(&byte_type);
        {
            let mut rust_byte_vector = borrow_move_vec_as_rust_vec_mut::<u8>(&mut byte_vector);
            rust_byte_vector.reserve_exact(name_slice.len());
            for byte in name_slice.bytes() {
                rust_byte_vector.push(byte);
            }
        }
        TypeName {
            name: MoveAsciiString {
                // safety: MoveUntypedVector and MoveByteVector have the same representation
                bytes: core::mem::transmute::<MoveUntypedVector, MoveByteVector>(byte_vector),
            },
        }
    }
}

mod unit_test {
    use crate::rt_types::*;

    #[export_name = "move_native_unit_test_create_signers_for_testing"]
    extern "C" fn create_signers_for_testing(num_signers: u64) -> MoveSignerVector {
        todo!()
    }
}

pub(crate) mod vector {
    use crate::conv::*;
    use crate::rt_types::*;
    use alloc::vec::Vec;
    use core::{mem, ptr};
    use ethnum::U256;

    // Safety: Even empty Rust vectors have non-null buffer pointers,
    // which must be correctly aligned. This function crates empty Rust vecs
    // of the correct type and converts them to untyped move vecs.
    #[export_name = "move_native_vector_empty"]
    pub extern "C" fn empty(type_r: &MoveType) -> MoveUntypedVector {
        let move_vec = match type_r.type_desc {
            TypeDesc::Bool => rust_vec_to_move_vec::<bool>(Vec::new()),
            TypeDesc::U8 => rust_vec_to_move_vec::<u8>(Vec::new()),
            TypeDesc::U16 => rust_vec_to_move_vec::<u16>(Vec::new()),
            TypeDesc::U32 => rust_vec_to_move_vec::<u32>(Vec::new()),
            TypeDesc::U64 => rust_vec_to_move_vec::<u64>(Vec::new()),
            TypeDesc::U128 => rust_vec_to_move_vec::<u128>(Vec::new()),
            TypeDesc::U256 => rust_vec_to_move_vec::<U256>(Vec::new()),
            TypeDesc::Address => rust_vec_to_move_vec::<MoveAddress>(Vec::new()),
            TypeDesc::Signer => rust_vec_to_move_vec::<MoveSigner>(Vec::new()),
            TypeDesc::Vector => {
                // Safety: need correct alignment for the internal vector
                // pointer of the outer vector, which is non-null even for
                // an unallocated vector. `MoveUntypedVector` has the same
                // size and alignment regardless of the type it contains, so
                // no need to interpret the vector type.
                rust_vec_to_move_vec::<MoveUntypedVector>(Vec::new())
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
            TypeDesc::Reference => rust_vec_to_move_vec::<MoveUntypedReference>(Vec::new()),
        };

        move_vec
    }

    #[export_name = "move_native_vector_length"]
    pub unsafe extern "C" fn length(type_ve: &MoveType, v: &MoveUntypedVector) -> u64 {
        // It is not strictly necessary to convert the vec for this op.
        // Doing it for consistency.
        let rust_vec = borrow_typed_move_vec_as_rust_vec(type_ve, v);

        let len = match rust_vec {
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

    #[export_name = "move_native_vector_borrow"]
    pub unsafe extern "C" fn borrow<'v>(
        type_ve: &'v MoveType,
        v: &'v MoveUntypedVector,
        i: u64,
    ) -> &'v AnyValue {
        let rust_vec = borrow_typed_move_vec_as_rust_vec(type_ve, v);

        let i = usize::try_from(i).expect("usize");
        let value = match rust_vec {
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

    #[rustfmt::skip]
    #[export_name = "move_native_vector_push_back"]
    pub unsafe extern "C" fn push_back(
        type_ve: &MoveType,
        v: &mut MoveUntypedVector,
        e: *mut AnyValue
    ) {
        let mut rust_vec = borrow_typed_move_vec_as_rust_vec_mut(type_ve, v);

        match rust_vec {
            TypedMoveBorrowedRustVecMut::Bool(mut v) => v.push(ptr::read(e as *const bool)),
            TypedMoveBorrowedRustVecMut::U8(mut v) => v.push(ptr::read(e as *const u8)),
            TypedMoveBorrowedRustVecMut::U16(mut v) => v.push(ptr::read(e as *const u16)),
            TypedMoveBorrowedRustVecMut::U32(mut v) => v.push(ptr::read(e as *const u32)),
            TypedMoveBorrowedRustVecMut::U64(mut v) => v.push(ptr::read(e as *const u64)),
            TypedMoveBorrowedRustVecMut::U128(mut v) => v.push(ptr::read(e as *const u128)),
            TypedMoveBorrowedRustVecMut::U256(mut v) => v.push(ptr::read(e as *const U256)),
            TypedMoveBorrowedRustVecMut::Address(mut v) => v.push(ptr::read(e as *const MoveAddress)),
            TypedMoveBorrowedRustVecMut::Signer(mut v) => v.push(ptr::read(e as *const MoveSigner)),
            TypedMoveBorrowedRustVecMut::Vector(_t, mut v) => v.push(ptr::read(e as *const MoveUntypedVector)),
            TypedMoveBorrowedRustVecMut::Struct(mut s) => s.push(e),
            TypedMoveBorrowedRustVecMut::Reference(_t, mut v) => v.push(ptr::read(e as *const MoveUntypedReference)),
        }
    }

    #[rustfmt::skip]
    #[export_name = "move_native_vector_borrow_mut"]
    unsafe extern "C" fn borrow_mut<'v>(
        type_ve: &'v MoveType,
        v: &'v mut MoveUntypedVector,
        i: u64
    ) -> &'v mut AnyValue {
        let mut rust_vec = borrow_typed_move_vec_as_rust_vec_mut(type_ve, v);

        let i = usize::try_from(i).expect("usize");
        let value = match rust_vec {
            TypedMoveBorrowedRustVecMut::Bool(mut v) => mem::transmute(&mut v[i]),
            TypedMoveBorrowedRustVecMut::U8(mut v) => mem::transmute(&mut v[i]),
            TypedMoveBorrowedRustVecMut::U16(mut v) => mem::transmute(&mut v[i]),
            TypedMoveBorrowedRustVecMut::U32(mut v) => mem::transmute(&mut v[i]),
            TypedMoveBorrowedRustVecMut::U64(mut v) => mem::transmute(&mut v[i]),
            TypedMoveBorrowedRustVecMut::U128(mut v) => mem::transmute(&mut v[i]),
            TypedMoveBorrowedRustVecMut::U256(mut v) => mem::transmute(&mut v[i]),
            TypedMoveBorrowedRustVecMut::Address(mut v) => mem::transmute(&mut v[i]),
            TypedMoveBorrowedRustVecMut::Signer(mut v) => mem::transmute(&mut v[i]),
            TypedMoveBorrowedRustVecMut::Vector(_t, mut v) => mem::transmute(&mut v[i]),
            TypedMoveBorrowedRustVecMut::Struct(mut s) => s.get_mut(i),
            TypedMoveBorrowedRustVecMut::Reference(_t, mut v) => mem::transmute(&mut v[i]),
        };

        value
    }

    #[export_name = "move_native_vector_pop_back"]
    pub unsafe extern "C" fn pop_back(
        type_ve: &MoveType,
        v: &mut MoveUntypedVector,
        r: *mut AnyValue,
    ) {
        let mut rust_vec = borrow_typed_move_vec_as_rust_vec_mut(type_ve, v);

        let msg = "popping from empty vec";
        match rust_vec {
            TypedMoveBorrowedRustVecMut::Bool(mut v) => {
                ptr::write(r as *mut bool, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::U8(mut v) => {
                ptr::write(r as *mut u8, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::U16(mut v) => {
                ptr::write(r as *mut u16, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::U32(mut v) => {
                ptr::write(r as *mut u32, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::U64(mut v) => {
                ptr::write(r as *mut u64, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::U128(mut v) => {
                ptr::write(r as *mut u128, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::U256(mut v) => {
                ptr::write(r as *mut U256, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::Address(mut v) => {
                ptr::write(r as *mut MoveAddress, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::Signer(mut v) => {
                ptr::write(r as *mut MoveSigner, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::Vector(_t, mut v) => {
                ptr::write(r as *mut MoveUntypedVector, v.pop().expect(msg));
            }
            TypedMoveBorrowedRustVecMut::Struct(mut s) => s.pop_into(r),
            TypedMoveBorrowedRustVecMut::Reference(_t, mut v) => {
                ptr::write(r as *mut MoveUntypedReference, v.pop().expect(msg));
            }
        }
    }

    #[export_name = "move_native_vector_destroy_empty"]
    pub unsafe extern "C" fn destroy_empty(type_ve: &MoveType, v: MoveUntypedVector) {
        assert_eq!(v.length, 0);
        match type_ve.type_desc {
            TypeDesc::Bool => drop(move_vec_to_rust_vec::<bool>(v)),
            TypeDesc::U8 => drop(move_vec_to_rust_vec::<u8>(v)),
            TypeDesc::U16 => drop(move_vec_to_rust_vec::<u16>(v)),
            TypeDesc::U32 => drop(move_vec_to_rust_vec::<u32>(v)),
            TypeDesc::U64 => drop(move_vec_to_rust_vec::<u64>(v)),
            TypeDesc::U128 => drop(move_vec_to_rust_vec::<u128>(v)),
            TypeDesc::U256 => drop(move_vec_to_rust_vec::<U256>(v)),
            TypeDesc::Address => drop(move_vec_to_rust_vec::<MoveAddress>(v)),
            TypeDesc::Signer => drop(move_vec_to_rust_vec::<MoveSigner>(v)),
            TypeDesc::Vector => {
                // Safety: need the correct internal pointer alignment to
                // deallocate; need the outer vector to be empty to avoid
                // dropping the inner vectors. As in `empty`,
                // MoveUntypedVector should have the same size/alignment
                // regardless of the contained type, so no need to interpret
                // the vector type.
                drop(move_vec_to_rust_vec::<MoveUntypedVector>(v))
            }
            TypeDesc::Struct => {
                // Safety: like in `empty` we want to deallocate here without
                // creating a `Vec` of a concrete type, since handling the
                // alignment would requiring enumerating many types.
                //
                // So here we're just going to free the pointer ourselves,
                // constructing a correct `Layout` value to pass to the
                // allocator.
                //
                // Note that this function can only be called on empty vecs,
                // so we don't need to care about dropping elements.

                let size = (*type_ve.type_info).struct_.size;
                let size = usize::try_from(size).expect("overflow");
                let alignment = (*type_ve.type_info).struct_.alignment;
                let alignment = usize::try_from(alignment).expect("overflow");
                let capacity = usize::try_from(v.capacity).expect("overflow");

                assert!(size != 0); // can't handle ZSTs

                if capacity != 0 {
                    let vec_byte_size = capacity.checked_mul(size).expect("overflow");
                    let layout =
                        alloc::alloc::Layout::from_size_align(vec_byte_size, alignment)
                        .expect("bad size or alignment");
                    alloc::alloc::dealloc(v.ptr, layout);
                }

                disarm_drop_bomb(v);
            }
            TypeDesc::Reference => drop(move_vec_to_rust_vec::<MoveUntypedReference>(v)),
        }
    }

    #[export_name = "move_native_vector_swap"]
    unsafe extern "C" fn swap(type_ve: &MoveType, v: &mut MoveUntypedVector, i: u64, j: u64) {
        let i = usize::try_from(i).expect("usize");
        let j = usize::try_from(j).expect("usize");

        let mut rust_vec = borrow_typed_move_vec_as_rust_vec_mut(type_ve, v);

        match rust_vec {
            TypedMoveBorrowedRustVecMut::Bool(mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::U8(mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::U16(mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::U32(mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::U64(mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::U128(mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::U256(mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::Address(mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::Signer(mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::Vector(_t, mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::Struct(mut v) => v.swap(i, j),
            TypedMoveBorrowedRustVecMut::Reference(_t, mut v) => v.swap(i, j),
        }
    }
}
