// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

mod bcs {
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
    unsafe extern "C" fn test_from_bytes(
        type_v: &MoveType,
        bytes: &MoveByteVector,
        v: *mut AnyValue,
    ) {
        crate::serialization::deserialize(type_v, bytes, v)
    }
}

// nursery
mod debug {
    use crate::{conv::*, rt_types::*, target_defs};
    use alloc::format;

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
        _type_msg: &MoveType,
        _guid: MoveByteVector,
        _count: u64,
        _msg: *mut AnyValue,
    ) {
        todo!()
    }
}

mod hash {
    use crate::rt_types::*;
    use sha2::{Digest, Sha256};
    use sha3::Sha3_256;

    #[export_name = "move_native_hash_sha2_256"]
    unsafe extern "C" fn sha2_256(ptr: MoveByteVector) -> MoveByteVector {
        let rust_vec = ptr.into_rust_vec();

        let hash_vec = Sha256::digest(rust_vec.as_slice()).to_vec();
        MoveByteVector::from_rust_vec(hash_vec)
    }

    #[export_name = "move_native_hash_sha3_256"]
    unsafe extern "C" fn sha3_256(ptr: MoveByteVector) -> MoveByteVector {
        let rust_vec = ptr.into_rust_vec();

        let hash_vec = Sha3_256::digest(rust_vec.as_slice()).to_vec();
        MoveByteVector::from_rust_vec(hash_vec)
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
    use crate::rt_types::*;
    use core::str;

    #[export_name = "move_native_string_internal_check_utf8"]
    pub unsafe extern "C" fn internal_check_utf8(v: &MoveByteVector) -> bool {
        let rust_vec = v.as_rust_vec();
        let res = str::from_utf8(&rust_vec);

        res.is_ok()
    }

    #[export_name = "move_native_string_internal_is_char_boundary"]
    pub unsafe extern "C" fn internal_is_char_boundary(v: &MoveByteVector, i: u64) -> bool {
        let rust_vec = v.as_rust_vec();
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
        let rust_vec = v.as_rust_vec();
        let i = usize::try_from(i).expect("usize");
        let j = usize::try_from(j).expect("usize");

        let rust_str = str::from_utf8(&rust_vec).expect("invalid utf8");

        let sub_rust_vec = rust_str[i..j].as_bytes().to_vec();
        MoveByteVector::from_rust_vec(sub_rust_vec)
    }

    #[export_name = "move_native_string_internal_index_of"]
    pub unsafe extern "C" fn internal_index_of(s: &MoveByteVector, r: &MoveByteVector) -> u64 {
        let s_rust_vec = s.as_rust_vec();
        let s_rust_str = str::from_utf8(&s_rust_vec).expect("invalid utf8");
        let r_rust_vec = r.as_rust_vec();
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
    use crate::{rt_types::*, vector::MoveBorrowedRustVecMut};

    #[export_name = "move_native_type_name_get"]
    unsafe extern "C" fn get(type_: &MoveType) -> TypeName {
        let name_slice = type_.name.as_ascii_str();
        let byte_type = MoveType {
            name: DUMMY_TYPE_NAME,
            type_desc: TypeDesc::U8,
            type_info: &TypeInfo { nothing: 0 },
        };
        let mut byte_vector = MoveUntypedVector::empty(&byte_type);
        {
            let mut rust_byte_vector = MoveBorrowedRustVecMut::<u8>::new(&mut byte_vector);
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
    extern "C" fn create_signers_for_testing(_num_signers: u64) -> MoveSignerVector {
        todo!()
    }
}

mod vector {
    use crate::{
        rt_types::*,
        vector::{TypedMoveBorrowedRustVec, TypedMoveBorrowedRustVecMut},
    };

    // Safety: Even empty Rust vectors have non-null buffer pointers,
    // which must be correctly aligned. This function crates empty Rust vecs
    // of the correct type and converts them to untyped move vecs.
    #[export_name = "move_native_vector_empty"]
    unsafe extern "C" fn empty(type_r: &MoveType) -> MoveUntypedVector {
        MoveUntypedVector::empty(type_r)
    }

    #[export_name = "move_native_vector_length"]
    unsafe extern "C" fn length(type_ve: &MoveType, v: &MoveUntypedVector) -> u64 {
        TypedMoveBorrowedRustVec::new(type_ve, v).len()
    }

    #[export_name = "move_native_vector_borrow"]
    unsafe extern "C" fn borrow<'v>(
        type_ve: &'v MoveType,
        v: &'v MoveUntypedVector,
        i: u64,
    ) -> &'v AnyValue {
        TypedMoveBorrowedRustVec::new(type_ve, v).borrow(i)
    }

    #[export_name = "move_native_vector_push_back"]
    unsafe extern "C" fn push_back(
        type_ve: &MoveType,
        v: &mut MoveUntypedVector,
        e: *mut AnyValue,
    ) {
        TypedMoveBorrowedRustVecMut::new(type_ve, v).push_back(e)
    }

    #[export_name = "move_native_vector_borrow_mut"]
    unsafe extern "C" fn borrow_mut<'v>(
        type_ve: &'v MoveType,
        v: &'v mut MoveUntypedVector,
        i: u64,
    ) -> *mut AnyValue {
        TypedMoveBorrowedRustVecMut::new(type_ve, v).borrow_mut(i)
    }

    #[export_name = "move_native_vector_pop_back"]
    unsafe extern "C" fn pop_back(type_ve: &MoveType, v: &mut MoveUntypedVector, r: *mut AnyValue) {
        TypedMoveBorrowedRustVecMut::new(type_ve, v).pop_back(r)
    }

    #[export_name = "move_native_vector_destroy_empty"]
    unsafe extern "C" fn destroy_empty(type_ve: &MoveType, v: MoveUntypedVector) {
        v.destroy_empty(type_ve)
    }

    #[export_name = "move_native_vector_swap"]
    unsafe extern "C" fn swap(type_ve: &MoveType, v: &mut MoveUntypedVector, i: u64, j: u64) {
        TypedMoveBorrowedRustVecMut::new(type_ve, v).swap(i, j)
    }
}
