// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use super::*;
use crate::{
    rt_types::*,
    std::string::*,
    target_defs::ACCOUNT_ADDRESS_LENGTH,
    vector::{TypedMoveBorrowedRustVec, TypedMoveBorrowedRustVecMut},
};
use alloc::{string::String, vec};
use core::mem;

#[test]
fn test_string_check_utf8() {
    unsafe {
        let rust_vec = vec![240, 159, 146, 150];
        let move_vec = MoveByteVector::from_rust_vec(rust_vec);

        let is_utf8 = internal_check_utf8(&move_vec);
        assert!(is_utf8);

        move_vec.into_rust_vec();

        let rust_vec = vec![0, 159, 146, 150];
        let move_vec = MoveByteVector::from_rust_vec(rust_vec);

        let is_utf8 = internal_check_utf8(&move_vec);
        assert!(!is_utf8);

        move_vec.into_rust_vec();
    }
}

#[test]
fn test_string_is_char_boundary() {
    unsafe {
        let rust_vec = String::from("Löwe").into_bytes();
        let move_vec = MoveByteVector::from_rust_vec(rust_vec);

        let is_char_0 = internal_is_char_boundary(&move_vec, 0);
        assert!(is_char_0);

        let is_char_1 = internal_is_char_boundary(&move_vec, 2);
        assert!(!is_char_1);

        move_vec.into_rust_vec();
    }
}

#[test]
fn test_sub_string() {
    unsafe {
        let rust_vec = b"sub string test".to_vec();
        let move_vec = MoveByteVector::from_rust_vec(rust_vec);

        let move_vec_sub_string = internal_sub_string(&move_vec, 0, 10);
        let rust_vec_sub_string = move_vec_sub_string.into_rust_vec();

        assert_eq!(rust_vec_sub_string, b"sub string");

        move_vec.into_rust_vec();
    }
}

#[test]
fn test_string_index_of() {
    unsafe {
        let rust_vec = b"bears love snow".to_vec();
        let move_vec = MoveByteVector::from_rust_vec(rust_vec);

        let rust_vec_sub = b"love".to_vec();
        let move_vec_sub = MoveByteVector::from_rust_vec(rust_vec_sub);

        let index = internal_index_of(&move_vec, &move_vec_sub);

        assert_eq!(index, 6);

        move_vec.into_rust_vec();
        move_vec_sub.into_rust_vec();
    }
}

#[test]
fn test_vec_with_bool() {
    unsafe {
        static ELEMENT_TYPE: MoveType = MoveType {
            name: DUMMY_TYPE_NAME,
            type_desc: TypeDesc::Bool,
            type_info: &TypeInfo { nothing: 0 },
        };

        let mut move_vec = MoveUntypedVector::empty(&ELEMENT_TYPE);
        assert_eq!(move_vec.length, 0);
        assert_eq!(move_vec.capacity, 0);

        let move_vec_len = TypedMoveBorrowedRustVec::new(&ELEMENT_TYPE, &move_vec).len();
        assert_eq!(move_vec_len, 0);

        let mut new_element: bool = true;
        let new_element_ptr = &mut new_element as *mut _ as *mut AnyValue;
        TypedMoveBorrowedRustVecMut::new(&ELEMENT_TYPE, &mut move_vec).push_back(new_element_ptr);
        assert_eq!(move_vec.length, 1);

        let mut popped_element: bool = false;
        let popped_element_ptr = &mut popped_element as *mut _ as *mut AnyValue;

        TypedMoveBorrowedRustVecMut::new(&ELEMENT_TYPE, &mut move_vec).pop_back(popped_element_ptr);
        assert_eq!(move_vec.length, 0);
        assert_eq!(popped_element, true);

        move_vec.destroy_empty(&ELEMENT_TYPE);
    }
}

#[test]
fn test_vec_with_vector() {
    unsafe {
        static INNER_ELEMENT_TYPE: MoveType = MoveType {
            name: DUMMY_TYPE_NAME,
            type_desc: TypeDesc::Bool,
            type_info: &TypeInfo { nothing: 0 },
        };

        static VECTORTYPEINFO: MoveType = MoveType {
            name: DUMMY_TYPE_NAME,
            type_desc: TypeDesc::Vector,
            type_info: &TypeInfo {
                vector: VectorTypeInfo {
                    element_type: &INNER_ELEMENT_TYPE,
                },
            },
        };

        static OUTER_ELEMENT_TYPE: MoveType = MoveType {
            name: DUMMY_TYPE_NAME,
            type_desc: TypeDesc::Vector,
            type_info: &TypeInfo {
                vector: VectorTypeInfo {
                    element_type: &VECTORTYPEINFO,
                },
            },
        };

        let mut move_vec = MoveUntypedVector::empty(&OUTER_ELEMENT_TYPE);
        assert_eq!(move_vec.length, 0);
        assert_eq!(move_vec.capacity, 0);

        let move_vec_len = TypedMoveBorrowedRustVec::new(&OUTER_ELEMENT_TYPE, &move_vec).len();
        assert_eq!(move_vec_len, 0);

        let mut new_element_vec = MoveUntypedVector::empty(&INNER_ELEMENT_TYPE);

        let mut new_element_inner_0 = true;
        let new_element_inner_ptr_0 = &mut new_element_inner_0 as *mut _ as *mut AnyValue;
        TypedMoveBorrowedRustVecMut::new(&INNER_ELEMENT_TYPE, &mut new_element_vec)
            .push_back(new_element_inner_ptr_0);

        let mut new_element_inner_1 = false;
        let new_element_inner_ptr_1 = &mut new_element_inner_1 as *mut _ as *mut AnyValue;
        TypedMoveBorrowedRustVecMut::new(&INNER_ELEMENT_TYPE, &mut new_element_vec)
            .push_back(new_element_inner_ptr_1);

        let new_element_vec_len =
            TypedMoveBorrowedRustVec::new(&INNER_ELEMENT_TYPE, &new_element_vec).len();
        assert_eq!(new_element_vec_len, 2);

        let new_element_vec_ptr = &mut new_element_vec as *mut _ as *mut AnyValue;
        TypedMoveBorrowedRustVecMut::new(&OUTER_ELEMENT_TYPE, &mut move_vec)
            .push_back(new_element_vec_ptr);
        assert_eq!(move_vec.length, 1);

        let mut popped_element = MoveUntypedVector::empty(&INNER_ELEMENT_TYPE);
        let popped_element_ptr = &mut popped_element as *mut _ as *mut AnyValue;

        TypedMoveBorrowedRustVecMut::new(&OUTER_ELEMENT_TYPE, &mut move_vec)
            .pop_back(popped_element_ptr);
        assert_eq!(move_vec.length, 0);

        let mut popped_element_inner_0: bool = true;
        let popped_element_inner_ptr_0 = &mut popped_element_inner_0 as *mut _ as *mut AnyValue;
        TypedMoveBorrowedRustVecMut::new(&INNER_ELEMENT_TYPE, &mut popped_element)
            .pop_back(popped_element_inner_ptr_0);
        assert_eq!(popped_element_inner_0, false);

        let mut popped_element_inner_1: bool = false;
        let popped_element_inner_ptr_1 = &mut popped_element_inner_1 as *mut _ as *mut AnyValue;
        TypedMoveBorrowedRustVecMut::new(&INNER_ELEMENT_TYPE, &mut popped_element)
            .pop_back(popped_element_inner_ptr_1);
        assert_eq!(popped_element_inner_1, true);

        assert_eq!(popped_element.length, 0);

        popped_element.destroy_empty(&INNER_ELEMENT_TYPE);
        move_vec.destroy_empty(&OUTER_ELEMENT_TYPE);
    }
}

#[test]
fn test_vec_with_signer() {
    unsafe {
        static ELEMENT_TYPE: MoveType = MoveType {
            name: DUMMY_TYPE_NAME,
            type_desc: TypeDesc::Signer,
            type_info: &TypeInfo { nothing: 0 },
        };

        let mut move_vec = MoveUntypedVector::empty(&ELEMENT_TYPE);
        assert_eq!(move_vec.length, 0);
        assert_eq!(move_vec.capacity, 0);

        let move_vec_len = TypedMoveBorrowedRustVec::new(&ELEMENT_TYPE, &move_vec).len();
        assert_eq!(move_vec_len, 0);

        let mut new_element: MoveSigner =
            MoveSigner(MoveAddress([u8::MIN; ACCOUNT_ADDRESS_LENGTH]));
        let new_element_ptr = &mut new_element as *mut _ as *mut AnyValue;
        TypedMoveBorrowedRustVecMut::new(&ELEMENT_TYPE, &mut move_vec).push_back(new_element_ptr);
        assert_eq!(move_vec.length, 1);

        let mut popped_element: MoveSigner =
            MoveSigner(MoveAddress([u8::MAX; ACCOUNT_ADDRESS_LENGTH]));
        let popped_element_ptr = &mut popped_element as *mut _ as *mut AnyValue;

        TypedMoveBorrowedRustVecMut::new(&ELEMENT_TYPE, &mut move_vec).pop_back(popped_element_ptr);
        assert_eq!(move_vec.length, 0);
        assert_eq!(
            popped_element,
            MoveSigner(MoveAddress([u8::MIN; ACCOUNT_ADDRESS_LENGTH]))
        );

        move_vec.destroy_empty(&ELEMENT_TYPE);
    }
}

#[test]
fn test_vec_with_struct() {
    unsafe {
        static DUMMY_FLD_NAME1_SLICE: &[u8] = b"fld_a";
        pub static DUMMY_FLD_NAME1: StaticName = StaticName {
            ptr: DUMMY_FLD_NAME1_SLICE as *const [u8] as *const u8,
            len: 5,
        };

        static DUMMY_FLD_NAME2_SLICE: &[u8] = b"another_fld";
        pub static DUMMY_FLD_NAME2: StaticName = StaticName {
            ptr: DUMMY_FLD_NAME2_SLICE as *const [u8] as *const u8,
            len: 11,
        };

        static STRUCT_FIELD_TYPE: MoveType = MoveType {
            name: DUMMY_TYPE_NAME,
            type_desc: TypeDesc::Bool,
            type_info: &TypeInfo { nothing: 0 },
        };

        static STRUCT_FIELD_INFO: [StructFieldInfo; 2] = [
            StructFieldInfo {
                type_: STRUCT_FIELD_TYPE,
                offset: 0,
                name: DUMMY_FLD_NAME1,
            },
            StructFieldInfo {
                type_: STRUCT_FIELD_TYPE,
                offset: 1,
                name: DUMMY_FLD_NAME2,
            },
        ];

        static ELEMENT_TYPE: MoveType = MoveType {
            name: DUMMY_TYPE_NAME,
            type_desc: TypeDesc::Struct,
            type_info: &TypeInfo {
                struct_: StructTypeInfo {
                    field_array_ptr: &STRUCT_FIELD_INFO[0],
                    field_array_len: 2,
                    size: mem::size_of::<SimpleStruct>() as u64,
                    alignment: mem::align_of::<SimpleStruct>() as u64,
                },
            },
        };

        let mut move_vec = MoveUntypedVector::empty(&ELEMENT_TYPE);
        assert_eq!(move_vec.length, 0);
        assert_eq!(move_vec.capacity, 0);

        let move_vec_len = TypedMoveBorrowedRustVec::new(&ELEMENT_TYPE, &move_vec).len();
        assert_eq!(move_vec_len, 0);

        #[repr(C)]
        #[derive(Copy, Clone, Debug, PartialEq)]
        struct SimpleStruct {
            is_black: bool,
            is_white: bool,
        }

        let mut new_element: SimpleStruct = SimpleStruct {
            is_black: true,
            is_white: false,
        };
        let new_element_ptr = &mut new_element as *mut _ as *mut AnyValue;

        TypedMoveBorrowedRustVecMut::new(&ELEMENT_TYPE, &mut move_vec).push_back(new_element_ptr);
        assert_eq!(move_vec.length, 1);

        let mut popped_element: SimpleStruct = SimpleStruct {
            is_black: false,
            is_white: true,
        };
        let popped_element_ptr = &mut popped_element as *mut _ as *mut AnyValue;

        TypedMoveBorrowedRustVecMut::new(&ELEMENT_TYPE, &mut move_vec).pop_back(popped_element_ptr);
        assert_eq!(move_vec.length, 0);
        assert_eq!(
            popped_element,
            SimpleStruct {
                is_black: true,
                is_white: false,
            }
        );

        move_vec.destroy_empty(&ELEMENT_TYPE);
    }
}
