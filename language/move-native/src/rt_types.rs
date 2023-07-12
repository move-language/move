// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::target_defs;

/// A Move vector with an untyped buffer.
///
/// Used in the API for generic vector arguments.
///
/// The only way to interact with these is to convert them from / to Rust
/// vectors or references to Rust vectors, with functions in the [`conv`]
/// module.
///
/// The only way to create and destroy them is with the
/// [`move_native_vec_empty`] and [`move_native_vec_destroy_empty`] native
/// calls.
#[repr(C)]
#[derive(Debug)]
pub struct MoveUntypedVector {
    pub ptr: *mut u8,  // Safety: must be correctly aligned per type
    pub capacity: u64, // in typed elements, not u8
    pub length: u64,   // in typed elements, not u8
}
pub const MOVE_UNTYPED_VEC_DESC_SIZE: u64 = core::mem::size_of::<MoveUntypedVector>() as u64;

/// A Move vector of bytes.
///
/// These occur in the API enough to warrant their own type, and there are
/// dedicated functions to convert them to Rust vectors.
#[repr(C)]
pub struct MoveByteVector {
    pub ptr: *mut u8,
    pub capacity: u64,
    pub length: u64,
}

/// A Move vector of signers.
///
/// This type occurs in the native API, but it will probably be removed, in
/// favor of just using `MoveUntypedVector`.
#[repr(C)]
pub struct MoveSignerVector {
    pub ptr: *mut MoveSigner,
    pub capacity: u64,
    pub length: u64,
}

/// A reification of the Move runtime type description.
///
/// This is structured as a `TypeDesc` indicating which type a thing is,
/// and an undiscriminated union holding additional information about the
/// type.
///
/// cc runtime_types::Type
///
/// # Safety
///
/// The pointer must be to static memory and never mutated.
#[repr(C)]
#[derive(Copy, Clone)]
pub struct MoveType {
    pub name: StaticTypeName,
    pub type_desc: TypeDesc,
    pub type_info: *const TypeInfo,
}
pub const MOVE_TYPE_DESC_SIZE: u64 = core::mem::size_of::<MoveType>() as u64;

// Needed to make the MoveType, which contains raw pointers,
// Sync, so that it can be stored in statics for test cases.
unsafe impl Sync for MoveType { }

impl core::fmt::Debug for MoveType {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        // fixme: implement this better
        unsafe {
            write!(f, "{}", self.name.as_ascii_str());
        }
        Ok(())
    }
}

/// # Safety
///
/// The pointer must be to static memory and never mutated.
#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct StaticTypeName {
    pub ptr: *const u8,
    pub len: u64,
}

impl StaticTypeName {
    pub unsafe fn as_ascii_str<'a>(&'a self) -> &'a str {
        core::str::from_utf8_unchecked(core::slice::from_raw_parts(
            self.ptr,
            usize::try_from(self.len).expect("overflow"),
        ))
    }
}

pub type StaticName = StaticTypeName;

static DUMMY_TYPE_NAME_SLICE: &[u8] = b"dummy";
pub static DUMMY_TYPE_NAME: StaticTypeName = StaticTypeName {
    ptr: DUMMY_TYPE_NAME_SLICE as *const [u8] as *const u8,
    len: 5,
};

unsafe impl Sync for StaticTypeName {}

#[repr(u64)]
#[derive(Copy, Clone, Debug)]
pub enum TypeDesc {
    Bool = 1,
    U8 = 2,
    U16 = 3,
    U32 = 4,
    U64 = 5,
    U128 = 6,
    U256 = 7,
    Address = 8,
    Signer = 9,
    Vector = 10,
    Struct = 11,
    Reference = 12,
    //MutableReference = 13,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub union TypeInfo {
    pub nothing: u8, // if no type info is needed
    pub vector: VectorTypeInfo,
    pub struct_: StructTypeInfo,
    pub struct_instantiation: u8, // todo
    pub reference: ReferenceTypeInfo,
    pub mutable_reference: ReferenceTypeInfo,
    pub ty_param: u8, // todo
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct VectorTypeInfo {
    pub element_type: &'static MoveType,
}

/// # Safety
///
/// This type is `Sync` so that it can be declared statically. The value
/// pointed to by `field_array_ptr` should not be mutated, or `Sync` will be
/// violated.
#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct StructTypeInfo {
    /// Pointer to an array of field infos.
    ///
    /// This would ideally be a Rust static slice, but the layout is
    /// seemingly undefined.
    pub field_array_ptr: *const StructFieldInfo,
    pub field_array_len: u64,
    /// Size of the struct within an array.
    pub size: u64,
    /// Alignment of the struct.
    pub alignment: u64,
}

unsafe impl Sync for StructTypeInfo {}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct StructFieldInfo {
    pub type_: MoveType,
    /// Offset in bytes within the struct.
    pub offset: u64,
    pub name: StaticName,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct ReferenceTypeInfo {
    pub element_type: &'static MoveType,
}

#[repr(transparent)]
pub struct AnyValue(u8);

#[repr(transparent)]
#[derive(Debug, PartialEq)]
#[derive(borsh::BorshSerialize, borsh::BorshDeserialize)]
pub struct MoveSigner(pub MoveAddress);

/// A Move address.
///
/// This is mapped to the address size of the target platform, and may
/// differ from Move VM.
///
/// Bytes are in little-endian order.
#[repr(transparent)]
#[derive(PartialEq)]
#[derive(borsh::BorshSerialize, borsh::BorshDeserialize)]
pub struct MoveAddress(pub [u8; target_defs::ACCOUNT_ADDRESS_LENGTH]);

impl core::fmt::Debug for MoveAddress {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str("@")?;
        for byte in self.0.iter().rev() {
            f.write_fmt(core::format_args!("{byte:02X?}"))?;
        }
        Ok(())
    }
}

// Defined in std::type_name; not a primitive.
//
// todo how is drop glue handled?
#[repr(C)]
pub struct TypeName {
    pub name: MoveAsciiString,
}

// Defined in std::ascii; not a primitive.
//
// todo how is drop glue handled?
#[repr(C)]
pub struct MoveAsciiString {
    pub bytes: MoveByteVector,
}

// todo this would be more correct with a lifetime attached
#[repr(transparent)]
#[derive(Debug)]
pub struct MoveUntypedReference(pub *const AnyValue);

mod drop_bomb {
    // fixme this is pretty awkward - this is intended to be a no-std crate
    #[cfg(test)]
    extern crate std;

    #[cfg(not(test))]
    pub fn run(name: &str) {
        panic!("forgot to destroy {}", name);
    }

    #[cfg(test)]
    pub fn run(name: &str) {
        if !std::thread::panicking() {
            panic!("forgot to destroy {}", name);
        } else {
            std::eprintln!("forgot to destroy {}", name);
        }
    }
}

// Drop-bomb. Catch errors in test cases.
impl Drop for MoveUntypedVector {
    fn drop(&mut self) {
        drop_bomb::run("MoveUntypedVector");
    }
}

// Drop-bomb. Catch errors in test cases.
impl Drop for MoveByteVector {
    fn drop(&mut self) {
        drop_bomb::run("MoveByteVector");
    }
}

// Drop-bomb. Catch errors in test cases.
impl Drop for MoveSignerVector {
    fn drop(&mut self) {
        drop_bomb::run("MoveSignerVector");
    }
}

/// Don't run destructors.
///
/// Some of these runtime types hold allocations, and need to be destroyed
/// in specific ways. If they are not, then they panic. These types must be
/// destroyed by calling `mem::forget`, for which this function is a
/// synonym.
pub fn disarm_drop_bomb<T>(v: T) {
    core::mem::forget(v)
}
