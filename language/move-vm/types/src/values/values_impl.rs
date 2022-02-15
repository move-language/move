// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::loaded_data::runtime_types::Type;
use move_binary_format::{
    errors::*,
    file_format::{Constant, SignatureToken},
};
use move_core_types::{
    account_address::AccountAddress,
    gas_schedule::{
        AbstractMemorySize, GasAlgebra, GasCarrier, CONST_SIZE, MIN_EXISTS_DATA_SIZE,
        REFERENCE_SIZE, STRUCT_SIZE,
    },
    value::{MoveStructLayout, MoveTypeLayout},
    vm_status::{sub_status::NFE_VECTOR_ERROR_BASE, StatusCode},
};
use std::{
    borrow::Borrow,
    cell::RefCell,
    fmt::{self, Debug, Display},
    mem::size_of,
    ops::Add,
    rc::Rc,
};

/***************************************************************************************
 *
 * Internal Types
 *
 *   Internal representation of the Move value calculus. These types are abstractions
 *   over the concrete Move concepts and may carry additonal information that is not
 *   defined by the language, but required by the implementation.
 *
 **************************************************************************************/

/// Runtime representation of a Move value.
#[derive(Debug, Clone)]
enum ValueImpl {
    Invalid,

    U8(u8),
    U64(u64),
    U128(u128),
    Bool(bool),
    Address(AccountAddress),

    Container(Container),

    Reference(ReferenceImpl),
}

/// A container is a collection of values. It is used to represent data structures like a
/// Move vector or struct.
///
/// There is one general container that can be used to store an array of any values, same
/// type or not, and a few specialized flavors to offer compact memory layout for small
/// primitive types.
///
/// Except when not owned by the VM stack, a container always lives inside an Rc<RefCell<>>,
/// making it possible to be shared by references.
#[derive(Debug, Clone)]
enum Container {
    Vec(Vec<ValueImpl>),
    Struct(Vec<ValueImpl>),
    VecU8(Vec<u8>),
    VecU64(Vec<u64>),
    VecU128(Vec<u128>),
    VecBool(Vec<bool>),
    VecAddress(Vec<AccountAddress>),
}

/// Status for external data, usually from global storage
/// Clean - the data was only read.
/// Dirty - the data was possibly modified.
#[derive(Debug, Clone, Copy)]
enum ExternalDataStatus {
    Clean,
    Dirty,
}

#[derive(Debug, Clone, Copy)]
/// A simple reference to data, represented as a pointer
/// Pointer access is safe due to Move's static reference safety rules
enum CheckedRef {
    U8(*mut u8),
    U64(*mut u64),
    U128(*mut u128),
    Bool(*mut bool),
    Address(*mut AccountAddress),
    Container(*mut Container),
}

#[derive(Debug, Clone)]
struct ExternalRef {
    /// Maintains dirt/clean status of the source, if it is from Move's global storage
    data_status: Option<Rc<RefCell<ExternalDataStatus>>>,
    /// ref counted source of the value. not actually accessed but kept so the value isn't dropped
    /// TODO this could likely just be an Rc once get_mut_unchecked is stabilized
    source: Rc<RefCell<ValueImpl>>,
    /// pointer into the source
    checked_ref: CheckedRef,
}

/***************************************************************************************
 *
 * Public Types
 *
 *   Types visible from outside the module. They are almost exclusively wrappers around
 *   the internal representation, acting as public interfaces. The methods they provide
 *   closely resemble the Move concepts their names suggest: move_local, borrow_field,
 *   pack, unpack, etc.
 *
 *   They are opaque to an external caller by design -- no knowledge about the internal
 *   representation is given and they can only be manipulated via the public methods,
 *   which is to ensure no arbitrary invalid states can be created unless some crucial
 *   internal invariants are violated.
 *
 **************************************************************************************/

/// A Move value -- a wrapper around `ValueImpl` which can be created only through valid
/// means.
#[derive(Debug, Clone)]
pub struct Value(ValueImpl);

/// The locals for a function frame. It allows values to be read, written or taken
/// reference from.
#[derive(Debug)]
pub struct Locals(Vec<ValueImpl>);

/// A generic Move reference that offers two functionalities: read_ref & write_ref.
#[derive(Debug)]
pub struct Reference(ReferenceImpl);

/// An umbrella enum for references. It is used to hide the internals of the public type
/// Reference.
#[derive(Debug, Clone)]
enum ReferenceImpl {
    CheckedRef(CheckedRef),
    ExternalRef(ExternalRef),
}

/// A reference to a Move struct that allows you to take a reference to one of its fields.
#[derive(Debug)]
pub struct StructRef(ReferenceImpl);

// A reference to a signer. Clients can attempt a cast to this struct if they are
// expecting a Signer on the stack or as an argument.
#[derive(Debug)]
pub struct SignerRef(ReferenceImpl);

// A reference to a vector. This is an alias for a ContainerRef for now but we may change
// it once Containers are restructured.
// It's used from vector native functions to get a reference to a vector and operate on that.
// There is an impl for VectorRef which implements the API private to this module.
#[derive(Debug)]
pub struct VectorRef(ReferenceImpl);

// A vector. This is an alias for a Container for now but we may change
// it once Containers are restructured.
// It's used from vector native functions to get a vector and operate on that.
// There is an impl for Vector which implements the API private to this module.
#[derive(Debug)]
pub struct Vector(Container);

/// An integer value in Move.
#[derive(Debug)]
pub enum IntegerValue {
    U8(u8),
    U64(u64),
    U128(u128),
}

/// A Move struct.
#[derive(Debug)]
pub struct Struct {
    fields: Vec<ValueImpl>,
}

/// A special "slot" in global storage that can hold a resource. It also keeps track of the status
/// of the resource relative to the global state, which is necessary to compute the effects to emit
/// at the end of transaction execution.
#[derive(Debug)]
enum GlobalValueImpl {
    /// No resource resides in this slot or in storage.
    None,
    /// A resource has been published to this slot and it did not previously exist in storage.
    Fresh { value: Rc<RefCell<ValueImpl>> },
    /// A resource resides in this slot and also in storage. The status flag indicates whether
    /// it has potentially been altered.
    Cached {
        value: Rc<RefCell<ValueImpl>>,
        status: Rc<RefCell<ExternalDataStatus>>,
    },
    /// A resource used to exist in storage but has been deleted by the current transaction.
    Deleted,
}

/// A wrapper around `GlobalValueImpl`, representing a "slot" in global storage that can
/// hold a resource.
#[derive(Debug)]
pub struct GlobalValue(GlobalValueImpl);

/// Simple enum for the change state of a GlobalValue, used by `into_effect`.
pub enum GlobalValueEffect<T> {
    /// There was no value, or the value was not changed
    None,
    /// The value was removed
    Deleted,
    /// Updated with a new value
    Changed(T),
}

/***************************************************************************************
 *
 * Misc
 *
 *   Miscellaneous helper functions.
 *
 **************************************************************************************/

impl Container {
    fn len(&self) -> usize {
        match self {
            Self::Struct(r) | Self::Vec(r) => r.len(),
            Self::VecU8(r) => r.len(),
            Self::VecU64(r) => r.len(),
            Self::VecU128(r) => r.len(),
            Self::VecBool(r) => r.len(),
            Self::VecAddress(r) => r.len(),
        }
    }

    fn signer(x: AccountAddress) -> Self {
        Container::Struct(vec![ValueImpl::Address(x)])
    }
}

/***************************************************************************************
 *
 * Borrows (Internal)
 *
 *   Helper functions to handle Rust borrows. When borrowing from a RefCell, we want
 *   to return an error instead of panicking.
 *
 **************************************************************************************/

impl CheckedRef {
    fn new(value: &mut ValueImpl) -> PartialVMResult<Self> {
        Ok(match value {
            ValueImpl::U8(u) => Self::U8(u),
            ValueImpl::U64(u) => Self::U64(u),
            ValueImpl::U128(u) => Self::U128(u),
            ValueImpl::Bool(b) => Self::Bool(b),
            ValueImpl::Address(a) => Self::Address(a),
            ValueImpl::Container(c) => Self::Container(c),
            v @ ValueImpl::Invalid | v @ ValueImpl::Reference(_) => {
                return Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR)
                    .with_message(format!("cannot borrow loc: {:?}", v)))
            }
        })
    }
}

impl ExternalRef {
    fn new_source(
        global_status: Option<Rc<RefCell<ExternalDataStatus>>>,
        source: Rc<RefCell<ValueImpl>>,
    ) -> PartialVMResult<Self> {
        let checked_ref = CheckedRef::new(&mut *source.borrow_mut())?;
        Ok(Self {
            data_status: global_status,
            source,
            checked_ref,
        })
    }

    fn mark_dirty(&self) {
        if let Some(status) = &self.data_status {
            *status.borrow_mut() = ExternalDataStatus::Dirty;
        }
    }
}

impl ReferenceImpl {
    fn checked_ref(&self) -> CheckedRef {
        match self {
            Self::CheckedRef(checked_ref) | Self::ExternalRef(ExternalRef { checked_ref, .. }) => {
                *checked_ref
            }
        }
    }

    fn container(&self) -> PartialVMResult<*mut Container> {
        match self.checked_ref() {
            CheckedRef::Container(c) => Ok(c),
            _ => {
                return Err(
                    PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                        .with_message(
                            "tried to borrow a container on a non-container ref".to_string(),
                        ),
                )
            }
        }
    }

    fn mark_dirty(&self) {
        if let Self::ExternalRef(ext) = self {
            ext.mark_dirty()
        }
    }

    fn extend_ref(&self, extension: CheckedRef) -> Self {
        match self {
            Self::CheckedRef(_) => Self::CheckedRef(extension),
            Self::ExternalRef(ExternalRef {
                data_status: global_status,
                source,
                checked_ref: _,
            }) => Self::ExternalRef(ExternalRef {
                data_status: global_status.clone(),
                source: source.clone(),
                checked_ref: extension,
            }),
        }
    }

    fn borrow_elem(&self, idx: usize) -> PartialVMResult<ReferenceImpl> {
        let extension = match unsafe { &mut *self.container()? } {
            Container::Vec(v) | Container::Struct(v) => CheckedRef::new(&mut v[idx])?,
            Container::VecU8(v) => CheckedRef::U8(&mut v[idx]),
            Container::VecU64(v) => CheckedRef::U64(&mut v[idx]),
            Container::VecU128(v) => CheckedRef::U128(&mut v[idx]),
            Container::VecBool(v) => CheckedRef::Bool(&mut v[idx]),
            Container::VecAddress(v) => CheckedRef::Address(&mut v[idx]),
        };
        Ok(self.extend_ref(extension))
    }
}

/***************************************************************************************
 *
 * Reference Conversions (Internal)
 *
 *   Helpers to obtain a Rust reference to a value via a VM reference. Required for
 *   equalities.
 *
 **************************************************************************************/
trait VMValueRef<T> {
    fn value_ref(&self) -> PartialVMResult<&T>;
}

macro_rules! impl_vm_value_ref {
    ($ty: ty, $tc: ident) => {
        impl VMValueRef<$ty> for ValueImpl {
            fn value_ref(&self) -> PartialVMResult<&$ty> {
                match self {
                    ValueImpl::$tc(x) => Ok(x),
                    _ => Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR)
                        .with_message(format!("cannot take {:?} as &{}", self, stringify!($ty)))),
                }
            }
        }
    };
}

impl_vm_value_ref!(u8, U8);
impl_vm_value_ref!(u64, U64);
impl_vm_value_ref!(u128, U128);
impl_vm_value_ref!(bool, Bool);
impl_vm_value_ref!(AccountAddress, Address);

/***************************************************************************************
 *
 * Copy Value
 *
 *   Implementation of Move copy. Extra care needs to be taken when copying references.
 *   It is intentional we avoid implementing the standard library trait Clone, to prevent
 *   surprising behaviors from happening.
 *
 **************************************************************************************/
// impl ValueImpl {
//     fn copy_value(&self) -> PartialVMResult<Self> {
//         use ValueImpl::*;

//         Ok(match self {
//             Invalid => Invalid,

//             U8(x) => U8(*x),
//             U64(x) => U64(*x),
//             U128(x) => U128(*x),
//             Bool(x) => Bool(*x),
//             Address(x) => Address(*x),

//             Reference(r) => Reference(r.copy_value()),

//             // When cloning a container, we need to make sure we make a deep
//             // copy of the data instead of a shallow copy of the Rc.
//             Container(c) => Container(c.copy_value()?),
//         })
//     }
// }

// impl Container {
//     fn copy_value(&self) -> PartialVMResult<Self> {
//         let copy_vec_val = |r: &Vec<ValueImpl>| -> PartialVMResult<Vec<ValueImpl>> {
//             Ok(r.iter()
//                 .map(|v| v.copy_value())
//                 .collect::<PartialVMResult<_>>()?)
//         };

//         Ok(match self {
//             Self::Vec(v) => Self::Vec(copy_vec_val(v)?),
//             Self::Struct(v) => Self::Struct(copy_vec_val(v)?),

//             Self::VecU8(v) => Self::VecU8(v.clone()),
//             Self::VecU64(v) => Self::VecU64(v.clone()),
//             Self::VecU128(v) => Self::VecU128(v.clone()),
//             Self::VecBool(v) => Self::VecBool(v.clone()),
//             Self::VecAddress(v) => Self::VecAddress(v.clone()),
//         })
//     }
// }

// impl ReferenceImpl {
//     fn copy_value(&self) -> Self {
//         match self {
//             ReferenceImpl::CheckedRef(r) => ReferenceImpl::CheckedRef(*r),
//             ReferenceImpl::ExternalRef(r) => ReferenceImpl::ExternalRef(r.copy_value()),
//         }
//     }
// }

// impl ExternalRef {
//     fn copy_value(&self) -> Self {
//         let ExternalRef {
//             global_status,
//             source,
//             checked_ref,
//         } = self;
//         ExternalRef {
//             global_status: global_status.clone(),
//             source: source.clone(),
//             checked_ref: *checked_ref,
//         }
//     }
// }

// impl Value {
//     pub fn copy_value(&self) -> PartialVMResult<Self> {
//         Ok(Self(self.0.copy_value()?))
//     }
// }

/***************************************************************************************
 *
 * Equality
 *
 *   Equality tests of Move values. Errors are raised when types mismatch.
 *
 *   It is intented to NOT use or even implement the standard library traits Eq and
 *   Partial Eq due to:
 *     1. They do not allow errors to be returned.
 *     2. They can be invoked without the user being noticed thanks to operator
 *        overloading.
 *
 *   Eq and Partial Eq must also NOT be derived for the reasons above plus that the
 *   derived implementation differs from the semantics we want.
 *
 **************************************************************************************/

impl ValueImpl {
    fn equals(&self, other: &Self) -> PartialVMResult<bool> {
        use ValueImpl::*;

        let res = match (self, other) {
            (U8(l), U8(r)) => l == r,
            (U64(l), U64(r)) => l == r,
            (U128(l), U128(r)) => l == r,
            (Bool(l), Bool(r)) => l == r,
            (Address(l), Address(r)) => l == r,

            (Container(l), Container(r)) => l.equals(r)?,

            (Reference(l), Reference(r)) => l.equals(r)?,

            _ => {
                return Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR)
                    .with_message(format!("cannot compare values: {:?}, {:?}", self, other)))
            }
        };

        Ok(res)
    }
}

impl Container {
    fn equals(&self, other: &Self) -> PartialVMResult<bool> {
        use Container::*;

        let res = match (self, other) {
            (Vec(l), Vec(r)) | (Struct(l), Struct(r)) => {
                if l.len() != r.len() {
                    return Ok(false);
                }
                for (v1, v2) in l.iter().zip(r.iter()) {
                    if !v1.equals(v2)? {
                        return Ok(false);
                    }
                }
                true
            }
            (VecU8(l), VecU8(r)) => l == r,
            (VecU64(l), VecU64(r)) => l == r,
            (VecU128(l), VecU128(r)) => l == r,
            (VecBool(l), VecBool(r)) => l == r,
            (VecAddress(l), VecAddress(r)) => l == r,

            (Vec(_), _)
            | (Struct(_), _)
            | (VecU8(_), _)
            | (VecU64(_), _)
            | (VecU128(_), _)
            | (VecBool(_), _)
            | (VecAddress(_), _) => {
                return Err(
                    PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR).with_message(format!(
                        "cannot compare container values: {:?}, {:?}",
                        self, other
                    )),
                )
            }
        };

        Ok(res)
    }
}

impl ReferenceImpl {
    pub(crate) fn equals(&self, other: &ReferenceImpl) -> PartialVMResult<bool> {
        unsafe {
            match (self.checked_ref(), other.checked_ref()) {
                (CheckedRef::U8(l), CheckedRef::U8(r)) => Ok(*l == *r),
                (CheckedRef::U64(l), CheckedRef::U64(r)) => Ok(*l == *r),
                (CheckedRef::U128(l), CheckedRef::U128(r)) => Ok(&*l == &*r),
                (CheckedRef::Bool(l), CheckedRef::Bool(r)) => Ok(*l == *r),
                (CheckedRef::Address(l), CheckedRef::Address(r)) => Ok(&*l == &*r),
                (CheckedRef::Container(l), CheckedRef::Container(r)) => (&*l).equals(&*r),
                (l, r) => Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR)
                    .with_message(format!("cannot compare ref values: {:?}, {:?}", l, r))),
            }
        }
    }
}

impl Value {
    pub fn equals(&self, other: &Self) -> PartialVMResult<bool> {
        self.0.equals(&other.0)
    }
}

/***************************************************************************************
 *
 * Read Ref
 *
 *   Implementation of the Move operation read ref.
 *
 **************************************************************************************/

impl ReferenceImpl {
    fn read_ref(self) -> Value {
        let v = unsafe {
            match self.checked_ref() {
                CheckedRef::U8(u) => ValueImpl::U8(*u),
                CheckedRef::U64(u) => ValueImpl::U64(*u),
                CheckedRef::U128(u) => ValueImpl::U128(*u),
                CheckedRef::Bool(b) => ValueImpl::Bool(*b),
                CheckedRef::Address(a) => ValueImpl::Address(*a),
                CheckedRef::Container(c) => ValueImpl::Container((&*c).clone()),
            }
        };
        Value(v)
    }
}

impl Reference {
    pub fn read_ref(self) -> Value {
        self.0.read_ref()
    }
}

/***************************************************************************************
 *
 * Write Ref
 *
 *   Implementation of the Move operation write ref.
 *
 **************************************************************************************/

impl ReferenceImpl {
    fn write_ref(self, v: Value) -> PartialVMResult<()> {
        self.mark_dirty();
        unsafe {
            match (self.checked_ref(), v.0) {
                (CheckedRef::U8(r), ValueImpl::U8(v)) => *r = v,
                (CheckedRef::U64(r), ValueImpl::U64(v)) => *r = v,
                (CheckedRef::U128(r), ValueImpl::U128(v)) => *r = v,
                (CheckedRef::Bool(r), ValueImpl::Bool(v)) => *r = v,
                (CheckedRef::Address(r), ValueImpl::Address(v)) => *r = v,
                (CheckedRef::Container(r), ValueImpl::Container(v)) => *r = v,
                (r, v) => {
                    return Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR)
                        .with_message(format!("cannot assign ref with value: {:?}, {:?}", r, v)))
                }
            }
        };
        Ok(())
    }
}

impl Reference {
    /// Writes to a reference, replacing the value with a new one.
    /// Potentially unsafe if Move's reference safety rules are not upheld
    pub unsafe fn write_ref(self, x: Value) -> PartialVMResult<()> {
        self.0.write_ref(x)
    }
}

/***************************************************************************************
 *
 * Borrows (Move)
 *
 *   Implementation of borrowing in Move: borrow field, borrow local and infrastructure
 *   to support borrowing an element from a vector.
 *
 **************************************************************************************/

impl StructRef {
    pub fn borrow_field(&self, idx: usize) -> PartialVMResult<Value> {
        Ok(Value(ValueImpl::Reference(self.0.borrow_elem(idx)?)))
    }
}

impl Locals {
    pub fn borrow_loc(&mut self, idx: usize) -> PartialVMResult<Value> {
        let locals = &mut self.0;
        if idx >= locals.len() {
            return Err(
                PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR).with_message(
                    format!(
                        "index out of bounds when borrowing local: got: {}, len: {}",
                        idx,
                        locals.len()
                    ),
                ),
            );
        }
        let vref = CheckedRef::new(&mut locals[idx])?;
        Ok(Value(ValueImpl::Reference(ReferenceImpl::CheckedRef(vref))))
    }
}

impl SignerRef {
    pub fn borrow_signer(&self) -> PartialVMResult<Value> {
        Ok(Value(ValueImpl::Reference(self.0.borrow_elem(0)?)))
    }
}

/***************************************************************************************
 *
 * Reference wrappers
 *
 *   Helper functions for the typed reference wrappers
 *
 **************************************************************************************/

impl StructRef {
    pub fn into_ref(self) -> Reference {
        Reference(self.0)
    }
}

impl SignerRef {
    pub fn into_ref(self) -> Reference {
        Reference(self.0)
    }
}

impl VectorRef {
    pub fn into_ref(self) -> Reference {
        Reference(self.0)
    }
}

/***************************************************************************************
 *
 * Locals
 *
 *   Public APIs for Locals to support reading, writing and moving of values.
 *
 **************************************************************************************/

impl Locals {
    pub fn new(n: usize) -> Self {
        Self(vec![ValueImpl::Invalid; n])
    }

    pub fn copy_loc(&self, idx: usize) -> PartialVMResult<Value> {
        let v = &self.0;
        match v.get(idx) {
            Some(ValueImpl::Invalid) => Err(PartialVMError::new(
                StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR,
            )
            .with_message(format!("cannot copy invalid value at index {}", idx))),
            Some(v) => Ok(Value(v.clone())),
            None => Err(
                PartialVMError::new(StatusCode::VERIFIER_INVARIANT_VIOLATION).with_message(
                    format!("local index out of bounds: got {}, len: {}", idx, v.len()),
                ),
            ),
        }
    }

    fn swap_loc(&mut self, idx: usize, x: Value) -> PartialVMResult<Value> {
        let v = &mut self.0;
        match v.get_mut(idx) {
            Some(v) => Ok(Value(std::mem::replace(v, x.0))),
            None => Err(
                PartialVMError::new(StatusCode::VERIFIER_INVARIANT_VIOLATION).with_message(
                    format!("local index out of bounds: got {}, len: {}", idx, v.len()),
                ),
            ),
        }
    }

    pub fn move_loc(&mut self, idx: usize) -> PartialVMResult<Value> {
        match self.swap_loc(idx, Value(ValueImpl::Invalid))? {
            Value(ValueImpl::Invalid) => Err(PartialVMError::new(
                StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR,
            )
            .with_message(format!("cannot move invalid value at index {}", idx))),
            v => Ok(v),
        }
    }

    pub fn store_loc(&mut self, idx: usize, x: Value) -> PartialVMResult<()> {
        self.swap_loc(idx, x)?;
        Ok(())
    }
}

/***************************************************************************************
 *
 * Public Value Constructors
 *
 *   Constructors to allow values to be created outside this module.
 *
 **************************************************************************************/
impl Value {
    pub fn u8(x: u8) -> Self {
        Self(ValueImpl::U8(x))
    }

    pub fn u64(x: u64) -> Self {
        Self(ValueImpl::U64(x))
    }

    pub fn u128(x: u128) -> Self {
        Self(ValueImpl::U128(x))
    }

    pub fn bool(x: bool) -> Self {
        Self(ValueImpl::Bool(x))
    }

    pub fn address(x: AccountAddress) -> Self {
        Self(ValueImpl::Address(x))
    }

    pub fn signer(x: AccountAddress) -> Self {
        Self(ValueImpl::Container(Container::signer(x)))
    }

    /// Create a "unowned" reference to a signer value (&signer) for populating the &signer in
    /// execute function
    pub fn signer_reference(x: AccountAddress) -> Self {
        Self(ValueImpl::Reference(ReferenceImpl::ExternalRef(
            ExternalRef::new_source(None, Rc::new(RefCell::new(Self::signer(x).0))).expect(
                "impossible to have an invalid reference as the value is a valid container",
            ),
        )))
    }

    pub fn struct_(s: Struct) -> Self {
        Self(ValueImpl::Container(Container::Struct(s.fields)))
    }

    // TODO: consider whether we want to replace these with fn vector(v: Vec<Value>).
    pub fn vector_u8(it: impl IntoIterator<Item = u8>) -> Self {
        Self(ValueImpl::Container(Container::VecU8(
            it.into_iter().collect(),
        )))
    }

    pub fn vector_u64(it: impl IntoIterator<Item = u64>) -> Self {
        Self(ValueImpl::Container(Container::VecU64(
            it.into_iter().collect(),
        )))
    }

    pub fn vector_u128(it: impl IntoIterator<Item = u128>) -> Self {
        Self(ValueImpl::Container(Container::VecU128(
            it.into_iter().collect(),
        )))
    }

    pub fn vector_bool(it: impl IntoIterator<Item = bool>) -> Self {
        Self(ValueImpl::Container(Container::VecBool(
            it.into_iter().collect(),
        )))
    }

    pub fn vector_address(it: impl IntoIterator<Item = AccountAddress>) -> Self {
        Self(ValueImpl::Container(Container::VecAddress(
            it.into_iter().collect(),
        )))
    }

    // REVIEW: This API can break
    pub fn vector_for_testing_only(it: impl IntoIterator<Item = Value>) -> Self {
        Self(ValueImpl::Container(Container::Vec(
            it.into_iter().map(|v| v.0).collect(),
        )))
    }
}

/***************************************************************************************
 *
 * Casting
 *
 *   Due to the public value types being opaque to an external user, the following
 *   public APIs are required to enable conversion between types in order to gain access
 *   to specific operations certain more refined types offer.
 *   For example, one must convert a `Value` to a `Struct` before unpack can be called.
 *
 *   It is expected that the caller will keep track of the invariants and guarantee
 *   the conversion will succeed. An error will be raised in case of a violation.
 *
 **************************************************************************************/
pub trait VMValueCast<T> {
    fn cast(self) -> PartialVMResult<T>;
}

macro_rules! impl_vm_value_cast {
    ($ty: ty, $tc: ident) => {
        impl VMValueCast<$ty> for Value {
            fn cast(self) -> PartialVMResult<$ty> {
                match self.0 {
                    ValueImpl::$tc(x) => Ok(x),
                    v => Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR)
                        .with_message(format!("cannot cast {:?} to {}", v, stringify!($ty)))),
                }
            }
        }
    };
}

impl_vm_value_cast!(u8, U8);
impl_vm_value_cast!(u64, U64);
impl_vm_value_cast!(u128, U128);
impl_vm_value_cast!(bool, Bool);
impl_vm_value_cast!(AccountAddress, Address);
impl_vm_value_cast!(ReferenceImpl, Reference);

impl VMValueCast<IntegerValue> for Value {
    fn cast(self) -> PartialVMResult<IntegerValue> {
        match self.0 {
            ValueImpl::U8(x) => Ok(IntegerValue::U8(x)),
            ValueImpl::U64(x) => Ok(IntegerValue::U64(x)),
            ValueImpl::U128(x) => Ok(IntegerValue::U128(x)),
            v => Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR)
                .with_message(format!("cannot cast {:?} to integer", v,))),
        }
    }
}

impl VMValueCast<Reference> for Value {
    fn cast(self) -> PartialVMResult<Reference> {
        match self.0 {
            ValueImpl::Reference(r) => Ok(Reference(r)),
            v => Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR)
                .with_message(format!("cannot cast {:?} to reference", v,))),
        }
    }
}

impl VMValueCast<Container> for Value {
    fn cast(self) -> PartialVMResult<Container> {
        match self.0 {
            ValueImpl::Container(c) => Ok(c),
            v => Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR)
                .with_message(format!("cannot cast {:?} to container", v,))),
        }
    }
}

impl VMValueCast<Struct> for Value {
    fn cast(self) -> PartialVMResult<Struct> {
        match self.0 {
            ValueImpl::Container(Container::Struct(r)) => Ok(Struct { fields: r }),
            v => Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR)
                .with_message(format!("cannot cast {:?} to struct", v,))),
        }
    }
}

impl VMValueCast<StructRef> for Value {
    fn cast(self) -> PartialVMResult<StructRef> {
        Ok(StructRef(VMValueCast::cast(self)?))
    }
}

impl VMValueCast<Vec<u8>> for Value {
    fn cast(self) -> PartialVMResult<Vec<u8>> {
        match self.0 {
            ValueImpl::Container(Container::VecU8(r)) => Ok(r),
            v => Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR)
                .with_message(format!("cannot cast {:?} to vector<u8>", v,))),
        }
    }
}

impl VMValueCast<SignerRef> for Value {
    fn cast(self) -> PartialVMResult<SignerRef> {
        match self.0 {
            ValueImpl::Reference(r) => Ok(SignerRef(r)),
            v => Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR)
                .with_message(format!("cannot cast {:?} to Signer reference", v,))),
        }
    }
}

impl VMValueCast<VectorRef> for Value {
    fn cast(self) -> PartialVMResult<VectorRef> {
        match self.0 {
            ValueImpl::Reference(r) => Ok(VectorRef(r)),
            v => Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR)
                .with_message(format!("cannot cast {:?} to vector reference", v,))),
        }
    }
}

impl VMValueCast<Vector> for Value {
    fn cast(self) -> PartialVMResult<Vector> {
        match self.0 {
            ValueImpl::Container(c) => Ok(Vector(c)),
            v => Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR)
                .with_message(format!("cannot cast {:?} to vector", v,))),
        }
    }
}

impl Value {
    pub fn value_as<T>(self) -> PartialVMResult<T>
    where
        Self: VMValueCast<T>,
    {
        VMValueCast::cast(self)
    }
}

impl VMValueCast<u8> for IntegerValue {
    fn cast(self) -> PartialVMResult<u8> {
        match self {
            Self::U8(x) => Ok(x),
            v => Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR)
                .with_message(format!("cannot cast {:?} to u8", v,))),
        }
    }
}

impl VMValueCast<u64> for IntegerValue {
    fn cast(self) -> PartialVMResult<u64> {
        match self {
            Self::U64(x) => Ok(x),
            v => Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR)
                .with_message(format!("cannot cast {:?} to u64", v,))),
        }
    }
}

impl VMValueCast<u128> for IntegerValue {
    fn cast(self) -> PartialVMResult<u128> {
        match self {
            Self::U128(x) => Ok(x),
            v => Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR)
                .with_message(format!("cannot cast {:?} to u128", v,))),
        }
    }
}

impl IntegerValue {
    pub fn value_as<T>(self) -> PartialVMResult<T>
    where
        Self: VMValueCast<T>,
    {
        VMValueCast::cast(self)
    }
}

/***************************************************************************************
 *
 * Integer Operations
 *
 *   Arithmetic operations and conversions for integer values.
 *
 **************************************************************************************/
impl IntegerValue {
    pub fn add_checked(self, other: Self) -> PartialVMResult<Self> {
        use IntegerValue::*;
        let res = match (self, other) {
            (U8(l), U8(r)) => u8::checked_add(l, r).map(IntegerValue::U8),
            (U64(l), U64(r)) => u64::checked_add(l, r).map(IntegerValue::U64),
            (U128(l), U128(r)) => u128::checked_add(l, r).map(IntegerValue::U128),
            (l, r) => {
                let msg = format!("Cannot add {:?} and {:?}", l, r);
                return Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR).with_message(msg));
            }
        };
        res.ok_or_else(|| PartialVMError::new(StatusCode::ARITHMETIC_ERROR))
    }

    pub fn sub_checked(self, other: Self) -> PartialVMResult<Self> {
        use IntegerValue::*;
        let res = match (self, other) {
            (U8(l), U8(r)) => u8::checked_sub(l, r).map(IntegerValue::U8),
            (U64(l), U64(r)) => u64::checked_sub(l, r).map(IntegerValue::U64),
            (U128(l), U128(r)) => u128::checked_sub(l, r).map(IntegerValue::U128),
            (l, r) => {
                let msg = format!("Cannot sub {:?} from {:?}", r, l);
                return Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR).with_message(msg));
            }
        };
        res.ok_or_else(|| PartialVMError::new(StatusCode::ARITHMETIC_ERROR))
    }

    pub fn mul_checked(self, other: Self) -> PartialVMResult<Self> {
        use IntegerValue::*;
        let res = match (self, other) {
            (U8(l), U8(r)) => u8::checked_mul(l, r).map(IntegerValue::U8),
            (U64(l), U64(r)) => u64::checked_mul(l, r).map(IntegerValue::U64),
            (U128(l), U128(r)) => u128::checked_mul(l, r).map(IntegerValue::U128),
            (l, r) => {
                let msg = format!("Cannot mul {:?} and {:?}", l, r);
                return Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR).with_message(msg));
            }
        };
        res.ok_or_else(|| PartialVMError::new(StatusCode::ARITHMETIC_ERROR))
    }

    pub fn div_checked(self, other: Self) -> PartialVMResult<Self> {
        use IntegerValue::*;
        let res = match (self, other) {
            (U8(l), U8(r)) => u8::checked_div(l, r).map(IntegerValue::U8),
            (U64(l), U64(r)) => u64::checked_div(l, r).map(IntegerValue::U64),
            (U128(l), U128(r)) => u128::checked_div(l, r).map(IntegerValue::U128),
            (l, r) => {
                let msg = format!("Cannot div {:?} by {:?}", l, r);
                return Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR).with_message(msg));
            }
        };
        res.ok_or_else(|| PartialVMError::new(StatusCode::ARITHMETIC_ERROR))
    }

    pub fn rem_checked(self, other: Self) -> PartialVMResult<Self> {
        use IntegerValue::*;
        let res = match (self, other) {
            (U8(l), U8(r)) => u8::checked_rem(l, r).map(IntegerValue::U8),
            (U64(l), U64(r)) => u64::checked_rem(l, r).map(IntegerValue::U64),
            (U128(l), U128(r)) => u128::checked_rem(l, r).map(IntegerValue::U128),
            (l, r) => {
                let msg = format!("Cannot rem {:?} by {:?}", l, r);
                return Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR).with_message(msg));
            }
        };
        res.ok_or_else(|| PartialVMError::new(StatusCode::ARITHMETIC_ERROR))
    }

    pub fn bit_or(self, other: Self) -> PartialVMResult<Self> {
        use IntegerValue::*;
        Ok(match (self, other) {
            (U8(l), U8(r)) => IntegerValue::U8(l | r),
            (U64(l), U64(r)) => IntegerValue::U64(l | r),
            (U128(l), U128(r)) => IntegerValue::U128(l | r),
            (l, r) => {
                let msg = format!("Cannot bit_or {:?} and {:?}", l, r);
                return Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR).with_message(msg));
            }
        })
    }

    pub fn bit_and(self, other: Self) -> PartialVMResult<Self> {
        use IntegerValue::*;
        Ok(match (self, other) {
            (U8(l), U8(r)) => IntegerValue::U8(l & r),
            (U64(l), U64(r)) => IntegerValue::U64(l & r),
            (U128(l), U128(r)) => IntegerValue::U128(l & r),
            (l, r) => {
                let msg = format!("Cannot bit_and {:?} and {:?}", l, r);
                return Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR).with_message(msg));
            }
        })
    }

    pub fn bit_xor(self, other: Self) -> PartialVMResult<Self> {
        use IntegerValue::*;
        Ok(match (self, other) {
            (U8(l), U8(r)) => IntegerValue::U8(l ^ r),
            (U64(l), U64(r)) => IntegerValue::U64(l ^ r),
            (U128(l), U128(r)) => IntegerValue::U128(l ^ r),
            (l, r) => {
                let msg = format!("Cannot bit_xor {:?} and {:?}", l, r);
                return Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR).with_message(msg));
            }
        })
    }

    pub fn shl_checked(self, n_bits: u8) -> PartialVMResult<Self> {
        use IntegerValue::*;

        Ok(match self {
            U8(x) => {
                if n_bits >= 8 {
                    return Err(PartialVMError::new(StatusCode::ARITHMETIC_ERROR));
                }
                IntegerValue::U8(x << n_bits)
            }
            U64(x) => {
                if n_bits >= 64 {
                    return Err(PartialVMError::new(StatusCode::ARITHMETIC_ERROR));
                }
                IntegerValue::U64(x << n_bits)
            }
            U128(x) => {
                if n_bits >= 128 {
                    return Err(PartialVMError::new(StatusCode::ARITHMETIC_ERROR));
                }
                IntegerValue::U128(x << n_bits)
            }
        })
    }

    pub fn shr_checked(self, n_bits: u8) -> PartialVMResult<Self> {
        use IntegerValue::*;

        Ok(match self {
            U8(x) => {
                if n_bits >= 8 {
                    return Err(PartialVMError::new(StatusCode::ARITHMETIC_ERROR));
                }
                IntegerValue::U8(x >> n_bits)
            }
            U64(x) => {
                if n_bits >= 64 {
                    return Err(PartialVMError::new(StatusCode::ARITHMETIC_ERROR));
                }
                IntegerValue::U64(x >> n_bits)
            }
            U128(x) => {
                if n_bits >= 128 {
                    return Err(PartialVMError::new(StatusCode::ARITHMETIC_ERROR));
                }
                IntegerValue::U128(x >> n_bits)
            }
        })
    }

    pub fn lt(self, other: Self) -> PartialVMResult<bool> {
        use IntegerValue::*;

        Ok(match (self, other) {
            (U8(l), U8(r)) => l < r,
            (U64(l), U64(r)) => l < r,
            (U128(l), U128(r)) => l < r,
            (l, r) => {
                let msg = format!(
                    "Cannot compare {:?} and {:?}: incompatible integer types",
                    l, r
                );
                return Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR).with_message(msg));
            }
        })
    }

    pub fn le(self, other: Self) -> PartialVMResult<bool> {
        use IntegerValue::*;

        Ok(match (self, other) {
            (U8(l), U8(r)) => l <= r,
            (U64(l), U64(r)) => l <= r,
            (U128(l), U128(r)) => l <= r,
            (l, r) => {
                let msg = format!(
                    "Cannot compare {:?} and {:?}: incompatible integer types",
                    l, r
                );
                return Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR).with_message(msg));
            }
        })
    }

    pub fn gt(self, other: Self) -> PartialVMResult<bool> {
        use IntegerValue::*;

        Ok(match (self, other) {
            (U8(l), U8(r)) => l > r,
            (U64(l), U64(r)) => l > r,
            (U128(l), U128(r)) => l > r,
            (l, r) => {
                let msg = format!(
                    "Cannot compare {:?} and {:?}: incompatible integer types",
                    l, r
                );
                return Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR).with_message(msg));
            }
        })
    }

    pub fn ge(self, other: Self) -> PartialVMResult<bool> {
        use IntegerValue::*;

        Ok(match (self, other) {
            (U8(l), U8(r)) => l >= r,
            (U64(l), U64(r)) => l >= r,
            (U128(l), U128(r)) => l >= r,
            (l, r) => {
                let msg = format!(
                    "Cannot compare {:?} and {:?}: incompatible integer types",
                    l, r
                );
                return Err(PartialVMError::new(StatusCode::INTERNAL_TYPE_ERROR).with_message(msg));
            }
        })
    }

    pub fn into_value(self) -> Value {
        use IntegerValue::*;

        match self {
            U8(x) => Value::u8(x),
            U64(x) => Value::u64(x),
            U128(x) => Value::u128(x),
        }
    }
}

impl IntegerValue {
    pub fn cast_u8(self) -> PartialVMResult<u8> {
        use IntegerValue::*;

        match self {
            U8(x) => Ok(x),
            U64(x) => {
                if x > (std::u8::MAX as u64) {
                    Err(PartialVMError::new(StatusCode::ARITHMETIC_ERROR)
                        .with_message(format!("Cannot cast u64({}) to u8", x)))
                } else {
                    Ok(x as u8)
                }
            }
            U128(x) => {
                if x > (std::u8::MAX as u128) {
                    Err(PartialVMError::new(StatusCode::ARITHMETIC_ERROR)
                        .with_message(format!("Cannot cast u128({}) to u8", x)))
                } else {
                    Ok(x as u8)
                }
            }
        }
    }

    pub fn cast_u64(self) -> PartialVMResult<u64> {
        use IntegerValue::*;

        match self {
            U8(x) => Ok(x as u64),
            U64(x) => Ok(x),
            U128(x) => {
                if x > (std::u64::MAX as u128) {
                    Err(PartialVMError::new(StatusCode::ARITHMETIC_ERROR)
                        .with_message(format!("Cannot cast u128({}) to u64", x)))
                } else {
                    Ok(x as u64)
                }
            }
        }
    }

    pub fn cast_u128(self) -> PartialVMResult<u128> {
        use IntegerValue::*;

        Ok(match self {
            U8(x) => x as u128,
            U64(x) => x as u128,
            U128(x) => x,
        })
    }
}

/***************************************************************************************
*
* Vector
*
*   Implemented as a built-in data type.
*
**************************************************************************************/

pub const INDEX_OUT_OF_BOUNDS: u64 = NFE_VECTOR_ERROR_BASE + 1;
pub const POP_EMPTY_VEC: u64 = NFE_VECTOR_ERROR_BASE + 2;
pub const VEC_UNPACK_PARITY_MISMATCH: u64 = NFE_VECTOR_ERROR_BASE + 3;

fn check_elem_layout(ty: &Type, v: &Container) -> PartialVMResult<()> {
    match (ty, v) {
        (Type::U8, Container::VecU8(_))
        | (Type::U64, Container::VecU64(_))
        | (Type::U128, Container::VecU128(_))
        | (Type::Bool, Container::VecBool(_))
        | (Type::Address, Container::VecAddress(_))
        | (Type::Signer, Container::Struct(_)) => Ok(()),

        (Type::Vector(_), Container::Vec(_)) => Ok(()),

        (Type::Struct(_), Container::Vec(_))
        | (Type::Signer, Container::Vec(_))
        | (Type::StructInstantiation(_, _), Container::Vec(_)) => Ok(()),

        (Type::Reference(_), _) | (Type::MutableReference(_), _) | (Type::TyParam(_), _) => Err(
            PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                .with_message(format!("invalid type param for vector: {:?}", ty)),
        ),

        (Type::U8, _)
        | (Type::U64, _)
        | (Type::U128, _)
        | (Type::Bool, _)
        | (Type::Address, _)
        | (Type::Signer, _)
        | (Type::Vector(_), _)
        | (Type::Struct(_), _)
        | (Type::StructInstantiation(_, _), _) => Err(PartialVMError::new(
            StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR,
        )
        .with_message(format!(
            "vector elem layout mismatch, expected {:?}, got {:?}",
            ty, v
        ))),
    }
}

impl VectorRef {
    pub fn len(&self, type_param: &Type) -> PartialVMResult<Value> {
        let c = unsafe { &*self.0.container()? };
        check_elem_layout(type_param, c)?;

        let len = match c {
            Container::VecU8(r) => r.len(),
            Container::VecU64(r) => r.len(),
            Container::VecU128(r) => r.len(),
            Container::VecBool(r) => r.len(),
            Container::VecAddress(r) => r.len(),
            Container::Vec(r) => r.len(),
            Container::Struct(_) => unreachable!(),
        };
        Ok(Value::u64(len as u64))
    }

    pub fn push_back(&self, e: Value, type_param: &Type) -> PartialVMResult<()> {
        let c = unsafe { &mut *self.0.container()? };
        check_elem_layout(type_param, c)?;

        match c {
            Container::VecU8(r) => r.push(e.value_as()?),
            Container::VecU64(r) => r.push(e.value_as()?),
            Container::VecU128(r) => r.push(e.value_as()?),
            Container::VecBool(r) => r.push(e.value_as()?),
            Container::VecAddress(r) => r.push(e.value_as()?),
            Container::Vec(r) => r.push(e.0),
            Container::Struct(_) => unreachable!(),
        }

        self.0.mark_dirty();
        Ok(())
    }

    pub fn borrow_elem(&self, idx: usize, type_param: &Type) -> PartialVMResult<Value> {
        let c = unsafe { &mut *self.0.container()? };
        check_elem_layout(type_param, c)?;
        if idx >= c.len() {
            return Err(PartialVMError::new(StatusCode::VECTOR_OPERATION_ERROR)
                .with_sub_status(INDEX_OUT_OF_BOUNDS));
        }
        Ok(Value(ValueImpl::Reference(self.0.borrow_elem(idx)?)))
    }

    pub fn pop(&self, type_param: &Type) -> PartialVMResult<Value> {
        let c = unsafe { &mut *self.0.container()? };
        check_elem_layout(type_param, c)?;

        macro_rules! err_pop_empty_vec {
            () => {
                return Err(PartialVMError::new(StatusCode::VECTOR_OPERATION_ERROR)
                    .with_sub_status(POP_EMPTY_VEC))
            };
        }

        let res = match c {
            Container::VecU8(r) => match r.pop() {
                Some(x) => Value::u8(x),
                None => err_pop_empty_vec!(),
            },
            Container::VecU64(r) => match r.pop() {
                Some(x) => Value::u64(x),
                None => err_pop_empty_vec!(),
            },
            Container::VecU128(r) => match r.pop() {
                Some(x) => Value::u128(x),
                None => err_pop_empty_vec!(),
            },
            Container::VecBool(r) => match r.pop() {
                Some(x) => Value::bool(x),
                None => err_pop_empty_vec!(),
            },
            Container::VecAddress(r) => match r.pop() {
                Some(x) => Value::address(x),
                None => err_pop_empty_vec!(),
            },
            Container::Vec(r) => match r.pop() {
                Some(x) => Value(x),
                None => err_pop_empty_vec!(),
            },
            Container::Struct(_) => unreachable!(),
        };

        self.0.mark_dirty();
        Ok(res)
    }

    pub fn swap(&self, idx1: usize, idx2: usize, type_param: &Type) -> PartialVMResult<()> {
        let c = unsafe { &mut *self.0.container()? };
        check_elem_layout(type_param, c)?;

        macro_rules! swap {
            ($v: expr) => {{
                let v = $v;
                if idx1 >= v.len() || idx2 >= v.len() {
                    return Err(PartialVMError::new(StatusCode::VECTOR_OPERATION_ERROR)
                        .with_sub_status(INDEX_OUT_OF_BOUNDS));
                }
                v.swap(idx1, idx2);
            }};
        }

        match c {
            Container::VecU8(r) => swap!(r),
            Container::VecU64(r) => swap!(r),
            Container::VecU128(r) => swap!(r),
            Container::VecBool(r) => swap!(r),
            Container::VecAddress(r) => swap!(r),
            Container::Vec(r) => swap!(r),
            Container::Struct(_) => unreachable!(),
        }

        self.0.mark_dirty();
        Ok(())
    }
}

impl Vector {
    pub fn pack(type_param: &Type, elements: Vec<Value>) -> PartialVMResult<Value> {
        let container = match type_param {
            Type::U8 => Value::vector_u8(
                elements
                    .into_iter()
                    .map(|v| v.value_as())
                    .collect::<PartialVMResult<Vec<_>>>()?,
            ),
            Type::U64 => Value::vector_u64(
                elements
                    .into_iter()
                    .map(|v| v.value_as())
                    .collect::<PartialVMResult<Vec<_>>>()?,
            ),
            Type::U128 => Value::vector_u128(
                elements
                    .into_iter()
                    .map(|v| v.value_as())
                    .collect::<PartialVMResult<Vec<_>>>()?,
            ),
            Type::Bool => Value::vector_bool(
                elements
                    .into_iter()
                    .map(|v| v.value_as())
                    .collect::<PartialVMResult<Vec<_>>>()?,
            ),
            Type::Address => Value::vector_address(
                elements
                    .into_iter()
                    .map(|v| v.value_as())
                    .collect::<PartialVMResult<Vec<_>>>()?,
            ),

            Type::Signer | Type::Vector(_) | Type::Struct(_) | Type::StructInstantiation(_, _) => {
                Value(ValueImpl::Container(Container::Vec(
                    elements.into_iter().map(|v| v.0).collect(),
                )))
            }

            Type::Reference(_) | Type::MutableReference(_) | Type::TyParam(_) => {
                return Err(
                    PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                        .with_message(format!("invalid type param for vector: {:?}", type_param)),
                )
            }
        };

        Ok(container)
    }

    pub fn empty(type_param: &Type) -> PartialVMResult<Value> {
        Self::pack(type_param, vec![])
    }

    pub fn unpack(self, type_param: &Type, expected_num: u64) -> PartialVMResult<Vec<Value>> {
        check_elem_layout(type_param, &self.0)?;
        let elements: Vec<_> = match self.0 {
            Container::VecU8(r) => r.into_iter().map(Value::u8).collect(),
            Container::VecU64(r) => r.into_iter().map(Value::u64).collect(),
            Container::VecU128(r) => r.into_iter().map(Value::u128).collect(),
            Container::VecBool(r) => r.into_iter().map(Value::bool).collect(),
            Container::VecAddress(r) => r.into_iter().map(Value::address).collect(),
            Container::Vec(r) => r.into_iter().map(Value).collect(),
            Container::Struct(_) => unreachable!(),
        };
        if expected_num as usize == elements.len() {
            Ok(elements)
        } else {
            Err(PartialVMError::new(StatusCode::VECTOR_OPERATION_ERROR)
                .with_sub_status(VEC_UNPACK_PARITY_MISMATCH))
        }
    }

    pub fn destroy_empty(self, type_param: &Type) -> PartialVMResult<()> {
        self.unpack(type_param, 0)?;
        Ok(())
    }

    pub fn to_vec_u8(self) -> PartialVMResult<Vec<u8>> {
        check_elem_layout(&Type::U8, &self.0)?;
        if let Container::VecU8(r) = self.0 {
            Ok(take_unique_ownership(r)?.into_iter().collect())
        } else {
            Err(
                PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                    .with_message("expected vector<u8>".to_string()),
            )
        }
    }
}

/***************************************************************************************
 *
 * Gas
 *
 *   Abstract memory sizes of the VM values.
 *
 **************************************************************************************/

impl Container {
    fn size(&self) -> AbstractMemorySize<GasCarrier> {
        match self {
            Self::Vec(r) | Self::Struct(r) => Struct::size_impl(&*r.borrow()),
            Self::VecU8(r) => AbstractMemorySize::new((r.len() * size_of::<u8>()) as u64),
            Self::VecU64(r) => AbstractMemorySize::new((r.len() * size_of::<u64>()) as u64),
            Self::VecU128(r) => AbstractMemorySize::new((r.len() * size_of::<u128>()) as u64),
            Self::VecBool(r) => AbstractMemorySize::new((r.len() * size_of::<bool>()) as u64),
            Self::VecAddress(r) => {
                AbstractMemorySize::new((r.len() * size_of::<AccountAddress>()) as u64)
            }
        }
    }
}

impl CheckedRef {
    fn size(&self) -> AbstractMemorySize<GasCarrier> {
        REFERENCE_SIZE
    }
}

impl ExternalRef {
    fn size(&self) -> AbstractMemorySize<GasCarrier> {
        REFERENCE_SIZE
    }
}

impl ValueImpl {
    fn size(&self) -> AbstractMemorySize<GasCarrier> {
        use ValueImpl::*;

        match self {
            Invalid | U8(_) | U64(_) | U128(_) | Bool(_) => CONST_SIZE,
            Address(_) => AbstractMemorySize::new(AccountAddress::LENGTH as u64),
            Reference(r) => r.size(),
            // TODO: in case the borrow fails the VM will panic.
            Container(c) => c.size(),
        }
    }
}

impl Struct {
    fn size_impl(fields: &[ValueImpl]) -> AbstractMemorySize<GasCarrier> {
        fields
            .iter()
            .fold(STRUCT_SIZE, |acc, v| acc.map2(v.size(), Add::add))
    }

    pub fn size(&self) -> AbstractMemorySize<GasCarrier> {
        Self::size_impl(&self.fields)
    }
}

impl Value {
    pub fn size(&self) -> AbstractMemorySize<GasCarrier> {
        self.0.size()
    }
}

impl ReferenceImpl {
    fn size(&self) -> AbstractMemorySize<GasCarrier> {
        match self {
            Self::CheckedRef(r) => r.size(),
            Self::ExternalRef(r) => r.size(),
        }
    }
}

impl Reference {
    pub fn size(&self) -> AbstractMemorySize<GasCarrier> {
        self.0.size()
    }
}

impl GlobalValue {
    pub fn size(&self) -> AbstractMemorySize<GasCarrier> {
        // REVIEW: this doesn't seem quite right. Consider changing it to
        // a constant positive size or better, something proportional to the size of the value.
        match &self.0 {
            GlobalValueImpl::Fresh { .. } | GlobalValueImpl::Cached { .. } => REFERENCE_SIZE,
            GlobalValueImpl::Deleted | GlobalValueImpl::None => MIN_EXISTS_DATA_SIZE,
        }
    }
}

/***************************************************************************************
 *
 * Struct Operations
 *
 *   Public APIs for Struct.
 *
 **************************************************************************************/
impl Struct {
    pub fn pack<I: IntoIterator<Item = Value>>(vals: I) -> Self {
        Self {
            fields: vals.into_iter().map(|v| v.0).collect(),
        }
    }

    pub fn unpack(self) -> PartialVMResult<impl Iterator<Item = Value>> {
        Ok(self.fields.into_iter().map(Value))
    }
}

/***************************************************************************************
 *
 * Global Value Operations
 *
 *   Public APIs for GlobalValue. They allow global values to be created from external
 *   source (a.k.a. storage), and references to be taken from them. At the end of the
 *   transaction execution the dirty ones can be identified and wrote back to storage.
 *
 **************************************************************************************/
#[allow(clippy::unnecessary_wraps)]
impl GlobalValueImpl {
    fn cached(val: ValueImpl, status: ExternalDataStatus) -> PartialVMResult<Self> {
        match &val {
            ValueImpl::Container(Container::Struct(_)) => {
                let status = Rc::new(RefCell::new(status));
                Ok(Self::Cached {
                    value: Rc::new(RefCell::new(val)),
                    status,
                })
            }
            _ => Err(
                PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                    .with_message("failed to publish cached: not a resource".to_string()),
            ),
        }
    }

    fn fresh(val: ValueImpl) -> PartialVMResult<Self> {
        match &val {
            ValueImpl::Container(Container::Struct(_)) => Ok(Self::Fresh {
                value: Rc::new(RefCell::new(val)),
            }),
            _ => Err(
                PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                    .with_message("failed to publish fresh: not a resource".to_string()),
            ),
        }
    }

    fn move_from(&mut self) -> PartialVMResult<ValueImpl> {
        let value = match self {
            Self::None | Self::Deleted => {
                return Err(PartialVMError::new(StatusCode::MISSING_DATA))
            }
            Self::Fresh { .. } => match std::mem::replace(self, Self::None) {
                Self::Fresh { value } => value,
                _ => unreachable!(),
            },
            Self::Cached { .. } => match std::mem::replace(self, Self::Deleted) {
                Self::Cached { value, .. } => value,
                _ => unreachable!(),
            },
        };
        if Rc::strong_count(&value) != 1 {
            return Err(
                PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                    .with_message("moving global resource with dangling reference".to_string()),
            );
        }
        Ok(value.replace(ValueImpl::Invalid))
    }

    fn move_to(&mut self, val: ValueImpl) -> PartialVMResult<()> {
        match self {
            Self::Fresh { .. } | Self::Cached { .. } => {
                return Err(PartialVMError::new(StatusCode::RESOURCE_ALREADY_EXISTS))
            }
            Self::None => *self = Self::fresh(val)?,
            Self::Deleted => *self = Self::cached(val, ExternalDataStatus::Dirty)?,
        }
        Ok(())
    }

    fn exists(&self) -> PartialVMResult<bool> {
        match self {
            Self::Fresh { .. } | Self::Cached { .. } => Ok(true),
            Self::None | Self::Deleted => Ok(false),
        }
    }

    fn borrow_global(&self) -> PartialVMResult<ValueImpl> {
        let (global_status, value) = match self {
            Self::None | Self::Deleted => {
                return Err(PartialVMError::new(StatusCode::MISSING_DATA))
            }
            Self::Fresh { value } => (None, value),
            Self::Cached { value, status } => (Some(status.clone()), value),
        };
        Ok(ValueImpl::Reference(ReferenceImpl::ExternalRef(
            ExternalRef::new_source(global_status, value.clone())?,
        )))
    }

    fn into_effect(self) -> PartialVMResult<GlobalValueEffect<ValueImpl>> {
        Ok(match self {
            Self::None => GlobalValueEffect::None,
            Self::Deleted => GlobalValueEffect::Deleted,
            Self::Fresh { value } => GlobalValueEffect::Changed(value.replace(ValueImpl::Invalid)),
            Self::Cached { value, status } => match &*status.as_ref().borrow() {
                ExternalDataStatus::Dirty => {
                    GlobalValueEffect::Changed(value.replace(ValueImpl::Invalid))
                }
                ExternalDataStatus::Clean => GlobalValueEffect::None,
            },
        })
    }

    fn is_mutated(&self) -> bool {
        match self {
            Self::None => false,
            Self::Deleted => true,
            Self::Fresh { value: _ } => true,
            Self::Cached { value: _, status } => match &*status.as_ref().borrow() {
                ExternalDataStatus::Dirty => true,
                ExternalDataStatus::Clean => false,
            },
        }
    }
}

impl GlobalValue {
    pub fn none() -> Self {
        Self(GlobalValueImpl::None)
    }

    pub fn cached(val: Value) -> PartialVMResult<Self> {
        Ok(Self(GlobalValueImpl::cached(
            val.0,
            ExternalDataStatus::Clean,
        )?))
    }

    pub fn move_from(&mut self) -> PartialVMResult<Value> {
        Ok(Value(self.0.move_from()?))
    }

    pub fn move_to(&mut self, val: Value) -> PartialVMResult<()> {
        self.0.move_to(val.0)
    }

    pub fn borrow_global(&self) -> PartialVMResult<Value> {
        Ok(Value(self.0.borrow_global()?))
    }

    pub fn exists(&self) -> PartialVMResult<bool> {
        self.0.exists()
    }

    pub fn into_effect(self) -> PartialVMResult<GlobalValueEffect<Value>> {
        Ok(match self.0.into_effect()? {
            GlobalValueEffect::None => GlobalValueEffect::None,
            GlobalValueEffect::Deleted => GlobalValueEffect::Deleted,
            GlobalValueEffect::Changed(v) => GlobalValueEffect::Changed(Value(v)),
        })
    }

    pub fn is_mutated(&self) -> bool {
        self.0.is_mutated()
    }
}

/***************************************************************************************
*
* Display
*
*   Implementation of the Display trait for VM Values. These are supposed to be more
*   friendly & readable than the generated Debug dump.
*
**************************************************************************************/

impl Display for ValueImpl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Invalid => write!(f, "Invalid"),

            Self::U8(x) => write!(f, "U8({})", x),
            Self::U64(x) => write!(f, "U64({})", x),
            Self::U128(x) => write!(f, "U128({})", x),
            Self::Bool(x) => write!(f, "{}", x),
            Self::Address(addr) => write!(f, "Address({})", addr.short_str_lossless()),

            Self::Container(r) => write!(f, "{}", r),

            Self::Reference(r) => write!(f, "{}", r),
        }
    }
}

fn display_list_of_items<T, I>(items: I, f: &mut fmt::Formatter) -> fmt::Result
where
    T: Display,
    I: IntoIterator<Item = T>,
{
    write!(f, "[")?;
    let mut items = items.into_iter();
    if let Some(x) = items.next() {
        write!(f, "{}", x)?;
        for x in items {
            write!(f, ", {}", x)?;
        }
    }
    write!(f, "]")
}

impl Display for ExternalRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self {
            data_status: global_status,
            source,
            checked_ref,
        } = self;
        write!(
            f,
            "&({:?}, {}, {})",
            &*global_status.as_ref().borrow(),
            Rc::strong_count(source),
            checked_ref,
        )
    }
}

impl Display for CheckedRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unsafe {
            match *self {
                CheckedRef::U8(p) => write!(f, "&{}", ValueImpl::U8(*p)),
                CheckedRef::U64(p) => write!(f, "&{}", ValueImpl::U64(*p)),
                CheckedRef::U128(p) => write!(f, "&{}", ValueImpl::U128(*p)),
                CheckedRef::Bool(p) => write!(f, "&{:?}", ValueImpl::Bool(*p)),
                CheckedRef::Address(p) => write!(f, "&{}", ValueImpl::Address(*p)),
                CheckedRef::Container(p) => write!(f, "&{}", &*p),
            }
        }
    }
}

impl Display for ReferenceImpl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::CheckedRef(r) => write!(f, "{}", r),
            Self::ExternalRef(r) => write!(f, "{}", r),
        }
    }
}

impl Display for Container {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Vec(r) | Self::Struct(r) => display_list_of_items(r.iter(), f),
            Self::VecU8(r) => display_list_of_items(r.iter(), f),
            Self::VecU64(r) => display_list_of_items(r.iter(), f),
            Self::VecU128(r) => display_list_of_items(r.iter(), f),
            Self::VecBool(r) => display_list_of_items(r.iter(), f),
            Self::VecAddress(r) => display_list_of_items(r.iter(), f),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl Display for Locals {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .enumerate()
                .map(|(idx, val)| format!("[{}] {}", idx, val))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

#[allow(dead_code)]
pub mod debug {
    use super::*;
    use std::fmt::Write;

    fn print_invalid<B: Write>(buf: &mut B) -> PartialVMResult<()> {
        debug_write!(buf, "-")
    }

    fn print_u8<B: Write>(buf: &mut B, x: &u8) -> PartialVMResult<()> {
        debug_write!(buf, "{}", x)
    }

    fn print_u64<B: Write>(buf: &mut B, x: &u64) -> PartialVMResult<()> {
        debug_write!(buf, "{}", x)
    }

    fn print_u128<B: Write>(buf: &mut B, x: &u128) -> PartialVMResult<()> {
        debug_write!(buf, "{}", x)
    }

    fn print_bool<B: Write>(buf: &mut B, x: &bool) -> PartialVMResult<()> {
        debug_write!(buf, "{}", x)
    }

    fn print_address<B: Write>(buf: &mut B, x: &AccountAddress) -> PartialVMResult<()> {
        debug_write!(buf, "{}", x)
    }

    fn print_value_impl<B: Write>(buf: &mut B, val: &ValueImpl) -> PartialVMResult<()> {
        match val {
            ValueImpl::Invalid => print_invalid(buf),

            ValueImpl::U8(x) => print_u8(buf, x),
            ValueImpl::U64(x) => print_u64(buf, x),
            ValueImpl::U128(x) => print_u128(buf, x),
            ValueImpl::Bool(x) => print_bool(buf, x),
            ValueImpl::Address(x) => print_address(buf, x),

            ValueImpl::Container(c) => print_container(buf, c),

            ValueImpl::Reference(r) => print_reference_impl(buf, r),
        }
    }

    fn print_list<'a, B, I, X, F>(
        buf: &mut B,
        begin: &str,
        items: I,
        print: F,
        end: &str,
    ) -> PartialVMResult<()>
    where
        B: Write,
        X: 'a,
        I: IntoIterator<Item = &'a X>,
        F: Fn(&mut B, &X) -> PartialVMResult<()>,
    {
        debug_write!(buf, "{}", begin)?;
        let mut it = items.into_iter();
        if let Some(x) = it.next() {
            print(buf, x)?;
            for x in it {
                debug_write!(buf, ", ")?;
                print(buf, x)?;
            }
        }
        debug_write!(buf, "{}", end)?;
        Ok(())
    }

    fn print_container<B: Write>(buf: &mut B, c: &Container) -> PartialVMResult<()> {
        match c {
            Container::Vec(r) => print_list(buf, "[", r.iter(), print_value_impl, "]"),

            Container::Struct(r) => print_list(buf, "{ ", r.iter(), print_value_impl, " }"),

            Container::VecU8(r) => print_list(buf, "[", r.iter(), print_u8, "]"),
            Container::VecU64(r) => print_list(buf, "[", r.iter(), print_u64, "]"),
            Container::VecU128(r) => print_list(buf, "[", r.iter(), print_u128, "]"),
            Container::VecBool(r) => print_list(buf, "[", r.iter(), print_bool, "]"),
            Container::VecAddress(r) => print_list(buf, "[", r.iter(), print_address, "]"),
        }
    }

    fn print_slice_elem<B, X, F>(buf: &mut B, v: &[X], idx: usize, print: F) -> PartialVMResult<()>
    where
        B: Write,
        F: FnOnce(&mut B, &X) -> PartialVMResult<()>,
    {
        match v.get(idx) {
            Some(x) => print(buf, x),
            None => Err(
                PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR)
                    .with_message("ref index out of bounds".to_string()),
            ),
        }
    }

    fn print_checked_ref<B: Write>(buf: &mut B, checked_ref: CheckedRef) -> PartialVMResult<()> {
        unsafe {
            match checked_ref {
                CheckedRef::U8(p) => print_value_impl(buf, &ValueImpl::U8(*p)),
                CheckedRef::U64(p) => print_value_impl(buf, &ValueImpl::U64(*p)),
                CheckedRef::U128(p) => print_value_impl(buf, &ValueImpl::U128(*p)),
                CheckedRef::Bool(p) => print_value_impl(buf, &ValueImpl::Bool(*p)),
                CheckedRef::Address(p) => print_value_impl(buf, &ValueImpl::Address(*p)),
                CheckedRef::Container(p) => print_container(buf, &*p),
            }
        }
    }

    pub fn print_reference<B: Write>(buf: &mut B, r: &Reference) -> PartialVMResult<()> {
        print_reference_impl(buf, &r.0)
    }

    fn print_reference_impl<B: Write>(buf: &mut B, r: &ReferenceImpl) -> PartialVMResult<()> {
        match r {
            ReferenceImpl::CheckedRef(checked_ref)
            | ReferenceImpl::ExternalRef(ExternalRef { checked_ref, .. }) => {
                print_checked_ref(buf, *checked_ref)
            }
        }
    }

    pub fn print_locals<B: Write>(buf: &mut B, locals: &Locals) -> PartialVMResult<()> {
        // REVIEW: The number of spaces in the indent is currently hard coded.
        for (idx, val) in locals.0.iter().enumerate() {
            debug_write!(buf, "            [{}] ", idx)?;
            print_value_impl(buf, val)?;
            debug_writeln!(buf)?;
        }
        Ok(())
    }

    pub fn print_value<B: Write>(buf: &mut B, val: &Value) -> PartialVMResult<()> {
        print_value_impl(buf, &val.0)
    }
}

/***************************************************************************************
 *
 * Serialization & Deserialization
 *
 *   BCS implementation for VM values. Note although values are represented as Rust
 *   enums that carry type info in the tags, we should NOT rely on them for
 *   serialization:
 *     1) Depending on the specific internal representation, it may be impossible to
 *        reconstruct the layout from a value. For example, one cannot tell if a general
 *        container is a struct or a value.
 *     2) Even if 1) is not a problem at a certain time, we may change to a different
 *        internal representation that breaks the 1-1 mapping. Extremely speaking, if
 *        we switch to untagged unions one day, none of the type info will be carried
 *        by the value.
 *
 *   Therefore the appropriate & robust way to implement serialization & deserialization
 *   is to involve an explicit representation of the type layout.
 *
 **************************************************************************************/
use serde::{
    de::Error as DeError,
    ser::{Error as SerError, SerializeSeq, SerializeTuple},
    Deserialize,
};

impl Value {
    pub fn simple_deserialize(blob: &[u8], layout: &MoveTypeLayout) -> Option<Value> {
        bcs::from_bytes_seed(SeedWrapper { layout }, blob).ok()
    }

    pub fn simple_serialize(&self, layout: &MoveTypeLayout) -> Option<Vec<u8>> {
        bcs::to_bytes(&AnnotatedValue {
            layout,
            val: &self.0,
        })
        .ok()
    }
}

impl Struct {
    pub fn simple_deserialize(blob: &[u8], layout: &MoveStructLayout) -> Option<Struct> {
        bcs::from_bytes_seed(SeedWrapper { layout }, blob).ok()
    }

    pub fn simple_serialize(&self, layout: &MoveStructLayout) -> Option<Vec<u8>> {
        bcs::to_bytes(&AnnotatedValue {
            layout,
            val: &self.fields,
        })
        .ok()
    }
}

struct AnnotatedValue<'a, 'b, T1, T2> {
    layout: &'a T1,
    val: &'b T2,
}

fn invariant_violation<S: serde::Serializer>(message: String) -> S::Error {
    S::Error::custom(
        PartialVMError::new(StatusCode::UNKNOWN_INVARIANT_VIOLATION_ERROR).with_message(message),
    )
}

impl<'a, 'b> serde::Serialize for AnnotatedValue<'a, 'b, MoveTypeLayout, ValueImpl> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match (self.layout, self.val) {
            (MoveTypeLayout::U8, ValueImpl::U8(x)) => serializer.serialize_u8(*x),
            (MoveTypeLayout::U64, ValueImpl::U64(x)) => serializer.serialize_u64(*x),
            (MoveTypeLayout::U128, ValueImpl::U128(x)) => serializer.serialize_u128(*x),
            (MoveTypeLayout::Bool, ValueImpl::Bool(x)) => serializer.serialize_bool(*x),
            (MoveTypeLayout::Address, ValueImpl::Address(x)) => x.serialize(serializer),

            (MoveTypeLayout::Struct(struct_layout), ValueImpl::Container(Container::Struct(r))) => {
                (AnnotatedValue {
                    layout: struct_layout,
                    val: &*r.borrow(),
                })
                .serialize(serializer)
            }

            (MoveTypeLayout::Vector(layout), ValueImpl::Container(c)) => {
                let layout = &**layout;
                match (layout, c) {
                    (MoveTypeLayout::U8, Container::VecU8(r)) => r.serialize(serializer),
                    (MoveTypeLayout::U64, Container::VecU64(r)) => r.serialize(serializer),
                    (MoveTypeLayout::U128, Container::VecU128(r)) => r.serialize(serializer),
                    (MoveTypeLayout::Bool, Container::VecBool(r)) => r.serialize(serializer),
                    (MoveTypeLayout::Address, Container::VecAddress(r)) => r.serialize(serializer),

                    (_, Container::Vec(v)) => {
                        let mut t = serializer.serialize_seq(Some(v.len()))?;
                        for val in v.iter() {
                            t.serialize_element(&AnnotatedValue { layout, val })?;
                        }
                        t.end()
                    }

                    (layout, container) => Err(invariant_violation::<S>(format!(
                        "cannot serialize container {:?} as {:?}",
                        container, layout
                    ))),
                }
            }

            (MoveTypeLayout::Signer, ValueImpl::Container(Container::Struct(v))) => {
                if v.len() != 1 {
                    return Err(invariant_violation::<S>(format!(
                        "cannot serialize container as a signer -- expected 1 field got {}",
                        v.len()
                    )));
                }
                (AnnotatedValue {
                    layout: &MoveTypeLayout::Address,
                    val: &v[0],
                })
                .serialize(serializer)
            }

            (ty, val) => Err(invariant_violation::<S>(format!(
                "cannot serialize value {:?} as {:?}",
                val, ty
            ))),
        }
    }
}

impl<'a, 'b> serde::Serialize for AnnotatedValue<'a, 'b, MoveStructLayout, Vec<ValueImpl>> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let values = &self.val;
        let fields = self.layout.fields();
        if fields.len() != values.len() {
            return Err(invariant_violation::<S>(format!(
                "cannot serialize struct value {:?} as {:?} -- number of fields mismatch",
                self.val, self.layout
            )));
        }
        let mut t = serializer.serialize_tuple(values.len())?;
        for (field_layout, val) in fields.iter().zip(values.iter()) {
            t.serialize_element(&AnnotatedValue {
                layout: field_layout,
                val,
            })?;
        }
        t.end()
    }
}

#[derive(Clone)]
struct SeedWrapper<L> {
    layout: L,
}

impl<'d> serde::de::DeserializeSeed<'d> for SeedWrapper<&MoveTypeLayout> {
    type Value = Value;

    fn deserialize<D: serde::de::Deserializer<'d>>(
        self,
        deserializer: D,
    ) -> Result<Self::Value, D::Error> {
        use MoveTypeLayout as L;

        match self.layout {
            L::Bool => bool::deserialize(deserializer).map(Value::bool),
            L::U8 => u8::deserialize(deserializer).map(Value::u8),
            L::U64 => u64::deserialize(deserializer).map(Value::u64),
            L::U128 => u128::deserialize(deserializer).map(Value::u128),
            L::Address => AccountAddress::deserialize(deserializer).map(Value::address),
            L::Signer => AccountAddress::deserialize(deserializer).map(Value::signer),

            L::Struct(struct_layout) => Ok(Value::struct_(
                SeedWrapper {
                    layout: struct_layout,
                }
                .deserialize(deserializer)?,
            )),

            L::Vector(layout) => {
                let container = match &**layout {
                    L::U8 => Container::VecU8(Vec::deserialize(deserializer)?),
                    L::U64 => Container::VecU64(Vec::deserialize(deserializer)?),
                    L::U128 => Container::VecU128(Vec::deserialize(deserializer)?),
                    L::Bool => Container::VecBool(Vec::deserialize(deserializer)?),
                    L::Address => Container::VecAddress(Vec::deserialize(deserializer)?),
                    layout => {
                        let v = deserializer
                            .deserialize_seq(VectorElementVisitor(SeedWrapper { layout }))?;
                        Container::Vec(v)
                    }
                };
                Ok(Value(ValueImpl::Container(container)))
            }
        }
    }
}

impl<'d> serde::de::DeserializeSeed<'d> for SeedWrapper<&MoveStructLayout> {
    type Value = Struct;

    fn deserialize<D: serde::de::Deserializer<'d>>(
        self,
        deserializer: D,
    ) -> Result<Self::Value, D::Error> {
        let field_layouts = self.layout.fields();
        let fields = deserializer
            .deserialize_tuple(field_layouts.len(), StructFieldVisitor(field_layouts))?;
        Ok(Struct::pack(fields))
    }
}

struct VectorElementVisitor<'a>(SeedWrapper<&'a MoveTypeLayout>);

impl<'d, 'a> serde::de::Visitor<'d> for VectorElementVisitor<'a> {
    type Value = Vec<ValueImpl>;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("Vector")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'d>,
    {
        let mut vals = Vec::new();
        while let Some(elem) = seq.next_element_seed(self.0.clone())? {
            vals.push(elem.0)
        }
        Ok(vals)
    }
}

struct StructFieldVisitor<'a>(&'a [MoveTypeLayout]);

impl<'d, 'a> serde::de::Visitor<'d> for StructFieldVisitor<'a> {
    type Value = Vec<Value>;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("Struct")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'d>,
    {
        let mut val = Vec::new();
        for (i, field_layout) in self.0.iter().enumerate() {
            if let Some(elem) = seq.next_element_seed(SeedWrapper {
                layout: field_layout,
            })? {
                val.push(elem)
            } else {
                return Err(A::Error::invalid_length(i, &self));
            }
        }
        Ok(val)
    }
}

/***************************************************************************************
*
* Constants
*
*   Implementation of deseserialization of constant data into a runtime value
*
**************************************************************************************/

impl Value {
    fn constant_sig_token_to_layout(constant_signature: &SignatureToken) -> Option<MoveTypeLayout> {
        use MoveTypeLayout as L;
        use SignatureToken as S;

        Some(match constant_signature {
            S::Bool => L::Bool,
            S::U8 => L::U8,
            S::U64 => L::U64,
            S::U128 => L::U128,
            S::Address => L::Address,
            S::Signer => return None,
            S::Vector(inner) => L::Vector(Box::new(Self::constant_sig_token_to_layout(inner)?)),
            // Not yet supported
            S::Struct(_) | S::StructInstantiation(_, _) => return None,
            // Not allowed/Not meaningful
            S::TypeParameter(_) | S::Reference(_) | S::MutableReference(_) => return None,
        })
    }

    pub fn deserialize_constant(constant: &Constant) -> Option<Value> {
        let layout = Self::constant_sig_token_to_layout(&constant.type_)?;
        Value::simple_deserialize(&constant.data, &layout)
    }
}

/***************************************************************************************
 *
 * Prop Testing
 *
 *   Random generation of values that fit into a given layout.
 *
 **************************************************************************************/
#[cfg(feature = "fuzzing")]
pub mod prop {
    use super::*;
    use move_core_types::value::{MoveStruct, MoveValue};
    use proptest::{collection::vec, prelude::*};

    pub fn value_strategy_with_layout(layout: &MoveTypeLayout) -> impl Strategy<Value = Value> {
        use MoveTypeLayout as L;

        match layout {
            L::U8 => any::<u8>().prop_map(Value::u8).boxed(),
            L::U64 => any::<u64>().prop_map(Value::u64).boxed(),
            L::U128 => any::<u128>().prop_map(Value::u128).boxed(),
            L::Bool => any::<bool>().prop_map(Value::bool).boxed(),
            L::Address => any::<AccountAddress>().prop_map(Value::address).boxed(),
            L::Signer => any::<AccountAddress>().prop_map(Value::signer).boxed(),

            L::Vector(layout) => match &**layout {
                L::U8 => vec(any::<u8>(), 0..10)
                    .prop_map(|vals| {
                        Value(ValueImpl::Container(Container::VecU8(Rc::new(
                            RefCell::new(vals),
                        ))))
                    })
                    .boxed(),
                L::U64 => vec(any::<u64>(), 0..10)
                    .prop_map(|vals| {
                        Value(ValueImpl::Container(Container::VecU64(Rc::new(
                            RefCell::new(vals),
                        ))))
                    })
                    .boxed(),
                L::U128 => vec(any::<u128>(), 0..10)
                    .prop_map(|vals| {
                        Value(ValueImpl::Container(Container::VecU128(Rc::new(
                            RefCell::new(vals),
                        ))))
                    })
                    .boxed(),
                L::Bool => vec(any::<bool>(), 0..10)
                    .prop_map(|vals| {
                        Value(ValueImpl::Container(Container::VecBool(Rc::new(
                            RefCell::new(vals),
                        ))))
                    })
                    .boxed(),
                L::Address => vec(any::<AccountAddress>(), 0..10)
                    .prop_map(|vals| {
                        Value(ValueImpl::Container(Container::VecAddress(Rc::new(
                            RefCell::new(vals),
                        ))))
                    })
                    .boxed(),
                layout => vec(value_strategy_with_layout(layout), 0..10)
                    .prop_map(|vals| {
                        Value(ValueImpl::Container(Container::Vec(Rc::new(RefCell::new(
                            vals.into_iter().map(|val| val.0).collect(),
                        )))))
                    })
                    .boxed(),
            },

            L::Struct(struct_layout) => struct_layout
                .fields()
                .iter()
                .map(|layout| value_strategy_with_layout(layout))
                .collect::<Vec<_>>()
                .prop_map(move |vals| Value::struct_(Struct::pack(vals)))
                .boxed(),
        }
    }

    pub fn layout_strategy() -> impl Strategy<Value = MoveTypeLayout> {
        use MoveTypeLayout as L;

        let leaf = prop_oneof![
            1 => Just(L::U8),
            1 => Just(L::U64),
            1 => Just(L::U128),
            1 => Just(L::Bool),
            1 => Just(L::Address),
            1 => Just(L::Signer),
        ];

        leaf.prop_recursive(8, 32, 2, |inner| {
            prop_oneof![
                1 => inner.clone().prop_map(|layout| L::Vector(Box::new(layout))),
                1 => vec(inner, 0..1).prop_map(|f_layouts| {
                     L::Struct(MoveStructLayout::new(f_layouts))}),
            ]
        })
    }

    pub fn layout_and_value_strategy() -> impl Strategy<Value = (MoveTypeLayout, Value)> {
        layout_strategy().no_shrink().prop_flat_map(|layout| {
            let value_strategy = value_strategy_with_layout(&layout);
            (Just(layout), value_strategy)
        })
    }

    impl ValueImpl {
        pub fn as_move_value(&self, layout: &MoveTypeLayout) -> MoveValue {
            use MoveTypeLayout as L;

            match (layout, &self) {
                (L::U8, ValueImpl::U8(x)) => MoveValue::U8(*x),
                (L::U64, ValueImpl::U64(x)) => MoveValue::U64(*x),
                (L::U128, ValueImpl::U128(x)) => MoveValue::U128(*x),
                (L::Bool, ValueImpl::Bool(x)) => MoveValue::Bool(*x),
                (L::Address, ValueImpl::Address(x)) => MoveValue::Address(*x),

                (L::Struct(struct_layout), ValueImpl::Container(Container::Struct(r))) => {
                    let mut fields = vec![];
                    for (v, field_layout) in r.borrow().iter().zip(struct_layout.fields().iter()) {
                        fields.push(v.as_move_value(field_layout));
                    }
                    MoveValue::Struct(MoveStruct::new(fields))
                }

                (L::Vector(inner_layout), ValueImpl::Container(c)) => MoveValue::Vector(match c {
                    Container::VecU8(r) => r.borrow().iter().map(|u| MoveValue::U8(*u)).collect(),
                    Container::VecU64(r) => r.borrow().iter().map(|u| MoveValue::U64(*u)).collect(),
                    Container::VecU128(r) => {
                        r.borrow().iter().map(|u| MoveValue::U128(*u)).collect()
                    }
                    Container::VecBool(r) => {
                        r.borrow().iter().map(|u| MoveValue::Bool(*u)).collect()
                    }
                    Container::VecAddress(r) => {
                        r.borrow().iter().map(|u| MoveValue::Address(*u)).collect()
                    }
                    Container::Vec(r) => r
                        .borrow()
                        .iter()
                        .map(|v| v.as_move_value(inner_layout.as_ref()))
                        .collect(),
                    Container::Struct(_) => {
                        panic!("got struct container when converting vec")
                    }
                    Container::Locals(_) => panic!("got locals container when converting vec"),
                }),

                (L::Signer, ValueImpl::Container(Container::Struct(r))) => {
                    let v = r.borrow();
                    if v.len() != 1 {
                        panic!("Unexpected signer layout: {:?}", v);
                    }
                    match &v[0] {
                        ValueImpl::Address(a) => MoveValue::Signer(*a),
                        v => panic!("Unexpected non-address while converting signer: {:?}", v),
                    }
                }

                (layout, val) => panic!("Cannot convert value {:?} as {:?}", val, layout),
            }
        }
    }

    impl Value {
        pub fn as_move_value(&self, layout: &MoveTypeLayout) -> MoveValue {
            self.0.as_move_value(layout)
        }
    }
}
