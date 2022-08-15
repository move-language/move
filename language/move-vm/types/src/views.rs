// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use move_core_types::{gas_algebra::AbstractMemorySize, language_storage::TypeTag};

/// Trait that provides an abstract view into a Move type.
///
/// This is used to expose certain info to clients (e.g. the gas meter),
/// usually in a lazily evaluated fashion.
pub trait TypeView {
    /// Returns the `TypeTag` (fully qualified name) of the type.
    fn to_type_tag(&self) -> TypeTag;
}

/// Trait that provides an abstract view into a Move Value.
///
/// This is used to expose certain info to clients (e.g. the gas meter),
/// usually in a lazily evaluated fashion.
pub trait ValueView {
    /// Returns the abstract memory size of the value.
    ///
    /// The concept of abstract memory size is not well-defined and is only kept for backward compatibility.
    /// New applications should avoid using this.
    fn legacy_abstract_memory_size(&self) -> AbstractMemorySize;
}

impl<T> ValueView for &T
where
    T: ValueView,
{
    fn legacy_abstract_memory_size(&self) -> AbstractMemorySize {
        <T as ValueView>::legacy_abstract_memory_size(*self)
    }
}

impl<T> TypeView for &T
where
    T: TypeView,
{
    fn to_type_tag(&self) -> TypeTag {
        <T as TypeView>::to_type_tag(*self)
    }
}
