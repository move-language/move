// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

//! Implementation for Move values
//!
//! REFERENCE SAFETY EXPLANATION
//! References in Move are proved safe statically in the reference safety check
//! (bytecode_verifier/reference_safety). This check guarantees that there are no dangling
//! references and no memory leaks. But the access pattern for reading/writing to Values must
//! abide by these safety rules (which the VM interpreter does. You should refrain from writing to
//! reference values outside of the VM or native functions (with special care))
//! However, it is **very important** that any instance of `Locals` is not dropped until after
//! all Values are consumed. If this is not followed, reading/writing to Values *will* read from
//! invalid pointers.

pub mod values_impl;

#[cfg(test)]
mod value_tests;

#[cfg(all(test, feature = "fuzzing"))]
mod value_prop_tests;

pub use values_impl::*;
