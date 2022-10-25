// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0
#![cfg_attr(feature = "nostd", no_std)]
#![forbid(unsafe_code)]

//! The core Move VM logic.
//!
//! It is a design goal for the Move VM to be independent of the Diem blockchain, so that
//! other blockchains can use it as well. The VM isn't there yet, but hopefully will be there
//! soon.
#[cfg(feature = "nostd")]
extern crate alloc;

pub mod data_cache;
mod interpreter;
mod loader;
pub mod logging;
pub mod move_vm;
pub mod native_extensions;
pub mod native_functions;
mod runtime;
pub mod session;
#[macro_use]
#[cfg(not(feature = "nostd"))]
mod tracing;

// Only include debugging functionality in debug builds
#[cfg(not(feature = "nostd"))]
#[cfg(any(debug_assertions, feature = "debugging"))]
mod debug;
#[cfg(not(feature = "nostd"))]
#[cfg(test)]
mod unit_tests;
