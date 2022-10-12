// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#![forbid(unsafe_code)]
#![cfg_attr(any(feature = "nostd"), no_std)]
#[cfg(feature = "nostd")]
extern crate alloc;

pub mod address;
pub mod character_sets;
#[cfg(not(feature = "nostd"))]
pub mod env;
pub mod files;
pub mod movey_constants;
pub mod parser;
#[cfg(not(feature = "nostd"))]
pub mod testing;
pub mod types;
pub mod values;
