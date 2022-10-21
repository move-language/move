// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

mod binary_tests;
#[cfg(not(feature = "nostd"))]
mod compatibility_tests;
mod deserializer_tests;
mod number_tests;
#[cfg(not(feature = "nostd"))]
mod signature_token_tests;
