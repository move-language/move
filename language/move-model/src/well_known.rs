// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! Names of well-known functions.
//!
//! This currently only contains those declarations used somewhere, not all well-known
//! declarations. It can be extended on the go.

pub const VECTOR_BORROW_MUT: &str = "vector::borrow_mut";
pub const EVENT_EMIT_EVENT: &str = "event::emit_event";

pub const TYPE_NAME_MOVE: &str = "type_info::type_name";
pub const TYPE_NAME_SPEC: &str = "type_info::$type_name";
pub const TYPE_INFO_MOVE: &str = "type_info::type_of";
pub const TYPE_INFO_SPEC: &str = "type_info::$type_of";
pub const TYPE_SPEC_IS_STRUCT: &str = "type_info::spec_is_struct";
