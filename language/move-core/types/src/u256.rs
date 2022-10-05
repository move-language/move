// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use std::fmt;

use primitive_types::U256;
use serde::{Deserialize, Serialize};

#[allow(non_camel_case_types)]
#[derive(Clone, Deserialize, Serialize, Debug, PartialEq, Eq, Hash)]
pub struct u256(U256);

impl fmt::Display for u256 {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
