// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

use crate::abi_signature_type::ABIJsonSignature;

/// Key for metadata
const _ABI_ETHER_MOVE_KEY: &str = "abi_ethereum_move";

#[derive(Serialize, Deserialize)]
pub struct ABIMoveSignature {
    // Move type -> Ethereum event abi
    pub event_map: BTreeMap<String, ABIJsonSignature>,

    // Move function -> Ethereum pub function abi
    pub func_map: BTreeMap<String, ABIJsonSignature>,
}
