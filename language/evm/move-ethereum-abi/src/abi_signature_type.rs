// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

// This file defines JSON-ABI types.

use anyhow::anyhow;
use ethabi::{Event, Function};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;

#[derive(Serialize, Deserialize)]
pub struct ABIJsonArg {
    #[serde(rename = "type")]
    pub ty: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub indexed: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub components: Option<Vec<ABIJsonArg>>,
    pub name: String,
}

#[derive(Serialize, Deserialize)]
pub struct ABIJsonSignature {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: String,
    pub inputs: Vec<ABIJsonArg>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub outputs: Option<Vec<ABIJsonArg>>,
    #[serde(rename = "stateMutability", skip_serializing_if = "Option::is_none")]
    pub state_mutability: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub anonymous: Option<bool>,
}

impl TryFrom<&ABIJsonSignature> for Function {
    type Error = anyhow::Error;

    fn try_from(abi: &ABIJsonSignature) -> Result<Function, anyhow::Error> {
        serde_json::from_str(
            &serde_json::to_string_pretty(abi)
                .map_err(|err| anyhow!("Malformed abi: {:?}", err))?,
        )
        .map_err(|err| anyhow!("Unable to decode into ethabi::Function: {:?}", err))
    }
}

impl TryFrom<&ABIJsonSignature> for Event {
    type Error = anyhow::Error;

    fn try_from(abi: &ABIJsonSignature) -> Result<Event, anyhow::Error> {
        serde_json::from_str(
            &serde_json::to_string_pretty(abi)
                .map_err(|err| anyhow!("Malformed abi: {:?}", err))?,
        )
        .map_err(|err| anyhow!("Unable to decode into ethabi::Function: {:?}", err))
    }
}
