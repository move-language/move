// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

// This file defines functions for generating JSON-ABI.

use serde::{Deserialize, Serialize};

use crate::{
    attributes::FunctionAttribute,
    events::EventSignature,
    solidity_ty::{SoliditySignature, SolidityType},
};

#[derive(Serialize, Deserialize)]
pub(crate) struct ABIJsonArg {
    #[serde(rename = "type")]
    pub ty: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub indexed: Option<bool>,
    pub name: String,
}

#[derive(Serialize, Deserialize)]
pub(crate) struct ABIJsonSignature {
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

impl ABIJsonArg {
    pub(crate) fn from_ty(ty: &SolidityType, name: String) -> Self {
        ABIJsonArg {
            ty: ty.to_string(),
            indexed: None,
            name,
        }
    }

    pub(crate) fn from_event_ty(ty: &SolidityType, indexed: bool, name: String) -> Self {
        ABIJsonArg {
            ty: ty.to_string(),
            indexed: Some(indexed),
            name,
        }
    }
}

impl ABIJsonSignature {
    pub(crate) fn from_solidity_sig(
        sig: &SoliditySignature,
        attr: Option<FunctionAttribute>,
        fun_typ: &str,
    ) -> Self {
        let name = sig.sig_name.clone();
        let mut inputs = vec![];
        let mut outputs = vec![];
        for (ty, para_name, _) in &sig.para_types {
            inputs.push(ABIJsonArg::from_ty(ty, para_name.clone()));
        }
        for (ty, _) in &sig.ret_types {
            outputs.push(ABIJsonArg::from_ty(ty, "".to_string()));
        }
        let state_mutability = (if let Some(FunctionAttribute::View) = attr {
            "view"
        } else if let Some(FunctionAttribute::Pure) = attr {
            "pure"
        } else if let Some(FunctionAttribute::Payable) = attr {
            "payable"
        } else {
            "nonpayable"
        })
        .to_string();
        let anonymous = None;
        ABIJsonSignature {
            name,
            ty: fun_typ.to_string(),
            inputs,
            outputs: Some(outputs),
            state_mutability: Some(state_mutability),
            anonymous,
        }
    }

    pub(crate) fn from_event_sig(sig: &EventSignature) -> Self {
        let name = sig.event_name.clone();
        let ty = "event".to_string();
        let mut inputs = vec![];
        for (_, ty, _, indexed_flag, ev_name) in &sig.para_types {
            inputs.push(ABIJsonArg::from_event_ty(
                ty,
                *indexed_flag,
                ev_name.clone(),
            ));
        }
        ABIJsonSignature {
            name,
            ty,
            inputs,
            outputs: None,
            state_mutability: None,
            anonymous: Some(false),
        }
    }
}
