// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::util::{convert_move_value, convert_rlp_value, ModuleCache};
use anyhow::{anyhow, bail, Result};
use move_core_types::{
    identifier::Identifier, language_storage::ModuleId, resolver::ModuleResolver, value::MoveValue,
};
use move_vm_runtime::session::SerializedReturnValues;

pub struct FunctionCallAnnotator<R: ModuleResolver>(ModuleCache<R>);

pub struct MoveCall {
    pub module_id: ModuleId,
    pub function_name: Identifier,
    pub data: Vec<Vec<u8>>,
}

impl<R: ModuleResolver> FunctionCallAnnotator<R> {
    pub fn new(resolver: R) -> Self {
        Self(ModuleCache::new(resolver))
    }

    pub fn annotate_eth_call(
        &mut self,
        module_id: &ModuleId,
        call_data: &[u8],
    ) -> Result<MoveCall> {
        if call_data.len() < 4 {
            bail!("Call data needs at least 4 bytes for the selector")
        }

        let mut selector: [u8; 4] = [0; 4];
        selector.copy_from_slice(&call_data[0..4]);

        let (function_name, func_sig) = self.0.get_function(module_id, selector)?;

        let tokens = func_sig
            .decode_input(&call_data[4..])
            .map_err(|err| anyhow!("Failed to decode from ethereum input: {:?}", err))?;

        let mut data = vec![];
        for token in tokens {
            let move_value = convert_rlp_value(&token)?
                .simple_serialize()
                .ok_or_else(|| anyhow!("Failed to serialize Move value"))?;
            data.push(move_value);
        }

        Ok(MoveCall {
            module_id: module_id.clone(),
            function_name,
            data,
        })
    }

    pub fn encode_return_value(
        &mut self,
        module_id: &ModuleId,
        call_data: &[u8],
        return_value: SerializedReturnValues,
    ) -> Result<Vec<u8>> {
        if call_data.len() < 4 {
            bail!("Call data needs at least 4 bytes for the selector")
        }

        let mut selector: [u8; 4] = [0; 4];
        selector.copy_from_slice(&call_data[0..4]);

        let (_, func_sig) = self.0.get_function(module_id, selector)?;

        let mut tokens = vec![];
        for (bytes, layout) in return_value.return_values {
            let move_value =
                MoveValue::simple_deserialize(bytes.as_slice(), &layout).map_err(|err| {
                    anyhow!("Failed to deserialize from move return value: {:?}", err)
                })?;
            tokens.push(convert_move_value(&move_value))
        }

        func_sig
            .encode_input(tokens.as_slice())
            .map_err(|err| anyhow!("Unable to encode to ethereum format: {:?}", err))
    }
}
