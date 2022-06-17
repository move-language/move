// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::util::{convert_rlp_value, ModuleCache};
use anyhow::{anyhow, bail, Result};
use move_core_types::{
    identifier::Identifier, language_storage::ModuleId, resolver::ModuleResolver,
};

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
}
