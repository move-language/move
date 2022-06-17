// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    util::{convert_move_value, ModuleCache},
    LRU_CACHE_SIZE,
};
use anyhow::{anyhow, bail, Result};
use ethabi::{encode, RawLog};
use ethereum_types::H256;
use lru::LruCache;
use move_core_types::value::MoveValue;
use move_core_types::{
    effects::Event, language_storage::TypeTag, resolver::MoveResolver, value::MoveTypeLayout,
};
use move_vm_runtime::move_vm::MoveVM;

pub struct MoveEventAnnotator<R: MoveResolver> {
    resolver: R,
    abi_cache: ModuleCache<R>,
    layout_cache: LruCache<TypeTag, MoveTypeLayout>,
}

impl<R: MoveResolver + Clone> MoveEventAnnotator<R> {
    pub fn new(resolver: R) -> Self {
        Self {
            resolver: resolver.clone(),
            abi_cache: ModuleCache::new(resolver),
            layout_cache: LruCache::new(LRU_CACHE_SIZE),
        }
    }

    pub fn annotate_move_event(&mut self, event: Event) -> Result<RawLog> {
        let struct_tag = {
            use TypeTag::*;
            match &event.2 {
                Struct(s) => s,
                Bool | U8 | U64 | U128 | Address | Signer | Vector(_) => {
                    bail!("Unsupported primitive types")
                }
            }
        };

        let move_value = {
            if !self.layout_cache.contains(&event.2) {
                let move_vm = MoveVM::new(vec![].into_iter()).unwrap();
                let layout = move_vm
                    .new_session(&self.resolver)
                    .get_type_layout(&event.2)
                    .map_err(|err| {
                        anyhow!("Failed to get layout from type {:?}: {:?}", event.2, err)
                    })?;
                self.layout_cache.push(event.2.clone(), layout);
            }
            MoveValue::simple_deserialize(
                event.0.as_slice(),
                self.layout_cache.get(&event.2).unwrap(),
            )
            .map_err(|err| anyhow!("Failed to deserialize move value from events: {:?}", err))?
        };

        let event_abi = self.abi_cache.get_event(struct_tag)?;

        let tokens = {
            use MoveValue::*;
            match move_value {
                Struct(s) => s
                    .fields()
                    .iter()
                    .map(convert_move_value)
                    .collect::<Vec<_>>(),
                U128(_) | U8(_) | U64(_) | Address(_) | Signer(_) | Bool(_) | Vector(_) => {
                    bail!("Unsupported primitive types")
                }
            }
        };

        if tokens.len() != event_abi.inputs.len() {
            bail!("Event argument length mismatch")
        }

        let mut topics = vec![];

        for (token, param) in tokens.iter().zip(event_abi.inputs.iter()) {
            if param.indexed {
                topics.push(H256::from_slice(encode(&[token.clone()]).as_slice()));
            }
        }

        Ok(RawLog {
            data: encode(tokens.as_slice()),
            topics,
        })
    }
}
