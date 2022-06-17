// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::LRU_CACHE_SIZE;
use anyhow::{anyhow, bail, Result};
use ethabi::{Event, Function, Token};
use ethereum_types::U256;
use lru::LruCache;
use move_binary_format::CompiledModule;
use move_core_types::{
    account_address::AccountAddress,
    identifier::Identifier,
    language_storage::{ModuleId, StructTag},
    resolver::ModuleResolver,
    value::MoveValue,
};
use move_ethereum_abi::abi_move_type::{ABIMoveSignature, ABI_ETHER_MOVE_KEY};
use std::{collections::HashMap, convert::TryFrom, str::FromStr};

pub(crate) struct ModuleCache<R: ModuleResolver> {
    resolver: R,
    cache: LruCache<ModuleId, CacheEntry>,
    // TODO: Review the selector map's value type. Will function name be sufficient here or could there be hash collision issues?
    selector_cache: LruCache<[u8; 4], String>,
}

struct CacheEntry {
    function_table: HashMap<String, Function>,
    event_table: HashMap<String, Event>,
}

impl CacheEntry {
    pub fn new(abi: &ABIMoveSignature) -> Result<Self> {
        let mut function_table = HashMap::new();
        for (func_name, sig) in abi.func_map.iter() {
            function_table.insert(func_name.clone(), Function::try_from(sig)?);
        }

        let mut event_table = HashMap::new();
        for (event_name, sig) in abi.func_map.iter() {
            event_table.insert(event_name.clone(), Event::try_from(sig)?);
        }

        Ok(CacheEntry {
            function_table,
            event_table,
        })
    }
}

impl<R: ModuleResolver> ModuleCache<R> {
    pub fn new(resolver: R) -> Self {
        Self {
            resolver,
            cache: LruCache::new(LRU_CACHE_SIZE),
            selector_cache: LruCache::new(LRU_CACHE_SIZE),
        }
    }

    // TODO: Revisit whether to return `Result<Option<&Event>>` instead of a single result.
    pub fn get_event(&mut self, tag: &StructTag) -> Result<&Event> {
        let module_id = tag.module_id();
        if !self.cache.contains(&module_id) {
            self.fetch_module(&module_id)?;
        };
        self.cache
            .get(&module_id)
            .unwrap()
            .event_table
            .get(tag.name.as_str())
            .ok_or_else(|| anyhow!("Event name doesn't exist"))
    }

    pub fn get_function(
        &mut self,
        module_id: &ModuleId,
        function_hash: [u8; 4],
    ) -> Result<(Identifier, &Function)> {
        if !self.cache.contains(module_id) || !self.selector_cache.contains(&function_hash) {
            self.fetch_module(module_id)?;
        };
        let entry = self.cache.get(module_id).unwrap();
        let func_name = self
            .selector_cache
            .get(&function_hash)
            .ok_or_else(|| anyhow!("Selector doesn't exist"))?;

        Ok((
            Identifier::from_str(func_name.as_str())
                .map_err(|_| anyhow!("Malformed function name"))?,
            entry
                .function_table
                .get(func_name.as_str())
                .ok_or_else(|| anyhow!("Function name doesn't exist"))?,
        ))
    }

    fn fetch_module(&mut self, module_id: &ModuleId) -> Result<()> {
        let module_bytes = self
            .resolver
            .get_module(module_id)
            .map_err(|err| anyhow!("Failed to get module: {:?}", err))?
            .ok_or_else(|| anyhow!("Module not found for: {:?}", module_id))?;
        let module = CompiledModule::deserialize(module_bytes.as_slice()).map_err(|err| {
            anyhow!(
                "Failed to deserialize from bytes for module {:?}: {:?}",
                module_id,
                err
            )
        })?;
        let metadata = module
            .metadata
            .iter()
            .find(|metadata| &metadata.key == ABI_ETHER_MOVE_KEY.as_bytes())
            .ok_or_else(|| anyhow!("Target module doesn't have ethereum abi metadata"))?;
        let module_abi: ABIMoveSignature =
            serde_json::from_slice(&metadata.value).map_err(|err| {
                anyhow!(
                    "Failed to read ethereum abi for module {:?}: {:?}",
                    module_id,
                    err
                )
            })?;

        let cache_entry = CacheEntry::new(&module_abi)?;
        for (func_name, func) in cache_entry.function_table.iter() {
            self.selector_cache
                .push(func.short_signature(), func_name.clone());
        }
        self.cache.push(module_id.clone(), cache_entry);
        Ok(())
    }
}

pub(crate) fn convert_move_value(v: &MoveValue) -> Token {
    match v {
        MoveValue::U8(u) => Token::Uint(U256::from(*u)),
        MoveValue::U64(u) => Token::Uint(U256::from(*u)),
        MoveValue::U128(u) => Token::Uint(U256::from(*u)),
        MoveValue::Bool(b) => Token::Bool(*b),
        // Move address are converted as U256 as ethereum will encode all U128, U160, U256 as U256 in encoding process.
        MoveValue::Address(a) => Token::Uint(U256::from(a.as_slice())),
        MoveValue::Vector(values) => Token::Array(values.iter().map(convert_move_value).collect()),
        MoveValue::Struct(s) => {
            Token::FixedArray(s.fields().iter().map(convert_move_value).collect())
        }
        MoveValue::Signer(a) => Token::Uint(U256::from(a.as_slice())),
    }
}

pub(crate) fn convert_rlp_value(t: &Token) -> Result<MoveValue> {
    Ok(match t {
        Token::Address(addr) => MoveValue::Address(
            if cfg!(feature = "address20") {
                AccountAddress::from_bytes(addr.as_fixed_bytes())
            } else if cfg!(feature = "address32") {
                let mut slice = [0_u8; 32];
                // Fill the top bytes with zero
                slice[13..32].copy_from_slice(addr.as_bytes());
                AccountAddress::from_bytes(slice)
            } else {
                // Only take the bottom 16 bytes for Move.
                AccountAddress::from_bytes(&addr.as_bytes()[5..20])
            }
            .unwrap(),
        ),
        Token::Array(tokens) => MoveValue::Vector(
            tokens
                .iter()
                .map(convert_rlp_value)
                .collect::<Result<Vec<_>>>()?,
        ),
        Token::Bool(b) => MoveValue::Bool(*b),
        Token::Bytes(b) => MoveValue::vector_u8(b.to_vec()),
        Token::FixedBytes(b) => MoveValue::vector_u8(b.to_vec()),
        Token::String(s) => MoveValue::vector_u8(s.clone().into_bytes()),
        Token::Uint(u) => MoveValue::U128(u.as_u128()),
        Token::FixedArray(_) | Token::Int(_) | Token::Tuple(_) => bail!("Unsupported types"),
    })
}
