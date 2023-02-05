// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    file_format::{
        AbilitySet, AddressIdentifierPool, Bytecode, CodeUnit, CompiledScript, ConstantPool,
        FieldHandle, FieldInstantiation, FunctionDefinition, FunctionHandle, FunctionInstantiation,
        IdentifierPool, ModuleHandle, ModuleHandleIndex, SignatureIndex, SignaturePool,
        StructDefInstantiation, StructDefinition, StructHandle,
    },
    file_format_common::Opcodes,
    CompiledModule,
};
use anyhow::{format_err, Result};
use move_core_types::{metadata::Metadata, u256::U256};
use serde::{Deserialize, Serialize, Serializer};
use std::str::FromStr;

#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub(crate) struct CompiledModuleJsonFormat {
    pub version: u32,
    pub self_module_handle_idx: ModuleHandleIndex,
    pub module_handles: Vec<ModuleHandle>,
    pub struct_handles: Vec<StructHandle>,
    pub function_handles: Vec<FunctionHandle>,
    pub field_handles: Vec<FieldHandle>,
    pub friend_decls: Vec<ModuleHandle>,
    pub struct_def_instantiations: Vec<StructDefInstantiation>,
    pub function_instantiations: Vec<FunctionInstantiation>,
    pub field_instantiations: Vec<FieldInstantiation>,
    pub signatures: SignaturePool,
    pub identifiers: IdentifierPool,
    pub address_identifiers: AddressIdentifierPool,
    pub constant_pool: ConstantPool,
    pub metadata: Vec<Metadata>,
    pub struct_defs: Vec<StructDefinition>,
    pub function_defs: Vec<FunctionDefinition>,
}

impl From<CompiledModule> for CompiledModuleJsonFormat {
    fn from(module: CompiledModule) -> Self {
        let CompiledModule {
            version,
            self_module_handle_idx,
            module_handles,
            struct_handles,
            function_handles,
            field_handles,
            friend_decls,
            struct_def_instantiations,
            function_instantiations,
            field_instantiations,
            signatures,
            identifiers,
            address_identifiers,
            constant_pool,
            metadata,
            struct_defs,
            function_defs,
        } = module;
        Self {
            version,
            self_module_handle_idx,
            module_handles,
            struct_handles,
            function_handles,
            field_handles,
            friend_decls,
            struct_def_instantiations,
            function_instantiations,
            field_instantiations,
            signatures,
            identifiers,
            address_identifiers,
            constant_pool,
            metadata,
            struct_defs,
            function_defs,
        }
    }
}

impl From<CompiledModuleJsonFormat> for CompiledModule {
    fn from(val: CompiledModuleJsonFormat) -> Self {
        let CompiledModuleJsonFormat {
            version,
            self_module_handle_idx,
            module_handles,
            struct_handles,
            function_handles,
            field_handles,
            friend_decls,
            struct_def_instantiations,
            function_instantiations,
            field_instantiations,
            signatures,
            identifiers,
            address_identifiers,
            constant_pool,
            metadata,
            struct_defs,
            function_defs,
        } = val;
        CompiledModule {
            version,
            self_module_handle_idx,
            module_handles,
            struct_handles,
            function_handles,
            field_handles,
            friend_decls,
            struct_def_instantiations,
            function_instantiations,
            field_instantiations,
            signatures,
            identifiers,
            address_identifiers,
            constant_pool,
            metadata,
            struct_defs,
            function_defs,
        }
    }
}

impl CompiledModule {
    /// Serialize the module into a JSON string
    /// Note: We do not add `Serialize`, `Deserialize` to `CompiledModule` because we want to avoid misuse serialization
    /// see more detail: https://github.com/move-language/move/pull/508
    pub fn to_json(&self) -> Result<String> {
        serde_json::to_string(&CompiledModuleJsonFormat::from(self.clone())).map_err(Into::into)
    }

    /// Deserialize a module from a JSON string
    pub fn from_json(json: &str) -> Result<Self> {
        let module: CompiledModuleJsonFormat = serde_json::from_str(json)?;
        Ok(module.into())
    }
}

#[derive(Clone, Default, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct CompiledScriptJsonFormat {
    pub version: u32,
    pub module_handles: Vec<ModuleHandle>,
    pub struct_handles: Vec<StructHandle>,
    pub function_handles: Vec<FunctionHandle>,
    pub function_instantiations: Vec<FunctionInstantiation>,
    pub signatures: SignaturePool,
    pub identifiers: IdentifierPool,
    pub address_identifiers: AddressIdentifierPool,
    pub constant_pool: ConstantPool,
    pub metadata: Vec<Metadata>,
    pub code: CodeUnit,
    pub type_parameters: Vec<AbilitySet>,
    pub parameters: SignatureIndex,
}

impl From<CompiledScript> for CompiledScriptJsonFormat {
    fn from(script: CompiledScript) -> Self {
        let CompiledScript {
            version,
            module_handles,
            struct_handles,
            function_handles,
            function_instantiations,
            signatures,
            identifiers,
            address_identifiers,
            constant_pool,
            metadata,
            code,
            type_parameters,
            parameters,
        } = script;
        Self {
            version,
            module_handles,
            struct_handles,
            function_handles,
            function_instantiations,
            signatures,
            identifiers,
            address_identifiers,
            constant_pool,
            metadata,
            code,
            type_parameters,
            parameters,
        }
    }
}

impl From<CompiledScriptJsonFormat> for CompiledScript {
    fn from(val: CompiledScriptJsonFormat) -> Self {
        let CompiledScriptJsonFormat {
            version,
            module_handles,
            struct_handles,
            function_handles,
            function_instantiations,
            signatures,
            identifiers,
            address_identifiers,
            constant_pool,
            metadata,
            code,
            type_parameters,
            parameters,
        } = val;
        CompiledScript {
            version,
            module_handles,
            struct_handles,
            function_handles,
            function_instantiations,
            signatures,
            identifiers,
            address_identifiers,
            constant_pool,
            metadata,
            code,
            type_parameters,
            parameters,
        }
    }
}

impl CompiledScript {
    pub fn to_json(&self) -> Result<String> {
        serde_json::to_string(&CompiledScriptJsonFormat::from(self.clone())).map_err(Into::into)
    }

    pub fn from_json(json: &str) -> Result<Self> {
        let script: CompiledScriptJsonFormat = serde_json::from_str(json)?;
        Ok(script.into())
    }
}

impl ToString for Opcodes {
    fn to_string(&self) -> String {
        match self {
            Opcodes::POP => "POP",
            Opcodes::RET => "RET",
            Opcodes::BR_TRUE => "BR_TRUE",
            Opcodes::BR_FALSE => "BR_FALSE",
            Opcodes::BRANCH => "BRANCH",
            Opcodes::LD_U8 => "LD_U8",
            Opcodes::LD_U64 => "LD_U64",
            Opcodes::LD_U128 => "LD_U128",
            Opcodes::LD_CONST => "LD_CONST",
            Opcodes::LD_TRUE => "LD_TRUE",
            Opcodes::LD_FALSE => "LD_FALSE",
            Opcodes::COPY_LOC => "COPY_LOC",
            Opcodes::MOVE_LOC => "MOVE_LOC",
            Opcodes::ST_LOC => "ST_LOC",
            Opcodes::MUT_BORROW_LOC => "MUT_BORROW_LOC",
            Opcodes::IMM_BORROW_LOC => "IMM_BORROW_LOC",
            Opcodes::MUT_BORROW_FIELD => "MUT_BORROW_FIELD",
            Opcodes::IMM_BORROW_FIELD => "IMM_BORROW_FIELD",
            Opcodes::CALL => "CALL",
            Opcodes::PACK => "PACK",
            Opcodes::UNPACK => "UNPACK",
            Opcodes::READ_REF => "READ_REF",
            Opcodes::WRITE_REF => "WRITE_REF",
            Opcodes::ADD => "ADD",
            Opcodes::SUB => "SUB",
            Opcodes::MUL => "MUL",
            Opcodes::MOD => "MOD",
            Opcodes::DIV => "DIV",
            Opcodes::BIT_OR => "BIT_OR",
            Opcodes::BIT_AND => "BIT_AND",
            Opcodes::XOR => "XOR",
            Opcodes::OR => "OR",
            Opcodes::AND => "AND",
            Opcodes::NOT => "NOT",
            Opcodes::EQ => "EQ",
            Opcodes::NEQ => "NEQ",
            Opcodes::LT => "LT",
            Opcodes::GT => "GT",
            Opcodes::LE => "LE",
            Opcodes::GE => "GE",
            Opcodes::ABORT => "ABORT",
            Opcodes::NOP => "NOP",
            Opcodes::EXISTS => "EXISTS",
            Opcodes::MUT_BORROW_GLOBAL => "MUT_BORROW_GLOBAL",
            Opcodes::IMM_BORROW_GLOBAL => "IMM_BORROW_GLOBAL",
            Opcodes::MOVE_FROM => "MOVE_FROM",
            Opcodes::MOVE_TO => "MOVE_TO",
            Opcodes::FREEZE_REF => "FREEZE_REF",
            Opcodes::SHL => "SHL",
            Opcodes::SHR => "SHR",
            Opcodes::CAST_U8 => "CAST_U8",
            Opcodes::CAST_U64 => "CAST_U64",
            Opcodes::CAST_U128 => "CAST_U128",
            Opcodes::MUT_BORROW_FIELD_GENERIC => "MUT_BORROW_FIELD_GENERIC",
            Opcodes::IMM_BORROW_FIELD_GENERIC => "IMM_BORROW_FIELD_GENERIC",
            Opcodes::CALL_GENERIC => "CALL_GENERIC",
            Opcodes::PACK_GENERIC => "PACK_GENERIC",
            Opcodes::UNPACK_GENERIC => "UNPACK_GENERIC",
            Opcodes::EXISTS_GENERIC => "EXISTS_GENERIC",
            Opcodes::MUT_BORROW_GLOBAL_GENERIC => "MUT_BORROW_GLOBAL_GENERIC",
            Opcodes::IMM_BORROW_GLOBAL_GENERIC => "IMM_BORROW_GLOBAL_GENERIC",
            Opcodes::MOVE_FROM_GENERIC => "MOVE_FROM_GENERIC",
            Opcodes::MOVE_TO_GENERIC => "MOVE_TO_GENERIC",
            Opcodes::VEC_PACK => "VEC_PACK",
            Opcodes::VEC_LEN => "VEC_LEN",
            Opcodes::VEC_IMM_BORROW => "VEC_IMM_BORROW",
            Opcodes::VEC_MUT_BORROW => "VEC_MUT_BORROW",
            Opcodes::VEC_PUSH_BACK => "VEC_PUSH_BACK",
            Opcodes::VEC_POP_BACK => "VEC_POP_BACK",
            Opcodes::VEC_UNPACK => "VEC_UNPACK",
            Opcodes::VEC_SWAP => "VEC_SWAP",
            Opcodes::LD_U16 => "LD_U16",
            Opcodes::LD_U32 => "LD_U32",
            Opcodes::LD_U256 => "LD_U256",
            Opcodes::CAST_U16 => "CAST_U16",
            Opcodes::CAST_U32 => "CAST_U32",
            Opcodes::CAST_U256 => "CAST_U256",
        }
        .to_string()
    }
}

impl FromStr for Opcodes {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "POP" => Ok(Opcodes::POP),
            "RET" => Ok(Opcodes::RET),
            "BR_TRUE" => Ok(Opcodes::BR_TRUE),
            "BR_FALSE" => Ok(Opcodes::BR_FALSE),
            "BRANCH" => Ok(Opcodes::BRANCH),
            "LD_U8" => Ok(Opcodes::LD_U8),
            "LD_U16" => Ok(Opcodes::LD_U16),
            "LD_U32" => Ok(Opcodes::LD_U32),
            "LD_U64" => Ok(Opcodes::LD_U64),
            "LD_U128" => Ok(Opcodes::LD_U128),
            "LD_U256" => Ok(Opcodes::LD_U256),
            "LD_CONST" => Ok(Opcodes::LD_CONST),
            "LD_TRUE" => Ok(Opcodes::LD_TRUE),
            "LD_FALSE" => Ok(Opcodes::LD_FALSE),
            "COPY_LOC" => Ok(Opcodes::COPY_LOC),
            "MOVE_LOC" => Ok(Opcodes::MOVE_LOC),
            "ST_LOC" => Ok(Opcodes::ST_LOC),
            "MUT_BORROW_LOC" => Ok(Opcodes::MUT_BORROW_LOC),
            "IMM_BORROW_LOC" => Ok(Opcodes::IMM_BORROW_LOC),
            "MUT_BORROW_FIELD" => Ok(Opcodes::MUT_BORROW_FIELD),
            "IMM_BORROW_FIELD" => Ok(Opcodes::IMM_BORROW_FIELD),
            "CALL" => Ok(Opcodes::CALL),
            "PACK" => Ok(Opcodes::PACK),
            "UNPACK" => Ok(Opcodes::UNPACK),
            "READ_REF" => Ok(Opcodes::READ_REF),
            "WRITE_REF" => Ok(Opcodes::WRITE_REF),
            "ADD" => Ok(Opcodes::ADD),
            "SUB" => Ok(Opcodes::SUB),
            "MUL" => Ok(Opcodes::MUL),
            "MOD" => Ok(Opcodes::MOD),
            "DIV" => Ok(Opcodes::DIV),
            "BIT_OR" => Ok(Opcodes::BIT_OR),
            "BIT_AND" => Ok(Opcodes::BIT_AND),
            "XOR" => Ok(Opcodes::XOR),
            "OR" => Ok(Opcodes::OR),
            "AND" => Ok(Opcodes::AND),
            "NOT" => Ok(Opcodes::NOT),
            "EQ" => Ok(Opcodes::EQ),
            "NEQ" => Ok(Opcodes::NEQ),
            "LT" => Ok(Opcodes::LT),
            "GT" => Ok(Opcodes::GT),
            "LE" => Ok(Opcodes::LE),
            "GE" => Ok(Opcodes::GE),
            "ABORT" => Ok(Opcodes::ABORT),
            "NOP" => Ok(Opcodes::NOP),
            "EXISTS" => Ok(Opcodes::EXISTS),
            "MUT_BORROW_GLOBAL" => Ok(Opcodes::MUT_BORROW_GLOBAL),
            "IMM_BORROW_GLOBAL" => Ok(Opcodes::IMM_BORROW_GLOBAL),
            "MOVE_FROM" => Ok(Opcodes::MOVE_FROM),
            "MOVE_TO" => Ok(Opcodes::MOVE_TO),
            "FREEZE_REF" => Ok(Opcodes::FREEZE_REF),
            "SHL" => Ok(Opcodes::SHL),
            "SHR" => Ok(Opcodes::SHR),
            "CAST_U8" => Ok(Opcodes::CAST_U8),
            "CAST_U16" => Ok(Opcodes::CAST_U16),
            "CAST_U32" => Ok(Opcodes::CAST_U32),
            "CAST_U64" => Ok(Opcodes::CAST_U64),
            "CAST_U128" => Ok(Opcodes::CAST_U128),
            "CAST_U256" => Ok(Opcodes::CAST_U256),
            "MUT_BORROW_FIELD_GENERIC" => Ok(Opcodes::MUT_BORROW_FIELD_GENERIC),
            "IMM_BORROW_FIELD_GENERIC" => Ok(Opcodes::IMM_BORROW_FIELD_GENERIC),
            "CALL_GENERIC" => Ok(Opcodes::CALL_GENERIC),
            "PACK_GENERIC" => Ok(Opcodes::PACK_GENERIC),
            "UNPACK_GENERIC" => Ok(Opcodes::UNPACK_GENERIC),
            "EXISTS_GENERIC" => Ok(Opcodes::EXISTS_GENERIC),
            "MUT_BORROW_GLOBAL_GENERIC" => Ok(Opcodes::MUT_BORROW_GLOBAL_GENERIC),
            "IMM_BORROW_GLOBAL_GENERIC" => Ok(Opcodes::IMM_BORROW_GLOBAL_GENERIC),
            "MOVE_FROM_GENERIC" => Ok(Opcodes::MOVE_FROM_GENERIC),
            "MOVE_TO_GENERIC" => Ok(Opcodes::MOVE_TO_GENERIC),
            "VEC_PACK" => Ok(Opcodes::VEC_PACK),
            "VEC_LEN" => Ok(Opcodes::VEC_LEN),
            "VEC_IMM_BORROW" => Ok(Opcodes::VEC_IMM_BORROW),
            "VEC_MUT_BORROW" => Ok(Opcodes::VEC_MUT_BORROW),
            "VEC_PUSH_BACK" => Ok(Opcodes::VEC_PUSH_BACK),
            "VEC_POP_BACK" => Ok(Opcodes::VEC_POP_BACK),
            "VEC_UNPACK" => Ok(Opcodes::VEC_UNPACK),
            "VEC_SWAP" => Ok(Opcodes::VEC_SWAP),
            _ => Err(format_err!("Invalid Opcodes: {}", s)),
        }
    }
}

impl ToString for Bytecode {
    fn to_string(&self) -> String {
        let opcodes: Opcodes = self.into();
        match self {
            Bytecode::BrTrue(offset) | Bytecode::BrFalse(offset) | Bytecode::Branch(offset) => {
                format!("{} {}", opcodes.to_string(), offset)
            }
            Bytecode::LdConst(int_const) => format!("{} {}", opcodes.to_string(), int_const),
            Bytecode::CopyLoc(local_index)
            | Bytecode::MoveLoc(local_index)
            | Bytecode::StLoc(local_index) => format!("{} {}", opcodes.to_string(), local_index),
            Bytecode::MutBorrowLoc(local_index) | Bytecode::ImmBorrowLoc(local_index) => {
                format!("{} {}", opcodes.to_string(), local_index)
            }
            Bytecode::MutBorrowField(field_handle_index)
            | Bytecode::ImmBorrowField(field_handle_index) => {
                format!("{} {}", opcodes.to_string(), field_handle_index)
            }
            Bytecode::MutBorrowFieldGeneric(field_inst_index)
            | Bytecode::ImmBorrowFieldGeneric(field_inst_index) => {
                format!("{} {}", opcodes.to_string(), field_inst_index)
            }
            Bytecode::Call(function_handle_index) => {
                format!("{} {}", opcodes.to_string(), function_handle_index,)
            }
            Bytecode::Pack(struct_def_index) | Bytecode::Unpack(struct_def_index) => {
                format!("{} {}", opcodes.to_string(), struct_def_index,)
            }
            Bytecode::PackGeneric(struct_def_inst_index)
            | Bytecode::UnpackGeneric(struct_def_inst_index) => {
                format!("{} {}", opcodes.to_string(), struct_def_inst_index)
            }
            Bytecode::Pop
            | Bytecode::Ret
            | Bytecode::LdTrue
            | Bytecode::LdFalse
            | Bytecode::Shl
            | Bytecode::Shr
            | Bytecode::ReadRef
            | Bytecode::WriteRef
            | Bytecode::FreezeRef
            | Bytecode::Add
            | Bytecode::Sub
            | Bytecode::Mul
            | Bytecode::Mod
            | Bytecode::Div
            | Bytecode::BitOr
            | Bytecode::BitAnd
            | Bytecode::Xor
            | Bytecode::Or
            | Bytecode::And
            | Bytecode::Not
            | Bytecode::Eq
            | Bytecode::Neq
            | Bytecode::Lt
            | Bytecode::Gt
            | Bytecode::Le
            | Bytecode::Ge
            | Bytecode::Abort
            | Bytecode::CastU8
            | Bytecode::CastU16
            | Bytecode::CastU32
            | Bytecode::CastU64
            | Bytecode::CastU128
            | Bytecode::CastU256
            | Bytecode::Nop => opcodes.to_string(),
            Bytecode::MutBorrowGlobal(struct_idx)
            | Bytecode::ImmBorrowGlobal(struct_idx)
            | Bytecode::Exists(struct_idx)
            | Bytecode::MoveFrom(struct_idx) => {
                format!("{} {}", opcodes.to_string(), struct_idx,)
            }
            Bytecode::MutBorrowGlobalGeneric(resource_inst_index)
            | Bytecode::ImmBorrowGlobalGeneric(resource_inst_index)
            | Bytecode::ExistsGeneric(resource_inst_index)
            | Bytecode::MoveFromGeneric(resource_inst_index) => {
                format!("{} {}", opcodes.to_string(), resource_inst_index)
            }
            Bytecode::LdU8(value) => format!("{} {}", opcodes.to_string(), value),
            Bytecode::LdU64(value) => format!("{} {}", opcodes.to_string(), value),
            Bytecode::LdU128(value) => format!("{} {}", opcodes.to_string(), value),
            Bytecode::LdU16(value) => format!("{} {}", opcodes.to_string(), value),
            Bytecode::LdU32(value) => format!("{} {}", opcodes.to_string(), value),
            Bytecode::LdU256(value) => format!("{} {}", opcodes.to_string(), value),
            Bytecode::CallGeneric(idx) => format!("{} {}", opcodes.to_string(), idx),
            Bytecode::MoveTo(struct_idx) => {
                format!("{} {}", opcodes.to_string(), struct_idx,)
            }
            Bytecode::MoveToGeneric(resource_inst_index) => {
                format!("{} {}", opcodes.to_string(), resource_inst_index)
            }
            Bytecode::VecLen(idx)
            | Bytecode::VecImmBorrow(idx)
            | Bytecode::VecMutBorrow(idx)
            | Bytecode::VecPushBack(idx)
            | Bytecode::VecPopBack(idx)
            | Bytecode::VecSwap(idx) => format!("{} {}", opcodes.to_string(), idx),
            Bytecode::VecPack(idx, n) | Bytecode::VecUnpack(idx, n) => {
                format!("{} {} {}", opcodes.to_string(), idx, n)
            }
        }
    }
}

impl FromStr for Bytecode {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tokens = s.split_whitespace();
        let opcodes = tokens
            .next()
            .ok_or_else(|| anyhow::anyhow!("Invalid bytecode: {}. Expected opcodes, found EOF", s))
            .and_then(Opcodes::from_str)?;
        match opcodes {
            Opcodes::BR_TRUE => {
                let offset = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected offset, found EOF", s)
                })??;
                Ok(Bytecode::BrTrue(offset))
            }
            Opcodes::BR_FALSE => {
                let offset = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected offset, found EOF", s)
                })??;
                Ok(Bytecode::BrFalse(offset))
            }
            Opcodes::BRANCH => {
                let offset = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected offset, found EOF", s)
                })??;
                Ok(Bytecode::Branch(offset))
            }
            Opcodes::LD_CONST => {
                let constant = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected constant, found EOF", s)
                })??;
                Ok(Bytecode::LdConst(constant.into()))
            }
            Opcodes::COPY_LOC => {
                let local_index = tokens.next().map(|s| s.parse::<u8>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected local_index, found EOF", s)
                })??;
                Ok(Bytecode::CopyLoc(local_index))
            }
            Opcodes::MOVE_LOC => {
                let local_index = tokens.next().map(|s| s.parse::<u8>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected local_index, found EOF", s)
                })??;
                Ok(Bytecode::MoveLoc(local_index))
            }
            Opcodes::ST_LOC => {
                let local_index = tokens.next().map(|s| s.parse::<u8>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected local_index, found EOF", s)
                })??;
                Ok(Bytecode::StLoc(local_index))
            }
            Opcodes::CALL => {
                let function_handle_index =
                    tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                        anyhow::anyhow!(
                            "Invalid bytecode: {}. Expected function_handle_index, found EOF",
                            s
                        )
                    })??;
                Ok(Bytecode::Call(function_handle_index.into()))
            }
            Opcodes::RET => Ok(Bytecode::Ret),
            Opcodes::ADD => Ok(Bytecode::Add),
            Opcodes::SUB => Ok(Bytecode::Sub),
            Opcodes::MUL => Ok(Bytecode::Mul),
            Opcodes::MOD => Ok(Bytecode::Mod),
            Opcodes::DIV => Ok(Bytecode::Div),
            Opcodes::BIT_OR => Ok(Bytecode::BitOr),
            Opcodes::BIT_AND => Ok(Bytecode::BitAnd),
            Opcodes::XOR => Ok(Bytecode::Xor),
            Opcodes::OR => Ok(Bytecode::Or),
            Opcodes::AND => Ok(Bytecode::And),
            Opcodes::NOT => Ok(Bytecode::Not),
            Opcodes::EQ => Ok(Bytecode::Eq),
            Opcodes::NEQ => Ok(Bytecode::Neq),
            Opcodes::LT => Ok(Bytecode::Lt),
            Opcodes::GT => Ok(Bytecode::Gt),
            Opcodes::LE => Ok(Bytecode::Le),
            Opcodes::GE => Ok(Bytecode::Ge),
            Opcodes::ABORT => Ok(Bytecode::Abort),
            Opcodes::CAST_U8 => Ok(Bytecode::CastU8),
            Opcodes::CAST_U16 => Ok(Bytecode::CastU16),
            Opcodes::CAST_U32 => Ok(Bytecode::CastU32),
            Opcodes::CAST_U64 => Ok(Bytecode::CastU64),
            Opcodes::CAST_U128 => Ok(Bytecode::CastU128),
            Opcodes::CAST_U256 => Ok(Bytecode::CastU256),
            Opcodes::EXISTS => {
                let struct_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected struct_index, found EOF", s)
                })??;
                Ok(Bytecode::Exists(struct_index.into()))
            }
            Opcodes::MUT_BORROW_FIELD => {
                let field_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected field_index, found EOF", s)
                })??;
                Ok(Bytecode::MutBorrowField(field_index.into()))
            }
            Opcodes::IMM_BORROW_FIELD => {
                let field_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected field_index, found EOF", s)
                })??;
                Ok(Bytecode::ImmBorrowField(field_index.into()))
            }
            Opcodes::POP => Ok(Bytecode::Pop),
            Opcodes::LD_TRUE => Ok(Bytecode::LdTrue),
            Opcodes::LD_FALSE => Ok(Bytecode::LdFalse),
            Opcodes::FREEZE_REF => Ok(Bytecode::FreezeRef),
            Opcodes::LD_U64 => {
                let value = tokens.next().map(|s| s.parse::<u64>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected value, found EOF", s)
                })??;
                Ok(Bytecode::LdU64(value))
            }
            Opcodes::MUT_BORROW_LOC => {
                let local_index = tokens.next().map(|s| s.parse::<u8>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected local_index, found EOF", s)
                })??;
                Ok(Bytecode::MutBorrowLoc(local_index))
            }
            Opcodes::IMM_BORROW_LOC => {
                let local_index = tokens.next().map(|s| s.parse::<u8>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected local_index, found EOF", s)
                })??;
                Ok(Bytecode::ImmBorrowLoc(local_index))
            }
            Opcodes::PACK => {
                let struct_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected struct_index, found EOF", s)
                })??;
                Ok(Bytecode::Pack(struct_index.into()))
            }
            Opcodes::UNPACK => {
                let struct_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected struct_index, found EOF", s)
                })??;
                Ok(Bytecode::Unpack(struct_index.into()))
            }
            Opcodes::READ_REF => Ok(Bytecode::ReadRef),
            Opcodes::WRITE_REF => Ok(Bytecode::WriteRef),
            Opcodes::NOP => Ok(Bytecode::Nop),
            Opcodes::MUT_BORROW_GLOBAL => {
                let struct_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected struct_index, found EOF", s)
                })??;
                Ok(Bytecode::MutBorrowGlobal(struct_index.into()))
            }
            Opcodes::IMM_BORROW_GLOBAL => {
                let struct_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected struct_index, found EOF", s)
                })??;
                Ok(Bytecode::ImmBorrowGlobal(struct_index.into()))
            }
            Opcodes::MOVE_FROM => {
                let struct_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected struct_index, found EOF", s)
                })??;
                Ok(Bytecode::MoveFrom(struct_index.into()))
            }
            Opcodes::MOVE_TO => {
                let struct_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected struct_index, found EOF", s)
                })??;
                Ok(Bytecode::MoveTo(struct_index.into()))
            }
            Opcodes::SHL => Ok(Bytecode::Shl),
            Opcodes::SHR => Ok(Bytecode::Shr),
            Opcodes::LD_U8 => {
                let value = tokens.next().map(|s| s.parse::<u8>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected value, found EOF", s)
                })??;
                Ok(Bytecode::LdU8(value))
            }
            Opcodes::LD_U16 => {
                let value = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected value, found EOF", s)
                })??;
                Ok(Bytecode::LdU16(value))
            }
            Opcodes::LD_U32 => {
                let value = tokens.next().map(|s| s.parse::<u32>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected value, found EOF", s)
                })??;
                Ok(Bytecode::LdU32(value))
            }
            Opcodes::LD_U128 => {
                let value = tokens.next().map(|s| s.parse::<u128>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected value, found EOF", s)
                })??;
                Ok(Bytecode::LdU128(value))
            }
            Opcodes::LD_U256 => {
                let value = tokens.next().map(|s| s.parse::<U256>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected value, found EOF", s)
                })??;
                Ok(Bytecode::LdU256(value))
            }
            Opcodes::MUT_BORROW_FIELD_GENERIC => {
                let field_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected field_index, found EOF", s)
                })??;
                Ok(Bytecode::MutBorrowFieldGeneric(field_index.into()))
            }
            Opcodes::IMM_BORROW_FIELD_GENERIC => {
                let field_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected field_index, found EOF", s)
                })??;
                Ok(Bytecode::ImmBorrowFieldGeneric(field_index.into()))
            }
            Opcodes::CALL_GENERIC => {
                let function_index =
                    tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                        anyhow::anyhow!(
                            "Invalid bytecode: {}. Expected function_index, found EOF",
                            s
                        )
                    })??;
                Ok(Bytecode::CallGeneric(function_index.into()))
            }
            Opcodes::PACK_GENERIC => {
                let struct_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected struct_index, found EOF", s)
                })??;
                Ok(Bytecode::PackGeneric(struct_index.into()))
            }
            Opcodes::UNPACK_GENERIC => {
                let struct_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected struct_index, found EOF", s)
                })??;
                Ok(Bytecode::UnpackGeneric(struct_index.into()))
            }
            Opcodes::EXISTS_GENERIC => {
                let struct_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected struct_index, found EOF", s)
                })??;
                Ok(Bytecode::ExistsGeneric(struct_index.into()))
            }
            Opcodes::MUT_BORROW_GLOBAL_GENERIC => {
                let struct_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected struct_index, found EOF", s)
                })??;
                Ok(Bytecode::MutBorrowGlobalGeneric(struct_index.into()))
            }
            Opcodes::IMM_BORROW_GLOBAL_GENERIC => {
                let struct_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected struct_index, found EOF", s)
                })??;
                Ok(Bytecode::ImmBorrowGlobalGeneric(struct_index.into()))
            }
            Opcodes::MOVE_FROM_GENERIC => {
                let struct_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected struct_index, found EOF", s)
                })??;
                Ok(Bytecode::MoveFromGeneric(struct_index.into()))
            }
            Opcodes::MOVE_TO_GENERIC => {
                let struct_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected struct_index, found EOF", s)
                })??;
                Ok(Bytecode::MoveToGeneric(struct_index.into()))
            }
            Opcodes::VEC_PACK => {
                let type_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected type_index, found EOF", s)
                })??;
                let n = tokens.next().map(|s| s.parse::<u64>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected type_index, found EOF", s)
                })??;
                Ok(Bytecode::VecPack(type_index.into(), n))
            }
            Opcodes::VEC_LEN => {
                let type_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected type_index, found EOF", s)
                })??;
                Ok(Bytecode::VecLen(type_index.into()))
            }
            Opcodes::VEC_IMM_BORROW => {
                let type_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected type_index, found EOF", s)
                })??;
                Ok(Bytecode::VecImmBorrow(type_index.into()))
            }
            Opcodes::VEC_MUT_BORROW => {
                let type_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected type_index, found EOF", s)
                })??;
                Ok(Bytecode::VecMutBorrow(type_index.into()))
            }
            Opcodes::VEC_PUSH_BACK => {
                let type_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected type_index, found EOF", s)
                })??;
                Ok(Bytecode::VecPushBack(type_index.into()))
            }
            Opcodes::VEC_POP_BACK => {
                let type_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected type_index, found EOF", s)
                })??;
                Ok(Bytecode::VecPopBack(type_index.into()))
            }
            Opcodes::VEC_UNPACK => {
                let type_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected type_index, found EOF", s)
                })??;
                let n = tokens.next().map(|s| s.parse::<u64>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected type_index, found EOF", s)
                })??;
                Ok(Bytecode::VecUnpack(type_index.into(), n))
            }
            Opcodes::VEC_SWAP => {
                let type_index = tokens.next().map(|s| s.parse::<u16>()).ok_or_else(|| {
                    anyhow::anyhow!("Invalid bytecode: {}. Expected type_index, found EOF", s)
                })??;
                Ok(Bytecode::VecSwap(type_index.into()))
            }
        }
    }
}

impl Serialize for Bytecode {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for Bytecode {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = <String>::deserialize(deserializer)?;
        Bytecode::from_str(&s).map_err(serde::de::Error::custom)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        file_format::{basic_test_module, basic_test_script, CompiledScript},
        CompiledModule,
    };
    use proptest::prelude::*;

    #[test]
    fn test_module_json_format() {
        let module = basic_test_module();
        let json = module.to_json().unwrap();
        println!("{}", json);
        let deserialized_module = CompiledModule::from_json(&json).unwrap();
        assert_eq!(module, deserialized_module);
    }

    #[test]
    fn test_script_json_format() {
        let script = basic_test_script();
        let json = script.to_json().unwrap();
        println!("{}", json);
        let deserialized_script = CompiledScript::from_json(&json).unwrap();
        assert_eq!(script, deserialized_script);
    }

    proptest! {
        #[test]
        fn serializer_roundtrip_module(module in CompiledModule::valid_strategy(20)) {
            let json = module.to_json().unwrap();
            let deserialized_module = CompiledModule::from_json(&json).unwrap();
            prop_assert_eq!(module, deserialized_module);
        }
    }
}
