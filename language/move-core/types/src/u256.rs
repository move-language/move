// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use num::BigInt;
use std::{
    fmt,
    ops::{BitAnd, BitOr, BitXor, Shl, Shr},
};

// This U256 impl was chosen for now but we are open to changing it as needed
use primitive_types::U256;
use serde::{Deserialize, Serialize};

#[allow(non_camel_case_types)]
#[derive(Clone, Deserialize, Serialize, Debug, PartialEq, Eq, Hash, Copy, PartialOrd, Ord)]
pub struct u256(U256);

impl fmt::Display for u256 {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Shl<u8> for u256 {
    type Output = Self;

    fn shl(self, rhs: u8) -> Self::Output {
        let Self(lhs) = self;
        Self(lhs << rhs)
    }
}

impl Shr<u8> for u256 {
    type Output = Self;

    fn shr(self, rhs: u8) -> Self::Output {
        let Self(lhs) = self;
        Self(lhs >> rhs)
    }
}

impl BitOr<u256> for u256 {
    type Output = Self;

    fn bitor(self, rhs: u256) -> Self::Output {
        let Self(lhs) = self;
        let Self(rhs) = rhs;
        Self(lhs | rhs)
    }
}

impl BitAnd<u256> for u256 {
    type Output = Self;

    fn bitand(self, rhs: u256) -> Self::Output {
        let Self(lhs) = self;
        let Self(rhs) = rhs;
        Self(lhs & rhs)
    }
}

impl BitXor<u256> for u256 {
    type Output = Self;

    fn bitxor(self, rhs: u256) -> Self::Output {
        let Self(lhs) = self;
        let Self(rhs) = rhs;
        Self(lhs ^ rhs)
    }
}

impl u256 {
    // Unchecked downcasting
    pub fn unchecked_as_u8(&self) -> u8 {
        self.0.as_u128() as u8
    }

    pub fn unchecked_as_u16(&self) -> u16 {
        self.0.as_u128() as u16
    }

    pub fn unchecked_as_u32(&self) -> u32 {
        self.0.as_u128() as u32
    }

    pub fn unchecked_as_u64(&self) -> u64 {
        self.0.as_u128() as u64
    }

    pub fn unchecked_as_u128(&self) -> u128 {
        self.0.as_u128() as u128
    }

    // Check arithmetic
    pub fn checked_add(self, rhs: Self) -> Option<Self> {
        self.0.checked_add(rhs.0).map(Self)
    }
    pub fn checked_sub(self, rhs: Self) -> Option<Self> {
        self.0.checked_sub(rhs.0).map(Self)
    }
    pub fn checked_mul(self, rhs: Self) -> Option<Self> {
        self.0.checked_mul(rhs.0).map(Self)
    }
    pub fn checked_div(self, rhs: Self) -> Option<Self> {
        self.0.checked_div(rhs.0).map(Self)
    }
    pub fn checked_rem(self, rhs: Self) -> Option<Self> {
        self.0.checked_rem(rhs.0).map(Self)
    }
}

impl From<u8> for u256 {
    fn from(n: u8) -> Self {
        u256(U256::from(n))
    }
}

impl From<u16> for u256 {
    fn from(n: u16) -> Self {
        u256(U256::from(n))
    }
}

impl From<u32> for u256 {
    fn from(n: u32) -> Self {
        u256(U256::from(n))
    }
}

impl From<u64> for u256 {
    fn from(n: u64) -> Self {
        u256(U256::from(n))
    }
}

impl From<u128> for u256 {
    fn from(n: u128) -> Self {
        u256(U256::from(n))
    }
}

impl From<BigInt> for u256 {
    fn from(n: BigInt) -> Self {
        u256(U256::from_little_endian(&n.to_bytes_le().1))
    }
}
