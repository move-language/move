// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use ethnum::U256 as EthnumU256;
use num::{bigint::Sign, BigInt};
#[cfg(any(test, feature = "fuzzing"))]
use proptest::{prelude::*, strategy::BoxedStrategy};
use std::{
    fmt,
    ops::{BitAnd, BitOr, BitXor, Shl, Shr},
};
use uint::FromStrRadixErr;

pub type U256FromStrError = FromStrRadixErr;

// This U256 impl was chosen for now but we are open to changing it as needed
use primitive_types::U256 as PrimitiveU256;
use serde::{Deserialize, Serialize};

#[allow(non_camel_case_types)]
#[derive(Clone, Deserialize, Serialize, Debug, PartialEq, Eq, Hash, Copy, PartialOrd, Ord)]
pub struct U256Inner(PrimitiveU256);

impl fmt::Display for U256Inner {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::str::FromStr for U256Inner {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self> {
        PrimitiveU256::from_str(s).map(Self).map_err(|e| e.into())
    }
}

impl Shl<u8> for U256Inner {
    type Output = Self;

    fn shl(self, rhs: u8) -> Self::Output {
        let Self(lhs) = self;
        Self(lhs << rhs)
    }
}

impl Shr<u8> for U256Inner {
    type Output = Self;

    fn shr(self, rhs: u8) -> Self::Output {
        let Self(lhs) = self;
        Self(lhs >> rhs)
    }
}

impl BitOr<U256Inner> for U256Inner {
    type Output = Self;

    fn bitor(self, rhs: U256Inner) -> Self::Output {
        let Self(lhs) = self;
        let Self(rhs) = rhs;
        Self(lhs | rhs)
    }
}

impl BitAnd<U256Inner> for U256Inner {
    type Output = Self;

    fn bitand(self, rhs: U256Inner) -> Self::Output {
        let Self(lhs) = self;
        let Self(rhs) = rhs;
        Self(lhs & rhs)
    }
}

impl BitXor<U256Inner> for U256Inner {
    type Output = Self;

    fn bitxor(self, rhs: U256Inner) -> Self::Output {
        let Self(lhs) = self;
        let Self(rhs) = rhs;
        Self(lhs ^ rhs)
    }
}

impl U256Inner {
    pub const fn zero() -> Self {
        Self(PrimitiveU256::zero())
    }

    pub const fn max() -> Self {
        Self(PrimitiveU256::max_value())
    }

    pub fn from_str_radix(src: &str, radix: u32) -> Result<Self, U256FromStrError> {
        PrimitiveU256::from_str_radix(src.trim_start_matches('0'), radix).map(Self)
    }

    pub fn from_le_bytes(slice: &[u8]) -> Self {
        Self(PrimitiveU256::from_little_endian(slice))
    }

    pub fn to_le_bytes(self) -> [u8; 32] {
        let mut bytes = [0u8; 32];
        self.0.to_little_endian(&mut bytes);
        bytes
    }

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

impl From<u8> for U256Inner {
    fn from(n: u8) -> Self {
        U256Inner(PrimitiveU256::from(n))
    }
}

impl From<u16> for U256Inner {
    fn from(n: u16) -> Self {
        U256Inner(PrimitiveU256::from(n))
    }
}

impl From<u32> for U256Inner {
    fn from(n: u32) -> Self {
        U256Inner(PrimitiveU256::from(n))
    }
}

impl From<u64> for U256Inner {
    fn from(n: u64) -> Self {
        U256Inner(PrimitiveU256::from(n))
    }
}

impl From<u128> for U256Inner {
    fn from(n: u128) -> Self {
        U256Inner(PrimitiveU256::from(n))
    }
}

impl From<BigInt> for U256Inner {
    fn from(n: BigInt) -> Self {
        U256Inner(PrimitiveU256::from_little_endian(&n.to_bytes_le().1))
    }
}

impl From<&U256Inner> for BigInt {
    fn from(n: &U256Inner) -> Self {
        BigInt::from_bytes_le(Sign::Plus, &n.to_le_bytes())
    }
}

impl From<U256Inner> for EthnumU256 {
    fn from(n: U256Inner) -> EthnumU256 {
        // TODO (ade): use better solution for conversion
        // Currently using str because EthnumU256 can be little or big endian
        let num_str = format!("{:X}", n.0);
        // TODO (ade): remove expect()
        EthnumU256::from_str_hex(&num_str).expect("Cannot convert to U256")
    }
}

impl From<U256Inner> for u8 {
    fn from(n: U256Inner) -> Self {
        n.0.as_u64() as u8
    }
}

impl From<U256Inner> for u16 {
    fn from(n: U256Inner) -> Self {
        n.0.as_u64() as u16
    }
}

impl From<U256Inner> for u32 {
    fn from(n: U256Inner) -> Self {
        n.0.as_u64() as u32
    }
}

impl From<U256Inner> for u64 {
    fn from(n: U256Inner) -> Self {
        n.0.as_u64()
    }
}

impl From<U256Inner> for u128 {
    fn from(n: U256Inner) -> Self {
        n.0.as_u128()
    }
}

#[cfg(any(test, feature = "fuzzing"))]
impl Arbitrary for U256Inner {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = ();
    fn arbitrary_with(_params: Self::Parameters) -> Self::Strategy {
        // TODO (ade): improve this as is it not exhaustive
        proptest::bits::u8::masked(0xFF)
            .prop_map(|u| U256Inner(PrimitiveU256::from_little_endian(&[u, 32])))
            .boxed()
    }
}
