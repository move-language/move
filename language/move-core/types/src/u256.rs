// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::anyhow;
use arbitrary::Unstructured;
use ethnum::U256 as EthnumU256;
use num::{bigint::Sign, BigInt};
#[cfg(any(test, feature = "fuzzing"))]
use proptest::strategy::BoxedStrategy;
use proptest::strategy::Strategy;
use std::{
    fmt,
    mem::size_of,
    ops::{BitAnd, BitOr, BitXor, Shl, Shr},
};
use uint::FromStrRadixErr;

const NUM_BITS_PER_BYTE: usize = 8;
const U256_NUM_BITS: usize = 256;
const U256_NUM_BYTES: usize = U256_NUM_BITS / NUM_BITS_PER_BYTE;
pub type U256FromStrError = FromStrRadixErr;

// This U256 impl was chosen for now but we are open to changing it as needed
use primitive_types::U256 as PrimitiveU256;
use serde::{ser::SerializeTuple, Deserialize, Deserializer, Serialize, Serializer};

#[allow(non_camel_case_types)]
#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy, PartialOrd, Ord)]
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

impl<'de> Deserialize<'de> for U256Inner {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(U256Inner::from_le_bytes(
            &(<[u8; U256_NUM_BYTES]>::deserialize(deserializer)?),
        ))
    }
}

impl Serialize for U256Inner {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let b: [u8; U256_NUM_BYTES] = self.to_le_bytes();
        let mut seq = serializer.serialize_tuple(U256_NUM_BYTES)?;
        for x in b {
            seq.serialize_element(&x)?;
        }
        seq.end()
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

    pub const fn one() -> Self {
        Self(PrimitiveU256::one())
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

    pub fn to_le_bytes(self) -> [u8; U256_NUM_BYTES] {
        let mut bytes = [0u8; U256_NUM_BYTES];
        self.0.to_little_endian(&mut bytes);
        bytes
    }

    // Unchecked downcasting
    pub fn unchecked_as_u8(&self) -> u8 {
        self.0.low_u128() as u8
    }

    pub fn unchecked_as_u16(&self) -> u16 {
        self.0.low_u128() as u16
    }

    pub fn unchecked_as_u32(&self) -> u32 {
        self.0.low_u128() as u32
    }

    pub fn unchecked_as_u64(&self) -> u64 {
        self.0.low_u128() as u64
    }

    pub fn unchecked_as_u128(&self) -> u128 {
        self.0.low_u128() as u128
    }

    // Check arithmetic
    /// Checked integer addition. Computes self + rhs, returning None if overflow occurred.
    pub fn checked_add(self, rhs: Self) -> Option<Self> {
        self.0.checked_add(rhs.0).map(Self)
    }

    /// Checked integer subtraction. Computes self - rhs, returning None if overflow occurred.
    pub fn checked_sub(self, rhs: Self) -> Option<Self> {
        self.0.checked_sub(rhs.0).map(Self)
    }

    /// Checked integer multiplication. Computes self * rhs, returning None if overflow occurred.
    pub fn checked_mul(self, rhs: Self) -> Option<Self> {
        self.0.checked_mul(rhs.0).map(Self)
    }

    /// Checked integer division. Computes self / rhs, returning None if rhs == 0.
    pub fn checked_div(self, rhs: Self) -> Option<Self> {
        self.0.checked_div(rhs.0).map(Self)
    }

    /// Checked integer remainder. Computes self % rhs, returning None if rhs == 0.
    pub fn checked_rem(self, rhs: Self) -> Option<Self> {
        self.0.checked_rem(rhs.0).map(Self)
    }

    /// Checked integer remainder. Computes self % rhs, returning None if rhs == 0.
    pub fn checked_shl(self, rhs: u32) -> Option<Self> {
        if rhs >= U256_NUM_BITS as u32 {
            return None;
        }
        Some(Self(self.0.shl(rhs)))
    }

    /// Checked shift right. Computes self >> rhs, returning None if rhs is larger than or equal to the number of bits in self.
    pub fn checked_shr(self, rhs: u32) -> Option<Self> {
        if rhs >= U256_NUM_BITS as u32 {
            return None;
        }
        Some(Self(self.0.shr(rhs)))
    }

    /// Downcast to a an unsigned value of type T
    /// T must be at most u128
    pub fn down_cast_lossy<T: std::convert::TryFrom<u128>>(self) -> T {
        // Size of this type
        let type_size = size_of::<T>();
        // Maximum value for this type
        let max_val: u128 = if type_size < 16 {
            (1u128 << (NUM_BITS_PER_BYTE * type_size)) - 1u128
        } else {
            u128::MAX
        };
        // This should never fail
        match T::try_from(self.0.low_u128() & max_val) {
            Ok(w) => w,
            Err(_) => panic!("Fatal! Downcast failed"),
        }
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

impl From<&U256Inner> for EthnumU256 {
    fn from(n: &U256Inner) -> EthnumU256 {
        // TODO (ade): use better solution for conversion
        // Currently using str because EthnumU256 can be little or big endian
        let num_str = format!("{:X}", n.0);
        // TODO (ade): remove expect()
        EthnumU256::from_str_radix(&num_str, 16).expect("Cannot convert to U256")
    }
}

impl TryFrom<U256Inner> for u8 {
    type Error = anyhow::Error;
    fn try_from(n: U256Inner) -> Result<Self, Self::Error> {
        let n = n.0.low_u64();
        if n > u8::MAX as u64 {
            Err(anyhow!("Cast failed. {n} too large for u8."))
        } else {
            Ok(n as u8)
        }
    }
}

impl TryFrom<U256Inner> for u16 {
    type Error = anyhow::Error;

    fn try_from(n: U256Inner) -> Result<Self, Self::Error> {
        let n = n.0.low_u64();
        if n > u16::MAX as u64 {
            Err(anyhow!("Cast failed. {n} too large for u16."))
        } else {
            Ok(n as u16)
        }
    }
}

impl TryFrom<U256Inner> for u32 {
    type Error = anyhow::Error;

    fn try_from(n: U256Inner) -> Result<Self, Self::Error> {
        let n = n.0.low_u64();
        if n > u32::MAX as u64 {
            Err(anyhow!("Cast failed. {n} too large for u32."))
        } else {
            Ok(n as u32)
        }
    }
}

impl TryFrom<U256Inner> for u64 {
    type Error = anyhow::Error;

    fn try_from(n: U256Inner) -> Result<Self, Self::Error> {
        let n = n.0.low_u128();
        if n > u64::MAX as u128 {
            Err(anyhow!("Cast failed. {n} too large for u64."))
        } else {
            Ok(n as u64)
        }
    }
}

impl TryFrom<U256Inner> for u128 {
    type Error = anyhow::Error;

    fn try_from(n: U256Inner) -> Result<Self, Self::Error> {
        if n > U256Inner::from(u128::MAX) {
            Err(anyhow!("Cast failed. {n} too large for u128."))
        } else {
            Ok(n.0.low_u128())
        }
    }
}

#[cfg(any(test, feature = "fuzzing"))]
impl proptest::prelude::Arbitrary for U256Inner {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = ();
    fn arbitrary_with(_params: Self::Parameters) -> Self::Strategy {
        // TODO (ade): improve this as is it not exhaustive
        proptest::bits::u8::masked(0xFF)
            .prop_map(|u| U256Inner(PrimitiveU256::from_little_endian(&[u; U256_NUM_BYTES])))
            .boxed()
    }
}

impl<'a> arbitrary::Arbitrary<'a> for U256Inner {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        let bytes = <[u8; U256_NUM_BYTES]>::arbitrary(u)?;
        Ok(U256Inner::from_le_bytes(&bytes))
    }
}
