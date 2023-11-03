//! SS58 address format converter for Substrate and Move accounts.

use anyhow::{bail, Result};
use blake2::{Blake2b512, Digest};
use move_core_types::account_address::AccountAddress;

// Substrate address prefix
// Read more: https://docs.substrate.io/reference/address-formats/
const SS58_PREFIX: &[u8] = b"SS58PRE";

// Public key length in bytes
const PUB_KEY_LEN: usize = 32;

// Checksum length in bytes
const CHECKSUM_LEN: usize = 2;

// Blake2b512 hash length in bytes
const HASH_LEN: usize = 64;

// Maximum supported address type length in bytes
const ADDR_TYPE_MAX_LEN: usize = 2;

// Minimum supported address type length in bytes
const ADDR_TYPE_MIN_LEN: usize = 1;

// Maximum supported SS58 address length in bytes
const SS58_MAX_LEN: usize = ADDR_TYPE_MAX_LEN + PUB_KEY_LEN + CHECKSUM_LEN;

// Minimum supported SS58 address length in bytes
const SS58_MIN_LEN: usize = ADDR_TYPE_MIN_LEN + PUB_KEY_LEN + CHECKSUM_LEN;

// Universal address type for back conversions to SS58
const UNIVERSAL_ADDRESS_TYPE: u8 = 42;

/// Convert ss58 address string to Move address structure
/// In case if such conversion is not possible, return error.
/// ```
/// use move_vm_support::ss58_address::ss58_to_move_address;
/// let substrate_address = "gkNW9pAcCHxZrnoVkhLkEQtsLsW5NWTC75cdAdxAMs9LNYCYg";
/// let move_address = ss58_to_move_address(substrate_address).unwrap();
/// assert_eq!(
///    "0x8EAF04151687736326C9FEA17E25FC5287613693C912909CB226AA4794F26A48",
///   format!("{:#X}", move_address)
/// );
/// ```
pub fn ss58_to_move_address(ss58: &str) -> Result<AccountAddress> {
    // Decoded format: <addr_type>|<address>|<checksum>
    //  Size in bytes:   1 or 2   |   32    |    2
    let decoded_ss58 = bs58::decode(ss58).into_vec()?;

    // Check if the length is valid and figure out the address type length.
    let addr_type_len = match decoded_ss58.len() {
        SS58_MIN_LEN => ADDR_TYPE_MIN_LEN,
        SS58_MAX_LEN => ADDR_TYPE_MAX_LEN,
        len => bail!("unsupported ss58 address length: {len}"),
    };

    let (type_and_addr, checksum) = &decoded_ss58.split_at(addr_type_len + PUB_KEY_LEN);

    if *checksum != ss58_checksum(type_and_addr) {
        bail!("invalid address checksum");
    }

    let (addr_type, address) = type_and_addr.split_at(addr_type_len);

    // Sanity checks here
    match addr_type_len {
        ADDR_TYPE_MIN_LEN if (64..=127).contains(&addr_type[0]) => {
            bail!("invalid address length, address types from 64 to 127 are two bytes long");
        }
        ADDR_TYPE_MAX_LEN if (0..=63).contains(&addr_type[0]) => {
            bail!("invalid address length, address types from 0 to 63 are exactly one byte long");
        }
        _ => (),
    }

    AccountAddress::from_bytes(address).map_err(anyhow::Error::msg)
}

/// Convert SS58 address to Move address string.
pub fn ss58_to_move_address_string(ss58: &str) -> Result<String> {
    Ok(format!("{:#X}", ss58_to_move_address(ss58)?))
}

/// Convert Move address to ss58 address string
/// Read more about the address structure: https://docs.substrate.io/reference/address-formats/
///
/// For now this function is supporting only one-byte address types, with one universal address type
/// for all Move accounts and it's 42. We can't do otherwise as we've no information about the
/// network we're currently compiling for.
///
/// Move developers should be aware, that information about any fails during the compilation of
/// Move code will point to the addresses converted to the 42 address type.
/// ```
/// use move_core_types::account_address::AccountAddress;
/// use move_vm_support::ss58_address::move_address_to_ss58_string;
/// let move_address = "0xd43593c715fdd31c61141abd04a99fd6822c8558854ccde39a5684e7a56da27d";
/// let substrate_address = move_address_to_ss58_string(&AccountAddress::from_hex_literal(move_address).unwrap());
/// assert_eq!(
///   "5GrwvaEF5zXb26Fz9rcQpDWS57CtERHpNehXCPcNoHGKutQY",
///  substrate_address
/// );
/// ```
pub fn move_address_to_ss58_string(addr: &AccountAddress) -> String {
    let mut ss58_address = [0; SS58_MIN_LEN];
    ss58_address[0] = UNIVERSAL_ADDRESS_TYPE;
    ss58_address[1..SS58_MIN_LEN - CHECKSUM_LEN].copy_from_slice(&addr.to_vec());
    let checksum = ss58_checksum(&ss58_address[0..SS58_MIN_LEN - CHECKSUM_LEN]);
    ss58_address[SS58_MIN_LEN - CHECKSUM_LEN..].copy_from_slice(&checksum);
    bs58::encode(ss58_address).into_string()
}

// Helper function which calculates the BLAKE2b512 hash of the given data.
fn ss58_checksum(data: &[u8]) -> [u8; CHECKSUM_LEN] {
    let mut hasher = Blake2b512::new();
    hasher.update(SS58_PREFIX);
    hasher.update(data);
    let hash: [u8; HASH_LEN] = hasher.finalize().into();
    let checksum = &hash[..CHECKSUM_LEN];
    // Convert checksum to fixed size array (always possible as hasher always return fixed size array)
    checksum.try_into().expect("checksum length is invalid")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ss58_to_move_correct() {
        let substrate_address = "gkNW9pAcCHxZrnoVkhLkEQtsLsW5NWTC75cdAdxAMs9LNYCYg";
        let move_address = ss58_to_move_address_string(substrate_address).unwrap();

        assert_eq!(
            (move_address.len() - 2) / 2, // 2 hex chars per byte
            PUB_KEY_LEN
        );

        assert_eq!(
            "0x8EAF04151687736326C9FEA17E25FC5287613693C912909CB226AA4794F26A48",
            move_address
        );

        let substrate_address = "G7UkJAutjbQyZGRiP8z5bBSBPBJ66JbTKAkFDq3cANwENyX";
        let move_address = ss58_to_move_address_string(substrate_address).unwrap();

        assert_eq!(
            (move_address.len() - 2) / 2, // 2 hex chars per byte
            PUB_KEY_LEN
        );

        assert_eq!(
            "0x9C786090E2598AE884FF9D1F01D6A1A9BAF13A9E61F73633A8928F4D80BF7DFE",
            move_address
        );
    }

    #[test]
    fn test_ss58_to_move_fail() {
        let substrate_address = "G7UkJAutjbQyZGRiP8z5bBSBPBJ66JbTKAkFDq3c"; // too short
        assert!(ss58_to_move_address_string(substrate_address).is_err());
    }

    #[test]
    fn test_ss58checksum() {
        let msg = b"hello, world!";
        let hash = ss58_checksum(msg).to_vec();

        assert_eq!(hex_literal::hex!("656f").to_vec(), hash);
    }

    #[test]
    fn move_address_to_ss58_string_correct() {
        let move_address = "0x8EAF04151687736326C9FEA17E25FC5287613693C912909CB226AA4794F26A48";
        let substrate_address =
            move_address_to_ss58_string(&AccountAddress::from_hex_literal(move_address).unwrap());

        assert_eq!(
            "5FHneW46xGXgs5mUiveU4sbTyGBzmstUspZC92UhjJM694ty",
            substrate_address
        );

        let move_address = "0x90b5ab205c6974c9ea841be688864633dc9ca8a357843eeacf2314649965fe22";
        let substrate_address =
            move_address_to_ss58_string(&AccountAddress::from_hex_literal(move_address).unwrap());

        assert_eq!(
            "5FLSigC9HGRKVhB9FiEo4Y3koPsNmBmLJbpXg2mp1hXcS59Y",
            substrate_address
        );
    }

    #[test]
    #[should_panic]
    fn move_address_to_ss58_string_fail() {
        let move_address = "0x8EAF04151687736326C9FEA17E25FC5287613693C912909CB226AA4794F26A48AA"; // too long
        let substrate_address =
            move_address_to_ss58_string(&AccountAddress::from_hex_literal(move_address).unwrap());
    }
}
