#[interface]
/// The interface for ERC-721.
/// This module defines the API for the interface of ERC-721 and
/// the utility functions such as selectors and `interfaceId`.
module Evm::IERC721 {
    use Evm::Evm::{Unit};
    use Evm::Result::{Result};

    #[interface]
    public native fun safeTransferFrom(contract: address, from: address, to: address, tokenId: u128): Result<Unit, vector<u8>>;

    #[interface(name=safeTransferFrom)]
    public native fun safeTransferFrom_with_data(contract: address, from: address, to: address, tokenId: u128, data: vector<u8>): Result<Unit, vector<u8>>;

    #[interface_id]
    /// Return the interface identifier.
    // TODO: complete this function.
   public native fun interfaceId(): vector<u8>;

    // TODO: complete this module.
}
