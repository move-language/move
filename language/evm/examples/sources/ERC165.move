#[contract]
/// An implementation of the ERC-165.
module Evm::ERC165 {
    use Evm::IERC165;
    use Std::Errors;
    use Std::Vector;

    // Query if a contract implements an interface.
    // The length of `interfaceId` is required to be 4.
    public fun supportInterface(interfaceId: vector<u8>): bool {
        assert!(Vector::length(&interfaceId) == 4, Errors::invalid_argument(0));
        (interfaceId == IERC165::selector_supportInterface())
    }
}
