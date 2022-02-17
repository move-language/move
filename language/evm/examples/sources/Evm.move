/// Module which provides access to EVM functionality, including information about the executing transaction.
////
/// This currently only represents a basic subset of what we may want to expose.
module Evm::Evm {
    use Std::Vector;
    use Std::Errors;

    /// Returns the address of the executing contract.
    public native fun self(): address;

    /// Returns the address of the transaction sender.
    public native fun sender(): address;

    /// If this is a payable transaction, returns the value (in Wei) associated with it.
    /// TODO: need u256
    public native fun value(): u128;

    /// Returns the balance, in Wei, of any account.
    public native fun balance(addr: address): u128;

    /// Transfers the given amount to the target account.
    public native fun transfer(addr: address, amount: u128);

    /// Emits an event. The type passed for `E` must be annotated with #[event].
    public native fun emit<E>(e: E);

    /// Creates a signer for the contract's address space.
    public native fun sign(addr: address): &signer;

    /// Returns the keccak256 hash for the input `data`.
    /// The length of the resulting vector should be 32.
    public native fun keccak256(data: vector<u8>): vector<u8>;

    /// Returns the first four bytes of `data`.
    /// This is the equivalent to the type casting operation `bytes4` in Solidity.
    public fun bytes4(data: vector<u8>): vector<u8> {
        assert!(Vector::length(&data) >= 4, Errors::invalid_argument(0));
        let res = Vector::empty<u8>();
        Vector::push_back(&mut res, *Vector::borrow(&data, 0));
        Vector::push_back(&mut res, *Vector::borrow(&data, 1));
        Vector::push_back(&mut res, *Vector::borrow(&data, 2));
        Vector::push_back(&mut res, *Vector::borrow(&data, 3));
        res
    }

    /// Returns the result of (`v1` xor `v2`).
    // TODO: implment this in Move.
    public native fun bytes_xor(v1: vector<u8>, v2: vector<u8>): vector<u8>;

    /// Returns true iff `addr` is a contract address.
    // This can be implemented in Solidity as follows:
    // ```
    // uint32 size;
    // assembly {
    //   size := extcodesize(_addr)
    // }
    // return (size > 0);
    // ```
    public native fun isContract(addr: address): bool;

    /// Define the unit (null or void) type.
    struct Unit {}
}
