#[contract]
/// An implementation of the ERC-721 Non-Fungible Token Standard.
module Evm::ERC721 {
    use Evm::Evm::{sender, self, sign, emit, isContract};
    use Evm::Result;
    use Evm::IERC721Receiver;
    use Evm::IERC721;
    use Evm::IERC165;
    use Evm::Table::{Self, Table};
    use Std::Errors;

    #[event]
    struct Transfer {
        from: address,
        to: address,
        tokenId: u128,
    }

    #[event]
    struct Approval {
        owner: address,
        approved: address,
        tokenId: u128,
    }

    #[event]
    struct ApprovalForAll {
        owner: address,
        operator: address,
        approved: bool,
    }

    #[storage]
    /// Represents the state of this contract. This is located at `borrow_global<State>(self())`.
    struct State has key {
        owners: Table<u128, address>,
        balances: Table<address, u128>,
        tokenApprovals: Table<u128, address>,
        operatorApprovals: Table<address, Table<address, bool>>,
    }

    #[create]
    /// Constructor of this contract.
    public fun create() {
        // Initial state of contract
        move_to<State>(
            sign(self()),
            State {
                owners: Table::empty<u128, address>(),
                balances: Table::empty<address, u128>(),
                tokenApprovals: Table::empty<u128, address>(),
                operatorApprovals: Table::empty<address, Table<address, bool>>(),
            }
        );
    }

    #[callable]
    /// Count all NFTs assigned to an owner.
    public fun balanceOf(owner: address): u128 acquires State {
        let s = borrow_global_mut<State>(self());
        *mut_balanceOf(s, owner)
    }

    #[callable]
    /// Find the owner of an NFT.
    public fun ownerOf(tokenId: u128): address acquires State {
        let s = borrow_global_mut<State>(self());
        *mut_ownerOf(s, tokenId)
    }

    #[callable(name=safeTransferFrom)] // Overloading `safeTransferFrom`
    /// Transfers the ownership of an NFT from one address to another address.
    public fun safeTransferFrom_with_data(from: address, to: address, tokenId: u128, data: vector<u8>) acquires State {
        transferFrom(from, to, tokenId);
        doSafeTransferAcceptanceCheck(from, to, tokenId, data);

    }

    #[callable]
    /// Transfers the ownership of an NFT from one address to another address.
    public fun safeTransferFrom(from: address, to: address, tokenId: u128) acquires State {
        safeTransferFrom_with_data(from, to, tokenId, b"");
    }

    #[callable]
    /// Transfer ownership of an NFT. THE CALLER IS RESPONSIBLE
    ///  TO CONFIRM THAT `_to` IS CAPABLE OF RECEIVING NFTS OR ELSE
    ///  THEY MAY BE PERMANENTLY LOST
    public fun transferFrom(from: address, to: address, tokenId: u128) acquires State {
        assert!(isApprovedOrOwner(sender(), tokenId), Errors::invalid_argument(0));
        assert!(ownerOf(tokenId) == from, Errors::invalid_argument(0));
        assert!(to != @0x0, Errors::invalid_argument(0));

        // Clear approvals from the previous owner
        approve(@0x0, tokenId);

        let s = borrow_global_mut<State>(self());

        let mut_balance_from = mut_balanceOf(s, from);
        *mut_balance_from = *mut_balance_from - 1;

        let mut_balance_to = mut_balanceOf(s, to);
        *mut_balance_to = *mut_balance_to + 1;

        let mut_owner_token = mut_ownerOf(s, tokenId);
        *mut_owner_token = to;

        emit(Transfer{from, to, tokenId});
    }

    #[callable]
    /// Change or reaffirm the approved address for an NFT.
    public fun approve(approved: address, tokenId: u128) acquires State {
        let owner = ownerOf(tokenId);
        assert!(owner != @0x0, Errors::invalid_argument(0));
        assert!(approved != owner, Errors::invalid_argument(0));
        assert!(sender() == owner || isApprovedForAll(owner, sender()), Errors::invalid_argument(0));

        let s = borrow_global_mut<State>(self());
        let mut_tokenApproval_tokenId = mut_tokenApproval(s, tokenId);
        *mut_tokenApproval_tokenId = approved;
        emit(Approval{ owner, approved, tokenId})
    }

    #[callable]
    /// Enable or disable approval for a third party ("operator") to manage
    ///  all of the sender's assets.
    public fun setApprovalForAll(operator: address, approved: bool) acquires State {
        let s = borrow_global_mut<State>(self());
        *mut_operatorApproval(s, sender(), operator) = approved;
    }

    #[callable]
    /// Get the approved address for a single NFT.
    public fun getApproved(tokenId: u128): address acquires State {
        let s = borrow_global_mut<State>(self());
        assert!(tokenExists(s, tokenId), Errors::invalid_argument(0));
        *mut_tokenApproval(s, tokenId)
    }

    #[callable]
    /// Query if an address is an authorized operator for another address.
    public fun isApprovedForAll(owner: address, operator: address): bool acquires State {
        let s = borrow_global_mut<State>(self());
        *mut_operatorApproval(s, owner, operator)
    }

    #[callable]
    // Query if this contract implements a certain interface.
    public fun supportsInterface(interfaceId: vector<u8>): bool {
        copy interfaceId == IERC721::interfaceId() || interfaceId == IERC165::interfaceId()
    }

    /// Helper function to return true iff `spender` is the owner or an approved one for `tokenId`.
    fun isApprovedOrOwner(spender: address, tokenId: u128): bool acquires State {
        let s = borrow_global_mut<State>(self());
        assert!(tokenExists(s, tokenId), Errors::invalid_argument(0));
        let owner = ownerOf(tokenId);
        return (spender == owner || getApproved(tokenId) == spender || isApprovedForAll(owner, spender))
    }

    /// Helper function to return a mut ref to the balance of a owner.
    fun mut_balanceOf(s: &mut State, owner: address): &mut u128 {
        Table::borrow_mut_with_default(&mut s.balances, owner, 0)
    }

    /// Helper function to return a mut ref to the balance of a owner.
    fun mut_ownerOf(s: &mut State, tokenId: u128): &mut address {
        Table::borrow_mut_with_default(&mut s.owners, tokenId, @0x0)
    }

    /// Helper function to return a mut ref to the balance of a owner.
    fun mut_tokenApproval(s: &mut State, tokenId: u128): &mut address {
        Table::borrow_mut_with_default(&mut s.tokenApprovals, tokenId, @0x0)
    }

    /// Helper function to return a mut ref to the operator approval.
    fun mut_operatorApproval(s: &mut State, owner: address, operator: address): &mut bool {
        if(!Table::contains(&s.operatorApprovals, &owner)) {
            Table::insert(
                &mut s.operatorApprovals,
                owner,
                Table::empty<address, bool>()
            )
        };
        let approvals = Table::borrow_mut(&mut s.operatorApprovals, &owner);
        Table::borrow_mut_with_default(approvals, operator, false)
    }

    /// Helper function to return true iff the token exists.
    fun tokenExists(s: &mut State, tokenId: u128): bool {
        let mut_ownerOf_tokenId = mut_ownerOf(s, tokenId);
        *mut_ownerOf_tokenId != @0x0
    }

    /// Helper function for the acceptance check.
    fun doSafeTransferAcceptanceCheck(from: address, to: address, tokenId: u128, data: vector<u8>) {
        if (isContract(to)) {
            // try IERC721Receiver(to).onERC721Received(_msgSender(), from, tokenId, _data) returns (bytes4 retval) {
            let result = IERC721Receiver::call_onERC721Received(to, sender(), from, tokenId, data);
            if (Result::is_ok(&result)) {
                let retval = Result::unwrap(result);
                let expected = IERC721Receiver::selector_onERC721Received();
                assert!(retval == expected, Errors::custom(0));
            }
            else {
                let _error = Result::unwrap_err(result);
                abort(Errors::custom(1)) // TODO: abort with the `_error` value.
            }
        }
    }
}
