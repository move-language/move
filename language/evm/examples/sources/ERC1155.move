#[contract]
/// An implementation of the ERC-1155 Multi Token Standard.
module Evm::ERC1155 {
    use Evm::Evm::{sender, self, sign, emit, isContract};
    use Evm::IERC1155Receiver;
    use Evm::IERC165;
    use Evm::IERC1155;
    use Evm::Table::{Self, Table};
    use Evm::Result;
    use Std::ASCII::{String};
    use Std::Errors;
    use Std::Vector;

    #[event]
    struct TransferSingle {
        operator: address,
        from: address,
        to: address,
        id: u128,
        value: u128,
    }

    #[event]
    struct TransferBatch {
        operator: address,
        from: address,
        to: address,
        ids: vector<u128>,
        values: vector<u128>,
    }

    #[event]
    struct ApprovalForAll {
        owner: address,
        operator: address,
        approved: bool,
    }

    #[event]
    struct URI {
        value: String,
        id: u128,
    }

    #[storage]
    /// Represents the state of this contract. This is located at `borrow_global<State>(self())`.
    struct State has key {
        balances: Table<u128, Table<address, u128>>,
        operatorApprovals: Table<address, Table<address, bool>>,
        uri: String,
    }

    #[create]
    /// Constructor of this contract.
    public fun create(uri: String) {
        // Initial state of contract
        move_to<State>(
            sign(self()),
            State {
                balances: Table::empty<u128, Table<address, u128>>(),
                operatorApprovals: Table::empty<address, Table<address, bool>>(),
                uri,
            }
        );
    }

    #[callable, view]
    /// Returns the name of the token
    public fun uri(): String acquires State {
        *&borrow_global<State>(self()).uri
    }

    #[callable, view]
    /// Get the balance of an account's token.
    public fun balanceOf(account: address, id: u128): u128 acquires State {
        let s = borrow_global_mut<State>(self());
        *mut_balanceOf(s, id, account)
    }

    #[callable, view]
    /// Get the balance of multiple account/token pairs.
    public fun balanceOfBatch(accounts: vector<address>, ids: vector<u128>): vector<u128> acquires State {
        assert!(Vector::length(&accounts) == Vector::length(&ids), Errors::invalid_argument(0));
        let len = Vector::length(&accounts);
        let i = 0;
        let balances = Vector::empty<u128>();
        while(i < len) {
            Vector::push_back(
                &mut balances,
                balanceOf(
                    *Vector::borrow(&accounts, i),
                    *Vector::borrow(&ids, i)
                )
            );
            i = i + 1;
        };
        balances
    }

    #[callable, view]
    /// Enable or disable approval for a third party ("operator") to manage all of the caller's tokens.
    public fun setApprovalForAll(operator: address, approved: bool) acquires State {
        let s = borrow_global_mut<State>(self());
        let operatorApproval = mut_operatorApprovals(s, sender(), operator);
        *operatorApproval = approved;
        emit(ApprovalForAll{owner: sender(), operator, approved});
    }

    #[callable, view]
    /// Queries the approval status of an operator for a given owner.
    public fun isApprovalForAll(account: address, operator: address): bool acquires State {
        let s = borrow_global_mut<State>(self());
        *mut_operatorApprovals(s, account, operator)
    }

    #[callable]
    /// Transfers `_value` amount of an `_id` from the `_from` address to the `_to` address specified (with safety call).
    public fun safeTransferFrom(from: address, to: address, id: u128, amount: u128, data: vector<u8>) acquires State {
        assert!(from == sender() || isApprovalForAll(from, sender()), Errors::invalid_argument(0));
        let operator = sender();
        let s = borrow_global_mut<State>(self());
        let mut_balance_from = mut_balanceOf(s, id, from);
        assert!(*mut_balance_from >= amount, Errors::invalid_argument(0));
        *mut_balance_from = *mut_balance_from - amount;
        let mut_balance_to = mut_balanceOf(s, id, to);
        *mut_balance_to = *mut_balance_to + amount;
        emit(TransferSingle{operator, from, to, id, value: amount});
        doSafeTransferAcceptanceCheck(operator, from, to, id, amount, data);
    }

    #[callable]
    /// Transfers `_value` amount of an `_id` from the `_from` address to the `_to` address specified (with safety call).
    public fun safeBatchTransferFrom(from: address, to: address, ids: vector<u128>, amounts: vector<u128>, data: vector<u8>) acquires State {
        assert!(from == sender() || isApprovalForAll(from, sender()), Errors::invalid_argument(0));
        assert!(Vector::length(&amounts) == Vector::length(&ids), Errors::invalid_argument(0));
        let len = Vector::length(&amounts);
        let i = 0;

        let operator = sender();
        let s = borrow_global_mut<State>(self());

        while(i < len) {
            let id = *Vector::borrow(&ids, i);
            let amount = *Vector::borrow(&amounts, i);

            let mut_balance_from = mut_balanceOf(s, id, from);
            assert!(*mut_balance_from >= amount, Errors::invalid_argument(0));
            *mut_balance_from = *mut_balance_from - amount;
            let mut_balance_to = mut_balanceOf(s, id, to);
            *mut_balance_to = *mut_balance_to + amount;

            i = i + 1;
        };

        emit(TransferBatch{operator, from, to, ids: copy ids, values: copy amounts});

        doSafeBatchTransferAcceptanceCheck(operator, from, to, ids, amounts, data);
    }

    #[callable]
    // Query if this contract implements a certain interface.
    public fun supportsInterface(interfaceId: vector<u8>): bool {
        copy interfaceId == IERC1155::interfaceId() || interfaceId == IERC165::interfaceId()
    }

    /// Helper function to return a mut ref to the operatorApproval
    fun mut_operatorApprovals(s: &mut State, account: address, operator: address): &mut bool {
        if(!Table::contains(&s.operatorApprovals, &account)) {
            Table::insert(
                &mut s.operatorApprovals,
                account,
                Table::empty<address, bool>()
            )
        };
        let operatorApproval_account = Table::borrow_mut(
            &mut s.operatorApprovals,
            &account
        );
        Table::borrow_mut_with_default(operatorApproval_account, operator, false)
    }

    /// Helper function to return a mut ref to the balance of a owner.
    fun mut_balanceOf(s: &mut State, id: u128, account: address): &mut u128 {
        if(!Table::contains(&s.balances, &id)) {
            Table::insert(
                &mut s.balances,
                id,
                Table::empty<address, u128>()
            )
        };
        let balances_id = Table::borrow_mut(&mut s.balances, &id);
        Table::borrow_mut_with_default(balances_id, account, 0)
    }

    /// Helper function for the safe transfer acceptance check.
    fun doSafeTransferAcceptanceCheck(operator: address, from: address, to: address, id: u128, amount: u128, data: vector<u8>) {
        if (isContract(to)) {
            let result = IERC1155Receiver::call_onERC1155Received(to, operator, from, id, amount, data);
            if (Result::is_ok(&result)) {
                let retval = Result::unwrap(result);
                let expected = IERC1155Receiver::selector_onERC1155Received();
                assert!(retval == expected, Errors::custom(0));
            }
            else {
                let _error = Result::unwrap_err(result);
                abort(Errors::custom(1)) // TODO: abort with the `_error` value.
            }
        }
    }

    /// Helper function for the safe batch transfer acceptance check.
    fun doSafeBatchTransferAcceptanceCheck(operator: address, from: address, to: address, ids: vector<u128>, amounts: vector<u128>, data: vector<u8>) {
        if (isContract(to)) {
            let result = IERC1155Receiver::call_onERC1155BatchReceived(to, operator, from, ids, amounts, data);
            if (Result::is_ok(&result)) {
                let retval = Result::unwrap(result);
                let expected = IERC1155Receiver::selector_onERC1155BatchReceived();
                assert!(retval == expected, Errors::custom(0));
            }
            else {
                let _error = Result::unwrap_err(result);
                abort(Errors::custom(1)) // TODO: abort with the `_error` value.
            }
        }
    }
}
