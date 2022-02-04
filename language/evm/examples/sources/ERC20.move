#[contract]
/// Another implementation of ERC20 using Table.
module Evm::ERC20_ALT {
    use Evm::Evm::{sender, self, sign, emit};
    use Evm::Table::{Self, Table};
    use Std::ASCII::{String};
    use Std::Errors;

    #[event]
    struct Transfer {
        from: address,
        to: address,
        value: u128,
    }

    #[event]
    struct Approval {
        owner: address,
        spender: address,
        value: u128,
    }

    #[storage]
    /// Represents the state of this contract. This is located at `borrow_global<State>(self())`.
    struct State has key {
        balances: Table<address, u128>,
        allowances: Table<address, Table<address, u128>>,
        total_supply: u128,
        name: String,
        symbol: String,
    }

    #[create]
    /// Constructor of this contract.
    public fun create(name: String, symbol: String) {
        // Initial state of contract
        move_to<State>(
            sign(self()),
            State {
                total_supply: 0,
                balances: Table::empty<address, u128>(),
                allowances: Table::empty<address, Table<address, u128>>(),
                name,
                symbol,
            }
        );
    }

    #[callable, view]
    /// Returns the name of the token
    public fun name(): String acquires State {
        *&borrow_global<State>(self()).name
    }

    #[callable, view]
    /// Returns the symbol of the token, usually a shorter version of the name.
    public fun symbol(): String acquires State {
        *&borrow_global<State>(self()).symbol
    }

    #[callable, view]
    /// Returns the number of decimals used to get its user representation.
    public fun decimals(): u8 {
        18
    }

    #[callable, view]
    /// Returns the total supply of the token.
    public fun totalSupply(): u128 acquires State {
        borrow_global<State>(self()).total_supply
    }

    #[callable, view]
    /// Returns the balance of an account.
    public fun balanceOf(owner: address): u128 acquires State {
        let s = borrow_global_mut<State>(self());
        *mut_balanceOf(s, owner)
    }

    #[callable]
    /// Transfers the amount from the sending account to the given account
    public fun transfer(to: address, amount: u128): bool acquires State {
        assert!(sender() != to, Errors::invalid_argument(0));
        do_transfer(sender(), to, amount);
        true
    }

    #[callable]
    /// Transfers the amount on behalf of the `from` account to the given account.
    /// This evaluates and adjusts the allowance.
    public fun transferFrom(from: address, to: address, amount: u128): bool acquires State {
        assert!(sender() != to, Errors::invalid_argument(0));
        let s = borrow_global_mut<State>(self());
        let allowance_for_sender = mut_allowance(s, from, sender());
        assert!(*allowance_for_sender >= amount, Errors::limit_exceeded(0));
        *allowance_for_sender = *allowance_for_sender - amount;
        do_transfer(from, to, amount);
        true
    }

    #[callable]
    /// Approves that the spender can spent the given amount on behalf of the calling account.
    public fun approve(spender: address, amount: u128): bool acquires State {
        let s = borrow_global_mut<State>(self());
        if(!Table::contains(&s.allowances, &sender())) {
            Table::insert(&mut s.allowances, sender(), Table::empty<address, u128>())
        };
        let a = Table::borrow_mut(&mut s.allowances, &sender());
        Table::insert(a, spender, amount);
        emit(Approval{owner: sender(), spender, value: amount});
        true
    }

    #[callable, view]
    /// Returns the allowance an account owner has approved for the given spender.
    public fun allowance(owner: address, spender: address): u128 acquires State {
        let s = borrow_global_mut<State>(self());
        *mut_allowance(s, owner, spender)
    }

    /// Helper function to perform a transfer of funds.
    fun do_transfer(from: address, to: address, amount: u128) acquires State {
        let s = borrow_global_mut<State>(self());
        let from_bal = mut_balanceOf(s, from);
        assert!(*from_bal >= amount, Errors::limit_exceeded(0));
        *from_bal = *from_bal - amount;
        let to_bal = mut_balanceOf(s, to);
        *to_bal = *to_bal + amount;
        emit(Transfer{from, to, value: amount});
    }

    /// Helper function to return a mut ref to the allowance of a spender.
    fun mut_allowance(s: &mut State, owner: address, spender: address): &mut u128 {
        if(!Table::contains(&s.allowances, &owner)) {
            Table::insert(&mut s.allowances, owner, Table::empty<address, u128>())
        };
        let allowance_owner = Table::borrow_mut(&mut s.allowances, &owner);
        Table::borrow_mut_with_default(allowance_owner, spender, 0)
    }

    /// Helper function to return a mut ref to the balance of a owner.
    fun mut_balanceOf(s: &mut State, owner: address): &mut u128 {
        Table::borrow_mut_with_default(&mut s.balances, owner, 0)
    }
}
