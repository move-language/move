#[contract]
module Evm::Greeter {
    use Evm::Evm::{self};

    #[storage]
    struct State has key {
        greeting: vector<u8>,
    }

    // TODO: move-to-yul does not support constructor.
    // #[create]
    // public fun create(greeting: vector<u8>) {
    //     move_to<State>(
    //         &sign(self()),
    //         State {
    //             greeting,
    //         }
    //     );
    // }

    #[callable(sig=b"greet() returns (string)")]
    public fun greet(): vector<u8> acquires State {
        borrow_global<State>(self()).greeting
    }

    #[callable(sig=b"setGreeting(string)")]
    public fun setGreeting(greeting: vector<u8>) acquires State {
        borrow_global_mut<State>(self()).greeting = greeting;
    }
}
