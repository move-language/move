/// This module defines the Result type and its methods.
module Evm::Result {
    use Std::Option::{Self, Option};

    /// This struct will contain either a value of type T or an error value of type E.
    struct Result<T, E> has copy, drop, store {
        value: Option<T>,
        error: Option<E>
    }
    spec Result {
        /// `Result` cannot contain both a value and an error value.
        invariant Option::is_some(value) ==> Option::is_none(error);
        invariant Option::is_some(error) ==> Option::is_none(value);
    }

    /// Return a Result containing `value`.
    public fun ok<T, E>(value: T): Result<T, E> {
        Result<T, E>{value: Option::some(value), error: Option::none<E>()}
    }

    /// Return a Result containing 'error'.
    public fun err<T, E>(error: E): Result<T, E> {
        Result<T, E>{value: Option::none<T>(), error: Option::some(error)}
    }

    /// Return true if `result` holds a value.
    public fun is_ok<T, E>(result: &Result<T, E>): bool {
        Option::is_some(&result.value)
    }

    /// Return true if `result` holds an error value.
    public fun is_err<T, E>(result: &Result<T, E>): bool {
        Option::is_some(&result.error)
    }

    /// Destroy `result` and extract `value`.
    public fun unwrap<T, E>(result: Result<T, E>): T {
        let Result {value, error} = result;
        Option::destroy_none(error);
        Option::destroy_some(value)
    }

    /// Destroy `result` and extract `error`.
    public fun unwrap_err<T, E>(result: Result<T, E>): E {
        let Result {value, error} = result;
        Option::destroy_none(value);
        Option::destroy_some(error)
    }
}
