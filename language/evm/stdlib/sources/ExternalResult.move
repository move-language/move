/// This module defines the ExternalResult type and its methods
/// ExternalResult is used to receive the return value from an external call (https://docs.soliditylang.org/en/v0.8.13/control-structures.html?highlight=try#try-catch)
module Evm::ExternalResult {
    use Std::Option::{Self, Option};
    use Evm::U256::U256;

    /// This struct will contain either a value of type T or an error
    /// value stores return value when the execution succeeds
    /// err_reason stores the message as vector<u8> if the error was caused by revert("reasonString") or require(false, "reasonString")
    /// panic_code stores the error code if the error was caused by a panic
    /// err_data stores the error information as dynamic byte array (vector<u8>)
    struct ExternalResult<T> has copy, drop, store {
        value: Option<T>,
        err_data: Option<vector<u8>>,
        err_reason: Option<vector<u8>>,
        panic_code: Option<U256>
    }
    spec ExternalResult {
        /// `Result` cannot contain both a value and an error value.
        invariant Option::is_some(value) ==> (Option::is_none(err_data) && Option::is_none(err_reason) && Option::is_none(panic_code));
        invariant Option::is_some(err_data) ==> (Option::is_none(value) && Option::is_none(err_reason) && Option::is_none(panic_code));
        invariant Option::is_some(err_reason) ==> (Option::is_none(value) && Option::is_none(err_data) && Option::is_none(panic_code));
        invariant Option::is_some(panic_code) ==> (Option::is_none(value) && Option::is_none(err_data) && Option::is_none(err_reason));
    }

    /// Return a Result containing `value`.
    public fun ok<T>(value: T): ExternalResult<T> {
        ExternalResult<T>{value: Option::some(value), err_data: Option::none<vector<u8>>(), err_reason: Option::none<vector<u8>>(),
        panic_code: Option::none<U256>()}
    }

    /// Return a Result containing `err_data`.
    public fun err_data<T>(error: vector<u8>): ExternalResult<T> {
        ExternalResult<T>{value: Option::none<T>(), err_data: Option::some(error), err_reason: Option::none<vector<u8>>(),
        panic_code: Option::none<U256>()}
    }

    /// Return a Result containing `err_reason`.
    public fun err_reason<T>(error: vector<u8>): ExternalResult<T> {
        ExternalResult<T>{value: Option::none<T>(), err_reason: Option::some(error), err_data: Option::none<vector<u8>>(),
        panic_code: Option::none<U256>()}
    }

    /// Return a Result containing `panic_code`.
    public fun panic<T>(panic_code: U256): ExternalResult<T> {
        ExternalResult<T>{value: Option::none<T>(), err_reason: Option::none<vector<u8>>(), err_data: Option::none<vector<u8>>(),
        panic_code: Option::some(panic_code)}
    }

    /// Return true if `result` holds a value.
    public fun is_ok<T>(result: &ExternalResult<T>): bool {
        Option::is_some(&result.value)
    }

    /// Return true if `result` holds an err_data.
    public fun is_err_data<T>(result: &ExternalResult<T>): bool {
        Option::is_some(&result.err_data)
    }

    /// Return true if `result` holds an err_reason.
    public fun is_err_reason<T>(result: &ExternalResult<T>): bool {
        Option::is_some(&result.err_reason)
    }

    /// Return true if `result` holds a panic_code.
    public fun is_panic<T>(result: &ExternalResult<T>): bool {
        Option::is_some(&result.panic_code)
    }

    /// Destroy `result` and extract `value`.
    public fun unwrap<T>(result: ExternalResult<T>): T {
        let ExternalResult {value, err_data, err_reason, panic_code} = result;
        Option::destroy_none(err_data);
        Option::destroy_none(err_reason);
        Option::destroy_none(panic_code);
        Option::destroy_some(value)
    }

    /// Destroy `result` and extract `err_data`.
    public fun unwrap_err_data<T>(result: ExternalResult<T>): vector<u8> {
        let ExternalResult {value, err_data, err_reason, panic_code} = result;
        Option::destroy_none(value);
        Option::destroy_none(err_reason);
        Option::destroy_none(panic_code);
        Option::destroy_some(err_data)
    }

    /// Destroy `result` and extract `err_reason`.
    public fun unwrap_err_reason<T>(result: ExternalResult<T>): vector<u8> {
        let ExternalResult {value, err_data, err_reason, panic_code} = result;
        Option::destroy_none(value);
        Option::destroy_none(err_data);
        Option::destroy_none(panic_code);
        Option::destroy_some(err_reason)
    }

    /// Destroy `result` and extract `panic_code`.
    public fun unwrap_panic<T>(result: ExternalResult<T>): U256 {
        let ExternalResult {value, err_data, err_reason, panic_code} = result;
        Option::destroy_none(value);
        Option::destroy_none(err_reason);
        Option::destroy_none(err_data);
        Option::destroy_some(panic_code)
    }
}
