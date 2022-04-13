#[contract]
module Evm::ExternalCall {
    use Evm::ExternalResult::{Self, ExternalResult};
    use Evm::Evm::Unit;
    use Evm::Evm::abort_with;


    #[external(sig=b"forty_two() returns (uint64)")]
    public native fun external_call_forty_two(contract: address): u64;

    #[external(sig=b"forty_two() returns (uint64)")]
    public native fun try_external_call_forty_two(contract: address): ExternalResult<u64>;

    #[external(sig=b"revertWithMessage()")]
    public native fun external_call_revertWithMessage(contract: address);

    #[external(sig=b"revertWithMessage()")]
    public native fun try_external_call_revertWithMessage(contract: address): ExternalResult<Unit>;

    #[callable(sig=b"call_forty_two(address) returns (uint64)"), view]
    public fun call_forty_two(contract: address): u64 {
        external_call_forty_two(contract)
    }

    #[callable(sig=b"call_revertWithMessage(address)"), pure]
    public fun call_revertWithMessage(contract: address) {
        external_call_revertWithMessage(contract);
    }

    #[callable(sig=b"try_call_forty_two(address) returns (uint64)"), view]
    public fun try_call_forty_two(contract: address): u64 {
        let v = try_external_call_forty_two(contract);
        if (ExternalResult::is_ok(&v)) {
            return ExternalResult::unwrap(v)
        } else if (ExternalResult::is_err_reason(&v)) {
            abort_with(ExternalResult::unwrap_err_reason(v));
            return 0
        } else {
            abort_with(b"not implemented");
            return 1
        }
    }

    #[callable(sig=b"try_call_revertWithMessage(address)"), pure]
    public fun try_call_revertWithMessage(contract: address) {
        // TODO: try-call-catch. See `ExternalCall.sol`.
        let v = try_external_call_revertWithMessage(contract);
        if (ExternalResult::is_ok(&v)) {
        } else if (ExternalResult::is_err_reason(&v)) {
            abort_with(b"error reason");
        } else if (ExternalResult::is_panic(&v)) {
            abort_with(b"panic");
        } else {
            abort_with(b"other");
        }
    }
}
