#[contract]
module Evm::ExternalCall {
    use Evm::ExternalResult::{Self, ExternalResult};
    use Evm::Evm::Unit;
    use Evm::Evm::{abort_with, isContract, require, sender};
    use Evm::U256::{U256};
    use Std::Vector;

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
        } else if (ExternalResult::is_err_data(&v)) {
            abort_with(b"error data");
        } else if (ExternalResult::is_panic(&v)) {
            abort_with(b"panic");
        } else {
            abort_with(b"other");
        }
    }

    #[callable]
    public fun test_for_move_to_yul(from: address, to: address, tokenId: U256, data: vector<u8>) {
        // TODO: Uncomment the following line to see the error. The error occurs only when this function is set to be `callable`.
        //       See https://github.com/move-language/move/issues/30 for more details.
        let _ = IERC721Receiver_try_call_onERC721Received(to, sender(), from, tokenId, data);
    }

    #[callable(sig=b"doSafeTransferAcceptanceCheck(address,address,uint256,bytes)"), pure]
    public fun doSafeTransferAcceptanceCheck(from: address, to: address, tokenId: U256, data: vector<u8>) {
        if (isContract(to)) {
            let result = IERC721Receiver_try_call_onERC721Received(to, sender(), from, tokenId, data);
            if (ExternalResult::is_ok(&result)) {
                let retval = ExternalResult::unwrap(result);
                let expected = IERC721Receiver_selector_onERC721Received();
                require(retval == expected, b"Unexpected return value");
            }
            else {
                if(ExternalResult::is_err_reason(&result)) {
                    let reason = ExternalResult::unwrap_err_reason(result);
                    if (Vector::length(&reason) == 0) {
                        abort_with(b"ERC721: transfer to non ERC721Receiver implementer");
                    }
                    else {
                        abort_with(reason);
                    }
                };
                abort_with(b"Other");
            }
        }
    }

    #[external(sig=b"onERC721Received(address,address,uint256,bytes) returns (bytes4)")]
    public native fun IERC721Receiver_try_call_onERC721Received(contract: address, operator: address, from: address, tokenId: U256, bytes: vector<u8>): ExternalResult<vector<u8>>;

    public fun IERC721Receiver_selector_onERC721Received(): vector<u8> {
        x"150b7a02"
    }
}
