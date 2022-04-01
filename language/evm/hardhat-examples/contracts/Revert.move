module 0x1::Revert {
    use Evm::U256::{u256_from_u128, U256};

    #[callable, pure]
    public fun revertIf0(x: u64) {
        if (x == 0) {
            abort(0);
        }
    }

    #[callable, pure]
    public fun revertWithMessage() {
        // TODO: The native function `Evm::abortWith` is not implemented.
        abort(0);
    }
}
