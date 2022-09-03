module 0x1::Native {
    use Evm::Evm::{self, sender};

    #[callable, view]
    public fun getContractAddr(): address {
        self()
    }

    #[callable, view]
    public fun getSenderAddr(): address {
        sender()
    }
}
