// Tests native functions.
// dep: ../stdlib/sources/Evm.move
// dep: ../stdlib/sources/U256.move
#[contract]
module 0x2::NativeFunctions {
    use Evm::Evm;
    use Evm::U256;
    use Std::Signer;

    #[callable]
    fun call_native_functions() {
        let _ = Evm::blockhash(U256::one());
        let _ = Evm::block_basefee();
        let _ = Evm::block_chainid();
        let _ = Evm::block_coinbase();
        let _ = Evm::block_difficulty();
        let _ = Evm::block_gaslimit();
        let _ = Evm::block_number();
        let _ = Evm::block_timestamp();
        let _ = Evm::gasleft();
        let _ = Evm::msg_data();
        let _ = Evm::msg_sender();
        let _ = Evm::msg_sig();
        let _ = Evm::msg_value();
        let _ = Evm::tx_gasprice();
        let _ = Evm::tx_origin();
    }

    #[evm_test]
    fun test_signer_address_of() {
        let s = Evm::sign(@0x42);
        assert!(Signer::address_of(&s) == @0x42, 101);
    }
}
