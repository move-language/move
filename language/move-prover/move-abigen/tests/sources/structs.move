address 0x1 {
module ScriptFunInModule {
    struct Coin has drop {
        value: u64,
    }

    struct Wallet has drop {
        coin1: Self::Coin,
        coin2: Self::Coin,
    }

    struct Marker {} 
    
    #[extract_abi(type = b"0x1::ScriptFunInModule::CoinGeneric<0x1::ScriptFunInModule::Marker>")]
    struct CoinGeneric<phantom T> {
        value: u64,
    }

    #[extract_abi(type = b"0x1::ScriptFunInModule::Container<0x1::ScriptFunInModule::Coin>")]
    struct Container<T> {
        value: T,
    }
}
}