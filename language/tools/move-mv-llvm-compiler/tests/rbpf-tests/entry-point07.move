// input entry-point07.json

// A phony `signer` module until we build `move-stdlib`.
module 0x500::signer {
    native public fun borrow_address(acct: &signer): &address;

    // Copies the address of the signer
    public fun address_of(s: &signer): address {
        *borrow_address(s)
    }
}

module 0xa000::entry_point {
    use 0x500::signer;

    struct Coin has store, drop {
        value: u64,
        user: address,
    }

    public entry fun struct_arg(value: u64, coin: Coin, user: &signer): u64
    {
        assert!(coin.user == @0xada7a39d97958b89837f716d6b67656159534f4947433d3b352f2b29251f1d17, 0xf001);
        assert!(coin.value == 0x13110d0b07050302, 0xf002);
        assert!(value == 0x020305070b0d1113, 0xf003);
        assert!(signer::address_of(user) == @0x1b20575e7d08472535145241bd93d13f82f50d147d40c5b6fa44ae351b0ab43b, 0xf004);
        0
    }
}
