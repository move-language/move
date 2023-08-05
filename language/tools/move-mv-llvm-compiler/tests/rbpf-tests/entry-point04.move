// input entry-point04.json

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

    public entry fun bar(): u64 {
        0
    }

    public entry fun foo(): u64 {
        17
    }

    public entry fun transfer(
        sender: &signer,
        receiver: address,
        amount: u64,
    ): u64
    {
        assert!(signer::address_of(sender) == @0x1b20575e7d08472535145241bd93d13f82f50d147d40c5b6fa44ae351b0ab43b, 0xf000);
        assert!(receiver == @0xada7a39d97958b89837f716d6b67656159534f4947433d3b352f2b29251f1d17, 0xf001);
        assert!(amount == 0x13110d0b07050302, 0xf002);
        0
    }
}
