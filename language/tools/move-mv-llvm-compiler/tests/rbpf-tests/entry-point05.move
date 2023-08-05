// input entry-point05.json

// A phony `signer` module until we build `move-stdlib`.
module 0x500::signer {
    native public fun borrow_address(acct: &signer): &address;

    // Copies the address of the signer
    public fun address_of(s: &signer): address {
        *borrow_address(s)
    }
}

module 0x10::vector {
    //#[bytecode_instruction]
    /// Acquire an immutable reference to the `i`th element of the vector `v`.
    /// Aborts if `i` is out of bounds.
    native public fun borrow<Element>(v: &vector<Element>, i: u64): &Element;
}

module 0xa000::entry_point {
    use 0x500::signer;
    use 0x10::vector;

    public entry fun broadcast(
        sender: &signer,
        receivers: vector<address>,
        amount: u64,
    ): u64
    {
        assert!(signer::address_of(sender) == @0x1b20575e7d08472535145241bd93d13f82f50d147d40c5b6fa44ae351b0ab43b, 0xf000);
        assert!(*vector::borrow(&receivers, 0) == @0xada7a39d97958b89837f716d6b67656159534f4947433d3b352f2b29251f1d17, 0xf001);
        assert!(amount == 0x13110d0b07050302, 0xf002);
        0
    }
}
