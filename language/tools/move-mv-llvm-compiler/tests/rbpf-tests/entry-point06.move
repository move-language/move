// use-stdlib
// input entry-point06.json

module 0xa000::entry_point {
    use 0x1::signer;

    public entry fun misaligned1(x: bool, y: u64, s: &signer): u64
    {
        assert!(x, 0xf000);
        assert!(signer::address_of(s) == @0xada7a39d97958b89837f716d6b67656159534f4947433d3b352f2b29251f1d17, 0xf001);
        assert!(y == 0x13110d0b07050302, 0xf002);
        0
    }
}
