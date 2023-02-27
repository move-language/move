module 0x2::A {
    use std::bcs;

    #[test]
    public fun bcs_ops() {
        // address
        /* deactivate because of variable address size
        let addr = @0x89b9f9d1fadc027cf9532d6f99041522;
        let expected_output = x"89b9f9d1fadc027cf9532d6f99041522";
        assert!(bcs::to_bytes(&addr) == expected_output, 8001);
        */

        // bool
        let b = true;
        let expected_output = x"01";
        assert!(bcs::to_bytes(&b) == expected_output, 8002);

        // u8
        let i = 1u8;
        let expected_output = x"01";
        assert!(bcs::to_bytes(&i) == expected_output, 8003);

        // u64
        let i = 1u64;
        let expected_output = x"0100000000000000";
        assert!(bcs::to_bytes(&i) == expected_output, 8004);

        // u128
        let i = 1u128;
        let expected_output = x"01000000000000000000000000000000";
        assert!(bcs::to_bytes(&i) == expected_output, 8005);

        // vector<u8>
        let v = x"0f";
        let expected_output = x"010f";
        assert!(bcs::to_bytes(&v) == expected_output, 8006);
    }
}
