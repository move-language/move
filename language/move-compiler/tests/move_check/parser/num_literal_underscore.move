module 0x42::M {
    fun t() {
        // Single umderscore separations allowed
        let _ = 8_5u128;
        let _ = 8_5;
        let _: u8 = 8_5;

        // Multiple umderscore separations allowed
        let _ = 0_1_3_4u64;
        let _ = 0_1_3_4;
        let _: u64 = 0_1_3_4;

        // Single trailing allowed
        let _ = 567_u64;
        let _ = 567_;
        let _: u64 = 5_6_7;

        // Multiple trailing allowed
        let _ = 567___u64;
        let _ = 567___;
        let _: u64 = 567___;

        // Multiple underscore in tandem allowed
        let _ = 0__8u8;
        let _ = 0__8;
        let _: u8 = 0__8;
    }
}
