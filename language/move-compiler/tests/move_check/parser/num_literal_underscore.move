module 0x42::M {
    fun t() {
        // Single umderscore separations allowed
        let _ = 8_5u128;
        let _ = 8_5;

        // Multiple umderscore separations allowed
        let _ = 0_1_3_4u64;
        let _ = 0_1_3_4;

        // Single trailing allowed
        let _ = 567_u64;
        let _ = 567_;

        // Multiple trailing allowed
        let _ = 567___u64;
        let _ = 567___;

        // Multiple underscore in tandem allowed
        let _ = 0__8u8;
        let _ = 0__8;
    }
}
