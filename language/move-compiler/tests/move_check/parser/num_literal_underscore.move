module 0x42::M {
    fun t() {
        // Single umderscore separations allowed
        let _ = 8_5u128;

        // Multiple umderscore separations allowed
        let _ = 0_1_3_4u64;

        // Single trailing allowed
        let _ = 567_u64;

        // Multiple trailing allowed
        let _ = 567___u64;

        // Multiple underscore in tandem allowed
        let _ = 0__8u8;
    }
}
