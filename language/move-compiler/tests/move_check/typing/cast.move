module 0x8675309::M {
    fun t0(x8: u8, x64: u64, x128: u128) {
        let _: u8 = (0 as u8);
        let _: u64 = (0 as u64);
        let _: u128 = (0 as u128);

        let _: u8 = (x8 as u8);
        let _: u64 = (x64 as u64);
        let _: u128 = (x128 as u128);

        let _: u8 = (x64 as u8);
        let _: u64 = (x128 as u64);
        let _: u128 = (x8 as u128);

        let _: u8 = (x128 as u8);
        let _: u64 = (x8 as u64);
        let _: u128 = (x64 as u128);

        let _: u8 = (340282366920938463463374607431768211455u128 as u8);
        let _: u64 = (340282366920938463463374607431768211455u128 as u64);
        let _: u128 = (340282366920938463463374607431768211455u128 as u128);

        let _: u8 = 123 as u8;
        let _: u64 = 123456 as u64;
        let _: u128 = 123456789 as u128;

        let my_num: u128 = 12345;
        let _: u64 = mul_div_u64(
            my_num as u64,
            my_num as u64,
            my_num as u64,
        );
    }

    fun mul_div_u64(a: u64, b: u64, c: u64): u64 {
        let result = (a as u128) * (b as u128) / (c as u128);
        result as u64
    }
}
