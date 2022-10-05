//# run
script {
    fun main() {
        assert!(15u8 == 1_5u8, 15);
        assert!(15_u8 == 1_5u8, 15);
        assert!(1__5u8 == 15___u8, 15);
        assert!(1556u64 == 1_5_56u64, 1556);
        assert!(1_556u128 == 1_5_56__u128, 1556);
        assert!(015u8 == 1_5u8, 15);
        assert!(15_0u8 == 1_50u8, 150);
        assert!(00_1__5u8 == 15___u8, 15);
    }
}
