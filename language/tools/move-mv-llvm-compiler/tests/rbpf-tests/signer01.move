// signers 0xcafe,0xf00d,0xc0ffee,0xb00,0xb00c,0xb00c,0x123

// A phony `signer` module until we build `move-stdlib`.
module 0x500::signer {
    native public fun borrow_address(acct: &signer): &address;

    // Copies the address of the signer
    public fun address_of(s: &signer): address {
        *borrow_address(s)
    }
}

module 0x100::M5 {
    use 0x500::signer;

    public fun signer_by_val(a: signer) {
        assert!(signer::address_of(&a) == @0xcafe, 0xf00);
        assert!(*signer::borrow_address(&a) == @0xcafe, 0xf01);
    }

    public fun signer_by_ref(a: &signer) {
        assert!(signer::address_of(a) == @0xf00d, 0xf02);
    }

    public fun signers_by_ref(a: &signer, b: &signer) {
        assert!(*signer::borrow_address(a) == @0xc0ffee, 0xf03);
        assert!(*signer::borrow_address(b) == @0xb00, 0xf04);
    }

    public fun eq_signer_val(a: signer, b: signer): bool {
        a == b
    }

    public fun ne_signer_val(a: signer, b: signer): bool {
        a != b
    }

    public fun eq_signer_ref(a: &signer, b: &signer): bool {
        a == b
    }

    public fun ne_signer_ref(a: &signer, b: &signer): bool {
        a != b
    }
}

script {
    use 0x100::M5;

    fun main(s1: signer, s2: signer, s3: signer, s4: signer, s5: signer, s6: signer, s7: signer) {
        // Test signer arguments and std::signer routines.
        M5::signer_by_val(s1);
        M5::signer_by_ref(&s2);
        M5::signers_by_ref(&s3, &s4);

        // Test eq/ne comparisons of signers.
        assert!(M5::eq_signer_val(s5, s6), 0xf10);
        assert!(M5::ne_signer_val(s4, s7), 0xf11);

        assert!(M5::ne_signer_ref(&s2, &s3), 0xf12);
        assert!(!M5::eq_signer_ref(&s2, &s3), 0xf13);
    }
}
