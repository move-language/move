// exclude_for: cvc5
address 0x123 {

module TestFeatures {
    use std::vector;
    use std::error;
    use std::signer;

    /// The enabled features, represented by a bitset stored on chain.
    struct Features has key {
        features: vector<u8>,
    }

    spec Features {
        pragma bv=b"0";
    }

    /// Helper to check whether a feature flag is enabled.
    fun contains(features: &vector<u8>, feature: u64): bool {
        let byte_index = feature / 8;
        let bit_mask = 1 << ((feature % 8) as u8);
        byte_index < vector::length(features) && ((*vector::borrow(features, byte_index))) & bit_mask != 0
    }

    spec contains {
        pragma bv=b"0";
        aborts_if false;
        ensures result == ((feature / 8) < len(features) && spec_contains(features, feature));
    }

    fun set(features: &mut vector<u8>, feature: u64, include: bool) {
        let byte_index = feature / 8;
        let bit_mask = 1 << ((feature % 8) as u8);
        while (vector::length(features) <= byte_index) {
            let len = vector::length(features);
            vector::insert(features, 0, len)
        };
        let entry = vector::borrow_mut(features, byte_index);
        if (include)
            *entry = *entry | bit_mask
        else
            *entry = *entry & (0xff ^ bit_mask)
    }

    spec set {
        pragma bv=b"0";
        aborts_if false;
        ensures feature / 8 < len(features);
        ensures include == spec_contains(features, feature);
    }

    const EFRAMEWORK_SIGNER_NEEDED: u64 = 1;

    /// Function to enable and disable features. Can only be called by a signer of @std.
    public fun change_feature_flags(framework: &signer, enable: vector<u64>, disable: vector<u64>)
    acquires Features {
        assert!(signer::address_of(framework) == @std, error::permission_denied(EFRAMEWORK_SIGNER_NEEDED));
        if (!exists<Features>(@std)) {
            move_to<Features>(framework, Features{features: vector[]})
        };
        let features = &mut borrow_global_mut<Features>(@std).features;
        let i = 0;
        let n = vector::length(&enable);
        while (i < n) {
            set(features, *vector::borrow(&enable, i), true);
            spec {
                assert spec_contains(features, enable[i]);
            };
            i = i + 1
        };
        let i = 0;
        let n = vector::length(&disable);
        while (i < n) {
            set(features, *vector::borrow(&disable, i), false);
            spec {
                assert spec_compute_feature_flag(features, disable[i]) == (0 as u8);
            };
            i = i + 1
        };
    }

    spec fun spec_compute_feature_flag(features: vector<u8>, feature: u64): u8 {
        ((int2bv((((1 as u8) << ((feature % (8 as u64)) as u64)) as u8)) as u8)
            & features[feature/8] as u8)
    }

    spec fun spec_contains(features: vector<u8>, feature: u64): bool {
        spec_compute_feature_flag(features, feature) > (0 as u8)
    }

    spec change_feature_flags {
        aborts_if signer::address_of(framework) != @std;
    }

    fun enable_feature_flags(feature: u64) acquires Features {
        let features = &mut borrow_global_mut<Features>(@std).features;
        set(features, feature, true);
    }

    spec enable_feature_flags {
        let post features = borrow_global<Features>(@std).features;
        ensures spec_contains(features, feature);
    }

}
}
