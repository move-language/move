module 0x8675309::A {
    use std::signer;
    struct T1 has key {v: u64}

    public fun test(account: &signer) acquires T1, T1 {
        borrow_global_mut<T1>(signer::address_of(account));
    }
}

// check: DUPLICATE_ACQUIRES_RESOURCE_ANNOTATION_ERROR
