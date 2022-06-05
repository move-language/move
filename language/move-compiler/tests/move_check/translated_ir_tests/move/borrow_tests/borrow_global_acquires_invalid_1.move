module 0x8675309::A {
    use std::signer;
    struct T1 has key {v: u64}

    public fun test(account: &signer) acquires T1 {
        let x = borrow_global_mut<T1>(signer::address_of(account));
        acquires_t1(account);
        move x;
    }

    fun acquires_t1(account: &signer) acquires T1 {
        T1 { v: _ } = move_from<T1>(signer::address_of(account));
    }

}
