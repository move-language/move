// Tests of quantification over addresses.
module 0x42::AddressQuant {

    use std::signer;
    struct R has key {
        x: u64
    }

    spec module {
        pragma verify = true;
    }

    spec module {
       // helper functions
        fun atMostOne(): bool {
            forall a: address, b: address where exists<R>(a) && exists<R>(b) : a == b
        }
        fun atLeastOne(): bool {
            exists a: address : exists<R>(a)
        }
    }

    public fun initialize(sndr: &signer, special_addr: address) {
        assert!(signer::address_of(sndr) == special_addr, 0);
        move_to<R>(sndr, R{x:1});
    }
    spec initialize {
        requires forall a: address : !exists<R>(a);
        ensures forall a: address where exists<R>(a) : a == special_addr;
        ensures atMostOne();
        ensures atLeastOne();
    }

    // This is tricky. If it doesn't abort on the borrow_global,
    // or on overflow, it will successfully increment a.x
    public fun f(addr: address) acquires R {
        let x_ref = borrow_global_mut<R>(addr);
        x_ref.x = x_ref.x + 1;
    }
    spec f {
        ensures global<R>(addr).x == old(global<R>(addr).x) + 1;
    }

    // sender() might be different from special_addr,
    // so this should violate the invariant.
    public fun multiple_copy_incorrect(sndr: &signer) {
        move_to<R>(sndr, R{x:1});
    }

    // This asserts that there is at must one address with an R.
    // Literally, if addresses a and b have an R, then a and b are the same.
    spec schema ExactlyOne{
         invariant atMostOne();
         invariant atLeastOne();
    }

    spec module {
        apply ExactlyOne to * except initialize;
    }

}
