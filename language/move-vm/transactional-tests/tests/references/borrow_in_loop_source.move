//# publish
module 0x42::m {

struct S has copy, drop { f: u64 }
public fun foo() {
    let x = S { f: 0 };
    let z = 0;
    let r = &x;
    let f = &z;
    let y;
    loop {
        y = S { f: 1 };
        assert!(r.f == *f, 0);
        r = &y;
        f = &r.f;
    }
}
}

//# run 0x42::m::foo --gas-budget 100000
