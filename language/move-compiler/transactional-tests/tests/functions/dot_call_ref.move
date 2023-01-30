
//# publish

module 0x42::t {

struct X has copy, drop {}
struct Y has copy, drop { x: X }

public fun f(_self: &X): bool { true }

public fun owned(x: X, y: Y) {
    assert!(x.f(), 0);
    assert!(y.x.f(), 0);
}

public fun ref(x: &X, y: &Y) {
    assert!(x.f(), 0);
    assert!(y.x.f(), 0);
}

public fun mut_ref(x: &mut X, y: &mut Y) {
    assert!(x.f(), 0);
    assert!(y.x.f(), 0);
}

public fun tmp(x: &X, y: &Y) {
    assert!((*x).f(), 0);
    assert!((*&y.x).f(), 0);
}

public fun test() {
    let x = X{};
    let y = Y { x };
    ref(&x, &y);
    mut_ref(&mut x, &mut y);
    owned(x, y)
}

}

//# run 0x42::t::test
