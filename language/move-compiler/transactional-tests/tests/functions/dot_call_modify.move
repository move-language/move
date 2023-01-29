
//# publish

module 0x42::t {

struct X has copy, drop { count: u64 }
struct Y has copy, drop { x: X }

public fun bump(x: &mut X) { x.count = x.count + 1 }
public fun count(x: &X): u64 { x.count }

public fun owned(x: X, y: Y) {
    assert!(x.count() == 0, 0);
    assert!(y.x.count() == 0, 0);
    x.bump();
    y.x.bump();
    assert!(x.count() == 1, 0);
    assert!(y.x.count() == 1, 0);
}

public fun mut_ref(x: &mut X, y: &mut Y) {
    assert!(x.count() == 0, 0);
    assert!(y.x.count() == 0, 0);
    x.bump();
    y.x.bump();
    assert!(x.count() == 1, 0);
    assert!(y.x.count() == 1, 0);
}

public fun test() {
    let x = X { count: 0 };
    let y = Y { x };
    owned(x, y);

    let x = X { count: 0 };
    let y = Y { x };
    mut_ref(&mut x, &mut y);
}

}

//# run 0x42::t::test
