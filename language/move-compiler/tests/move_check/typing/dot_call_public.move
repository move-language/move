module 0x42::t {

friend 0x42::m;

struct X has drop {}

public fun pub(_: &X) {}
public(friend) fun fr(_: &X) {}

}

module 0x42::m {

use 0x42::t::X;

struct Y has drop { x: X }

public fun call(x: &X, y: Y) {
    x.pub();
    x.fr();
    y.x.pub();
    y.x.fr();
}

}
