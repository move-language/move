module 0x42::t {

struct X has drop {}
struct Y has drop { x: X }

fun f(_: X) {}

public fun foo (x: &X, y1: Y, y2: &Y) {
    x.f();
    y1.x.f();
    y2.x.f();
}

}
