module 0x42::t {
struct X has drop {}
fun f(_self: &X) {}
public fun foo (x: X) {
    x.f ();
}
}
