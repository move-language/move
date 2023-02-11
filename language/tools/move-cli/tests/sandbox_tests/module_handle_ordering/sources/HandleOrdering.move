module a::A {
    public fun f(): u64 {
        42
    }
}

module b::B {
    public fun f(): u64 {
        43
    }
}

module c::C {
    public fun f(): u64 {
        // B is used before A
        b::B::f() + a::A::f()
    }
}
