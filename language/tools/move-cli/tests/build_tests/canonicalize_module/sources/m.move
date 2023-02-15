module foo::a {
    public fun f(): u64 {
        42
    }
}

module bar::b {
    public fun f(): u64 {
        43
    }
}

module bar::c {
    public fun f(): u64 {
        foo::a::f() +
        bar::b::f() +
        baz::d::f() +
        qux::e::f()
    }
}

module baz::d {
    public fun f(): u64 {
        45
    }
}

module qux::e {
    public fun f(): u64 {
        46
    }
}
