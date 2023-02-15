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

script {
    fun main() {
        assert!(
            foo::a::f() +
            bar::b::f() +
            baz::c::f() +
            qux::d::f() ==
            42 + 43 + 44 + 45,
            0,
        );
    }
}

module baz::c {
    public fun f(): u64 {
        44
    }
}

module qux::d {
    public fun f(): u64 {
        45
    }
}
