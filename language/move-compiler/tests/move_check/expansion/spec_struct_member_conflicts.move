address 0x2 {

module X1 {
    spec module {
        struct Foo;
    }
    struct Foo {}
}

module X2 {
    spec module {
        struct Foo;
    }
    fun Foo() {}
}

module X3 {
    spec module {
        struct Foo;
    }
    spec schema Foo<T> {
        ensures true;
    }
}

module X4 {
    spec module {
        struct Foo;
    }
    spec module {
        fun Foo(): () {}
    }
}

module X5 {
    spec module {
        struct Foo;
    }
    spec module {
        struct Foo;
    }
}

module X6 {
    spec module {
        struct Foo;
    }
    spec fun Foo(): () {}
}
}
