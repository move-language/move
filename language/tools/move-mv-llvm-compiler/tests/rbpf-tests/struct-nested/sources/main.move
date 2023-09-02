// input ../input.json

module struct_nested::main {
    struct Box<T> has copy, drop, store { a: T }
    struct Box3<T> has copy, drop, store { b: Box<Box<T>> }
    struct Box7<T> has copy, drop, store { c: Box3<Box3<T>> }
    struct Box15<T> has copy, drop, store { d: Box7<Box7<T>> }
    struct Box31<T> has copy, drop, store { e: Box15<Box15<T>> }
    struct Box63<T> has copy, drop, store { f: Box31<Box31<T>> }
    struct Box127<T> has copy, drop, store { g: Box63<Box63<T>> }

    fun box3<T>(x: T): Box3<T> {
        Box3 { b: Box { a: Box { a: x } } }
    }
    fun box7<T>(x: T): Box7<T> {
        Box7 { c: box3(box3(x)) }
    }

    fun box15<T>(x: T): Box15<T> {
        Box15 { d: box7(box7(x)) }
    }

    fun box31<T>(x: T): Box31<T> {
        Box31 { e: box15(box15(x)) }
    }

    fun box63<T>(x: T): Box63<T> {
        Box63 { f: box31(box31(x)) }
    }

    fun box127<T>(x: T): Box127<T> {
        Box127 { g: box63(box63(x)) }
    }

    public entry fun foo() {
        let _x = box127(true);
        // compiler front-end crashes on this long sequence of struct field access
        /*
        assert!(x.g.f.e.d.c.b.a.a.b.a.a.c.b.a.a.b.a.a.d.c.b.a.a.b.a.a.c.b.a.a.b.a.a
                     .e.d.c.b.a.a.b.a.a.c.b.a.a.b.a.a.d.c.b.a.a.b.a.a.c.b.a.a.b.a.a
                   .f.e.d.c.b.a.a.b.a.a.c.b.a.a.b.a.a.d.c.b.a.a.b.a.a.c.b.a.a.b.a.a
                     .e.d.c.b.a.a.b.a.a.c.b.a.a.b.a.a.d.c.b.a.a.b.a.a.c.b.a.a.b.a.a,
                0,
        );
        */
    }

    public entry fun bar() {
        let x = box31(true);
        assert!(x.e.d.c.b.a.a.b.a.a.c.b.a.a.b.a.a.d.c.b.a.a.b.a.a.c.b.a.a.b.a.a, 0);
    }
}
