address 0x2 {
module M {
    use std::debug;
    use std::vector;

    struct Foo has drop {}
    struct Bar has drop { x: u128, y: Foo, z: bool }
    struct Box<T> has drop { x: T }

    public fun test()  {
        let x = 42;
        debug::print(&x);

        let v = vector::empty();
        vector::push_back(&mut v, 100);
        vector::push_back(&mut v, 200);
        vector::push_back(&mut v, 300);
        debug::print(&v);

        let foo = Foo {};
        debug::print(&foo);

        let bar = Bar { x: 404, y: Foo {}, z: true };
        debug::print(&bar);

        let box = Box { x: Foo {} };
        debug::print(&box);
    }
}
}
