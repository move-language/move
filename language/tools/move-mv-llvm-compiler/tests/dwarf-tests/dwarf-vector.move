module 0x101::vector {

    struct Foo has copy, drop { x: u64, y: bool }
    struct Bar has copy, drop { foo: Foo }

    native public fun empty<Element>(): vector<Element>;

    native public fun borrow<Element>(v: &vector<Element>, i: u64): &Element;

    native public fun push_back<Element>(v: &mut vector<Element>, e: Element);

    public fun singleton<Element>(e: Element): vector<Element> {
        let v = empty();
        push_back(&mut v, e);
        v
    }

    public fun test_singleton_contains() {
        let foo1 = Foo { x: 0, y: false };
        assert!(*borrow(&singleton(foo1), 0) == foo1, 0);

        let bar1 = Bar { foo: foo1 };
        assert!(*borrow(&singleton(bar1), 0) == bar1, 0);
    }
}
