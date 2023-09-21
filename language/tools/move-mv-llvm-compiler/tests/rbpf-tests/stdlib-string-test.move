// use-stdlib

module 0x10::string_tests {
    use 0x1::string;

    public fun test_valid_utf8() {
        let sparkle_heart = vector[240, 159, 146, 150];
        let s = string::utf8(sparkle_heart);
        assert!(string::length(&s) == 4, 22);
    }

    public fun test_sub_string() {
        let s = string::utf8(b"abcd");
        let sub = string::sub_string(&s, 2, 4);
        assert!(sub == string::utf8(b"cd"), 22)
    }

    public fun test_sub_string_empty() {
        let s = string::utf8(b"abcd");
        let sub = string::sub_string(&s, 4, 4);
        assert!(string::is_empty(&sub), 22)
    }

    public fun test_index_of() {
        let s = string::utf8(b"abcd");
        let r = string::utf8(b"bc");
        let p = string::index_of(&s, &r);
        assert!(p == 1, 22)
    }

    public fun test_index_of_fail() {
        let s = string::utf8(b"abcd");
        let r = string::utf8(b"bce");
        let p = string::index_of(&s, &r);
        assert!(p == 4, 22)
    }

    public fun test_append() {
        let s = string::utf8(b"abcd");
        string::append(&mut s, string::utf8(b"ef"));
        assert!(s == string::utf8(b"abcdef"), 22)
    }

    public fun test_insert() {
        let s = string::utf8(b"abcd");
        string::insert(&mut s, 1, string::utf8(b"xy"));
        assert!(s == string::utf8(b"axybcd"), 22)
    }
}

script {
    use 0x10::string_tests as ST;

    fun main() {
        ST::test_valid_utf8();
        ST::test_sub_string();
        ST::test_sub_string_empty();
        ST::test_index_of();
        ST::test_index_of_fail();
        ST::test_append();
        ST::test_insert();
    }
}
