module objects::color_object {
    #[test_only]
    use std::signer;

    struct ColorObject has key {
        red: u8,
        green: u8,
        blue: u8,
    }

    fun new(red: u8, green: u8, blue: u8): ColorObject {
        ColorObject {
            red,
            green,
            blue,
        }
    }

    public fun get_color(self: &ColorObject): (u8, u8, u8) {
        (self.red, self.green, self.blue)
    }

    public entry fun create(sender: &signer, red: u8, green: u8, blue: u8) {
        let color_object = new(red, green, blue);
        move_to(sender, color_object)
    }

    public entry fun transfer(receiver: &signer, sender: address) acquires ColorObject {
        let co = move_from<ColorObject>(sender);
        move_to(receiver, co);
    }

    #[test(owner=@0xabcd)]
    fun test_create(owner: signer) acquires ColorObject {
        create(&owner, 255, 0, 255);
        let co = borrow_global<ColorObject>(signer::address_of(&owner));
        let (red, green, blue) = get_color(co);
        assert!(red == 255, 0);
        assert!(green == 0, 1);
        assert!(blue == 255, 2);
    }
}
