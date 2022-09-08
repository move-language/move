
module 0x01::M {
    spec module {
        // this work
        let a = 1;

        // this doesn't work
        let a: u32;
    }
}
