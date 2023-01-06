module 0x42::Test {

    public inline fun borrows(t: vector<u64>) {
        let _x = &t;
    }

    public fun correct_usage() {
        let v = vector[1,2,3];
        borrows(v)
    }

    public fun incorrect_usage() {
        borrows(vector[1,2,3])
    }
}
