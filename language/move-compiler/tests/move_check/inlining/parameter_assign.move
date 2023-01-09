module 0x42::Test {

    public inline fun assign(t: vector<u64>) {
        t = vector[1,2,3];
    }

    public fun correct_usage() {
        let v = vector[];
        assign(v)
    }

    public fun incorrect_usage() {
        assign(vector[1,2,3])
    }
}
