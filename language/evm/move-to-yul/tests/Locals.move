#[contract]
module 0x2::M {
    #[callable]
	fun evaded(a: u64, b: u64): (u64, u64, u64, u64) {
	    let c = a;
	    let d = c + b;
	    let ar = &mut a;
	    let cr = &c;
	    *ar = *cr + 1;
	    (a, b, c, d)
	}
}
