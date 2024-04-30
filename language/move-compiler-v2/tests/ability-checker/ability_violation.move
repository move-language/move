module 0x42::ability {
	// TODO(#11377): uncomment will result in a crash
	// fun move_from_no_key<T>(addr: address) {
	// 	move_from<T>(addr);
	// }

	struct Impotent {}

	struct S<T> has key {
		x: T
	}

	fun no_key(addr: address) {
		move_from<Impotent>(addr);
		move_from<S<Impotent>>(addr);
		borrow_global_mut<Impotent>(addr);
		borrow_global<Impotent>(addr);
		exists<Impotent>(addr);
	}

	fun invalid_move_to(signer: &signer) {
		move_to(signer, Impotent {})
	}

	fun invalid_copy() {
		let x = Impotent {};
		(x, x);
	}
}
