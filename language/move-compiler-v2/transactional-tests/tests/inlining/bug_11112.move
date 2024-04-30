//#print-bytecode --input=module
module 0xcafe::vectors {
    use std::vector;

    fun test_for_each_mut() {
	let v = vector[1, 2, 3];
	let s = 2;
	vector::for_each_mut(&mut v, |e| { *e = s; s = s + 1 });
	assert!(v == vector[2, 3, 4], 0);
    }
}

//#publish --print-bytecode
module 0xcafe::vectors {
    use std::vector;

    fun test_for_each_mut() {
	let v = vector[1, 2, 3];
	let s = 2;
	vector::for_each_mut(&mut v, |e| { *e = s; s = s + 1 });
	assert!(v == vector[2, 3, 4], 0);
    }
}
