use move_bytecode_verifier::verify_module;
use move_binary_format::file_format::Bytecode;

#[cfg(kani)]
#[kani::proof]
#[kani::unwind(1)]
fn test_get_successors() {
    let opcode_num = kani::any::<u16>();
    let mut nops = vec![];
    for _ in 0..opcode_num {
        nops.push(kani::any());
    }
    Bytecode::get_successors(kani::any::<u16>(), &nops);
}

/*
#[cfg(kani)]
#[kani::proof]
fn test_bytecode_verifier() {
    let module = kani::any();
    let _ = move_bytecode_verifier::verify_module(&module);
}
 */
