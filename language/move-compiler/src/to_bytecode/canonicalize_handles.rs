// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use std::collections::HashMap;

use move_binary_format::{
    access::{ModuleAccess, ScriptAccess},
    file_format::{
        Bytecode, CodeUnit, CompiledScript, FunctionDefinition, FunctionDefinitionIndex,
        FunctionHandleIndex, ModuleHandleIndex, Signature, SignatureToken, StructDefinition,
        StructDefinitionIndex, StructFieldInformation, StructHandleIndex, TableIndex,
    },
    internals::ModuleIndex,
    CompiledModule,
};
use move_core_types::account_address::AccountAddress;
use move_symbol_pool::Symbol;

/// Pass to order handles in compiled modules and scripts stably and canonically.  Performs the
/// following canonicalizations:
///
/// - Module Handles are sorted so the self-module comes first, followed by modules with named
///   addresses in lexical order (by address name and module name), followed by unnamed addresses in
///   their original order.
///
/// - Struct and Function Handles are sorted so that definitions in the module come first, in
///   definition order, and remaining handles follow, in lexicographical order by fully-qualified
///   name.

/// Key for ordering module handles, distinguishing the module's self handle, handles with names,
/// and handles without names.
#[derive(Eq, PartialEq, Ord, PartialOrd)]
enum ModuleKey<'a> {
    SelfModule,
    Named { address: Symbol, module: &'a str },
    Unnamed,
}

/// Key for ordering function and struct handles, distinguishing handles for definitions in
/// the module and handles for externally defined functions and structs.
#[derive(Eq, PartialEq, Ord, PartialOrd)]
enum ReferenceKey<'a> {
    Internal(TableIndex),
    External {
        module: ModuleHandleIndex,
        name: &'a str,
    },
}

/// Forward the index at `$ix`, of type `$Ix` to its new location according to the `$perm`utation
/// array.
macro_rules! permute {
    ($Ix:ty, $ix:expr, $perm:expr) => {
        $ix = <$Ix>::new($perm[$ix.into_index()])
    };
}

/// Apply canonicalization to a compiled module.
pub fn in_module(
    module: &mut CompiledModule,
    address_names: &HashMap<(AccountAddress, &str), Symbol>,
) {
    // 1 (a). Choose ordering for module handles
    let modules = permutation(&module.module_handles, |_ix, handle| {
        // Order the self module first
        if handle == module.self_handle() {
            return ModuleKey::SelfModule;
        }

        let address = module.address_identifier_at(handle.address);
        let module = module.identifier_at(handle.name).as_str();

        // Preserve order between modules without a named address, pushing them to the end of the
        // pool.
        let Some(address_name) = address_names.get(&(*address, module)) else {
            return ModuleKey::Unnamed;
        };

        // Layout remaining modules in lexicographical order of named address and module name.
        ModuleKey::Named {
            address: *address_name,
            module,
        }
    });

    // 1 (b). Update references to module handles
    permute!(ModuleHandleIndex, module.self_module_handle_idx, modules);

    for fun in &mut module.function_handles {
        permute!(ModuleHandleIndex, fun.module, modules);
    }

    for struct_ in &mut module.struct_handles {
        permute!(ModuleHandleIndex, struct_.module, modules);
    }

    // 1 (c). Update ordering for module handles -- this needs to happen before other handles are
    //        replaced so that they can continue referencing modules in their own comparators.
    apply_permutation(&mut module.module_handles, modules);

    // 2 (a). Choose ordering for struct handles
    let struct_defs = struct_definition_order(&module.struct_defs);
    let structs = permutation(&module.struct_handles, |ix, handle| {
        if handle.module == module.self_handle_idx() {
            // Order structs from this module first, and in definition order
            let Some(def_position) = struct_defs.get(&StructHandleIndex(ix)) else {
                panic!("ICE struct handle from module without definition: {handle:?}");
            };
            ReferenceKey::Internal(def_position.0)
        } else {
            // Order the remaining handles afterwards, in lexicographical order of module, then
            // struct name.
            ReferenceKey::External {
                module: handle.module,
                name: module.identifier_at(handle.name).as_str(),
            }
        }
    });

    // 2 (b). Update references to struct handles
    for def in &mut module.struct_defs {
        permute!(StructHandleIndex, def.struct_handle, structs);
        if let StructFieldInformation::Declared(fields) = &mut def.field_information {
            for field in fields {
                permute_signature_token(&mut field.signature.0, &structs);
            }
        };
    }

    for Signature(tokens) in &mut module.signatures {
        for token in tokens {
            permute_signature_token(token, &structs);
        }
    }

    // 2 (c). Update ordering for struct handles.
    apply_permutation(&mut module.struct_handles, structs);

    // 3 (a). Choose ordering for function handles
    let function_defs = function_definition_order(&module.function_defs);
    let functions = permutation(&module.function_handles, |ix, handle| {
        if handle.module == module.self_handle_idx() {
            // Order functions from this module first, and in definition order
            let Some(def_position) = function_defs.get(&FunctionHandleIndex(ix)) else {
                panic!("ICE function handle from module without definition: {handle:?}");
            };
            ReferenceKey::Internal(def_position.0)
        } else {
            // Order the remaining handles afterwards, in lexicographical order of module, then
            // struct name.
            ReferenceKey::External {
                module: handle.module,
                name: module.identifier_at(handle.name).as_str(),
            }
        }
    });

    // 3 (b). Update references to function handles
    for inst in &mut module.function_instantiations {
        permute!(FunctionHandleIndex, inst.handle, functions);
    }

    for def in &mut module.function_defs {
        permute!(FunctionHandleIndex, def.function, functions);
        if let Some(code) = &mut def.code {
            permute_code(code, &functions);
        }
    }

    // 3 (c). Update ordering for function handles.
    apply_permutation(&mut module.function_handles, functions);
}

/// Apply canonicalization to a compiled script.
pub fn in_script(
    script: &mut CompiledScript,
    address_names: &HashMap<(AccountAddress, &str), Symbol>,
) {
    // 1 (a). Choose ordering for module handles
    let modules = permutation(&script.module_handles, |_ix, handle| {
        let address = script.address_identifier_at(handle.address);
        let name = script.identifier_at(handle.name);

        // Preserve order between modules without a named address, pushing them to the end of the
        // pool.
        let Some(address_name) = address_names.get(&(*address, name.as_str())) else {
            return ModuleKey::Unnamed;
        };

        // Layout remaining modules in lexicographical order of named address and module name.
        ModuleKey::Named {
            address: *address_name,
            module: name.as_str(),
        }
    });

    // 1 (b). Update references to module handles
    for fun in &mut script.function_handles {
        permute!(ModuleHandleIndex, fun.module, modules);
    }

    for struct_ in &mut script.struct_handles {
        permute!(ModuleHandleIndex, struct_.module, modules);
    }

    // 1 (c). Update ordering for module handles -- this needs to happen before other handles are
    //        replaced so that they can continue referencing modules in their own comparators.
    apply_permutation(&mut script.module_handles, modules);

    // 2 (a). Choose ordering for struct handles
    let structs = permutation(&script.struct_handles, |_ix, handle| {
        let name = script.identifier_at(handle.name).as_str();
        ReferenceKey::External {
            module: handle.module,
            name,
        }
    });

    // 2 (b). Update references to struct handles
    for Signature(tokens) in &mut script.signatures {
        for token in tokens {
            permute_signature_token(token, &structs);
        }
    }

    // 2 (b). Update ordering for struct handles
    apply_permutation(&mut script.struct_handles, structs);

    // 3 (a). Choose ordering for function handles
    let functions = permutation(&script.function_handles, |_ix, handle| {
        // Order the remaining handles afterwards, in lexicographical order of module, then
        // struct name.
        ReferenceKey::External {
            module: handle.module,
            name: script.identifier_at(handle.name).as_str(),
        }
    });

    // 3 (b). Update references to function handles
    for inst in &mut script.function_instantiations {
        permute!(FunctionHandleIndex, inst.handle, functions);
    }

    permute_code(&mut script.code, &functions);

    // 3 (c). Update ordering for function handles.
    apply_permutation(&mut script.function_handles, functions);
}

/// Reverses mapping from `StructDefinition(Index)` to `StructHandle`, so that handles for structs
/// defined in a module can be arranged in definition order.
fn struct_definition_order(
    defs: &[StructDefinition],
) -> HashMap<StructHandleIndex, StructDefinitionIndex> {
    defs.iter()
        .enumerate()
        .map(|(ix, def)| (def.struct_handle, StructDefinitionIndex(ix as TableIndex)))
        .collect()
}

/// Reverses mapping from `FunctionDefinition(Index)` to `FunctionHandle`, so that handles for
/// structs defined in a module can be arranged in definition order.
fn function_definition_order(
    defs: &[FunctionDefinition],
) -> HashMap<FunctionHandleIndex, FunctionDefinitionIndex> {
    defs.iter()
        .enumerate()
        .map(|(ix, def)| (def.function, FunctionDefinitionIndex(ix as TableIndex)))
        .collect()
}

/// Update references to `StructHandle`s within signatures according to the permutation defined by
/// `structs`.
fn permute_signature_token(token: &mut SignatureToken, structs: &[TableIndex]) {
    use SignatureToken as T;
    match token {
        T::Bool
        | T::U8
        | T::U16
        | T::U32
        | T::U64
        | T::U128
        | T::U256
        | T::Address
        | T::Signer
        | T::TypeParameter(_) => (),

        T::Vector(token) | T::Reference(token) | T::MutableReference(token) => {
            permute_signature_token(token, structs)
        }

        T::Struct(handle) => permute!(StructHandleIndex, *handle, structs),

        T::StructInstantiation(handle, tokens) => {
            permute!(StructHandleIndex, *handle, structs);
            for token in tokens {
                permute_signature_token(token, structs)
            }
        }
    }
}

/// Update references to function handles within code according to the permutation defined by
/// `functions`.
fn permute_code(code: &mut CodeUnit, functions: &[TableIndex]) {
    for instr in &mut code.code {
        if let Bytecode::Call(function) = instr {
            permute!(FunctionHandleIndex, *function, functions);
        }
    }
}

/// Calculates the permutation of indices in `pool` that sorts it according to the key function
/// `key`:  The resulting `permutation` array is such that, new `pool'` defined by:
///
///   pool'[permutation[i]] = pool[i]
///
/// is sorted according to `key`.
fn permutation<T, K: Ord>(pool: &Vec<T>, key: impl Fn(TableIndex, &T) -> K) -> Vec<TableIndex> {
    let mut inverse: Vec<_> = (0..pool.len() as TableIndex).collect();
    inverse.sort_by_key(move |ix| key(*ix, &pool[*ix as usize]));

    let mut permutation = vec![0 as TableIndex; pool.len()];
    for (ix, jx) in inverse.into_iter().enumerate() {
        permutation[jx as usize] = ix as TableIndex;
    }

    permutation
}

/// Re-order `pool` according to the `permutation` array.  `permutation[i]` is the new location of
/// `pool[i].
fn apply_permutation<T>(pool: &mut Vec<T>, mut permutation: Vec<TableIndex>) {
    assert_eq!(pool.len(), permutation.len());

    // At every iteration we confirm that one more value is in its final position in the pool,
    // either because we discover it is already in the correct place, or we move it to its correct
    // place.
    for ix in 0..pool.len() {
        loop {
            let jx = permutation[ix] as usize;
            if ix == jx {
                break;
            }
            pool.swap(ix, jx);
            permutation.swap(ix, jx);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn permutation_reverse() {
        let mut orig = vec![0i32, 1, 2, 3];

        let perm = permutation(&orig, |_, i| -i);
        assert_eq!(perm, vec![3, 2, 1, 0]);

        apply_permutation(&mut orig, perm);
        assert_eq!(orig, vec![3, 2, 1, 0]);
    }

    #[test]
    fn permutation_stability() {
        let orig = vec![5, 3, 6, 2, 1, 4];

        // Generating the permutation
        let perm = permutation(&orig, |_, i| i % 2 == 1);
        assert_eq!(perm, vec![3, 4, 0, 1, 5, 2]);

        // Applying the permutation
        let mut sort = orig.clone();
        apply_permutation(&mut sort, perm.clone());
        assert_eq!(sort, vec![6, 2, 4, 5, 3, 1]);

        // Confirm the definition of the permutation array
        for (ix, i) in orig.iter().enumerate() {
            assert_eq!(sort[perm[ix] as usize], *i, "{ix}: {i}");
        }
    }
}
