// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use std::collections::HashMap;

use move_binary_format::{
    access::{ModuleAccess, ScriptAccess},
    file_format::{CompiledScript, ModuleHandleIndex, TableIndex},
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

/// Key for ordering module handles, distinguishing the module's self handle, handles with names,
/// and handles without names.
#[derive(Eq, PartialEq, Ord, PartialOrd)]
enum ModuleKey<'a> {
    SelfModule,
    Named { address: Symbol, module: &'a str },
    Unnamed,
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
    let modules = permutation(&module.module_handles, |handle| {
        // Order the self module first
        if handle == module.self_handle() {
            return ModuleKey::SelfModule;
        }

        let address = module.address_identifier_at(handle.address);
        let name = module.identifier_at(handle.name);

        // Preserve order between modules without a named address, pushing them to the end of the
        // pool.
        let Some(address_name) = address_names.get(&(*address, name.as_str())) else {
            return ModuleKey::Unnamed;
        };

        // Layout remaining modules in lexicographical order of named address and module name.
        ModuleKey::Named { address: *address_name, module: name.as_str() }
    });

    permute!(ModuleHandleIndex, module.self_module_handle_idx, modules);
    for fun in &mut module.function_handles {
        permute!(ModuleHandleIndex, fun.module, modules);
    }
    for struct_ in &mut module.struct_handles {
        permute!(ModuleHandleIndex, struct_.module, modules);
    }
    apply_permutation(&mut module.module_handles, modules);
}

/// Apply canonicalization to a compiled script.
pub fn in_script(
    script: &mut CompiledScript,
    address_names: &HashMap<(AccountAddress, &str), Symbol>,
) {
    let modules = permutation(&script.module_handles, |handle| {
        let address = script.address_identifier_at(handle.address);
        let name = script.identifier_at(handle.name);

        // Preserve order between modules without a named address, pushing them to the end of the
        // pool.
        let Some(address_name) = address_names.get(&(*address, name.as_str())) else {
            return ModuleKey::Unnamed;
        };

        // Layout remaining modules in lexicographical order of named address and module name.
        ModuleKey::Named { address: *address_name, module: name.as_str() }
    });

    for fun in &mut script.function_handles {
        permute!(ModuleHandleIndex, fun.module, modules);
    }
    for struct_ in &mut script.struct_handles {
        permute!(ModuleHandleIndex, struct_.module, modules);
    }
    apply_permutation(&mut script.module_handles, modules);
}

/// Calculates the permutation of indices in `pool` that sorts it according to the key function
/// `key`:  The resulting `permutation` array is such that, new `pool'` defined by:
///
///     pool'[permutation[i]] = pool[i]
///
/// is sorted according to `key`.
fn permutation<T, K: Ord>(pool: &Vec<T>, key: impl Fn(&T) -> K) -> Vec<TableIndex> {
    let mut inverse: Vec<_> = (0..pool.len() as TableIndex).collect();
    inverse.sort_by_key(move |ix| key(&pool[*ix as usize]));

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

        let perm = permutation(&orig, |i| -i);
        assert_eq!(perm, vec![3, 2, 1, 0]);

        apply_permutation(&mut orig, perm);
        assert_eq!(orig, vec![3, 2, 1, 0]);
    }

    #[test]
    fn permutation_stability() {
        let orig = vec![5, 3, 6, 2, 1, 4];

        // Generating the permutation
        let perm = permutation(&orig, |i| i % 2 == 1);
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
