// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

//! This module manages native extensions supported by the unit testing framework.
//! Such extensions are enabled by cfg features and must be compiled into the test
//! to be usable.

use move_vm_runtime::native_functions::NativeContextExtensions;
use std::fmt::Write;

#[cfg(feature = "table-extension")]
use itertools::Itertools;
#[cfg(feature = "table-extension")]
use move_table_extension::NativeTableContext;
#[cfg(feature = "table-extension")]
use move_vm_test_utils::BlankStorage;
#[cfg(feature = "table-extension")]
use once_cell::sync::Lazy;

/// Create all available native context extensions.
#[allow(unused_mut)]
pub(crate) fn new_extensions() -> NativeContextExtensions {
    let mut e = NativeContextExtensions::default();
    #[cfg(feature = "table-extension")]
    create_table_extension(&mut e);
    e
}

/// Print the change sets for available native context extensions.
#[allow(unused)]
pub(crate) fn print_change_sets<W: Write>(_w: &mut W, mut extensions: NativeContextExtensions) {
    #[cfg(feature = "table-extension")]
    print_table_extension(_w, &mut extensions);
}

// =============================================================================================
// Table Extensions

#[cfg(feature = "table-extension")]
fn create_table_extension(extensions: &mut NativeContextExtensions) {
    extensions.add(NativeTableContext::new(0, &*DUMMY_RESOLVER));
}

#[cfg(feature = "table-extension")]
fn print_table_extension<W: Write>(w: &mut W, extensions: &mut NativeContextExtensions) {
    let cs = extensions.remove::<NativeTableContext>().into_change_set();
    if let Ok(cs) = cs {
        if !cs.new_tables.is_empty() {
            writeln!(
                w,
                "new tables {}",
                cs.new_tables.iter().map(|h| h.to_string()).join(", ")
            )
            .unwrap();
        }
        if !cs.removed_tables.is_empty() {
            writeln!(
                w,
                "removed tables {}",
                cs.removed_tables.iter().map(|h| h.to_string()).join(", ")
            )
            .unwrap();
        }
        for (h, c) in cs.changes {
            writeln!(w, "for {}", h).unwrap();
            for (k, v) in c.entries {
                writeln!(w, "  {:X?} := {:X?}", k, v).unwrap();
            }
        }
    }
}

#[cfg(feature = "table-extension")]
static DUMMY_RESOLVER: Lazy<BlankStorage> = Lazy::new(|| BlankStorage);
