// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#[macro_use]
extern crate lazy_static;

#[macro_export]
macro_rules! impl_convert_loc {
    ($struct_name : ident) => {
        impl ConvertLoc for $struct_name {
            fn convert_file_hash_filepath(&self, hash: &FileHash) -> Option<PathBuf> {
                self.hash_file
                    .as_ref()
                    .borrow()
                    .get_path(hash)
                    .map(|x| x.clone())
            }
            fn convert_loc_range(&self, loc: &Loc) -> Option<FileRange> {
                self.convert_file_hash_filepath(&loc.file_hash())
                    .map(|file| {
                        self.file_line_mapping.as_ref().borrow().translate(
                            &file,
                            loc.start(),
                            loc.end(),
                        )
                    })
                    .flatten()
            }
        }
    };
}

extern crate move_ir_types;
pub mod completion;
#[macro_use]
pub mod context;
pub mod code_lens;
pub mod document_symbol;
pub mod goto_definition;
pub mod hover;
#[cfg(test)]
mod ide_test;
pub mod item;
pub mod modules;
pub mod modules_visitor;
pub mod references;
pub mod scope;
pub mod scopes;
pub mod syntax;
pub mod types;
pub mod utils;
