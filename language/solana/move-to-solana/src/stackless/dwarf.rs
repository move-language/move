// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! LLVM wrappers.
//!
//! The stackless code generator accesses llvm only through this mod.
//!
//! It:
//!
//! - Runs dtors
//! - Encapsulates unsafety, though making LLVM fully memsafe is hard.
//! - Hides weirdly mutable array pointers.
//! - Provides high-level instruction builders compatible with the stackless bytecode model.

use llvm_sys::{
    core::*,
    debuginfo::{
        LLVMDIBuilderCreateCompileUnit, LLVMDIBuilderCreateModule, LLVMDIBuilderFinalize,
        LLVMDWARFEmissionKind, LLVMDWARFSourceLanguage::LLVMDWARFSourceLanguageRust,
    },
    prelude::*,
};

use std::{
    ffi::{CStr, CString},
    ptr,
};

use crate::stackless::Module;

pub use llvm_sys::{
    debuginfo::{LLVMCreateDIBuilder, LLVMDIBuilderCreateFile, LLVMDisposeDIBuilder},
    LLVMAttributeFunctionIndex, LLVMAttributeIndex, LLVMAttributeReturnIndex, LLVMIntPredicate,
    LLVMLinkage,
    LLVMLinkage::LLVMInternalLinkage,
    LLVMTypeKind::LLVMIntegerTypeKind,
};

#[derive(Clone, Debug)]
pub struct DIBuilderCore {
    module_di: LLVMModuleRef,
    builder_ref: LLVMDIBuilderRef,
    // fields below reserved for future usage
    builder_file: LLVMMetadataRef,
    compiled_unit: LLVMMetadataRef,
    compiled_module: LLVMMetadataRef,
    module_ref: LLVMModuleRef,
    module_source: String,
}

#[derive(Clone, Debug)]
pub struct DIBuilder(Option<DIBuilderCore>);

/// Convert &str to a CString
fn str_to_c_params(s: &str) -> (*const ::libc::c_char, ::libc::size_t) {
    (s.as_ptr() as *const libc::c_char, s.len())
}

/// Convert the Rust String to a CString (null-terminated C-style string)
fn string_to_c_params(s: String) -> (*const ::libc::c_char, ::libc::size_t) {
    let cstr = match CString::new(s) {
        Ok(cstr) => cstr,
        Err(_) => CString::new("").expect("Failed to create an empty CString"),
    };
    (cstr.as_ptr(), cstr.as_bytes().len())
}
fn path_to_c_params(
    file_path: &str,
) -> (
    *const ::libc::c_char,
    ::libc::size_t,
    *const ::libc::c_char,
    ::libc::size_t,
) {
    let path = std::path::Path::new(&file_path);
    let directory = path
        .parent()
        .expect("Failed to get directory")
        .to_str()
        .expect("Failed to convert to string");
    let (dir_ptr, dir_len) = str_to_c_params(directory);
    let file = path
        .file_name()
        .expect("Failed to get file name")
        .to_str()
        .expect("Failed to convert to string");
    let (filename_ptr, filename_len) = str_to_c_params(file);
    (filename_ptr, filename_len, dir_ptr, dir_len)
}

pub fn from_raw_slice_to_string(raw_ptr: *const i8, raw_len: ::libc::size_t) -> String {
    let byte_slice: &[i8] = unsafe { std::slice::from_raw_parts(raw_ptr, raw_len) };
    let byte_slice: &[u8] =
        unsafe { std::slice::from_raw_parts(byte_slice.as_ptr() as *const u8, byte_slice.len()) };
    String::from_utf8_lossy(byte_slice).to_string()
}

impl DIBuilder {
    pub fn new(module: &mut Module, source: &str, debug: bool) -> DIBuilder {
        use log::debug;
        if debug {
            let module_ref_name = module.get_module_id();
            let module_ref = module.as_mut();

            // create module
            let module_name = format!("{}.dbg_info", module_ref_name);
            let (mod_nm_ptr, _mod_nm_len) = str_to_c_params(module_name.as_str());
            let module_di = unsafe { LLVMModuleCreateWithName(mod_nm_ptr) };

            // set source to created module
            let (src_ptr, src_len) = str_to_c_params(source);
            unsafe { LLVMSetSourceFileName(module_di, src_ptr, src_len) };
            // check the name
            let mut src_len: ::libc::size_t = 0;
            let src_ptr = unsafe { LLVMGetSourceFileName(module_di, &mut src_len) };
            let src0 = from_raw_slice_to_string(src_ptr, src_len);
            debug!(target: "dwarf", "Module {:#?} has source {:#?}", module_name, src0);

            // create builder
            let builder_ref = unsafe { LLVMCreateDIBuilder(module_di) };

            // create builder file
            let (mod_nm_ptr, mod_nm_len, dir_ptr, dir_len) = path_to_c_params(source);
            let builder_file = unsafe {
                LLVMDIBuilderCreateFile(builder_ref, mod_nm_ptr, mod_nm_len, dir_ptr, dir_len)
            };

            let producer = "move-mv-llvm-compiler".to_string();
            let (producer_ptr, producer_len) = str_to_c_params(producer.as_str());
            let flags = "".to_string();
            let (flags_ptr, flags_len) = str_to_c_params(flags.as_str());
            let slash = "/".to_string();
            let (slash_ptr, slash_len) = str_to_c_params(slash.as_str());
            let none = String::new();
            let (none_ptr, none_len) = str_to_c_params(none.as_str());

            let compiled_unit = unsafe {
                LLVMDIBuilderCreateCompileUnit(
                    builder_ref,
                    LLVMDWARFSourceLanguageRust,
                    builder_file,
                    producer_ptr,
                    producer_len,
                    0, /* is_optimized */
                    flags_ptr,
                    flags_len,
                    0,                /* runtime_version */
                    std::ptr::null(), /* *const i8 */
                    0,                /* usize */
                    LLVMDWARFEmissionKind::LLVMDWARFEmissionKindFull,
                    0,         /* u32 */
                    0,         /* i32 */
                    0,         /* i32 */
                    slash_ptr, /* *const i8 */
                    slash_len, /* usize */
                    none_ptr,  /* *const i8 */
                    none_len,  /* usize */
                )
            };

            // check the name
            let mut src_len: ::libc::size_t = 0;
            let src_ptr = unsafe { LLVMGetSourceFileName(module_di, &mut src_len) };
            let src1 = from_raw_slice_to_string(src_ptr, src_len);
            debug!(target: "dwarf", "Self-check: module {:#?} has source {:#?}", module_name, src1);

            // create compiled unit
            let parent_scope = compiled_unit;
            let name = module_name;
            let (name_ptr, name_len) = str_to_c_params(name.as_str());
            let (config_macros_ptr, config_macros_len) = str_to_c_params(none.as_str());
            let (include_path_ptr, include_path_len) = str_to_c_params(none.as_str());
            let (api_notes_file_ptr, api_notes_file_len) = str_to_c_params(none.as_str());
            let compiled_module = unsafe {
                LLVMDIBuilderCreateModule(
                    builder_ref,
                    parent_scope,
                    name_ptr,
                    name_len,
                    config_macros_ptr,
                    config_macros_len,
                    include_path_ptr,
                    include_path_len,
                    api_notes_file_ptr,
                    api_notes_file_len,
                )
            };

            // store all control fields for future usage
            let builder_core = DIBuilderCore {
                module_di,
                builder_ref,
                builder_file,
                compiled_unit,
                compiled_module,
                module_ref,
                module_source: source.to_string(),
            };

            DIBuilder(Some(builder_core))
        } else {
            DIBuilder(None)
        }
    }

    pub fn module_di(&self) -> Option<LLVMModuleRef> {
        self.0.as_ref().map(|x| x.module_di)
    }

    pub fn builder_ref(&self) -> Option<LLVMDIBuilderRef> {
        self.0.as_ref().map(|x| x.builder_ref)
    }

    pub fn builder_file(&self) -> Option<LLVMMetadataRef> {
        self.0.as_ref().map(|x| x.builder_file)
    }

    pub fn compiled_unit(&self) -> Option<LLVMMetadataRef> {
        self.0.as_ref().map(|x| x.compiled_unit)
    }

    pub fn compiled_module(&self) -> Option<LLVMMetadataRef> {
        self.0.as_ref().map(|x| x.compiled_module)
    }

    pub fn module_ref(&self) -> Option<LLVMModuleRef> {
        self.0.as_ref().map(|x| x.module_ref)
    }

    pub fn module_source(&self) -> Option<String> {
        self.0.as_ref().map(|x| x.module_source.clone())
    }

    pub fn print_module_to_file(&self, file_path: String) {
        if let Some(x) = &self.0 {
            let mut err_string = ptr::null_mut();
            let (filename_ptr, _filename_ptr_len) = string_to_c_params(file_path);
            unsafe {
                let res = LLVMPrintModuleToFile(x.module_di, filename_ptr, &mut err_string);
                if res != 0 {
                    assert!(!err_string.is_null());
                    let msg = CStr::from_ptr(err_string).to_string_lossy();
                    print!("{msg}");
                    LLVMDisposeMessage(err_string);
                }
            };
        }
    }

    pub fn finalize(&self) {
        if let Some(x) = &self.0 {
            unsafe { LLVMDIBuilderFinalize(x.builder_ref) };
        }
    }
}
