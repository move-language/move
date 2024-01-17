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

use crate::stackless::{extensions::FunctionEnvExt, llvm::Module, FunctionContext, TargetData};
use anyhow::{Context, Result};
use codespan::Location;
use itertools::enumerate;
use llvm_sys::{
    core::*,
    debuginfo::{
        LLVMCreateDIBuilder, LLVMDIBuilderCreateBasicType, LLVMDIBuilderCreateCompileUnit,
        LLVMDIBuilderCreateFile, LLVMDIBuilderCreateFunction, LLVMDIBuilderCreateLexicalBlock,
        LLVMDIBuilderCreateMemberType, LLVMDIBuilderCreateModule, LLVMDIBuilderCreateNameSpace,
        LLVMDIBuilderCreateParameterVariable, LLVMDIBuilderCreatePointerType,
        LLVMDIBuilderCreateStructType, LLVMDIBuilderCreateSubroutineType,
        LLVMDIBuilderCreateUnspecifiedType, LLVMDIBuilderCreateVectorType, LLVMDIBuilderFinalize,
        LLVMDIBuilderGetOrCreateSubrange, LLVMDIFlagObjcClassComplete, LLVMDIFlagZero, LLVMDIFlags,
        LLVMDITypeGetName, LLVMDWARFEmissionKind,
        LLVMDWARFSourceLanguage::LLVMDWARFSourceLanguageRust, LLVMDWARFTypeEncoding,
        LLVMGetMetadataKind, LLVMMetadataKind,
    },
    prelude::*,
    LLVMValue,
};

use log::{debug, error, warn};
use move_model::model::{GlobalEnv, ModuleId, StructId};
use move_stackless_bytecode::stackless_bytecode::Bytecode;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    env,
    ffi::CStr,
    fs::File,
    io::{BufRead, BufReader, Read, Seek, SeekFrom},
    ptr,
};

use super::{GlobalContext, StructType, Type};

use move_model::ty as mty;

pub struct DIContext {
    // lives in GlobalContext
    pub type_struct_db: RefCell<HashMap<StructId, LLVMMetadataRef>>,
    pub unresolved_mty: RefCell<
        HashSet<(
            mty::Type,
            String, // mty name
            String, // general msg
        )>,
    >,
}

impl DIContext {
    pub fn new() -> DIContext {
        DIContext {
            type_struct_db: RefCell::new(HashMap::new()),
            unresolved_mty: RefCell::new(HashSet::new()),
        }
    }
}
#[derive(Clone)]
pub struct DIBuilderCore<'up> {
    g_ctx: &'up GlobalContext<'up>,
    module_di: LLVMModuleRef, // ref to the new module created here for DI purpose
    builder_ref: LLVMDIBuilderRef,
    // fields below reserved for future usage
    builder_file: LLVMMetadataRef,
    compiled_unit: LLVMMetadataRef,
    compiled_module: LLVMMetadataRef,
    module_ref: LLVMModuleRef, // ref to existed "Builder" Module, which was used in 'new'
    module_source: String,
    // basic types
    pub type_unspecified: LLVMMetadataRef,
    pub type_u8: LLVMMetadataRef,
    pub type_u16: LLVMMetadataRef,
    pub type_u32: LLVMMetadataRef,
    pub type_u64: LLVMMetadataRef,
    pub type_u128: LLVMMetadataRef,
    pub type_u256: LLVMMetadataRef,
    pub type_bool: LLVMMetadataRef,
    pub type_address: LLVMMetadataRef,
}

pub enum UnresolvedPrintLogLevel {
    Debug,
    Warning,
    Error,
}

pub struct Instruction<'up> {
    bc: &'up Bytecode,
    func_ctx: &'up FunctionContext<'up, 'up>,
    loc: move_model::model::Loc,
}

impl<'up> Instruction<'up> {
    fn new(bc: &'up Bytecode, func_ctx: &'up FunctionContext<'_, '_>) -> Instruction<'up> {
        let loc = func_ctx
            .env
            .get_bytecode_loc(bc.get_attr_id().as_usize() as u16);
        Instruction { bc, func_ctx, loc }
    }
    fn debug(&self) {
        let bc = self.bc;
        let loc = &self.loc;
        let (file, _line, _column, start, end) = self.loc_display();

        let (start_line, start_column) = find_line_column(&file, start as usize).unwrap_or((-1, 0));
        let (end_line, end_column) = find_line_column(&file, end as usize).unwrap_or((-1, 0));
        debug!(target: "bytecode", "{:#?} loc {:#?}", bc, loc);
        let substring = read_substring(&file, start as usize, end as usize)
            .unwrap_or("String not found".to_string());
        debug!(target: "bytecode", "file {:#?}[{start}-{end}]:{start_line}.{start_column}-{end_line}.{end_column} {substring}", file);
    }
    fn loc_display(&self) -> (String, u32, u32, u32, u32) {
        let g_env = self.func_ctx.module_cx.env.env;
        let loc = &self.loc;
        loc_display(loc, g_env)
    }
    fn create_load_store(&self, _load: *mut LLVMValue, _store: *mut LLVMValue, mty: &mty::Type) {
        let bc = self.bc;
        let loc = &self.loc;
        debug!(target: "bytecode", "create_load_store {:#?} loc {:#?} mty {:#?}", bc, loc, mty);
    }
}

pub fn type_get_name(x: LLVMMetadataRef) -> String {
    let mut length: ::libc::size_t = 0;
    let name_c_str = unsafe { LLVMDITypeGetName(x, &mut length) };
    let name = unsafe {
        std::ffi::CStr::from_ptr(name_c_str)
            .to_string_lossy()
            .into_owned()
    };
    name
}

impl<'up> DIBuilderCore<'up> {
    pub fn add_type_struct(&self, struct_id: StructId, ty: LLVMMetadataRef) {
        let name = type_get_name(ty);
        debug!(target: "struct", "set type {} for struct {:#?}", name, struct_id);
        self.g_ctx
            .di_context
            .type_struct_db
            .borrow_mut()
            .insert(struct_id, ty);
    }

    pub fn get_type_struct(
        &self,
        _module_id: ModuleId, // reserved for future usage and debugging
        struct_id: StructId,
        struct_name: &String,
    ) -> LLVMMetadataRef {
        let binding = self.g_ctx.di_context.type_struct_db.borrow_mut();
        let val: Option<&*mut llvm_sys::LLVMOpaqueMetadata> = binding.get(&struct_id);
        let ty = match val {
            Some(res) => *res,
            None => self.type_unspecified,
        };
        let type_name = type_get_name(ty);
        debug!(target: "struct", "get type {} for struct {} {:#?}", type_name, struct_name, struct_id);
        ty
    }

    pub fn add_unresolved_mty(&self, mty: mty::Type, mty_name: String, msg: String) {
        debug!(target: "struct", "unresolved mty type {}: {}", mty_name, msg);
        self.g_ctx
            .di_context
            .unresolved_mty
            .borrow_mut()
            .insert((mty, mty_name, msg));
    }

    pub fn print_log_unresolved_types(&self, lev: UnresolvedPrintLogLevel) {
        let binding = self.g_ctx.di_context.unresolved_mty.borrow_mut();
        for el in binding.clone().into_iter() {
            let (mty, name, msg) = el;
            match lev {
                UnresolvedPrintLogLevel::Debug => {
                    debug!(target: "struct", "Unresolved type {:#?} for struct {} {:#?}", mty, name, msg)
                }
                UnresolvedPrintLogLevel::Warning => {
                    warn!(target: "struct", "Unresolved type {:#?} for struct {} {:#?}", mty, name, msg)
                }
                _ => {
                    error!(target: "struct", "Unresolved type {:#?} for struct {} {:#?}", mty, name, msg)
                }
            }
        }
    }

    pub fn has_unresolved_types(&self) -> bool {
        return self.g_ctx.di_context.unresolved_mty.borrow_mut().capacity() > 0;
    }
}

#[derive(Clone)]
pub struct DIBuilder<'up>(Option<DIBuilderCore<'up>>);

macro_rules! to_cstring {
    ($x:expr) => {{
        let cstr = match std::ffi::CString::new($x) {
            Ok(cstr) => cstr,
            Err(_) => std::ffi::CString::new("unknown").expect("Failed to create CString"),
        };
        cstr
    }};
}

pub fn from_raw_slice_to_string(raw_ptr: *const i8, raw_len: ::libc::size_t) -> String {
    let byte_slice: &[i8] = unsafe { std::slice::from_raw_parts(raw_ptr, raw_len) };
    let byte_slice: &[u8] =
        unsafe { std::slice::from_raw_parts(byte_slice.as_ptr() as *const u8, byte_slice.len()) };
    String::from_utf8_lossy(byte_slice).to_string()
}

fn relative_to_absolute(relative_path: &str) -> std::io::Result<String> {
    let current_dir = env::current_dir()?;
    let absolute_path = current_dir
        .join(relative_path)
        .canonicalize()
        .expect("Cannot canonicanize path");

    Ok(absolute_path.to_string_lossy().to_string())
}

impl<'up> DIBuilder<'up> {
    pub fn new(
        g_ctx: &'up GlobalContext,
        module: &mut Module,
        source: &str,
        debug: bool,
    ) -> DIBuilder<'up> {
        if debug {
            let module_ref_name = module.get_module_id();
            let module_ref = module.as_mut();

            // create new module
            let module_name = module_ref_name + ".dbg_info";
            let cstr = to_cstring!(module_name.as_str());
            let (mut mod_nm_ptr, mut mod_nm_len) = (cstr.as_ptr(), cstr.as_bytes().len());
            let module_di =
                unsafe { LLVMModuleCreateWithName(mod_nm_ptr as *const ::libc::c_char) };

            // check dbg module name
            mod_nm_ptr = unsafe { LLVMGetModuleIdentifier(module_di, &mut mod_nm_len) };
            let module_di_name = &from_raw_slice_to_string(mod_nm_ptr, mod_nm_len);
            debug!(target: "dwarf", "Created dbg module {:#?}", module_di_name);

            let source = relative_to_absolute(source).expect("Must be the legal path");
            let cstr = to_cstring!(source.as_str());
            unsafe { LLVMSetSourceFileName(module_di, cstr.as_ptr(), cstr.as_bytes().len()) };

            // check the source name
            let mut src_len: ::libc::size_t = 0;
            let src_ptr = unsafe { LLVMGetSourceFileName(module_di, &mut src_len) };
            let module_src = &from_raw_slice_to_string(src_ptr, src_len);
            debug!(target: "dwarf", "Module {:#?} has source {:#?}", module_name, module_src);

            // create builder
            let builder_ref = unsafe { LLVMCreateDIBuilder(module_di) };

            // create file
            let path = std::path::Path::new(&source);
            let directory = path
                .parent()
                .expect("Failed to get directory")
                .to_str()
                .expect("Failed to convert to string");
            let cstr = to_cstring!(directory);
            let (dir_ptr, dir_len) = (cstr.as_ptr(), cstr.as_bytes().len());

            let file = path
                .file_name()
                .expect("Failed to get file name")
                .to_str()
                .expect("Failed to convert to string");
            let cstr = to_cstring!(file);
            let (filename_ptr, filename_len) = (cstr.as_ptr(), cstr.as_bytes().len());
            let (mod_nm_ptr, mod_nm_len, dir_ptr, dir_len) =
                (filename_ptr, filename_len, dir_ptr, dir_len);

            let builder_file = unsafe {
                LLVMDIBuilderCreateFile(builder_ref, mod_nm_ptr, mod_nm_len, dir_ptr, dir_len)
            };

            // create compile unit
            let producer = "move-mv-llvm-compiler".to_string();
            let cstr = to_cstring!(producer);
            let (producer_ptr, producer_len) = (cstr.as_ptr(), cstr.as_bytes().len());

            let flags = "".to_string();
            let cstr = to_cstring!(flags);
            let (flags_ptr, flags_len) = (cstr.as_ptr(), cstr.as_bytes().len());

            let slash = "/".to_string();
            let cstr = to_cstring!(slash);
            let (slash_ptr, slash_len) = (cstr.as_ptr(), cstr.as_bytes().len());

            let none = String::new();
            let cstr = to_cstring!(none);
            let (none_ptr, none_len) = (cstr.as_ptr(), cstr.as_bytes().len());

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

            // create di module
            let parent_scope = compiled_unit;
            let name = module_name;
            let cstr = to_cstring!(name);
            let (name_ptr, name_len) = (cstr.as_ptr(), cstr.as_bytes().len());

            let (config_macros_ptr, config_macros_len) = (none_ptr, none_len);
            let (include_path_ptr, include_path_len) = (none_ptr, none_len);
            let (api_notes_file_ptr, api_notes_file_len) = (none_ptr, none_len);
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

            fn create_type(
                builder_ref: LLVMDIBuilderRef,
                name: &str,
                size_in_bits: u64,
                encoding: LLVMDWARFTypeEncoding,
                flags: LLVMDIFlags,
            ) -> LLVMMetadataRef {
                let name_cstr = to_cstring!(name);
                let (name_ptr, name_len) = (name_cstr.as_ptr(), name_cstr.as_bytes().len());
                unsafe {
                    LLVMDIBuilderCreateBasicType(
                        builder_ref,
                        name_ptr,
                        name_len,
                        size_in_bits,
                        encoding,
                        flags,
                    )
                }
            }

            fn create_unspecified_type(builder_ref: LLVMDIBuilderRef) -> LLVMMetadataRef {
                let name_cstr = to_cstring!("unspecified type");
                let (name_ptr, name_len) = (name_cstr.as_ptr(), name_cstr.as_bytes().len());
                unsafe { LLVMDIBuilderCreateUnspecifiedType(builder_ref, name_ptr, name_len) }
            }

            // store all control fields for future usage
            let builder_core = DIBuilderCore {
                g_ctx,
                module_di,
                builder_ref,
                builder_file,
                compiled_unit,
                compiled_module,
                module_ref,
                module_source: source.to_string(),
                type_unspecified: create_unspecified_type(builder_ref),
                type_u8: create_type(builder_ref, "u8", 8, 0, LLVMDIFlagZero),
                type_u16: create_type(builder_ref, "u16", 16, 0, LLVMDIFlagZero),
                type_u32: create_type(builder_ref, "u32", 32, 0, LLVMDIFlagZero),
                type_u64: create_type(builder_ref, "u64", 64, 0, LLVMDIFlagZero),
                type_u128: create_type(builder_ref, "u128", 128, 0, LLVMDIFlagZero),
                type_u256: create_type(builder_ref, "u256", 256, 0, LLVMDIFlagZero),
                type_bool: create_type(builder_ref, "bool", 8, 0, LLVMDIFlagZero),
                type_address: create_type(builder_ref, "address", 256, 0, LLVMDIFlagZero),
            };

            DIBuilder(Some(builder_core))
        } else {
            DIBuilder(None)
        }
    }

    pub fn global_ctx(&self) -> Option<&GlobalContext> {
        self.0.as_ref().map(|x| x.g_ctx)
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

    pub fn core(&self) -> &DIBuilderCore {
        self.0.as_ref().unwrap()
    }

    // Get DI type for given mty. 'name' is used for debugging only.
    pub fn get_type(&self, mty: move_model::ty::Type, name: &String) -> LLVMMetadataRef {
        let core = self.core();
        match mty {
            mty::Type::Primitive(mty::PrimitiveType::Bool) => core.type_bool,
            mty::Type::Primitive(mty::PrimitiveType::U8) => core.type_u8,
            mty::Type::Primitive(mty::PrimitiveType::U16) => core.type_u16,
            mty::Type::Primitive(mty::PrimitiveType::U32) => core.type_u32,
            mty::Type::Primitive(mty::PrimitiveType::U64) => core.type_u64,
            mty::Type::Primitive(mty::PrimitiveType::U128) => core.type_u128,
            mty::Type::Primitive(mty::PrimitiveType::U256) => core.type_u256,
            mty::Type::Primitive(mty::PrimitiveType::Address) => core.type_address,
            mty::Type::Struct(mod_id, struct_id, _v) => {
                self.core().get_type_struct(mod_id, struct_id, name)
            }
            _ => core.type_unspecified,
        }
    }

    pub fn print_module_to_file(&self, file_path: String) {
        if let Some(x) = &self.0 {
            let mut err_string = ptr::null_mut();
            let cstr = to_cstring!(file_path);
            let (filename_ptr, _filename_ptr_len) = (cstr.as_ptr(), cstr.as_bytes().len());
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

    fn is_named_type(metadata: LLVMMetadataRef) -> bool {
        let kind = unsafe { LLVMGetMetadataKind(metadata) };
        match kind {
            LLVMMetadataKind::LLVMLocalAsMetadataMetadataKind => {
                // LLVMMetadataKind::LLVMGenericDINodeMetadataKind => {
                // It's a token node, which may include DIType
                let mut length: ::libc::size_t = 0;
                let name_c_str = unsafe { LLVMDITypeGetName(metadata, &mut length) };
                if !name_c_str.is_null() {
                    let name = unsafe {
                        std::ffi::CStr::from_ptr(name_c_str)
                            .to_string_lossy()
                            .into_owned()
                    };
                    println!("Name of type: {}", name);
                    true
                } else {
                    println!("Type does not have a name.");
                    false
                }
            }
            LLVMMetadataKind::LLVMDILocationMetadataKind => {
                // It's a location metadata
                // Process location metadata...
                false
            }
            _ => {
                // Handle other cases if needed
                println!("Unexpected metadata kind.");
                false
            }
        }
    }

    pub fn get_value_name_with_debug(&self, value: *mut LLVMValue) -> String {
        if self.0.is_some() {
            // Assuming that metadata is associated with the LLVMValue during debug info generation
            let metadata = unsafe { LLVMValueAsMetadata(value) };
            if Self::is_named_type(metadata) {
                type_get_name(metadata)
            } else {
                "unknown name".to_string()
            }
        } else {
            "unknown name".to_string()
        }
    }

    pub fn get_name(&self, value: *mut LLVMValue) -> String {
        if self.0.is_some() {
            // Assuming that metadata is associated with the LLVMValue during debug info generation
            let mut length: ::libc::size_t = 0;
            let name_ptr = unsafe { LLVMGetValueName2(value, &mut length) };
            let name_cstr = unsafe { std::ffi::CStr::from_ptr(name_ptr) };
            name_cstr.to_string_lossy().into_owned()
        } else {
            "unknown name".to_string()
        }
    }

    pub fn set_name(&self, value: LLVMValueRef, name: &str) {
        if self.0.is_some() {
            let cstr = std::ffi::CString::new(name).expect("Failed to create CString");
            unsafe {
                LLVMSetValueName2(value, cstr.as_ptr(), cstr.as_bytes().len());
            }
        }
    }

    pub fn print_log_unresoled_types(&self, lev: UnresolvedPrintLogLevel) {
        if let Some(core) = &self.0 {
            core.print_log_unresolved_types(lev);
        }
    }

    pub fn struct_fields_info(s: &StructType, data_layout: TargetData, msg: &str) {
        debug!(target: "struct", "{msg}: info {}", s.as_any_type().print_to_str());
        for idx in 0..s.count_struct_element_types() {
            let field_ty = s.struct_get_type_at_index(idx);
            let field_info = field_ty.print_to_str();
            let offset_of_element = s.offset_of_element(data_layout, idx);
            let sz = field_ty.get_int_type_width();
            let property = field_ty.dump_properties_to_str(data_layout);
            debug!(target: "struct", "{idx} field from struct_type: {:#?} {}, \nsz {sz}, offset_of_element {offset_of_element}, \n{}", field_ty, field_info, property);
        }
    }

    pub fn create_vector(
        &self,
        mvec: &[mty::Type],
        llvec: &Type,
        llmod: &Module,
        _parent: Option<LLVMMetadataRef>,
    ) {
        if mvec.is_empty() {
            return;
        };

        if let Some(_di_builder_core) = &self.0 {
            let di_builder = self.builder_ref().unwrap();

            let mty = mvec.get(0).unwrap(); // exists since !mvec.is_empty()
            let vec_di_type = self.get_type(mty.clone(), &"unnamed-vector".to_string());
            if vec_di_type == self.core().type_unspecified {
                return; // proceed only when type is known
            }

            let llvec_info = llvec.print_to_str();
            debug!(target: "vector", "create_vector {llvec_info}");

            let mut vec_name_length: ::libc::size_t = 0;
            let vec_name_c_str = unsafe { LLVMDITypeGetName(vec_di_type, &mut vec_name_length) };
            let vec_name = unsafe {
                std::ffi::CStr::from_ptr(vec_name_c_str)
                    .to_string_lossy()
                    .into_owned()
            };
            let dl = llmod.get_module_data_layout();
            let n_elements = [llvec].len();
            unsafe {
                let size_subrange =
                    LLVMDIBuilderGetOrCreateSubrange(di_builder, 0, n_elements as i64);
                let size_in_bits = llvec.size_of_type_in_bits(dl);
                let align_in_bits = llvec.abi_alignment_of_type(dl);
                let vector_di_type = LLVMDIBuilderCreateVectorType(
                    di_builder,
                    size_in_bits,
                    align_in_bits,
                    vec_di_type,
                    vec![size_subrange].as_mut_ptr(),
                    n_elements as u32,
                );

                let module_di = &self.module_di().unwrap();
                let module_ctx = LLVMGetModuleContext(*module_di);
                let meta_as_value = LLVMMetadataAsValue(module_ctx, vector_di_type);
                LLVMAddNamedMetadataOperand(*module_di, vec_name_c_str, meta_as_value);

                let out = LLVMPrintModuleToString(*module_di);
                let c_string: *mut i8 = out;
                let c_str = CStr::from_ptr(c_string)
                    .to_str()
                    .expect("Cannot convert to &str");
                debug!(target: "vectors", "vector {vec_name}: DI content: starting at next line and until line starting with !!!\n{}\n!!!\n", c_str);
            };
        }
    }

    pub fn create_function(
        &self,
        func_ctx: &FunctionContext<'_, '_>,
        _parent: Option<LLVMMetadataRef>,
    ) {
        if let Some(_di_builder_core) = &self.0 {
            let di_builder = self.builder_ref().unwrap();
            let di_builder_file = self.builder_file().unwrap();

            let fn_env = &func_ctx.env;
            let loc = &fn_env.get_loc();
            let (file, location) = fn_env
                .module_env
                .env
                .get_file_and_location(loc)
                .unwrap_or(("unknown".to_string(), Location::new(0, 0)));
            let lineno = location.line.0;

            let mod_cx = &func_ctx.module_cx;
            let module = mod_cx.llvm_module;
            let data_layout = module.get_module_data_layout();

            let param_count = func_ctx.env.get_parameter_count();

            let ll_fn = func_ctx
                .module_cx
                .lookup_move_fn_decl(fn_env.get_qualified_inst_id(func_ctx.type_params.to_vec()));
            let fn_name = fn_env.get_name_str();
            debug!(target: "functions", "Create DI for function {fn_name} {file}:{lineno} with {param_count} parameters");

            let ll_params = (0..param_count).map(|i| ll_fn.get_param(i));
            let parameters: Vec<_> = ll_params.clone().zip(func_ctx.locals.iter()).collect();
            // Only for debugging
            for (idx, (ll_param, local)) in parameters.iter().enumerate() {
                let mty = &local.mty();
                let mty_info = mty.display(&fn_env.get_type_display_ctx()).to_string();
                let llty = local.llty();
                let llty1 = local.llval().llvm_type();
                let properties = llty.dump_properties_to_str(data_layout);
                let properties1 = llty1.dump_properties_to_str(data_layout);
                let llval = ll_param.0;
                let llval1 = local.llval().get0();
                let param_name = func_ctx.module_cx.llvm_di_builder.get_name(llval);
                let param_name1 = func_ctx.module_cx.llvm_di_builder.get_name(llval1);
                debug!(target: "functions", "param {idx}: {param_name} {mty_info} {properties}");
                // use upper, not lower
                debug!(target: "functions", "param {idx}: {param_name1} {mty_info} {properties1}");
            }

            let name_cstr = to_cstring!(fn_name.clone());
            let (fn_nm_ptr, fn_nm_len) = (name_cstr.as_ptr(), name_cstr.as_bytes().len());

            let name_space = unsafe {
                LLVMDIBuilderCreateNameSpace(di_builder, di_builder_file, fn_nm_ptr, fn_nm_len, 0)
            };

            let mut fn_params: Vec<LLVMMetadataRef> = enumerate(parameters)
                .scan(0, |_state, (idx, (ll_param, local))| {
                    let llval = ll_param.0;
                    let param_name = func_ctx.module_cx.llvm_di_builder.get_name(llval);
                    let name_cstr = to_cstring!(param_name.clone());
                    let (param_nm_ptr, param_nm_len) =
                        (name_cstr.as_ptr(), name_cstr.as_bytes().len());
                    let mty = local.mty();
                    let param_type = self.get_type(mty.clone(), &param_name);

                    let fn_param = unsafe {
                        LLVMDIBuilderCreateParameterVariable(
                            di_builder,
                            name_space,
                            param_nm_ptr,
                            param_nm_len,
                            idx as u32,
                            di_builder_file,
                            idx as u32,
                            param_type,
                            0,
                            0,
                        )
                    };
                    Some(fn_param)
                })
                .collect();
            let fn_params_mut: *mut LLVMMetadataRef = fn_params.as_mut_ptr();

            let subroutine_ty = unsafe {
                LLVMDIBuilderCreateSubroutineType(
                    di_builder,
                    di_builder_file,
                    fn_params_mut,
                    fn_params.len() as u32,
                    0,
                )
            };

            let function = unsafe {
                LLVMDIBuilderCreateFunction(
                    di_builder,
                    name_space,
                    fn_nm_ptr,
                    fn_nm_len,
                    fn_nm_ptr,
                    fn_nm_len,
                    di_builder_file,
                    lineno,
                    subroutine_ty,
                    1, // IsLocalToUnit: TODO: may need change
                    1,
                    0, // ScopeLine: TODO: unclear param
                    0, // Flags: TODO: may need change
                    0, // IsOptimized: TODO: may need change
                )
            };

            // reserved for future usage
            let _lexical_block = unsafe {
                LLVMDIBuilderCreateLexicalBlock(
                    di_builder,
                    function,
                    di_builder_file,
                    fn_nm_len as u32,
                    0,
                )
            };

            let llvm_builder = &mod_cx.llvm_builder;

            let entry_bb = llvm_builder.get_entry_basic_block(ll_fn);
            dbg!(entry_bb);

            let module_di = &self.module_di().unwrap();
            let module_ctx = unsafe { LLVMGetModuleContext(*module_di) };
            let meta_as_value = unsafe { LLVMMetadataAsValue(module_ctx, function) };
            unsafe { LLVMAddNamedMetadataOperand(*module_di, fn_nm_ptr, meta_as_value) };

            let out = unsafe { LLVMPrintModuleToString(*module_di) };
            let c_string: *mut i8 = out;
            let c_str = unsafe {
                CStr::from_ptr(c_string)
                    .to_str()
                    .expect("Cannot convert to &str")
            };
            debug!(target: "functions", "function {fn_name}: DI content: starting at next line and until line starting with !!!\n{}\n!!!\n", c_str);
        }
    }

    pub fn create_type_subroutine(
        &self,
        name: &str,
        param_types: *mut LLVMMetadataRef,
        params_num: ::libc::c_uint,
        flags: LLVMDIFlags,
    ) {
        if let Some(_di_builder_core) = &self.0 {
            let di_builder = self.builder_ref().unwrap();
            let di_builder_file = self.builder_file().unwrap();
            let name_cstr = to_cstring!(name);
            let (_name_ptr, _name_len) = (name_cstr.as_ptr(), name_cstr.as_bytes().len());
            unsafe {
                LLVMDIBuilderCreateSubroutineType(
                    di_builder,
                    di_builder_file,
                    param_types,
                    params_num,
                    flags,
                );
            }
        }
    }

    pub fn create_struct(
        &self,
        func_ctx: &FunctionContext<'_, '_>,
        mod_id: &ModuleId, // reserved for future usage and debugging
        struct_id: &StructId,
        struct_llvm_name: &str,
        parent: Option<LLVMMetadataRef>,
    ) {
        if let Some(_di_builder_core) = &self.0 {
            let di_builder = self.builder_ref().unwrap();
            let di_builder_file = self.builder_file().unwrap();
            let mod_cx = &func_ctx.module_cx;
            let mod_env = &mod_cx.env;
            let struct_env = mod_env.clone().into_struct(*struct_id);
            let module = mod_cx.llvm_module;
            let data_layout = module.get_module_data_layout();

            let name = struct_env.get_full_name_str();
            debug!(target: "struct", "Creating dwarf info for struct move_name {}, llvm_name {} mod_id {:#?} struct_id {:#?}",
                name, struct_llvm_name, mod_id, struct_id);

            // FIXME: not clear whether to use 'name' or 'struct_llvm_name' for DWARF
            let struct_name = struct_llvm_name;
            let name_cstr = to_cstring!(struct_name);
            let (struct_nm_ptr, struct_nm_len) = (name_cstr.as_ptr(), name_cstr.as_bytes().len());
            let unique_id = std::ffi::CString::new("unique_id").expect("CString conversion failed");

            let name_space = unsafe {
                LLVMDIBuilderCreateNameSpace(
                    di_builder,
                    di_builder_file,
                    struct_nm_ptr,
                    struct_nm_len,
                    0,
                )
            };
            let loc = struct_env.get_loc();
            let (filename, location) = struct_env
                .module_env
                .env
                .get_file_and_location(&loc)
                .unwrap_or(("unknown".to_string(), Location::new(0, 0)));
            debug!(target: "struct", "{struct_name} {}:{}", filename, location.line.0);

            let struct_type = self
                .global_ctx()
                .unwrap()
                .llvm_cx
                .named_struct_type(struct_name)
                .expect("no struct type");

            let struct_info = struct_type.dump_to_string();
            debug!(target: "struct", "{struct_name} {}", struct_info);

            let struct_type_in_bits = struct_type.as_any_type().size_of_type_in_bits(data_layout);
            let struct_prefered_alignment = struct_type
                .as_any_type()
                .preferred_alignment_of_type(data_layout);

            let struct_ptr_type = struct_type.ptr_type();
            let struct_ptr_type_in_bits = struct_ptr_type.size_of_type_in_bits(data_layout);
            // Note: ignore preferred_alignment_of_type() for ptr
            let struct_ptr_prefered_alignment = struct_ptr_type_in_bits.try_into().unwrap();

            debug!(target: "struct",
                "{struct_name} sz {struct_type_in_bits} align {struct_prefered_alignment} ptr {struct_ptr_type_in_bits} align {struct_ptr_prefered_alignment}");

            Self::struct_fields_info(&struct_type, data_layout, "from struct_type");

            let struct_fields = struct_env.get_fields();
            let mut fields: Vec<LLVMMetadataRef> = enumerate(struct_fields).scan(0, |current_offset, (idx, field)| {
                let symbol = field.get_name();
                let fld_name = symbol.display(mod_env.symbol_pool()).to_string();
                let fld_name_cstr = to_cstring!(fld_name.clone());
                let (field_nm_ptr, field_nm_len) = (fld_name_cstr.as_ptr(), fld_name_cstr.as_bytes().len());
                let offset = field.get_offset();
                let mv_ty = field.get_type();
                let llvm_ty = struct_type.struct_get_type_at_index(offset);
                let store_size_of_type = llvm_ty.store_size_of_type(data_layout);
                let abi_size_of_type = llvm_ty.abi_size_of_type(data_layout);
                let abi_alignment_of_type = llvm_ty.abi_alignment_of_type(data_layout);
                let size_of_type_in_bits = llvm_ty.size_of_type_in_bits(data_layout);
                let preferred_alignment_of_type = llvm_ty.preferred_alignment_of_type(data_layout);
                let element_offset = struct_type.offset_of_element(data_layout, idx);
                debug!(target: "struct", "Struct at {idx} field {fld_name}: store_size_of_type {}, abi_size_of_type {}, abi_alignment_of_type {}, size_of_type_in_bits {}, preferred_alignment_of_type {}, element_offset {}",
                    store_size_of_type, abi_size_of_type, abi_alignment_of_type, size_of_type_in_bits, preferred_alignment_of_type, element_offset);

                let fld_loc = mod_env.find_named_constant(symbol).map_or_else(|| mod_env.env.unknown_loc(), |named_const| named_const.get_loc());
                let fld_loc_str = fld_loc.display(mod_env.env).to_string();
                debug!(target: "struct", "Field {}: {:#?} {}", &fld_name, &fld_loc, fld_loc_str);

                let fld_type = self.get_type(mv_ty.clone(), &fld_name);

                if fld_type == self.core().type_unspecified {
                    if let mty::Type::Struct(mod_id, struct_id, _v) = mv_ty.clone() {
                        debug!(target: "struct", "fld {fld_name} mod_id {:#?} struct_id {:#?}", mod_id, struct_id);
                        let fld_struct_type = llvm_ty.as_struct_type();
                        let fld_struct_info = fld_struct_type.dump_to_string();
                        debug!(target: "struct", "fld {fld_name} {}", fld_struct_info);
                        let msg = format!("Unresoled field in struct {}", struct_name);
                        self.core().add_unresolved_mty(mv_ty.clone(), fld_name.clone(), msg);
                    }
                }

                let vars = mv_ty.get_vars(); // FIXME: how vars can be used for DWARF?
                debug!(target: "struct", "vars {:#?}", vars);

                let sz_in_bits: u64 = size_of_type_in_bits;
                let align_in_bits: u32 = abi_alignment_of_type * 8;
                let fld = unsafe { LLVMDIBuilderCreateMemberType(
                    di_builder,
                    name_space,
                    field_nm_ptr,
                    field_nm_len,
                    di_builder_file, //File: LLVMMetadataRef,
                    location.line.0 + offset as u32,  // FIXME: Loc for fields is the index
                    sz_in_bits,
                    align_in_bits,
                    *current_offset,
                    0,
                    fld_type,
                )};

                let mut length: ::libc::size_t = 0;
                let name_c_str  = unsafe { LLVMDITypeGetName(fld, &mut length) };
                let field_name = unsafe { std::ffi::CStr::from_ptr(name_c_str).to_string_lossy().into_owned() };
                debug!(target: "struct", "Struct at {idx} field {fld_name}: created member type {field_name}");

                *current_offset += store_size_of_type * 8;
                Some(fld)
            }).collect();
            let fields_mut: *mut LLVMMetadataRef = fields.as_mut_ptr();

            let struct_meta = unsafe {
                LLVMDIBuilderCreateStructType(
                    di_builder,
                    name_space,
                    struct_nm_ptr,   // Name: *const ::libc::c_char,
                    struct_nm_len,   // NameLen: ::libc::size_t,
                    di_builder_file, //File: LLVMMetadataRef,
                    location.line.0,
                    struct_type_in_bits,
                    struct_prefered_alignment,
                    LLVMDIFlagObjcClassComplete, // FIXME! unclear how flags are used
                    parent.unwrap_or(ptr::null_mut()), // DerivedFrom: LLVMMetadataRef,
                    fields_mut,                  // Elements: *mut LLVMMetadataRef,
                    fields.len() as u32,         // NumElements: ::libc::c_uint,
                    0,               // RunTimeLang: ::libc::c_uint - FIXME: unclear how it is used
                    ptr::null_mut(), // VTableHolder: LLVMMetadataRef - FIXME: likely not used in MOVE
                    unique_id.as_ptr(), // UniqueId: *const ::libc::c_char - FIXME: not set for now, maybe useful
                    0,                  // UniqueIdLen: ::libc::size_t
                )
            };
            let struct_id: move_model::model::StructId = struct_env.get_id();
            self.core().add_type_struct(struct_id, struct_meta); // Add creted struct type to DB of struct types

            // Check the name in DWARF
            let struct_ref = struct_meta as LLVMMetadataRef;
            let struct_name_new = type_get_name(struct_ref);
            debug!(target: "struct", "Added struct type {}", struct_name_new);

            assert!(
                struct_name == struct_name_new,
                "Must create DRARF struct with the same name"
            );

            // FIXME: is it used/usefull?
            let struct_kind = unsafe { LLVMGetMetadataKind(struct_meta) };
            debug!(target: "struct", "struct_kind {:#?}", struct_kind);

            let name_cstr_for_ptr = to_cstring!(format!("{}__ptr", struct_name));
            let (name_cstr_for_ptr_nm_ptr, name_cstr_for_ptr_nm_len) = (
                name_cstr_for_ptr.as_ptr(),
                name_cstr_for_ptr.as_bytes().len(),
            );

            // DWARF wants this set
            let struct_ptr = unsafe {
                LLVMDIBuilderCreatePointerType(
                    di_builder,
                    struct_meta,
                    struct_ptr_type_in_bits, // FIXME: maybe ignored?
                    struct_ptr_prefered_alignment,
                    0,
                    name_cstr_for_ptr_nm_ptr,
                    name_cstr_for_ptr_nm_len,
                )
            };

            let module_di = &self.module_di().unwrap();
            let module_ctx = unsafe { LLVMGetModuleContext(*module_di) };
            let meta_as_value = unsafe { LLVMMetadataAsValue(module_ctx, struct_ptr) };
            unsafe { LLVMAddNamedMetadataOperand(*module_di, struct_nm_ptr, meta_as_value) };

            let out = unsafe { LLVMPrintModuleToString(*module_di) };
            let c_string: *mut i8 = out;
            let c_str = unsafe {
                CStr::from_ptr(c_string)
                    .to_str()
                    .expect("Cannot convert to &str")
            };
            debug!(target: "struct", "struct {struct_name}: DI content: starting at next line and until line starting with !!!\n{}\n!!!\n", c_str);
        }
    }

    pub fn create_instruction<'a>(
        &self,
        bc: &'a Bytecode,
        func_ctx: &'a FunctionContext<'_, '_>,
    ) -> Option<Instruction<'a>> {
        if let Some(_di_builder_core) = &self.0 {
            let instr = Instruction::new(bc, func_ctx);
            instr.debug();
            return Some(instr);
        }
        None
    }

    pub fn create_load_store(
        &self,
        instr: Option<Instruction>,
        load: *mut LLVMValue,
        store: *mut LLVMValue,
        mty: &mty::Type,
    ) {
        if let Some(_di_builder_core) = &self.0 {
            if let Some(instruction) = instr {
                instruction.create_load_store(load, store, mty);
            }
        }
    }

    pub fn finalize(&self) {
        if let Some(x) = &self.0 {
            unsafe { LLVMDIBuilderFinalize(x.builder_ref) };
        }
    }
}

fn loc_display(loc: &move_model::model::Loc, env: &GlobalEnv) -> (String, u32, u32, u32, u32) {
    if let Some((fname, pos)) = env.get_file_and_location(loc) {
        (
            fname,
            (pos.line + codespan::LineOffset(1)).0,
            (pos.column + codespan::ColumnOffset(1)).0,
            loc.span().start().0,
            loc.span().end().0,
        )
    } else {
        ("unknown source".to_string(), 0, 0, 0, 0)
    }
}

fn find_line_column(file_path: &str, find_offset: usize) -> Result<(i32, usize)> {
    // Open the file
    let file = File::open(file_path)?;
    let reader = BufReader::new(file);

    // Read the file line by line
    let mut current_offset = 0;
    let mut line_number = 1;

    for line in reader.lines() {
        let line = line?;
        let line_length = line.len() + 1; // Add 1 for the newline character

        if current_offset + line_length > find_offset {
            // Found the line where the substring starts
            let start_column = find_offset - current_offset;
            debug!(target: "bytecode", "{:#?} offset {find_offset} @ {line_number}:{start_column}", file_path);
            return Ok((line_number, start_column));
        }

        // Move to the next line
        current_offset += line_length;
        line_number += 1;
    }

    // Substring spans multiple lines. Unable to determine end position.
    let err_msg = "Substring spans multiple lines. Unable to determine end position.";
    Err(anyhow::anyhow!(err_msg)).with_context(|| format!("Error processing file: {}", file_path))
}

// for debugging: for a given ascii file returns substring in the range file[begin, end]
fn read_substring(file_path: &str, begin: usize, end: usize) -> anyhow::Result<String> {
    // Open the file
    let mut file = File::open(file_path)?;

    // Seek to the beginning of the substring
    file.seek(SeekFrom::Start(begin as u64))?;

    // Read the substring into a buffer
    let mut buffer = Vec::with_capacity(end - begin);
    file.take((end - begin) as u64).read_to_end(&mut buffer)?;

    // Convert the buffer to a String
    let substring = String::from_utf8(buffer)?;

    Ok(substring)
}
