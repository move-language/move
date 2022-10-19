use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::debug_info::DICompileUnit;
use inkwell::debug_info::DebugInfoBuilder;
use inkwell::module::{Linkage, Module};
use inkwell::OptimizationLevel;
use inkwell::targets::TargetTriple;
use once_cell::sync::OnceCell;

static LLVM_INIT: OnceCell<()> = OnceCell::new();

pub struct MoveBPFModule<'a> {
    pub name: String,
    pub module: Module<'a>,
    pub builder: Builder<'a>,
    pub dibuilder: DebugInfoBuilder<'a>,
    pub di_compile_unit: DICompileUnit<'a>,
    pub(crate) context: &'a Context,
    pub(crate) opt: OptimizationLevel,
}


fn llvm_target_triple() -> TargetTriple {
    TargetTriple::create("bpfel-unknown-unknown")
}

fn llvm_target_name() -> &'static str {
    "bpfel" // bpf little endian.
}

fn llvm_features() -> &'static str {
    "" // no additional target specific features.
}

impl<'a> MoveBPFModule<'a> {
    pub fn new(
        context: &'a Context,
        name: &str,
        filename: &str,
        opt: OptimizationLevel,
    ) -> Self {
        LLVM_INIT.get_or_init(|| {
            inkwell::targets::Target::initialize_bpf(&Default::default());
        });

        let triple = llvm_target_triple();
        let module = context.create_module(name);

        let debug_metadata_version = context.i32_type().const_int(3, false);
        module.add_basic_value_flag(
            "Debug Info Version",
            inkwell::module::FlagBehavior::Warning,
            debug_metadata_version,
        );

        let builder = context.create_builder();
        let (dibuilder, di_compile_unit) = module.create_debug_info_builder(
            true,
            inkwell::debug_info::DWARFSourceLanguage::C,
            filename,
            ".",
            "Move",
            false,
            "",
            0,
            "",
            inkwell::debug_info::DWARFEmissionKind::Full,
            0,
            false,
            false,
            "",
            "",
        );

        module.set_triple(&triple);
        module.set_source_file_name(filename);

        MoveBPFModule {
            name: name.to_owned(),
            module,
            builder,
            dibuilder,
            di_compile_unit,
            context,
            opt,
        }
    }
}