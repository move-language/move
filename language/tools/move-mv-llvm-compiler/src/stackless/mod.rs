use self::exts::*;
use move_model::ast as mast;
use move_model::model as mm;
use move_model::ty as mty;
use move_stackless_bytecode::stackless_bytecode as sbc;
use std::collections::HashMap;

mod llvm;

#[derive(Copy, Clone)]
pub enum Target {
    Solana,
}

impl Target {
    pub fn triple(&self) -> &'static str {
        match self {
            Target::Solana => "bpfel-unknown-unknown",
        }
    }
}

pub struct GlobalContext<'a> {
    env: &'a mm::GlobalEnv,
    llvm_cx: llvm::Context,
    target: Target,
}

impl<'a> GlobalContext<'a> {
    pub fn new(env: &mm::GlobalEnv, target: Target) -> GlobalContext {
        GlobalContext {
            env,
            llvm_cx: llvm::Context::new(),
            target,
        }
    }

    pub fn create_module_context(&self, id: mm::ModuleId) -> ModuleContext {
        let env = self.env.get_module(id);
        let name = env.llvm_module_name();
        ModuleContext {
            env,
            llvm_cx: &self.llvm_cx,
            llvm_module: self.llvm_cx.create_module(&name),
            llvm_builder: self.llvm_cx.create_builder(),
            target: self.target,
        }
    }
}

pub struct ModuleContext<'a> {
    env: mm::ModuleEnv<'a>,
    llvm_cx: &'a llvm::Context,
    llvm_module: llvm::Module,
    llvm_builder: llvm::Builder,
    target: Target,
}

impl<'a> ModuleContext<'a> {
    pub fn translate(self) -> llvm::Module {
        self.llvm_module.set_target(self.target.triple());

        let filename = self.env.get_source_path().to_str().expect("utf-8");
        self.llvm_module.set_source_file_name(filename);

        for fn_env in self.env.get_functions() {
            let fn_cx = self.create_fn_context(fn_env);
            fn_cx.translate();
        }

        self.llvm_module.verify();

        self.llvm_module
    }

    fn create_fn_context<'b>(&'b self, fn_env: mm::FunctionEnv<'b>) -> FunctionContext<'b> {
        let locals = Vec::with_capacity(fn_env.get_local_count());
        FunctionContext {
            env: fn_env,
            llvm_cx: &self.llvm_cx,
            llvm_module: &self.llvm_module,
            llvm_builder: &self.llvm_builder,
            label_blocks: HashMap::new(),
            locals,
        }
    }
}

struct FunctionContext<'a> {
    env: mm::FunctionEnv<'a>,
    llvm_cx: &'a llvm::Context,
    llvm_module: &'a llvm::Module,
    llvm_builder: &'a llvm::Builder,
    label_blocks: HashMap<sbc::Label, llvm::BasicBlock>,
    /// Corresponds to FunctionData:local_types
    locals: Vec<Local>,
}

/// A stackless move local variable, translated as an llvm alloca
struct Local {
    mty: mty::Type,
    llty: llvm::Type,
    llval: llvm::Alloca,
}

impl<'a> FunctionContext<'a> {
    fn translate(mut self) {
        use move_stackless_bytecode::stackless_bytecode_generator::StacklessBytecodeGenerator;

        let fn_data = StacklessBytecodeGenerator::new(&self.env).generate_function();

        dbg!(&fn_data);

        // Create the llvm function
        let ll_fn = {
            let ll_fnty = {
                let ll_rty = match fn_data.return_types.len() {
                    0 => self.llvm_cx.void_type(),
                    1 => self.llvm_type(&fn_data.return_types[0]),
                    _ => {
                        todo!()
                    }
                };

                let ll_parm_tys = self
                    .env
                    .get_parameter_types()
                    .iter()
                    .map(|mty| self.llvm_type(mty))
                    .collect::<Vec<_>>();

                llvm::FunctionType::new(ll_rty, &ll_parm_tys)
            };

            self.llvm_module
                .add_function(&self.env.llvm_symbol_name(), ll_fnty)
        };

        // Create basic blocks and position builder at entry block
        {
            let entry_block = ll_fn.append_basic_block("entry");

            // Create basic blocks for move labels
            for instr in &fn_data.code {
                match instr {
                    sbc::Bytecode::Label(_, label) => {
                        let name = llvm::Name::new("bb", label.as_usize());
                        let llbb = ll_fn.append_basic_block(name);
                        self.label_blocks.insert(*label, llbb);
                    }
                    _ => {}
                }
            }

            self.llvm_builder.position_at_end(entry_block);
        }

        // Declare all the locals as allocas
        {
            for (i, mty) in fn_data.local_types.iter().enumerate() {
                let llty = self.llvm_type(mty);
                let name = llvm::Name::new("local", i);
                let llval = self.llvm_builder.build_alloca(llty, name);
                self.locals.push(Local {
                    mty: mty.clone(), // fixme bad clone
                    llty,
                    llval,
                });
            }
        }

        // Store params into locals
        {
            let param_count = self.env.get_parameter_count();
            let ll_params = (0..param_count).map(|i| ll_fn.get_param(i));

            for (ll_param, local) in ll_params.zip(self.locals.iter()) {
                self.llvm_builder
                    .store_param_to_alloca(ll_param, local.llval);
            }
        }

        // Translate instructions
        for instr in &fn_data.code {
            self.translate_instruction(instr);
        }

        ll_fn.verify();
    }

    fn translate_instruction(&self, instr: &sbc::Bytecode) {
        match instr {
            sbc::Bytecode::Assign(_, dst, src, sbc::AssignKind::Move) => {
                let mty = &self.locals[*dst].mty;
                let llty = self.locals[*dst].llty;
                let dst_llval = self.locals[*dst].llval;
                let src_llval = self.locals[*src].llval;
                match mty {
                    mty::Type::Primitive(mty::PrimitiveType::Bool | mty::PrimitiveType::U8) => {
                        self.llvm_builder.load_store(llty, src_llval, dst_llval);
                    }
                    _ => todo!(),
                }
            }
            sbc::Bytecode::Assign(_, dst, src, sbc::AssignKind::Store) => {
                let mty = &self.locals[*dst].mty;
                let llty = self.locals[*dst].llty;
                let dst_llval = self.locals[*dst].llval;
                let src_llval = self.locals[*src].llval;
                match mty {
                    mty::Type::Primitive(mty::PrimitiveType::Bool | mty::PrimitiveType::U8) => {
                        self.llvm_builder.load_store(llty, src_llval, dst_llval);
                    }
                    _ => todo!(),
                }
            }
            sbc::Bytecode::Call(_, dst, op, src, None) => {
                self.translate_call(dst, op, src);
            }
            sbc::Bytecode::Ret(_, vals) => match vals.len() {
                0 => {
                    self.llvm_builder.build_return_void();
                }
                1 => {
                    let idx = vals[0];
                    let llval = self.locals[idx].llval;
                    let llty = self.locals[idx].llty;
                    self.llvm_builder.load_return(llty, llval);
                }
                _ => todo!(),
            },
            sbc::Bytecode::Load(_, idx, val) => {
                let local_llval = self.locals[*idx].llval;
                let const_llval = self.constant(val);
                self.llvm_builder.store_const(const_llval, local_llval);
            }
            sbc::Bytecode::Branch(_, label0, label1, cnd_idx) => {
                let cnd_llval = self.locals[*cnd_idx].llval;
                let cnd_llty = self.locals[*cnd_idx].llty;
                let bb0 = self.label_blocks[label0];
                let bb1 = self.label_blocks[label1];
                self.llvm_builder
                    .load_cond_br(cnd_llty, cnd_llval, bb0, bb1);
            }
            sbc::Bytecode::Jump(_, label) => {
                let llbb = self.label_blocks[label];
                self.llvm_builder.build_br(llbb);
            }
            sbc::Bytecode::Label(_, label) => {
                let llbb = self.label_blocks[label];
                self.llvm_builder.position_at_end(llbb);
            }
            sbc::Bytecode::Abort(_, local) => {
                self.emit_rtcall(RtCall::Abort(*local));
            }
            _ => {
                todo!()
            }
        }
    }

    fn translate_call(
        &self,
        dst: &[mast::TempIndex],
        op: &sbc::Operation,
        src: &[mast::TempIndex],
    ) {
        use sbc::Operation;
        match op {
            Operation::Destroy => {
                assert!(dst.is_empty());
                assert_eq!(src.len(), 1);
                let idx = src[0];
                let mty = &self.locals[idx].mty;
                match mty {
                    mty::Type::Primitive(_) => ( /* nop */ ),
                    _ => todo!(),
                }
            }
            Operation::Add => {
                assert_eq!(dst.len(), 1);
                assert_eq!(src.len(), 2);
                let dst_idx = dst[0];
                let src0_idx = src[0];
                let src1_idx = src[1];
                let dst_llval = self.locals[dst_idx].llval;
                let src0_llval = self.locals[src0_idx].llval;
                let src1_llval = self.locals[src1_idx].llval;
                let mty = &self.locals[src0_idx].mty;
                let llty = self.locals[src0_idx].llty;
                match mty {
                    mty::Type::Primitive(mty::PrimitiveType::U8) => {
                        self.llvm_builder
                            .load_add_store(llty, src0_llval, src1_llval, dst_llval);
                    }
                    _ => todo!(),
                }
            }
            Operation::Sub => todo!(),
            Operation::Mul => todo!(),
            Operation::Div => todo!(),
            Operation::Mod => todo!(),
            Operation::BitOr => todo!(),
            Operation::BitAnd => todo!(),
            Operation::Xor => todo!(),
            Operation::Shl => todo!(),
            Operation::Shr => todo!(),
            Operation::Eq => {
                assert_eq!(dst.len(), 1);
                assert_eq!(src.len(), 2);
                let dst_idx = dst[0];
                let src0_idx = src[0];
                let src1_idx = src[1];
                let dst_llval = self.locals[dst_idx].llval;
                let src0_llval = self.locals[src0_idx].llval;
                let src1_llval = self.locals[src1_idx].llval;
                let mty = &self.locals[src0_idx].mty;
                let llty = self.locals[src0_idx].llty;
                match mty {
                    mty::Type::Primitive(mty::PrimitiveType::U8) => {
                        self.llvm_builder.load_icmp_store(
                            llty,
                            src0_llval,
                            src1_llval,
                            dst_llval,
                            llvm::LLVMIntPredicate::LLVMIntEQ,
                        );
                    }
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
    }

    fn llvm_type(&self, mty: &mty::Type) -> llvm::Type {
        use mty::{PrimitiveType, Type};

        match mty {
            Type::Primitive(PrimitiveType::Bool) => self.llvm_cx.int1_type(),
            Type::Primitive(PrimitiveType::U8) => self.llvm_cx.int8_type(),
            Type::Primitive(PrimitiveType::U64) => self.llvm_cx.int64_type(),
            _ => {
                todo!()
            }
        }
    }

    fn constant(&self, mc: &sbc::Constant) -> llvm::Constant {
        use sbc::Constant;
        match mc {
            Constant::U8(val) => {
                let llty = self.llvm_cx.int8_type();
                llvm::Constant::int(llty, *val as u64)
            }
            Constant::U64(val) => {
                let llty = self.llvm_cx.int64_type();
                llvm::Constant::int(llty, *val)
            }
            _ => todo!(),
        }
    }

    fn emit_rtcall(&self, rtcall: RtCall) {
        match &rtcall {
            RtCall::Abort(local_idx) => {
                let (llfn, llfnty) = self.get_runtime_function(&rtcall);
                let local_llval = self.locals[*local_idx].llval;
                let local_llty = self.locals[*local_idx].llty;
                self.llvm_builder
                    .load_call(llfnty, llfn, &[(local_llty, local_llval)]);
                self.llvm_builder.build_unreachable();
            }
        }
    }

    fn get_runtime_function(&self, rtcall: &RtCall) -> (llvm::Function, llvm::FunctionType) {
        let name = match rtcall {
            RtCall::Abort(..) => "abort",
        };
        let name = format!("move_rt_{name}");
        let llfn = self.llvm_module.get_named_function(&name);
        if let Some(llfn) = llfn {
            let llty = llfn.llvm_type();
            (llfn, llty)
        } else {
            let (llty, attrs) = match rtcall {
                RtCall::Abort(..) => {
                    let ret_ty = self.llvm_cx.void_type();
                    let param_tys = &[self.llvm_cx.int64_type()];
                    let llty = llvm::FunctionType::new(ret_ty, param_tys);
                    let attrs = vec![llvm::AttributeKind::NoReturn];
                    (llty, attrs)
                }
            };

            let llfn = self
                .llvm_module
                .add_function_with_attrs(&name, llty, &attrs);
            (llfn, llty)
        }
    }
}

mod exts {
    use super::mm;
    use extension_trait::extension_trait;

    #[extension_trait]
    pub impl<'a> ModuleEnvExt for mm::ModuleEnv<'a> {
        fn llvm_module_name(&self) -> String {
            self.get_full_name_str().replace(':', "_")
        }
    }

    #[extension_trait]
    pub impl<'a> FunctionEnxExt for mm::FunctionEnv<'a> {
        fn llvm_symbol_name(&self) -> String {
            // fixme use get_full_name_str
            let name = self.get_name_str();
            if name == "<SELF>" {
                // fixme move-model names script fns "<SELF>".
                // we might want to preserve the actual names
                "main".to_string()
            } else {
                name
            }
        }
    }
}

pub enum RtCall {
    Abort(mast::TempIndex),
}
