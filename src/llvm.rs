use crate::ir::{BinOp, Instr, InstrOp, Register, Value};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassBuilderOptions,
    targets::{InitializationConfig, Target, TargetTriple},
    types::BasicType,
    values::{BasicValue, BasicValueEnum},
};
use std::{collections::HashMap, path::Path};

pub fn initialize_llvm() -> Context {
    Target::initialize_all(&InitializationConfig::default());
    Context::create()
}

pub struct CodeGen<'ctx> {
    ctx: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
}
impl<'ctx> CodeGen<'ctx> {
    pub fn new(name: &str, ctx: &'ctx Context) -> Self {
        let builder = ctx.create_builder();
        let module = ctx.create_module(name);
        Self {
            ctx,
            builder,
            module,
        }
    }

    pub fn generate(&self, fn_name: &str, ir: &[Instr]) {
        let fun = self
            .module
            .add_function(fn_name, self.ctx.void_type().fn_type(&[], false), None);
        let blk = self.ctx.append_basic_block(fun, "entry");
        self.builder.position_at_end(blk);

        let mut map: HashMap<Register, BasicValueEnum<'_>> = HashMap::new();

        for i in ir {
            match i {
                Instr::Assign { reg, op } => match op {
                    InstrOp::Stack => {
                        let v = self.builder.build_alloca(self.ctx.i32_type(), reg).unwrap();
                        map.insert(reg.clone(), v.as_basic_value_enum());
                    }
                    InstrOp::Load(register) => {
                        let v = self
                            .builder
                            .build_load(
                                self.ctx.i32_type().as_basic_type_enum(),
                                map[register].into_pointer_value(),
                                reg,
                            )
                            .unwrap();
                        map.insert(reg.clone(), v);
                    }
                    InstrOp::Op(op, l, r) => {
                        let l = match l {
                            Value::Number(n) => self
                                .ctx
                                .i32_type()
                                .const_int(*n as u64, true)
                                .as_basic_value_enum(),
                            Value::Register(register) => map[register].as_basic_value_enum(),
                        }
                        .into_int_value();
                        let r = match r {
                            Value::Number(n) => self
                                .ctx
                                .i32_type()
                                .const_int(*n as u64, true)
                                .as_basic_value_enum(),
                            Value::Register(register) => map[register].as_basic_value_enum(),
                        }
                        .into_int_value();

                        let v = match op {
                            BinOp::Add => self.builder.build_int_add(l, r, reg),
                            BinOp::Sub => self.builder.build_int_sub(l, r, reg),
                            BinOp::Mul => self.builder.build_int_mul(l, r, reg),
                            BinOp::Div => self.builder.build_int_signed_div(l, r, reg),
                        }
                        .unwrap();
                        map.insert(reg.clone(), v.as_basic_value_enum());
                    }
                    InstrOp::Neg(register) => {
                        let v = self
                            .builder
                            .build_int_neg(map[register].into_int_value(), reg)
                            .unwrap();
                        map.insert(reg.clone(), v.as_basic_value_enum());
                    }
                    InstrOp::Call(_, _) => {
                        todo!()
                    }
                },
                Instr::Store(register, value) => {
                    let v = match value {
                        Value::Number(n) => self
                            .ctx
                            .i32_type()
                            .const_int(*n as u64, true)
                            .as_basic_value_enum(),
                        Value::Register(register) => map[register].as_basic_value_enum(),
                    };
                    self.builder
                        .build_store(map[register].into_pointer_value(), v)
                        .unwrap();
                }
                Instr::Ret(Some(value)) => {
                    let v = match value {
                        Value::Number(n) => self
                            .ctx
                            .i32_type()
                            .const_int(*n as u64, true)
                            .as_basic_value_enum(),
                        Value::Register(register) => map[register].as_basic_value_enum(),
                    };
                    self.builder.build_return(Some(&v)).unwrap();
                }
                Instr::Ret(None) => {
                    self.builder.build_return(None).unwrap();
                }
            }
        }
    }

    pub fn print_ir(&self) {
        self.module.print_to_stderr();
    }

    pub fn optimize(&self) {
        let triple = TargetTriple::create("riscv32-unknown-elf");
        // let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                inkwell::OptimizationLevel::Default,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .unwrap();
        self.module
            .run_passes("default<O3>", &target_machine, PassBuilderOptions::create())
            .unwrap();
    }

    pub fn generate_object_file(&self, filename: impl AsRef<Path>) {
        let triple = TargetTriple::create("riscv32-unknown-elf");
        let target = Target::from_triple(&triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                inkwell::OptimizationLevel::Default,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .unwrap();
        target_machine
            .write_to_file(
                &self.module,
                inkwell::targets::FileType::Object,
                filename.as_ref(),
            )
            .unwrap();
    }
}
