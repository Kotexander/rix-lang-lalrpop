use crate::ast::{BinOp, ExprKind, Instr, InstrKind, UniOp};
use inkwell::{
    AddressSpace, IntPredicate,
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassBuilderOptions,
    targets::{InitializationConfig, Target, TargetMachine},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
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
    funcs: HashMap<String, FunctionValue<'ctx>>,
    strings: HashMap<String, PointerValue<'ctx>>,

    target: TargetMachine,
}
impl<'ctx> CodeGen<'ctx> {
    pub fn new(name: &str, ctx: &'ctx Context) -> Self {
        let builder = ctx.create_builder();
        let module = ctx.create_module(name);
        let mut funcs = HashMap::new();
        let strings = HashMap::new();

        // printf
        {
            let fn_type = ctx
                .void_type()
                .fn_type(&[ctx.ptr_type(AddressSpace::default()).into()], true);
            let fn_value = module.add_function("printf", fn_type, None);
            fn_value.add_attribute(
                AttributeLoc::Function,
                ctx.create_enum_attribute(Attribute::get_named_enum_kind_id("nounwind"), 0),
            );
            funcs.insert("printf".to_owned(), fn_value);
        }

        let triple = inkwell::targets::TargetMachine::get_default_triple();
        // let triple = inkwell::targets::TargetTriple::create("riscv32-unknown-elf");
        let target = Target::from_triple(&triple).unwrap();

        let target_machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                inkwell::OptimizationLevel::Default,
                inkwell::targets::RelocMode::PIC,
                inkwell::targets::CodeModel::Default,
            )
            .unwrap();
        Self {
            ctx,
            builder,
            module,
            funcs,
            strings,
            target: target_machine,
        }
    }

    pub fn generate(&mut self, fn_name: &str, ast: &[Instr]) {
        let fun = self
            .module
            .add_function(fn_name, self.ctx.void_type().fn_type(&[], false), None);
        fun.add_attribute(
            AttributeLoc::Function,
            self.ctx
                .create_enum_attribute(Attribute::get_named_enum_kind_id("nounwind"), 0),
        );
        self.funcs.insert(fn_name.to_owned(), fun);

        let mut map = HashMap::new();

        self.generate_block(fun, "entry", ast, &mut map);

        self.builder.build_return(None).unwrap();
    }

    fn generate_block(
        &mut self,
        fun: FunctionValue<'ctx>,
        name: &str,
        ast: &[Instr],
        map: &mut HashMap<String, BasicValueEnum<'ctx>>,
        // blk: BasicBlock<'ctx>,
    ) -> (BasicBlock<'ctx>, BasicBlock<'ctx>) {
        let mut blk = self.ctx.append_basic_block(fun, name);
        let start_block = blk;
        self.builder.position_at_end(blk);

        for i in ast {
            match &i.kind {
                InstrKind::VarInit { name, expr } => {
                    let ptr = self
                        .builder
                        .build_alloca(self.ctx.i32_type(), name)
                        .unwrap();
                    let val = generate_expr(&expr.kind, map, self).unwrap();
                    self.builder.build_store(ptr, val).unwrap();
                    map.insert(name.clone(), ptr.as_basic_value_enum());
                }
                InstrKind::Return(expr) => {
                    if let Some(expr) = expr {
                        let val = generate_expr(&expr.kind, map, self).unwrap();
                        self.builder.build_return(Some(&val)).unwrap();
                    } else {
                        self.builder.build_return(None).unwrap();
                    }
                }
                InstrKind::Expr(expr) => {
                    generate_expr(&expr.kind, map, self);
                }
                InstrKind::For {
                    var,
                    start,
                    end,
                    body,
                } => {
                    let ptr = self.builder.build_alloca(self.ctx.i32_type(), var).unwrap();
                    map.insert(var.clone(), ptr.as_basic_value_enum());

                    let start_val = generate_expr(&start.kind, map, self)
                        .unwrap()
                        .into_int_value();
                    let end_val = generate_expr(&end.kind, map, self)
                        .unwrap()
                        .into_int_value();
                    self.builder.build_store(ptr, start_val).unwrap();

                    let cond_block = self.ctx.append_basic_block(fun, "for.cond");
                    self.builder.build_unconditional_branch(cond_block).unwrap();
                    self.builder.position_at_end(cond_block);

                    let v = self
                        .builder
                        .build_load(self.ctx.i32_type(), ptr, var)
                        .unwrap()
                        .into_int_value();
                    let cond = self
                        .builder
                        .build_int_compare(IntPredicate::SLT, v, end_val, "")
                        .unwrap();

                    let (loop_body, _) = self.generate_block(fun, "for.body", body, map);

                    let v = self
                        .builder
                        .build_load(self.ctx.i32_type(), ptr, var)
                        .unwrap()
                        .into_int_value();
                    let one = self.ctx.i32_type().const_int(1, false);
                    let v_inc = self.builder.build_int_add(v, one, "").unwrap();
                    self.builder.build_store(ptr, v_inc).unwrap();
                    self.builder.build_unconditional_branch(cond_block).unwrap();

                    let after_block = self.ctx.append_basic_block(fun, "for.end");

                    self.builder.position_at_end(cond_block);
                    self.builder
                        .build_conditional_branch(cond, loop_body, after_block)
                        .unwrap();

                    blk = after_block;
                }
                InstrKind::If {
                    cond,
                    then,
                    elifs,
                    els,
                } => {
                    let cond = generate_expr(&cond.kind, map, self)
                        .unwrap()
                        .into_int_value();

                    // make all the blocks
                    let then_block = self.generate_block(fun, "if.then", then, map).1;
                    let elif_blocks: Vec<_> = elifs
                        .iter()
                        .map(|(_, elifs)| {
                            (
                                self.ctx.append_basic_block(fun, "if.else"),
                                self.generate_block(fun, "if.else.then", elifs, map).1,
                            )
                        })
                        .collect();
                    let else_block = els
                        .as_ref()
                        .map(|els| self.generate_block(fun, "if.else.then", els, map).1);
                    let after_block = self.ctx.append_basic_block(fun, "if.end");

                    self.builder.position_at_end(blk);
                    self.builder
                        .build_conditional_branch(
                            cond,
                            then_block,
                            elif_blocks
                                .first()
                                .map(|(b, _)| *b)
                                .or(else_block)
                                .unwrap_or(after_block),
                        )
                        .unwrap();

                    self.builder.position_at_end(then_block);
                    self.builder
                        .build_unconditional_branch(after_block)
                        .unwrap();

                    for (i, (elif_cond, _)) in elifs.iter().enumerate() {
                        let this_elif_block = elif_blocks[i];
                        let next_block = elif_blocks
                            .get(i + 1)
                            .map(|(b, _)| *b)
                            .or(else_block)
                            .unwrap_or(after_block);

                        self.builder.position_at_end(this_elif_block.0);

                        let cond = generate_expr(&elif_cond.kind, map, self)
                            .unwrap()
                            .into_int_value();
                        self.builder
                            .build_conditional_branch(cond, this_elif_block.1, next_block)
                            .unwrap();

                        self.builder.position_at_end(this_elif_block.1);
                        self.builder
                            .build_unconditional_branch(after_block)
                            .unwrap();
                    }
                    if let Some(els_block) = else_block {
                        self.builder.position_at_end(els_block);
                        self.builder
                            .build_unconditional_branch(after_block)
                            .unwrap();
                    }

                    blk = after_block;
                }
                InstrKind::Error => unimplemented!(),
            }
            self.builder.position_at_end(blk);
        }

        self.builder.position_at_end(blk);
        (start_block, blk)
    }

    fn get_string(&mut self, str: &str) -> PointerValue<'ctx> {
        if let Some(&ptr) = self.strings.get(str) {
            return ptr;
        }
        let global_str = self.ctx.const_string(str.as_bytes(), true);
        let global = self.module.add_global(global_str.get_type(), None, "str");
        global.set_initializer(&global_str);
        global.set_constant(true);
        global.set_linkage(inkwell::module::Linkage::Internal);
        global.set_unnamed_addr(true);
        global.set_unnamed_address(inkwell::values::UnnamedAddress::Global);
        let ptr = global.as_pointer_value();
        self.strings.insert(str.to_owned(), ptr);
        ptr
    }

    pub fn print_ir(&self) {
        self.module.print_to_stderr();
    }

    pub fn optimize(&self) {
        if let Err(err) = self.module.verify() {
            println!("{}\nAborting optimization", err.to_string_lossy());
            return;
        }

        self.module
            .run_passes("default<O3>", &self.target, PassBuilderOptions::create())
            .unwrap();
    }

    pub fn generate_object_file(&self, filename: impl AsRef<Path>) {
        if let Err(err) = self.module.verify() {
            println!("{}\nAborting object file generation", err.to_string_lossy());
            return;
        }

        self.target
            .write_to_file(
                &self.module,
                inkwell::targets::FileType::Object,
                filename.as_ref(),
            )
            .unwrap();
    }
}

fn generate_expr<'a>(
    expr: &ExprKind,
    map: &HashMap<String, BasicValueEnum<'a>>,
    cg: &mut CodeGen<'a>,
) -> Option<BasicValueEnum<'a>> {
    match expr {
        ExprKind::Number(n) => {
            Some(
                cg.ctx
                    .i32_type()
                    .const_int(*n as u64, false /* already sign exteded */)
                    .as_basic_value_enum(),
            )
        }
        ExprKind::Call { name, args } => {
            let es: Vec<_> = args
                .iter()
                .map(|e| generate_expr(&e.kind, map, cg).unwrap())
                .collect();
            let es_meta: Vec<BasicMetadataValueEnum> = es.iter().map(|&e| e.into()).collect();
            let v = cg.funcs[name];

            cg.builder
                .build_call(v, &es_meta, name)
                .unwrap()
                .try_as_basic_value()
                .left()
        }
        ExprKind::BinOp { op, l, r } => {
            let l = generate_expr(&l.kind, map, cg).unwrap().into_int_value();
            let r = generate_expr(&r.kind, map, cg).unwrap().into_int_value();
            Some(
                match op {
                    BinOp::Add => cg.builder.build_int_add(l, r, "").unwrap(),
                    BinOp::Sub => cg.builder.build_int_sub(l, r, "").unwrap(),
                    BinOp::Mul => cg.builder.build_int_mul(l, r, "").unwrap(),
                    BinOp::Div => cg.builder.build_int_signed_div(l, r, "").unwrap(),
                    BinOp::Mod => cg.builder.build_int_signed_rem(l, r, "").unwrap(),
                    BinOp::Eq => cg
                        .builder
                        .build_int_compare(IntPredicate::EQ, l, r, "")
                        .unwrap(),
                    BinOp::And => cg.builder.build_and(l, r, "").unwrap(),
                }
                .as_basic_value_enum(),
            )
        }
        ExprKind::UniOp { op, expr } => {
            let v = generate_expr(&expr.kind, map, cg).unwrap().into_int_value();
            Some(
                match op {
                    UniOp::Neg => cg.builder.build_int_neg(v, "").unwrap(),
                    UniOp::Ref => todo!(),
                    UniOp::Deref => todo!(),
                }
                .as_basic_value_enum(),
            )
        }
        ExprKind::String(s) => Some(cg.get_string(s).as_basic_value_enum()),
        ExprKind::Variable(v) => {
            let ptr = map[v].into_pointer_value();
            Some(cg.builder.build_load(cg.ctx.i32_type(), ptr, v).unwrap())
        }
    }
}
