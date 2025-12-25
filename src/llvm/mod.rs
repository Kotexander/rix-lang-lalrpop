use std::{collections::HashMap, path::Path};

use crate::{
    ast::{BinOp, Expr, ExprKind, Instr, InstrKind, Item, ItemKind, UniOp},
    resolver::{
        annotations::{Annotations, DefId},
        resource::Value,
        typ,
    },
    strings,
};
use inkwell::{
    IntPredicate, builder,
    context::Context,
    module::Module,
    passes::PassBuilderOptions,
    targets::{InitializationConfig, Target, TargetMachine},
    types::{AnyType, BasicType},
    values::{BasicValue, PointerValue},
};

pub fn initialize_llvm() -> Context {
    Target::initialize_all(&InitializationConfig::default());
    Context::create()
}

pub struct CodeGen<'ctx> {
    ctx: &'ctx Context,
    module: Module<'ctx>,

    funs: HashMap<DefId, inkwell::values::FunctionValue<'ctx>>,
    strings: HashMap<strings::Id, inkwell::values::GlobalValue<'ctx>>,

    interner: &'ctx strings::Interner,
    annotations: &'ctx Annotations,

    target_machine: TargetMachine,
}
impl<'ctx> CodeGen<'ctx> {
    pub fn new(
        module_name: &str,
        ctx: &'ctx Context,
        interner: &'ctx strings::Interner,
        annotations: &'ctx Annotations,
    ) -> Self {
        let module = ctx.create_module(module_name);

        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).expect("Failed to get target from triple");
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                inkwell::OptimizationLevel::Default,
                inkwell::targets::RelocMode::PIC,
                inkwell::targets::CodeModel::Default,
            )
            .expect("Failed to create target machine");

        let funs = HashMap::new();
        let strings = HashMap::new();

        module.set_data_layout(&target_machine.get_target_data().get_data_layout());

        Self {
            ctx,
            module,
            funs,
            strings,
            interner,
            annotations,
            target_machine,
        }
    }

    pub fn generate(&mut self, ast: &[Item]) {
        for item in ast {
            match &item.kind {
                ItemKind::Function { name, .. } => {
                    let name = self.interner.get(name.kind);
                    let def_id = self.annotations.def_of(item.id);
                    let val = self.annotations.get(def_id);
                    let fn_typ = match val {
                        Value::Function { typ, .. } => typ.as_ref().unwrap(),
                        _ => panic!(),
                    };
                    let ty = self.get_llvm_fn_type(fn_typ);
                    let fun = self.module.add_function(name, ty, None);
                    self.funs.insert(def_id, fun);
                }
                ItemKind::Struct { .. } => todo!(),
                ItemKind::Union { .. } => todo!(),
            }
        }

        for item in ast {
            if let ItemKind::Function {
                body: Some(body),
                args,
                ..
            } = &item.kind
            {
                let mut map = HashMap::new();

                let fun = self.funs[&self.annotations.def_of(item.id)];
                let blk = self.ctx.append_basic_block(fun, "entry");

                let builder = self.ctx.create_builder();
                builder.position_at_end(blk);

                for (arg_i, (arg_name, _)) in args.iter().enumerate() {
                    let arg_def_id = self.annotations.def_of(arg_name.id);
                    let param = fun.get_nth_param(arg_i as u32).unwrap();
                    let ptr = builder
                        .build_alloca(param.get_type(), self.interner.get(arg_name.kind))
                        .unwrap();
                    builder.build_store(ptr, param).unwrap();
                    map.insert(arg_def_id, ptr);
                }

                let _returned = self.generate_body(body, &builder, &mut map);
            }
        }
    }

    fn generate_body(
        &mut self,
        body: &[Instr],
        builder: &builder::Builder<'ctx>,
        map: &mut HashMap<DefId, PointerValue<'ctx>>,
    ) -> bool {
        for instr in body {
            self.generate_instr(instr, &builder, map);
            if builder
                .get_insert_block()
                .unwrap()
                .get_terminator()
                .is_some()
            {
                return true;
            }
        }
        false
    }

    fn generate_instr(
        &mut self,
        instr: &Instr,
        builder: &builder::Builder<'ctx>,
        map: &mut HashMap<DefId, PointerValue<'ctx>>,
    ) {
        match &instr.kind {
            InstrKind::Error => panic!("ast should not contain error nodes"),
            InstrKind::VarInit { name, typ: _, expr } => {
                let val = self.annotations.resolve(name.id);
                let typ = val.typ().unwrap();
                // allocate variable at the entry block
                let ptr = {
                    let tmp_builder = self.ctx.create_builder();
                    let entry = builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap()
                        .get_first_basic_block()
                        .unwrap();
                    if let Some(first) = entry.get_first_instruction() {
                        tmp_builder.position_before(&first);
                    } else {
                        tmp_builder.position_at_end(entry);
                    }
                    tmp_builder
                        .build_alloca(
                            to_basic_type(self.get_llvm_type(&typ)),
                            self.interner.get(name.kind),
                        )
                        .unwrap()
                };
                let v = self.generate_expr(expr, builder, map).unwrap();
                builder.build_store(ptr, v).unwrap();
                map.insert(self.annotations.def_of(name.id), ptr);
            }
            InstrKind::VarAssign { name, expr } => {
                let def_id = self.annotations.def_of(name.id);
                let ptr = map[&def_id];
                let v = self.generate_expr(expr, builder, map).unwrap();
                builder.build_store(ptr, v).unwrap();
            }
            InstrKind::Return(node) => {
                if let Some(expr) = node {
                    let v = self.generate_expr(expr, builder, map).unwrap();
                    builder.build_return(Some(&v)).unwrap();
                } else {
                    builder.build_return(None).unwrap();
                }
            }
            InstrKind::Expr(node) => {
                self.generate_expr(node, builder, map);
            }
            InstrKind::While { cond, body } => {
                let blk = builder.get_insert_block().unwrap();

                let cond_bb = self.ctx.insert_basic_block_after(blk, "while.cond");
                let body_bb = self.ctx.insert_basic_block_after(cond_bb, "while.body");
                let end_bb = self.ctx.insert_basic_block_after(body_bb, "while.end");

                builder.build_unconditional_branch(cond_bb).unwrap();

                // Condition block
                builder.position_at_end(cond_bb);
                let cond_val = self
                    .generate_expr(cond, builder, map)
                    .unwrap()
                    .into_int_value();
                builder
                    .build_conditional_branch(cond_val, body_bb, end_bb)
                    .unwrap();

                // Body block
                builder.position_at_end(body_bb);

                let returned = self.generate_body(body, builder, map);
                if !returned {
                    builder.build_unconditional_branch(cond_bb).unwrap();
                }
                // End block
                builder.position_at_end(end_bb);
            }
            InstrKind::Break => todo!(),
            InstrKind::Continue => todo!(),
            InstrKind::If {
                cond,
                then,
                elifs,
                els,
            } => {
                let blk = builder.get_insert_block().unwrap();

                let then_bb = self.ctx.insert_basic_block_after(blk, "if.then");
                let mut elif_conds_bbs = Vec::new();
                let mut elif_bbs = Vec::new();
                for _ in 0..elifs.len() {
                    let elif_bb = self.ctx.insert_basic_block_after(
                        elif_bbs.last().copied().unwrap_or(then_bb),
                        "else.if.cond",
                    );
                    elif_conds_bbs.push(elif_bb);
                    let elif_body_bb = self.ctx.insert_basic_block_after(elif_bb, "else.if.body");
                    elif_bbs.push(elif_body_bb);
                }
                let else_bb = if els.is_some() {
                    Some(self.ctx.insert_basic_block_after(
                        elif_bbs.last().copied().unwrap_or(then_bb),
                        "if.else",
                    ))
                } else {
                    None
                };
                let end_bb = self.ctx.insert_basic_block_after(
                    else_bb.or(elif_bbs.last().copied()).unwrap_or(then_bb),
                    "if.end",
                );

                // Condition block
                let cond_val = self
                    .generate_expr(cond, builder, map)
                    .unwrap()
                    .into_int_value();

                if !elifs.is_empty() {
                    builder
                        .build_conditional_branch(cond_val, then_bb, elif_conds_bbs[0])
                        .unwrap();
                } else if let Some(else_bb) = else_bb {
                    builder
                        .build_conditional_branch(cond_val, then_bb, else_bb)
                        .unwrap();
                } else {
                    builder
                        .build_conditional_branch(cond_val, then_bb, end_bb)
                        .unwrap();
                }

                // Then block
                builder.position_at_end(then_bb);
                let returned = self.generate_body(then, builder, map);
                if !returned {
                    builder.build_unconditional_branch(end_bb).unwrap();
                }

                // Elif blocks
                for (i, (elif_cond, elif_body)) in elifs.iter().enumerate() {
                    builder.position_at_end(elif_conds_bbs[i]);
                    let cond_val = self
                        .generate_expr(elif_cond, builder, map)
                        .unwrap()
                        .into_int_value();
                    let body_bb = elif_bbs[i];
                    if i + 1 < elifs.len() {
                        builder
                            .build_conditional_branch(cond_val, body_bb, elif_conds_bbs[i + 1])
                            .unwrap();
                    } else if let Some(else_bb) = else_bb {
                        builder
                            .build_conditional_branch(cond_val, body_bb, else_bb)
                            .unwrap();
                    } else {
                        builder
                            .build_conditional_branch(cond_val, body_bb, end_bb)
                            .unwrap();
                    }
                    builder.position_at_end(body_bb);
                    let returned = self.generate_body(elif_body, builder, map);
                    if !returned {
                        builder.build_unconditional_branch(end_bb).unwrap();
                    }
                }

                // Else block
                if let Some(els_body) = els {
                    builder.position_at_end(else_bb.unwrap());
                    let returned = self.generate_body(els_body, builder, map);
                    if !returned {
                        builder.build_unconditional_branch(end_bb).unwrap();
                    }
                }

                // End block
                builder.position_at_end(end_bb);
            }
        }
    }
    fn generate_expr(
        &mut self,
        expr: &Expr,
        builder: &builder::Builder<'ctx>,
        map: &HashMap<DefId, PointerValue<'ctx>>,
    ) -> Option<inkwell::values::BasicValueEnum<'ctx>> {
        match &expr.kind {
            ExprKind::Number(n) => Some(
                self.ctx
                    .i32_type()
                    .const_int(*n as u64, true)
                    .as_basic_value_enum(),
            ),
            ExprKind::ArrayInit(e, size) => {
                let e = self.generate_expr(e, builder, map).unwrap();
                let t = e.get_type().array_type(
                    self.annotations
                        .get_const(size.id)
                        .unwrap()
                        .try_into()
                        .unwrap(),
                );
                // TODO: not zero
                Some(t.const_zero().as_basic_value_enum())
                // Some(t.const_array(&[e.into_array_value()]).as_basic_value_enum())
            }
            ExprKind::String(id) => Some(
                self.get_or_insert_string(*id)
                    .as_pointer_value()
                    .as_basic_value_enum(),
            ),
            ExprKind::Variable(node) => {
                let def_id = self.annotations.def_of(node.id);
                let typ = self.annotations.get(def_id).typ().unwrap();

                Some(
                    builder
                        .build_load(to_basic_type(self.get_llvm_type(&typ)), map[&def_id], "")
                        .unwrap(),
                )
            }
            ExprKind::Call { name, args } => {
                let def_id = self.annotations.def_of(name.id);
                let fun = self.funs[&def_id];
                let mut llvm_args = Vec::new();
                for arg in args {
                    let v = self.generate_expr(arg, builder, map).unwrap();
                    llvm_args.push(v.into());
                }
                builder
                    .build_call(fun, &llvm_args, self.interner.get(name.kind))
                    .unwrap()
                    .try_as_basic_value()
                    .basic()
            }
            ExprKind::BinOp { op, l, r } => {
                let l = self
                    .generate_expr(l, builder, map)
                    .unwrap()
                    .into_int_value();
                let r = self
                    .generate_expr(r, builder, map)
                    .unwrap()
                    .into_int_value();
                Some(
                    match op {
                        BinOp::Add => builder.build_int_add(l, r, ""),
                        BinOp::Sub => builder.build_int_sub(l, r, ""),
                        BinOp::Mul => builder.build_int_mul(l, r, ""),
                        BinOp::Div => builder.build_int_signed_div(l, r, ""),
                        BinOp::Mod => builder.build_int_signed_rem(l, r, ""),
                        BinOp::Eq => builder.build_int_compare(IntPredicate::EQ, l, r, ""),
                        BinOp::Lt => builder.build_int_compare(IntPredicate::SLT, l, r, ""),
                        BinOp::Gt => builder.build_int_compare(IntPredicate::SGT, l, r, ""),
                        BinOp::Le => builder.build_int_compare(IntPredicate::SLE, l, r, ""),
                        BinOp::Ge => builder.build_int_compare(IntPredicate::SGE, l, r, ""),
                        BinOp::Ne => builder.build_int_compare(IntPredicate::NE, l, r, ""),
                        BinOp::And => builder.build_and(l, r, ""),
                        BinOp::Or => builder.build_or(l, r, ""),
                    }
                    .unwrap()
                    .as_basic_value_enum(),
                )
            }
            ExprKind::UniOp { op, expr } => {
                let expr = self.generate_expr(expr, builder, map).unwrap();
                Some(
                    match op {
                        UniOp::Neg => builder.build_int_neg(expr.into_int_value(), ""),
                        UniOp::Ref => todo!(),
                        UniOp::Deref => todo!(),
                        UniOp::Not => match expr {
                            inkwell::values::BasicValueEnum::IntValue(iv) => {
                                builder.build_not(iv, "")
                            }
                            inkwell::values::BasicValueEnum::PointerValue(pv) => {
                                builder.build_is_null(pv, "")
                            }
                            _ => panic!(),
                        },
                    }
                    .unwrap()
                    .as_basic_value_enum(),
                )
            }
            ExprKind::FieldAccess { .. } => todo!(),
            ExprKind::StructInit { .. } => todo!(),
        }
    }

    fn get_llvm_type(&self, typ: &typ::Type) -> inkwell::types::AnyTypeEnum<'ctx> {
        match typ {
            typ::Type::Primitive(p) => match p {
                typ::PrimitiveType::I32 => self.ctx.i32_type().as_any_type_enum(),
                typ::PrimitiveType::U8 => self.ctx.i8_type().as_any_type_enum(),
                typ::PrimitiveType::Bool => self.ctx.bool_type().as_any_type_enum(),
                typ::PrimitiveType::Void => self.ctx.void_type().as_any_type_enum(),
                typ::PrimitiveType::Error => panic!("ast should not contain error types"),
            },
            typ::Type::Ptr(_) | typ::Type::Ref(_) => self
                .ctx
                .ptr_type(inkwell::AddressSpace::default())
                .as_any_type_enum(),
            typ::Type::Slice(_) => todo!(),
            typ::Type::Array(t, size) => {
                let elem_typ = to_basic_type(self.get_llvm_type(t));
                elem_typ
                    .array_type((*size).try_into().unwrap())
                    .as_any_type_enum()
            }
            typ::Type::Function(t) => self.get_llvm_fn_type(t).as_any_type_enum(),
        }
    }
    fn get_llvm_fn_type(&self, t: &typ::FunctionType) -> inkwell::types::FunctionType<'ctx> {
        let ret_typ = self.get_llvm_type(&t.ret);
        let params = t
            .args
            .iter()
            .map(|p| to_basic_type(self.get_llvm_type(p)).into())
            .collect::<Vec<_>>();
        match ret_typ {
            inkwell::types::AnyTypeEnum::VoidType(v) => v.fn_type(&params, t.var_args),
            _ => to_basic_type(ret_typ).fn_type(&params, t.var_args),
        }
    }
    fn get_or_insert_string(&mut self, id: strings::Id) -> inkwell::values::GlobalValue<'ctx> {
        if let Some(gv) = self.strings.get(&id) {
            return *gv;
        }
        let str = self.interner.get(id);
        let global_str = self.ctx.const_string(str.as_bytes(), true);
        let global = self.module.add_global(global_str.get_type(), None, "str");
        global.set_initializer(&global_str);
        global.set_constant(true);
        global.set_linkage(inkwell::module::Linkage::Private);
        global.set_unnamed_addr(true);
        // global.set_unnamed_address(inkwell::values::UnnamedAddress::Global);
        self.strings.insert(id, global);
        global
    }
    // fn build_cond(&self) {}

    pub fn print(&self, file: impl AsRef<std::path::Path>) {
        if let Err(e) = self.module.print_to_file(file) {
            eprintln!("Failed to write LLVM IR to file: {}", e);
        }
    }

    pub fn optimize(&self) {
        if let Err(err) = self.module.verify() {
            println!("{}\nAborting optimization", err.to_string_lossy());
            return;
        }

        self.module
            .run_passes(
                "default<O3>",
                &self.target_machine,
                PassBuilderOptions::create(),
            )
            .unwrap();
    }

    pub fn generate_object_file(&self, filename: impl AsRef<Path>) {
        if let Err(err) = self.module.verify() {
            println!("{}\nAborting object file generation", err.to_string_lossy());
            return;
        }

        self.target_machine
            .write_to_file(
                &self.module,
                inkwell::targets::FileType::Object,
                filename.as_ref(),
            )
            .unwrap();
    }
}

fn to_basic_type<'ctx>(
    any_type: inkwell::types::AnyTypeEnum<'ctx>,
) -> inkwell::types::BasicTypeEnum<'ctx> {
    match any_type {
        inkwell::types::AnyTypeEnum::ArrayType(t) => t.as_basic_type_enum(),
        inkwell::types::AnyTypeEnum::FloatType(t) => t.as_basic_type_enum(),
        inkwell::types::AnyTypeEnum::IntType(t) => t.as_basic_type_enum(),
        inkwell::types::AnyTypeEnum::PointerType(t) => t.as_basic_type_enum(),
        inkwell::types::AnyTypeEnum::StructType(t) => t.as_basic_type_enum(),
        inkwell::types::AnyTypeEnum::VectorType(t) => t.as_basic_type_enum(),
        inkwell::types::AnyTypeEnum::ScalableVectorType(t) => t.as_basic_type_enum(),
        _ => panic!("cannot convert to BasicTypeEnum"),
    }
}
