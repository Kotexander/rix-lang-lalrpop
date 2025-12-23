pub mod annotations;
pub mod resource;
mod scope;
pub mod typ;

use std::rc::Rc;

use crate::{
    ast::{
        ArgTypKind, BinOp, Expr, ExprKind, Ident, Instr, InstrKind, Item, ItemKind, Span, Typ,
        TypKind, UniOp,
    },
    strings,
};
use annotations::Annotations;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use resource::Value;
use scope::ScopeStack;

pub struct Resolver<'a> {
    errors: &'a mut Vec<Diagnostic<usize>>,
    file: usize,
    bindings: Annotations,
    scope_stack: ScopeStack,
    interner: &'a mut strings::Interner,
}
impl<'a> Resolver<'a> {
    pub fn new(
        interner: &'a mut strings::Interner,
        errors: &'a mut Vec<Diagnostic<usize>>,
        file: usize,
    ) -> Self {
        let bindings = Annotations::new();
        let scope_stack = ScopeStack::new();

        Self {
            errors,
            file,
            bindings,
            scope_stack,
            interner,
        }
    }
    pub fn finish(self) -> Annotations {
        self.bindings
    }
    fn resolve_typ(&mut self, typ: &Typ) -> typ::Type {
        match &typ.kind {
            TypKind::Ident(id) => typ::Type::Primitive(match self.interner.get(id.kind) {
                "i32" => typ::PrimitiveType::I32,
                "u8" => typ::PrimitiveType::U8,
                "bool" => typ::PrimitiveType::Bool,
                "void" => typ::PrimitiveType::Void,
                _ => {
                    self.add_undef_typ_error(id);
                    typ::PrimitiveType::Error
                }
            }),
            TypKind::Ref(node) => typ::Type::Ref(std::rc::Rc::new(self.resolve_typ(node))),
            TypKind::Slice(node) => typ::Type::Slice(std::rc::Rc::new(self.resolve_typ(node))),
            TypKind::Ptr(node) => typ::Type::Ptr(std::rc::Rc::new(self.resolve_typ(node))),
        }
    }

    pub fn resolve(&mut self, items: &[Item]) {
        // first pass: declare all functions and types
        for item in items {
            match &item.kind {
                ItemKind::Function {
                    name,
                    args: _,
                    ret: _,
                    body: _,
                } => {
                    // typ is None because not all types have been resolved yet
                    let val = Value::Function {
                        span: name.span,
                        typ: None,
                    };
                    let def_id = self.bindings.bind(item.id, val);

                    if let Err(err) = self.scope_stack.declare(name.kind, def_id) {
                        let span = self.bindings.get(err).span();
                        self.add_dup_val_error(name, span);
                    }
                }
                ItemKind::Struct { .. } => {
                    eprintln!("structs are not implemented yet... skipping");
                }
                ItemKind::Union { .. } => {
                    eprintln!("unions  are not implemented yet... skipping");
                }
            }
        }

        // second pass: resolve function and structure field types
        for item in items {
            match &item.kind {
                ItemKind::Function {
                    name: _,
                    args,
                    ret,
                    body: _,
                } => {
                    let argc = args.len();
                    let mut var_args = false;
                    let mut arg_types = vec![];

                    for (i, (arg_name, arg_typ)) in args.iter().enumerate() {
                        let ty = match &arg_typ.kind {
                            ArgTypKind::Typ(node) => Some(self.resolve_typ(node)),
                            ArgTypKind::VarArgs => {
                                if i != argc - 1 {
                                    self.add_varargs_error(arg_typ.span);
                                }
                                var_args = true;
                                // TODO: varargs should be some type (builtin struct?)
                                None
                            }
                        };

                        let val = Value::Variable {
                            span: arg_name.span,
                            typ: ty.clone(),
                        };
                        let _arg_def_id = self.bindings.bind(arg_name.id, val);

                        if let Some(ty) = ty {
                            arg_types.push(ty);
                        }
                    }
                    let ret_typ = ret
                        .as_ref()
                        .map(|ret| self.resolve_typ(ret))
                        .unwrap_or(typ::Type::VOID);

                    let val = self.bindings.resolve_mut(item.id);
                    match val {
                        Value::Function { typ, .. } => {
                            *typ = Some(typ::FunctionType {
                                var_args,
                                ret: ret_typ.clone(),
                                args: arg_types,
                            });
                        }
                        _ => unreachable!(),
                    }
                }
                ItemKind::Struct { name: _, fields } => {
                    for (_field_name, field_type) in fields {
                        self.resolve_typ(field_type);
                    }
                }
                ItemKind::Union { name: _, variants } => {
                    for (_variant_name, variant_type) in variants {
                        self.resolve_typ(variant_type);
                    }
                }
            }
        }

        // third pass: resolve function bodies
        for item in items {
            match &item.kind {
                ItemKind::Function {
                    name: _,
                    args,
                    ret: _,
                    body,
                } => {
                    self.scope_stack.push();
                    for (arg_name, _) in args {
                        let arg_def_id = self.bindings.def_of(arg_name.id);

                        if let Err(prev) = self.scope_stack.declare(arg_name.kind, arg_def_id) {
                            let span = self.bindings.get(prev).span();
                            self.add_dup_val_error(arg_name, span);
                        }
                    }

                    let val = self.bindings.resolve(item.id);
                    let ret_typ = match val {
                        Value::Function { typ, .. } => typ.as_ref().unwrap().ret.clone(),
                        _ => unreachable!(),
                    };

                    if let Some(body_items) = body {
                        self.resolve_body(body_items, &ret_typ);
                    }
                    self.scope_stack.pop();
                }
                ItemKind::Struct { .. } => {}
                ItemKind::Union { .. } => {}
            }
        }
    }

    fn resolve_body(&mut self, body: &[Instr], ret_typ: &typ::Type) {
        for instr in body {
            self.resolve_instr(instr, ret_typ);
        }
    }

    fn resolve_instr(&mut self, instr: &Instr, ret_typ: &typ::Type) {
        match &instr.kind {
            InstrKind::VarInit { name, typ, expr } => {
                let typ = typ.as_ref().map(|t| self.resolve_typ(t));
                let expr_typ = self.resolve_expr(expr);

                if let Some(typ) = &typ
                    && &expr_typ != typ
                {
                    self.add_type_mismatch_error(typ, &expr_typ, expr.span);
                }

                let val = Value::Variable {
                    span: name.span,
                    typ: Some(typ.unwrap_or(expr_typ)),
                };
                let def_id = self.bindings.bind(name.id, val);

                // allow variable shadowing
                self.scope_stack.define(name.kind, def_id);
            }
            InstrKind::VarAssign { name, expr } => {
                if let Some(def_id) = self.scope_stack.resolve(name.kind) {
                    self.bindings.add_ref(name.id, def_id);
                } else {
                    self.add_undef_val_error(name);
                }
                self.resolve_expr(expr);
            }
            InstrKind::Return(expr) => {
                let expr_typ = expr.as_ref().map(|e| self.resolve_expr(e));
                match (expr_typ, ret_typ) {
                    (Some(expr_typ), typ) if &expr_typ != typ => {
                        self.add_type_mismatch_error(typ, &expr_typ, instr.span);
                    }
                    (None, typ) if typ != &typ::Type::VOID => {
                        self.add_type_mismatch_error(typ, &typ::Type::VOID, instr.span);
                    }
                    _ => {}
                }
            }
            InstrKind::Expr(expr) => {
                self.resolve_expr(expr);
            }
            InstrKind::While { cond, body } => {
                self.resolve_expr(cond);
                self.scope_stack.push();
                self.resolve_body(body, ret_typ);
                self.scope_stack.pop();
            }
            InstrKind::If {
                cond,
                then,
                elifs,
                els,
            } => {
                self.resolve_expr(cond);
                self.scope_stack.push();
                self.resolve_body(then, ret_typ);
                self.scope_stack.pop();
                for (elif_cond, elif_body) in elifs {
                    self.resolve_expr(elif_cond);
                    self.scope_stack.push();
                    self.resolve_body(elif_body, ret_typ);
                    self.scope_stack.pop();
                }
                if let Some(els_body) = els {
                    self.scope_stack.push();
                    self.resolve_body(els_body, ret_typ);
                    self.scope_stack.pop();
                }
            }
            InstrKind::Break => {}
            InstrKind::Continue => {}
            InstrKind::Error => {}
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) -> typ::Type {
        match &expr.kind {
            ExprKind::Number(_) => typ::Type::Primitive(typ::PrimitiveType::I32),
            ExprKind::String(_) => {
                typ::Type::Ptr(Rc::new(typ::Type::Primitive(typ::PrimitiveType::U8)))
            }
            ExprKind::Variable(var) => match self.scope_stack.resolve(var.kind) {
                Some(def_id) => {
                    let val = self.bindings.get(def_id);
                    let typ = val.typ().unwrap();
                    self.bindings.add_ref(var.id, def_id);
                    typ
                }
                None => {
                    self.add_undef_val_error(var);
                    typ::Type::ERROR
                }
            },
            ExprKind::Call { name, args } => match self.scope_stack.resolve(name.kind) {
                Some(def_id) => {
                    let val = self.bindings.get(def_id);
                    let typ = val.typ().unwrap();
                    self.bindings.add_ref(name.id, def_id);

                    match &typ {
                        typ::Type::Function(typ) => {
                            if !typ.var_args && args.len() > typ.args.len() {
                                self.add_too_many_args_error(expr.span);
                            } else if args.len() < typ.args.len() {
                                self.add_too_few_args_error(expr.span);
                            }
                            let mut i = 0;
                            while i < typ.args.len() && i < args.len() {
                                let passed_arg = &args[i];
                                let func_arg_typ = &typ.args[i];
                                i += 1;
                                let arg_typ = self.resolve_expr(passed_arg);
                                if &arg_typ != func_arg_typ {
                                    self.add_type_mismatch_error(
                                        func_arg_typ,
                                        &arg_typ,
                                        passed_arg.span,
                                    );
                                }
                            }
                            // handle varargs or do type checking on extra args anyway
                            while i < args.len() {
                                let _ = self.resolve_expr(&args[i]);
                                i += 1;
                            }

                            typ.ret.clone()
                        }
                        _ => {
                            self.add_not_a_function_error(name);
                            typ::Type::ERROR
                        }
                    }
                }
                None => {
                    self.add_undef_val_error(name);
                    typ::Type::ERROR
                }
            },
            ExprKind::BinOp { op, l, r } => {
                let l = self.resolve_expr(l);
                let r = self.resolve_expr(r);
                match l.binop(*op, &r) {
                    Some(t) => t,
                    None => {
                        self.add_binop_type_error(op, &l, &r, expr.span);
                        typ::Type::ERROR
                    }
                }
            }
            ExprKind::UniOp { op, expr } => {
                let typ = self.resolve_expr(expr);
                match typ.uniop(*op) {
                    Some(t) => t,
                    None => {
                        self.add_uniop_type_error(op, &typ, expr.span);
                        typ::Type::ERROR
                    }
                }
            }
            ExprKind::StructInit { .. } => {
                todo!()
            }
            ExprKind::FieldAccess { .. } => {
                todo!()
            }
        }
    }
}

impl<'a> Resolver<'a> {
    fn add_dup_val_error(&mut self, name: &Ident, err: Span) {
        self.errors.push(
            Diagnostic::error()
                .with_message(format!(
                    "the name '{}' is defined multiple times",
                    self.interner.get(name.kind)
                ))
                .with_labels(vec![
                    Label::primary(self.file, name.span)
                        .with_message(format!("'{}' redefined here", self.interner.get(name.kind))),
                    Label::secondary(self.file, err).with_message(format!(
                        "previous definition of value '{}' here",
                        self.interner.get(name.kind)
                    )),
                ]),
        )
    }
    fn add_undef_typ_error(&mut self, name: &Ident) {
        self.errors.push(
            Diagnostic::error()
                .with_message(format!(
                    "cannot find type '{}' in this scope",
                    self.interner.get(name.kind)
                ))
                .with_labels(vec![
                    Label::primary(self.file, name.span).with_message("not found in this scope"),
                ]),
        );
    }
    fn add_undef_val_error(&mut self, name: &Ident) {
        self.errors.push(
            Diagnostic::error()
                .with_message(format!(
                    "cannot find value '{}' in this scope",
                    self.interner.get(name.kind)
                ))
                .with_labels(vec![
                    Label::primary(self.file, name.span).with_message("not found in this scope"),
                ]),
        );
    }
    fn add_type_mismatch_error(&mut self, expected: &typ::Type, found: &typ::Type, span: Span) {
        if expected.is_error() || found.is_error() {
            return;
        }
        self.errors.push(
            Diagnostic::error()
                .with_message("type mismatch")
                .with_labels(vec![Label::primary(self.file, span).with_message(format!(
                    "expected type '{}', found type '{}'",
                    typ::DisplayType::new(self.interner, expected),
                    typ::DisplayType::new(self.interner, found)
                ))]),
        );
    }
    fn add_binop_type_error(
        &mut self,
        op: &BinOp,
        left: &typ::Type,
        right: &typ::Type,
        span: Span,
    ) {
        if left.is_error() || right.is_error() {
            return;
        }
        self.errors.push(
            Diagnostic::error()
                .with_message("invalid types for binary operation")
                .with_labels(vec![Label::primary(self.file, span).with_message(format!(
                    "cannot apply operator '{}' to types '{}' and '{}'",
                    op,
                    typ::DisplayType::new(self.interner, left),
                    typ::DisplayType::new(self.interner, right)
                ))]),
        );
    }
    fn add_uniop_type_error(&mut self, op: &UniOp, typ: &typ::Type, span: Span) {
        if typ.is_error() {
            return;
        }
        self.errors.push(
            Diagnostic::error()
                .with_message("invalid type for unary operation")
                .with_labels(vec![Label::primary(self.file, span).with_message(format!(
                    "cannot apply operator '{}' to type '{}'",
                    op,
                    typ::DisplayType::new(self.interner, typ)
                ))]),
        );
    }
    fn add_varargs_error(&mut self, span: Span) {
        self.errors.push(
            Diagnostic::error()
                .with_message("varargs must be the last argument")
                .with_labels(vec![
                    Label::primary(self.file, span).with_message("varargs defined here"),
                ]),
        );
    }
    fn add_not_a_function_error(&mut self, name: &Ident) {
        self.errors.push(
            Diagnostic::error()
                .with_message("called value is not a function")
                .with_labels(vec![Label::primary(self.file, name.span).with_message(
                    format!("'{}' is not a function", self.interner.get(name.kind)),
                )]),
        );
    }
    fn add_too_many_args_error(&mut self, span: Span) {
        self.errors.push(
            Diagnostic::error()
                .with_message("too many arguments provided to function")
                .with_labels(vec![
                    Label::primary(self.file, span).with_message("too many arguments here"),
                ]),
        );
    }
    fn add_too_few_args_error(&mut self, span: Span) {
        self.errors.push(
            Diagnostic::error()
                .with_message("too few arguments provided to function")
                .with_labels(vec![
                    Label::primary(self.file, span).with_message("too few arguments here"),
                ]),
        );
    }
}
