mod resource;
mod scope;

use crate::{
    ast::{Expr, ExprKind, Ident, Instr, InstrKind, Item, ItemKind, Span, Typ, TypKind},
    strings,
};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use resource::{PrimitiveType, Type, Value};
use scope::ScopeStack;

pub struct Resolver<'a> {
    errors: &'a mut Vec<Diagnostic<usize>>,
    file: usize,
    scope_stack: ScopeStack,
    interner: &'a mut strings::Interner,
}
impl<'a> Resolver<'a> {
    pub fn new(
        interner: &'a mut strings::Interner,
        errors: &'a mut Vec<Diagnostic<usize>>,
        file: usize,
    ) -> Self {
        let mut scope_stack = ScopeStack::new();
        scope_stack.define_typ(interner.intern("i32"), Type::Prim(PrimitiveType::I32));
        scope_stack.define_typ(interner.intern("u8"), Type::Prim(PrimitiveType::U8));
        scope_stack.define_typ(interner.intern("bool"), Type::Prim(PrimitiveType::Bool));
        Self {
            errors,
            file,
            scope_stack,
            interner,
        }
    }

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
        );
    }
    fn add_dup_typ_error(&mut self, name: &Ident, err: &Type) {
        match err {
            Type::Prim(_) => {
                self.errors.push(
                    Diagnostic::error()
                        .with_message(format!(
                            "can't redefine primitive type '{}'",
                            self.interner.get(name.kind)
                        ))
                        .with_labels(vec![Label::primary(self.file, name.span).with_message(
                            format!("'{}' redefined here", self.interner.get(name.kind)),
                        )]),
                );
            }
            Type::Custom(_id, span) => {
                self.errors.push(
                    Diagnostic::error()
                        .with_message(format!(
                            "the name '{}' is defined multiple times",
                            self.interner.get(name.kind)
                        ))
                        .with_labels(vec![
                            Label::primary(self.file, name.span).with_message(format!(
                                "'{}' redefined here",
                                self.interner.get(name.kind)
                            )),
                            Label::secondary(self.file, *span).with_message(format!(
                                "previous definition of type '{}' here",
                                self.interner.get(name.kind)
                            )),
                        ]),
                );
            }
        }
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
                    if let Err(err) = self
                        .scope_stack
                        .declare_val(name.kind, Value::Function { span: name.span })
                    {
                        self.add_dup_val_error(name, err.span());
                    }
                }
                ItemKind::Struct { name, fields: _ } => {
                    if let Err(err) = self
                        .scope_stack
                        .declare_typ(name.kind, Type::Custom(name.kind, name.span))
                    {
                        self.add_dup_typ_error(name, &err);
                    }
                }
                ItemKind::Union { name, variants: _ } => {
                    if let Err(err) = self
                        .scope_stack
                        .declare_typ(name.kind, Type::Custom(name.kind, name.span))
                    {
                        self.add_dup_typ_error(name, &err);
                    }
                }
            }
        }

        // second pass: resolve all types used in items
        for item in items {
            match &item.kind {
                ItemKind::Function {
                    name: _,
                    args,
                    ret,
                    body,
                } => {
                    for (_arg_name, arg_type) in args {
                        self.resolve_typ(arg_type);
                    }
                    if let Some(ret_type) = ret {
                        self.resolve_typ(ret_type);
                    }
                    self.scope_stack.push();
                    for (arg_name, arg_type) in args {
                        self.resolve_typ(arg_type);
                        if let Err(prev) = self.scope_stack.declare_val(
                            arg_name.kind,
                            Value::Variable {
                                span: arg_name.span,
                            },
                        ) {
                            self.add_dup_val_error(arg_name, prev.span());
                        }
                    }

                    if let Some(ret_type) = ret {
                        self.resolve_typ(ret_type);
                    }
                    if let Some(body_items) = body {
                        self.resolve_body(body_items);
                    }
                    self.scope_stack.pop();
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
    }

    fn resolve_body(&mut self, body: &[Instr]) {
        for instr in body {
            self.resolve_instr(instr);
        }
    }

    fn resolve_instr(&mut self, instr: &Instr) {
        match &instr.kind {
            InstrKind::VarInit { name, typ, expr } => {
                if let Some(typ) = typ {
                    self.resolve_typ(typ);
                }

                self.resolve_expr(expr);

                // allow variable shadowing
                self.scope_stack
                    .define_val(name.kind, Value::Variable { span: name.span });
            }
            InstrKind::VarAssign { name, expr } => {
                if self.scope_stack.get_val(name.kind).is_none() {
                    self.add_undef_val_error(name);
                }
                self.resolve_expr(expr);
            }
            InstrKind::Return(expr) => {
                if let Some(expr) = expr {
                    self.resolve_expr(expr);
                }
            }
            InstrKind::Expr(expr) => {
                self.resolve_expr(expr);
            }
            InstrKind::For {
                var,
                start,
                end,
                body,
            } => {
                self.resolve_expr(start);
                self.resolve_expr(end);
                self.scope_stack.push();
                self.scope_stack
                    .define_val(var.kind, Value::Variable { span: var.span });
                self.resolve_body(body);
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
                self.resolve_body(then);
                self.scope_stack.pop();
                for (elif_cond, elif_body) in elifs {
                    self.resolve_expr(elif_cond);
                    self.scope_stack.push();
                    self.resolve_body(elif_body);
                    self.scope_stack.pop();
                }
                if let Some(els_body) = els {
                    self.scope_stack.push();
                    self.resolve_body(els_body);
                    self.scope_stack.pop();
                }
            }
            InstrKind::Error => {}
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Number(_) => {}
            ExprKind::String(_) => {}
            ExprKind::Variable(var) => {
                if self.scope_stack.get_val(var.kind).is_none() {
                    self.add_undef_val_error(var);
                }
            }
            ExprKind::Call { name, args } => {
                if self.scope_stack.get_val(name.kind).is_none() {
                    self.add_undef_val_error(name);
                }
                for arg in args {
                    self.resolve_expr(arg);
                }
            }
            ExprKind::BinOp { op: _, l, r } => {
                self.resolve_expr(l);
                self.resolve_expr(r);
            }
            ExprKind::UniOp { op: _, expr } => {
                self.resolve_expr(expr);
            }
            ExprKind::StructInit { name, fields } => {
                if self.scope_stack.get_typ(name.kind).is_none() {
                    self.add_undef_typ_error(name);
                }
                for (_field_name, field_expr) in fields {
                    self.resolve_expr(field_expr);
                }
            }
        }
    }

    fn resolve_typ(&mut self, typ: &Typ) {
        match &typ.kind {
            TypKind::Ident(id) => {
                if self.scope_stack.get_typ(id.kind).is_none() {
                    self.add_undef_typ_error(id);
                }
            }
            TypKind::Ref(node) => {
                self.resolve_typ(node);
            }
            TypKind::Slice(node) => {
                self.resolve_typ(node);
            }
            TypKind::Ptr(node) => {
                self.resolve_typ(node);
            }
            TypKind::VarArgs => {}
        }
    }
}
