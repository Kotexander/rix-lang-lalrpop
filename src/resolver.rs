use crate::{
    ast::{Ident, Item, ItemKind, Span, Typ, TypKind},
    strings,
};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::collections::HashMap;

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
        scope_stack.scopes[0]
            .typ_bindings
            .insert(interner.intern("i32"), Type::Prim(PrimitiveType::I32));
        scope_stack.scopes[0]
            .typ_bindings
            .insert(interner.intern("u8"), Type::Prim(PrimitiveType::U8));
        scope_stack.scopes[0]
            .typ_bindings
            .insert(interner.intern("bool"), Type::Prim(PrimitiveType::Bool));
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
            Type::Prim(prim) => {
                self.errors.push(
                    Diagnostic::error()
                        .with_message(format!("can't redefine primitive type '{}'", prim.as_str()))
                        .with_labels(vec![Label::primary(self.file, name.span).with_message(
                            format!("'{}' redefined here", self.interner.get(name.kind)),
                        )]),
                );
                return;
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
                    "cannot find type '{}' im this scope",
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
                    if let Err(err) = self.scope_stack.declare_function(*name) {
                        self.add_dup_val_error(name, err.span());
                    }
                }
                ItemKind::Struct { name, fields: _ } => {
                    if let Err(err) = self.scope_stack.declare_type(*name) {
                        self.add_dup_typ_error(name, &err);
                    }
                }
                ItemKind::Union { name, variants: _ } => {
                    if let Err(err) = self.scope_stack.declare_type(*name) {
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
                    body: _,
                } => {
                    for (_arg_name, arg_type) in args {
                        self.resolve_typ(arg_type);
                    }
                    if let Some(ret_type) = ret {
                        self.resolve_typ(ret_type);
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
    }

    fn resolve_typ(&mut self, typ: &Typ) {
        match &typ.kind {
            TypKind::Ident(id) => {
                if self.scope_stack.get_typ(id.kind).is_none() {
                    self.add_undef_typ_error(&id);
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

#[derive(Default)]
struct Scope {
    val_bindings: HashMap<strings::Id, Resource>,
    typ_bindings: HashMap<strings::Id, Type>,
}
struct ScopeStack {
    scopes: Vec<Scope>,
}
impl ScopeStack {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
        }
    }
    pub fn get_val(&self, ident: strings::Id) -> Option<Resource> {
        for scope in self.scopes.iter().rev() {
            if let Some(res) = scope.val_bindings.get(&ident) {
                return Some(*res);
            }
        }
        None
    }
    pub fn get_typ(&self, ident: strings::Id) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(typ) = scope.typ_bindings.get(&ident) {
                return Some(*typ);
            }
        }
        None
    }
    pub fn declare_function(&mut self, name: Ident) -> Result<(), Resource> {
        if let Some(prev) = self.get_val(name.kind) {
            return Err(prev);
        }
        let scope = self.scopes.last_mut().unwrap();
        scope
            .val_bindings
            .insert(name.kind, Resource::Function { span: name.span });
        Ok(())
    }
    pub fn declare_type(&mut self, name: Ident) -> Result<(), Type> {
        if let Some(prev) = self.get_typ(name.kind) {
            return Err(prev);
        }
        let scope = self.scopes.last_mut().unwrap();
        scope
            .typ_bindings
            .insert(name.kind, Type::Custom(name.kind, name.span));
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
enum Resource {
    Function { span: Span },
}
impl Resource {
    fn span(&self) -> Span {
        match self {
            Resource::Function { span } => *span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum PrimitiveType {
    I32,
    U8,
    Bool,
}
impl PrimitiveType {
    fn as_str(&self) -> &str {
        match self {
            PrimitiveType::I32 => "i32",
            PrimitiveType::U8 => "u8",
            PrimitiveType::Bool => "bool",
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Type {
    Prim(PrimitiveType),
    Custom(strings::Id, Span),
}
