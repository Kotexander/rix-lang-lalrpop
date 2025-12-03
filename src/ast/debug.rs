use crate::strings;

pub struct DisplayItem<'a> {
    pub interner: &'a strings::Interner,
    pub item: &'a super::ItemKind,
}
impl<'a> DisplayItem<'a> {
    pub fn new(interner: &'a strings::Interner, item: &'a super::ItemKind) -> Self {
        DisplayItem { interner, item }
    }
}
impl<'a> std::fmt::Display for DisplayItem<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.item {
            super::ItemKind::Function {
                name,
                args,
                ret,
                body,
            } => {
                write!(f, "fn {}(", self.interner.get(name.kind))?;
                let mut first = true;
                for (arg_name, arg_typ) in args {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(
                        f,
                        "{}: {}",
                        self.interner.get(arg_name.kind),
                        DisplayTyp::new(self.interner, &arg_typ.kind)
                    )?;
                }
                write!(f, ")")?;
                if let Some(ret_typ) = ret {
                    write!(f, " : {}", DisplayTyp::new(self.interner, &ret_typ.kind))?;
                }
                if let Some(body) = body {
                    writeln!(f, " {{")?;
                    for instr in body {
                        write!(
                            f,
                            "{}",
                            DisplayInstr::new(self.interner, &instr.kind).indent()
                        )?;
                    }
                    writeln!(f, "}}")
                } else {
                    writeln!(f)
                }
            }
            super::ItemKind::Struct { name, fields } => {
                writeln!(f, "struct {} {{", self.interner.get(name.kind))?;
                for (field_name, field_typ) in fields {
                    writeln!(
                        f,
                        "    {}: {},",
                        self.interner.get(field_name.kind),
                        DisplayTyp::new(self.interner, &field_typ.kind)
                    )?;
                }
                writeln!(f, "}}")
            }
            super::ItemKind::Union { name, variants } => {
                writeln!(f, "union {} {{", self.interner.get(name.kind))?;
                for (variant_name, variant_typ) in variants {
                    writeln!(
                        f,
                        "    {}: {},",
                        self.interner.get(variant_name.kind),
                        DisplayTyp::new(self.interner, &variant_typ.kind)
                    )?;
                }
                writeln!(f, "}}")
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct DisplayInstr<'a> {
    pub interner: &'a strings::Interner,
    pub instr: &'a super::InstrKind,
    pub indent: usize,
}
impl<'a> DisplayInstr<'a> {
    pub fn new(interner: &'a strings::Interner, instr: &'a super::InstrKind) -> Self {
        DisplayInstr {
            interner,
            instr,
            indent: 0,
        }
    }
    pub fn with(self, instr: &'a super::InstrKind) -> Self {
        Self {
            interner: self.interner,
            instr,
            indent: self.indent,
        }
    }
    pub fn indent(self) -> Self {
        Self {
            interner: self.interner,
            instr: self.instr,
            indent: self.indent + 1,
        }
    }
}
impl<'a> std::fmt::Display for DisplayInstr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.indent {
            write!(f, "    ")?;
        }
        match self.instr {
            super::InstrKind::VarInit { name, typ, expr } => {
                if let Some(typ) = typ {
                    writeln!(
                        f,
                        "var {}: {} = {}",
                        self.interner.get(name.kind),
                        DisplayTyp::new(self.interner, &typ.kind),
                        DisplayExpr::new(self.interner, &expr.kind)
                    )
                } else {
                    writeln!(
                        f,
                        "var {} = {}",
                        self.interner.get(name.kind),
                        DisplayExpr::new(self.interner, &expr.kind)
                    )
                }
            }
            super::InstrKind::VarAssign { name, expr } => {
                writeln!(
                    f,
                    "{} = {}",
                    self.interner.get(name.kind),
                    DisplayExpr::new(self.interner, &expr.kind)
                )
            }
            super::InstrKind::Return(expr) => {
                if let Some(expr) = expr {
                    writeln!(f, "return {}", DisplayExpr::new(self.interner, &expr.kind))
                } else {
                    writeln!(f, "return")
                }
            }
            super::InstrKind::Expr(expr) => {
                writeln!(f, "{}", DisplayExpr::new(self.interner, &expr.kind))
            }
            super::InstrKind::For {
                var,
                start,
                end,
                body,
            } => {
                writeln!(
                    f,
                    "for {} in ({}..{}) {{",
                    self.interner.get(var.kind),
                    DisplayExpr::new(self.interner, &start.kind),
                    DisplayExpr::new(self.interner, &end.kind)
                )?;
                for instr in body {
                    write!(f, "{}", self.indent().with(&instr.kind))?;
                }
                for _ in 0..self.indent {
                    write!(f, "    ")?;
                }
                writeln!(f, "}}")
            }
            super::InstrKind::If {
                cond,
                then,
                elifs,
                els,
            } => {
                writeln!(f, "if ({}) {{", DisplayExpr::new(self.interner, &cond.kind))?;
                for instr in then {
                    write!(f, "{}", self.indent().with(&instr.kind))?;
                }
                for (elif_cond, elif_body) in elifs {
                    for _ in 0..self.indent {
                        write!(f, "    ")?;
                    }
                    writeln!(
                        f,
                        "}} else if {} {{",
                        DisplayExpr::new(self.interner, &elif_cond.kind)
                    )?;
                    for instr in elif_body {
                        write!(f, "{}", self.indent().with(&instr.kind))?;
                    }
                }
                if let Some(els_body) = els {
                    for _ in 0..self.indent {
                        write!(f, "    ")?;
                    }
                    writeln!(f, "}} else {{")?;
                    for instr in els_body {
                        write!(f, "{}", self.indent().with(&instr.kind))?;
                    }
                }
                for _ in 0..self.indent {
                    write!(f, "    ")?;
                }
                writeln!(f, "}}")
            }
            super::InstrKind::Error => write!(f, "<error>"),
        }
    }
}

#[derive(Clone, Copy)]
pub struct DisplayExpr<'a> {
    pub interner: &'a strings::Interner,
    pub expr: &'a super::ExprKind,
}
impl<'a> DisplayExpr<'a> {
    pub fn new(interner: &'a strings::Interner, expr: &'a super::ExprKind) -> Self {
        DisplayExpr { interner, expr }
    }
    pub fn with(self, expr: &'a super::ExprKind) -> Self {
        Self {
            interner: self.interner,
            expr,
        }
    }
}
impl<'a> std::fmt::Display for DisplayExpr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.expr {
            super::ExprKind::Number(n) => write!(f, "{}", n),
            super::ExprKind::String(id) => write!(f, "{:?}", self.interner.get(*id)),
            super::ExprKind::Variable(ident) => write!(f, "{}", self.interner.get(ident.kind)),
            super::ExprKind::Call { name, args } => {
                write!(f, "{}", self.interner.get(name.kind))?;
                write!(f, "(")?;
                let mut first = true;
                for arg in args {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{}", self.with(&arg.kind))?;
                }
                write!(f, ")")
            }
            super::ExprKind::BinOp { op, l, r } => {
                write!(f, "({} {} {})", self.with(&l.kind), op, self.with(&r.kind))
            }
            super::ExprKind::UniOp { op, expr } => {
                write!(f, "({}{})", op, self.with(&expr.kind))
            }
            super::ExprKind::StructInit { name, fields } => {
                write!(f, "{} {{ ", self.interner.get(name.kind))?;
                let mut first = true;
                for (field_name, field_expr) in fields {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(
                        f,
                        ".{} = {}",
                        self.interner.get(field_name.kind),
                        self.with(&field_expr.kind)
                    )?;
                }
                write!(f, " }}")
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct DisplayTyp<'a> {
    pub interner: &'a strings::Interner,
    pub typ: &'a super::TypKind,
}
impl<'a> DisplayTyp<'a> {
    pub fn new(interner: &'a strings::Interner, typ: &'a super::TypKind) -> Self {
        DisplayTyp { interner, typ }
    }
    pub fn with(self, typ: &'a super::TypKind) -> Self {
        Self {
            interner: self.interner,
            typ,
        }
    }
}
impl<'a> std::fmt::Display for DisplayTyp<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.typ {
            super::TypKind::Ident(id) => write!(f, "{}", self.interner.get(id.kind)),
            super::TypKind::Ref(typ) => write!(f, "&{}", self.with(&typ.kind)),
            super::TypKind::Slice(typ) => write!(f, "[{}]", self.with(&typ.kind)),
            super::TypKind::Ptr(typ) => write!(f, "*{}", self.with(&typ.kind)),
            super::TypKind::VarArgs => write!(f, "..."),
        }
    }
}
