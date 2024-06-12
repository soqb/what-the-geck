use core::fmt;

use crate::{typeck::TyHint, *};

pub trait Visitor {
    #[allow(unused_variables)]
    fn visit_script(&mut self, script: &Script) {}
    #[allow(unused_variables)]
    fn visit_event_impl(&mut self, item: Spanned<&EventImpl>) {}
    #[allow(unused_variables)]
    fn visit_ufd(&mut self, item: Spanned<&UserFunctionDefinition>) {}
    #[allow(unused_variables)]
    fn visit_body(&mut self, body: &FunctionBody) {}
    #[allow(unused_variables)]
    fn visit_block(&mut self, block: Spanned<&Block>) {}
    #[allow(unused_variables)]
    fn visit_statement(&mut self, statement: Spanned<&Statement>) {}
    #[allow(unused_variables)]
    fn visit_expression(&mut self, expression: Spanned<&Expression>) {}

    fn walk_script(&mut self, script: &Script) {
        self.visit_script(script);
        for event in &script.event_impls {
            self.walk_event_impl(event.as_ref());
        }
        for func in &script.ufds {
            self.walk_ufd(func.as_ref());
        }
    }
    fn walk_event_impl(&mut self, event_impl: Spanned<&EventImpl>) {
        self.visit_event_impl(event_impl);
        self.walk_body(&event_impl.inner().body);
    }
    fn walk_ufd(&mut self, ufd: Spanned<&UserFunctionDefinition>) {
        self.visit_ufd(ufd);
        self.walk_body(&ufd.inner().body);
    }
    fn walk_body(&mut self, body: &FunctionBody) {
        self.visit_body(body);
        self.walk_block(body.entrypoint.as_ref());
    }
    fn walk_block(&mut self, block: Spanned<&Block>) {
        self.visit_block(block);
        for stmt in &block.inner().statements {
            self.walk_statement(stmt.as_ref())
        }
    }
    fn walk_statement(&mut self, statement: Spanned<&Statement>) {
        self.visit_statement(statement);

        let span = statement.to_span();
        match statement.inner() {
            Statement::Express(expr) => self.walk_expression(span.of(expr)),
            Statement::SetVariable { value, .. } => self.walk_expression(value.as_ref()),
            Statement::Block(block) => self.walk_block(statement.to_span().of(block)),
            Statement::Branch { .. } => (),
        }
    }
    fn walk_expression(&mut self, expression: Spanned<&Expression>) {
        self.visit_expression(expression);

        match expression.inner() {
            Expression::Convert { value, .. } => self.walk_expression((**value).as_ref()),
            Expression::Operation(operation) => {
                for operand in &operation.operands {
                    self.walk_expression(operand.as_ref());
                }
            }
            Expression::PrimitiveValue(_) | Expression::FormRef(_) | Expression::GetVariable(_) => {
                ()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct NamePrinter {
    pub ident: String,
    pub component_name: String,
}

impl fmt::Display for NamePrinter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            component_name,
            ident,
        } = self;
        write!(f, "{component_name}:{ident}")
    }
}

#[derive(Debug, Clone)]
pub enum TyPrinter {
    Unit,
    String,
    Integer,
    Bool,
    Float,
    AnyRef,
    ObjectRef(FormTy),
    FormRef(FormTy),
    FoundAdt(NamePrinter),
    UnknownAdt(TypeIdx),
    Unresolvable,
}

impl TyPrinter {
    pub fn as_display_raw(&self) -> impl fmt::Display + '_ {
        struct Printer<'a> {
            ty: &'a TyPrinter,
        }

        impl<'a> fmt::Display for Printer<'a> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self.ty {
                    TyPrinter::Unit => write!(f, "unit"),
                    TyPrinter::String => write!(f, "string"),
                    TyPrinter::Integer => write!(f, "integer"),
                    TyPrinter::Bool => write!(f, "bool"),
                    TyPrinter::Float => write!(f, "float"),
                    TyPrinter::AnyRef => write!(f, "ref"),
                    TyPrinter::ObjectRef(form) => write!(f, "ref_object[{form}]"),
                    TyPrinter::FormRef(form) => write!(f, "ref_form[{form}]"),
                    TyPrinter::FoundAdt(name) => write!(f, "adt[{}]", name),
                    TyPrinter::UnknownAdt(idx) => write!(f, "adt[#{idx}]"),
                    TyPrinter::Unresolvable => write!(f, "unresolvable"),
                }
            }
        }

        Printer { ty: self }
    }
}

impl fmt::Display for TyPrinter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "`{}`", self.as_display_raw())
    }
}

#[derive(Debug, Clone)]
pub enum TyHintPrinter {
    Never,
    Exactly(TyPrinter),
    Union(Vec<TyPrinter>),
}

impl fmt::Display for TyHintPrinter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyHintPrinter::Never => write!(f, "`never`"),
            TyHintPrinter::Exactly(ty) => write!(f, "{ty}"),
            TyHintPrinter::Union(tys) => match &tys[..] {
                [] => fmt::Display::fmt(&TyHintPrinter::Never, f),
                [ty] => write!(f, "{ty}"),
                [leading @ .., last_ty] => {
                    for ty in leading {
                        write!(f, "{ty}, ")?;
                    }
                    write!(f, "or {last_ty}")
                }
            },
        }
    }
}

impl<C: Component> Resources<C> {
    pub fn print_name(&self, name: &Name, idx: impl Into<ComponentIdx>) -> NamePrinter {
        NamePrinter {
            ident: name.ident.clone(),
            component_name: self.component(idx.into()).unwrap().identifier().to_owned(),
        }
    }

    pub fn print_ty(&self, ty: impl Into<Tried<Ty>>) -> TyPrinter {
        match ty.into() {
            Resolved(Ty::Unit) => TyPrinter::Unit,
            Resolved(Ty::String) => TyPrinter::String,
            Resolved(Ty::Integer) => TyPrinter::Integer,
            Resolved(Ty::Bool) => TyPrinter::Bool,
            Resolved(Ty::Float) => TyPrinter::Float,
            Resolved(Ty::Ref(RefTy::Unknown)) => TyPrinter::AnyRef,
            Resolved(Ty::Ref(RefTy::Object(form))) => TyPrinter::ObjectRef(form),
            Resolved(Ty::Ref(RefTy::Form(form))) => TyPrinter::FormRef(form),
            Resolved(Ty::Adt(idx)) => match self.get_type(idx) {
                Some(ty_def) => TyPrinter::FoundAdt(self.print_name(&ty_def.name, idx)),
                None => TyPrinter::UnknownAdt(idx),
            },
            Unresolvable => TyPrinter::Unresolvable,
        }
    }

    pub fn print_ty_hint(&self, hint: TyHint) -> TyHintPrinter {
        fn fold_names<C: Component>(
            this: &Resources<C>,
            names: &mut Vec<TyPrinter>,
            union: &[TyHint],
        ) {
            for hint in union {
                match hint {
                    TyHint::Never => (),
                    &TyHint::Exactly(ty) => names.push(this.print_ty(ty)),
                    TyHint::Union(union) => fold_names(this, names, union),
                }
            }
        }

        match hint {
            TyHint::Never => TyHintPrinter::Never,
            TyHint::Exactly(ty) => TyHintPrinter::Exactly(self.print_ty(ty)),
            TyHint::Union(union) => {
                let mut names = Vec::with_capacity(union.len());
                fold_names(self, &mut names, union);
                TyHintPrinter::Union(names)
            }
        }
    }
}
