use core::fmt;
use std::ops::Deref;

use fir::{Span, ToSpan};
use rust_sitter::Spanned;

use crate::{lower::Respan, *};

impl Ident {
    pub fn new(text: String, span: impl ToSpan) -> Self {
        let span = span.to_span();
        Self {
            text: Spanned {
                value: text,
                span: (span.start, span.end),
            },
        }
    }
}

impl ToSpan for Reference {
    fn to_span(&self) -> fir::Span {
        self.path
            .iter()
            .map(|segment| segment.field.text.span.to_span())
            .fold(self.head.text.span(), |a, b| a.expand_to_include(b))
    }
}

impl Deref for Ident {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.text.as_str()
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.text.as_str(), f)
    }
}

impl BinaryOp {
    /// The priority of the operator.
    ///
    /// A higher precedence operator will be parsed more greedily
    /// than one with lower precedence.
    pub fn precedence(self) -> usize {
        match self {
            BinaryOp::Or => 1,
            BinaryOp::And => 2,
            BinaryOp::Eq | BinaryOp::Neq => 4,
            BinaryOp::Greater
            | BinaryOp::Lesser
            | BinaryOp::GreaterOrEqual
            | BinaryOp::LesserOrEqual => 5,
            BinaryOp::BitOr => 6,
            BinaryOp::BitAnd => 7,
            BinaryOp::LeftShift => 8,
            BinaryOp::RightShift => 8,
            BinaryOp::Add => 9,
            BinaryOp::Sub => 9,
            BinaryOp::Mul => 10,
            BinaryOp::Div => 10,
            BinaryOp::Mod => 10,
            BinaryOp::Pow => 11,
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOp::Or => "||",
                BinaryOp::And => "&&",
                BinaryOp::Eq => "==",
                BinaryOp::Neq => "-",
                BinaryOp::Greater => ">",
                BinaryOp::Lesser => "<",
                BinaryOp::GreaterOrEqual => ">=",
                BinaryOp::LesserOrEqual => "<=",
                BinaryOp::BitOr => "|",
                BinaryOp::BitAnd => "&",
                BinaryOp::LeftShift => "<",
                BinaryOp::RightShift => ">",
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::Mul => "*",
                BinaryOp::Div => "/",
                BinaryOp::Mod => "%",
                BinaryOp::Pow => "^",
            }
        )
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOp::Neg => "-",
                UnaryOp::ToString => "$",
                UnaryOp::ToNumber => "#",
                UnaryOp::Deref => "*",
                UnaryOp::Box => "&",
                UnaryOp::Not => "!",
            }
        )
    }
}

impl fmt::Display for Reference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.head)?;
        for segment in &self.path {
            write!(f, ".{}", segment.field)?;
        }

        Ok(())
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::DecInt(IntRepr::Good(value)) => write!(f, "{value}"),
            Literal::HexInt(IntRepr::Good(value)) => write!(f, "0x{value:x}"),
            Literal::Float(FloatRepr::Good(float)) => write!(f, "{float}"),
            Literal::String(string) => write!(f, "\"{string}\""),
            _ => write!(f, "malformed"),
        }
    }
}

impl fmt::Debug for ForgivingNewline {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ForgivingNewline")
    }
}

pub struct IfChainIter {
    triplet: Option<(Span, Spanned<Expression>, Vec<Statement>, NextIf)>,
    else_: Option<Else>,
}

impl IntoIterator for IfChain {
    type Item = (Span, Spanned<Expression>, Vec<Statement>);

    type IntoIter = IfChainIter;

    fn into_iter(self) -> Self::IntoIter {
        IfChainIter {
            triplet: Some((
                self.kw_if.span(),
                self.condition,
                self.statements,
                *self.next,
            )),
            else_: None,
        }
    }
}

impl IfChainIter {
    pub fn has_else_branch(&self) -> bool {
        self.else_.is_some()
    }

    pub fn else_branch(self) -> Option<Else> {
        self.else_
    }
}

impl Iterator for IfChainIter {
    type Item = (Span, Spanned<Expression>, Vec<Statement>);

    fn next(&mut self) -> Option<Self::Item> {
        let (span, expr, stmts, next) = self.triplet.take()?;
        self.triplet = match next {
            NextIf::ElseIf(else_if) => Some((
                else_if.kw_elseif.span(),
                else_if.condition,
                else_if.statements,
                *else_if.next,
            )),
            NextIf::Else(else_) => {
                self.else_ = Some(else_);
                None
            }
            NextIf::End(_) => None,
        };
        Some((span, expr, stmts))
    }
}
