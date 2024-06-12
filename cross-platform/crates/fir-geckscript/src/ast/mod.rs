use core::fmt;

use rowan::NodeOrToken;
use thiserror::Error;

// mod build;
// pub use build::*;

use crate::{
    // parse::Token,
    reporting::{Diagnostic, DiagnosticKind},
    span::{Span, Spanned},
};

#[derive(Error, miette::Diagnostic, Debug)]
pub enum AstError {
    // #[error("unexpected syntax {found}")]
    // Mismatched {
    //     #[label("expected {expected}")]
    //     found: Spanned<Token>,
    //     expected: Token,
    // },
    // #[error("unexpected syntax")]
    // Unexpected {
    //     #[label("did not expect this syntax {}; {note}", found)]
    //     found: Spanned<Token>,
    //     note: String,
    // },
    // #[error("unexpected syntax")]
    // UnexpectedKind {
    //     #[label("did not expect the {kind} {token}; {note}", kind = match kind {
    //         NodeOrToken::Node(_) => "node",
    //         NodeOrToken::Token(_) => "token",
    //     })]
    //     token: Spanned<Token>,
    //     kind: NodeOrToken<(), ()>,
    //     note: String,
    // },
    // #[error("too few children")]
    // TooFewChildren {
    //     #[label(
    //         "expected this syntax node to have at least {expected} children, but it had {found}"
    //     )]
    //     span: Span,
    //     expected: usize,
    //     found: usize,
    // },
    // #[error("too many children")]
    // TooManyChildren {
    //     #[label(
    //         "expected this syntax node to have at most {expected} children, but it had {found}"
    //     )]
    //     span: Span,
    //     expected: usize,
    //     found: usize,
    // },
    // #[error("failed to parse float")]
    // FloatParse(#[from] std::num::ParseFloatError),
    // #[error("failed to parse int")]
    // IntParse(#[from] std::num::ParseIntError),
}

impl Diagnostic for AstError {
    fn kind(&self) -> DiagnosticKind {
        DiagnosticKind::BuildAst
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VarTy {
    Int,
    Short,
    Long,
    Float,
    Ref,

    String,
    Array,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    Or,

    And,

    Eq,
    Neq,

    Greater,
    Lesser,
    GreaterOrEqual,
    LesserOrEqual,

    BitOr,

    BitAnd,
    LeftShift,
    RightShift,

    Add,
    Sub,

    Mul,
    Div,
    Mod,
    Pow,
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Neg,
    ToString,
    ToNumber,
    Deref,
    Box,
    Not,
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

#[derive(Debug, Clone, PartialEq)]
pub enum InvokeArg {
    Constant(Spanned<Literal>),
    Var(Property),
    UnaryOp {
        operator: Spanned<UnaryOp>,
        inner: Spanned<Expression>,
    },
    Inline(Spanned<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Invoke {
    pub name: Property,
    pub args: Vec<Spanned<InvokeArg>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    pub referent: Spanned<String>,
    pub path: Vec<Spanned<String>>,
}

impl fmt::Display for Property {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", *self.referent)?;
        for segment in &self.path {
            write!(f, ".{}", **segment)?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum IntRadix {
    Decimal,
    Hexadecimal,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int { value: i32, radix: IntRadix },
    Float(f64),
    String(String),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Int { value, radix } => match radix {
                IntRadix::Decimal => write!(f, "{value}"),
                IntRadix::Hexadecimal => write!(f, "0x{value:x}"),
            },
            Literal::Float(float) => write!(f, "{float}"),
            Literal::String(string) => write!(f, "\"{string}\""),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Constant(Spanned<Literal>),
    Invoke(Spanned<Invoke>),
    Group(Box<Spanned<Expression>>),
    BinaryOp {
        operator: Spanned<BinaryOp>,
        lhs: Box<Spanned<Expression>>,
        rhs: Box<Spanned<Expression>>,
    },
    UnaryOp {
        operator: Spanned<UnaryOp>,
        inner: Box<Spanned<Expression>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfBranch {
    pub condition: Spanned<Expression>,
    pub code: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    IfTree {
        ifs: Vec<IfBranch>,
        else_: Option<Block>,
    },
    Expression(Spanned<Expression>),
    Set {
        var: Property,
        to: Spanned<Expression>,
    },
    Variables(Variables),
    Let {
        assignment: Option<Spanned<BinaryOp>>,
        var: Property,
        value: Spanned<Expression>,
    },
    Return,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variables {
    pub ty: VarTy,
    pub names: Vec<Spanned<String>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<Spanned<Statement>>);

#[derive(Debug, Clone, PartialEq)]
pub enum Args {
    Const(i32),
    Var(Property),
    Parameters(Vec<Spanned<String>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Blocktype {
    pub name: Spanned<String>,
    pub args: Option<Spanned<Args>>,
    pub code: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Variables(Variables),
    Blocktype(Blocktype),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Script {
    pub name: String,
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AnyAst {
    Script(Script),
    Block(Block),
}
