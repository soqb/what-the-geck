use core::fmt;
use hashbrown::HashMap;

use crate::{Const, NumericTy, Spanned, Ty};

pub use Tried::{Resolved, Unresolvable};

// TODO: consider `hipstr` et al. or interning
type String = std::string::String;
pub type Ident = Spanned<String>;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Script {
    pub event_impls: Vec<Spanned<EventImpl>>,
    pub ufds: Vec<Spanned<UserFunctionDefinition>>,
    pub variables: HashMap<InternalVariableIdx, ExternalVariableIdx>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionBody {
    pub variables: HashMap<BodyVariableIdx, BodyVariableInfo>,
    pub entrypoint: Spanned<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UserFunctionDefinition {
    pub name: Ident,
    pub args: Vec<Ident>,
    pub body: FunctionBody,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BranchTarget {
    Break { depth: usize },
    Loop,
    Return,
    Unreachable,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Spanned<Statement>>,
    pub termination: BranchTarget,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BodyVariableInfo {
    pub ty: Spanned<Ty>,
    pub name: Ident,
}

/// An [`Option`]-like enum which represents the sum total effort to resolve some data.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Tried<T> {
    /// The data has been resolved.
    Resolved(T),
    /// The data _cannot_ be resolved.
    Unresolvable,
}

impl<T> From<T> for Tried<T> {
    fn from(value: T) -> Self {
        Resolved(value)
    }
}

impl<T> From<Option<T>> for Tried<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(x) => Resolved(x),
            None => Unresolvable,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EventImpl {
    pub event: Spanned<Tried<EventIdx>>,
    pub arguments: Vec<Spanned<Expression>>,
    pub body: FunctionBody,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PowKind {
    IntegerBase,
    FloatBase { exponent: NumericTy },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CoreOperator {
    Or,
    And,
    Eq(Ty),
    Neq(Ty),

    Greater(NumericTy),
    Lesser(NumericTy),
    GreaterOrEqual(NumericTy),
    LesserOrEqual(NumericTy),

    BitOr,
    BitAnd,
    BitXor,
    LeftShift,
    RightShift,

    Add(NumericTy),
    Sub(NumericTy),
    Mul(NumericTy),
    Div(NumericTy),
    Rem(NumericTy),

    Pow(PowKind),

    Neg(NumericTy),
    Not,
    BitNot,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    pub ident: String,
}

macro_rules! define_idx {
    (
        $(#[$attr:meta])*
        $vis:vis $name:ident($inner_vis:vis $inner:ty) <- $kind:literal
    ) => {
        #[doc = concat!("A unique index for every ", $kind, ".\n\n")]
        #[derive(Default, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        $(#[$attr])*
        $vis struct $name($inner_vis $inner);

        impl From<$name> for $inner {
            fn from(value: $name) -> $inner {
                value.0
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, concat!(stringify!($name), "(#{idx})"), idx = <$inner>::from(*self))
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, concat!($kind, " {idx}"), idx = <$inner>::from(*self))
            }
        }
    }
}

define_idx!(
    /// While backed by a `u16`, indices wrap after twelve bits (4095).
    pub ComponentIdx(pub u16) <- "component"
);

define_idx!(pub ExternalVariableIdx(pub u64) <- "variable");
define_idx!(pub FunctionIdx(pub u64) <- "function");
define_idx!(pub FormIdx(pub u64) <- "form");
define_idx!(pub EventIdx(pub u64) <- "event");
define_idx!(pub TypeIdx(pub u64) <- "type");
define_idx!(pub SourceIdx(pub u64) <- "source");

define_idx!(
    /// Unique only within a single function body.
    pub BodyVariableIdx(pub u32) <- "body variable"
);
define_idx!(
    /// Unique only within a single form.
    pub InternalVariableIdx(pub u32) <- "form variable"
);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ConversionReason {
    Explicit,
    VariableSet,
    OperatorParam,
    Other,
    FuncParam,
    SelfParam,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ConversionMethod {
    NarrowingTransmute,
    WideningTransmute,
    /// Turn a value of one type into another, preserving semantic meaning,
    /// and potentially losing precision.
    Translate,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Conversion {
    pub from: Ty,
    pub to: Ty,
    pub provenance: Spanned<ConversionReason>,
    pub method: ConversionMethod,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Core(CoreOperator),
    TypeError,
    Function { idx: Tried<FunctionIdx> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Operation {
    pub operands: Vec<Spanned<Expression>>,
    pub operator: Spanned<Operator>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveValue {
    NullReference,
    Unit,
    Bool(bool),
    Int(i64),
    String(String),
    Float(f64),
}

impl From<&Const> for PrimitiveValue {
    fn from(value: &Const) -> Self {
        match value {
            &Const::Integer(i) => Self::Int(i),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VariableIdx {
    BodyLocal(BodyVariableIdx),
    FormInternal(InternalVariableIdx),
    FormExternal(ExternalVariableIdx),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    PrimitiveValue(PrimitiveValue),
    FormRef(FormIdx),
    GetVariable(Tried<VariableIdx>),
    Convert {
        value: Box<Spanned<Expression>>,
        conversion: Conversion,
    },
    Operation(Operation),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Express(Expression),
    SetVariable {
        variable: Spanned<Tried<VariableIdx>>,
        value: Spanned<Expression>,
    },
    Block(Block),
    Branch {
        target: BranchTarget,
        kind: BranchKind,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BranchKind {
    Unconditional,
    IfTrue(Spanned<Expression>),
}
