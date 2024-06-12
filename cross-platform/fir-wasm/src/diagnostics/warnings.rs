use fir::{
    utils::{TyHintPrinter, TyPrinter},
    Span,
};
use thiserror::Error;

use super::{NotFound, Undefined};

#[derive(Debug, Error, miette::Diagnostic)]
pub enum CompileDiagnostic {
    #[error("statement has no effect")]
    UselessStatement {
        #[label("this does nothing")]
        span: Span,
    },

    #[error("unchecked transmutation")]
    UncheckedTransmute {
        #[label("coercing this from '{from}' to '{to}'")]
        span: Span,
        from: TyPrinter,
        to: TyHintPrinter,
    },

    #[error("too many arguments, expected no more than {count}")]
    TooManyArguments {
        #[label("did not expect these arguments")]
        span: Span,
        count: usize,
    },

    #[error("undefined {kind}")]
    Undefined {
        #[label("this {kind} was not defined")]
        span: Span,
        kind: NotFound,
    },

    #[error("type mismatch")]
    TypeMismatch {
        #[label("{found} is not applicable to {expected}")]
        span: Span,
        found: TyPrinter,
        expected: TyHintPrinter,
    },
}

impl From<Undefined> for CompileDiagnostic {
    fn from(value: Undefined) -> Self {
        Self::Undefined {
            span: value.span,
            kind: value.kind,
        }
    }
}

impl fir::Diagnostic for CompileDiagnostic {
    fn kind(&self) -> fir::DiagnosticKind {
        use fir::{DiagnosticKind as D, Warning};
        match self {
            CompileDiagnostic::UselessStatement { .. } => D::Warning(Warning::Superfluous),
            CompileDiagnostic::UncheckedTransmute { .. } => D::Warning(Warning::BadPractice),
            CompileDiagnostic::TooManyArguments { .. } => D::Warning(Warning::Superfluous),
            CompileDiagnostic::Undefined { .. } => D::CompileFail,
            CompileDiagnostic::TypeMismatch { .. } => D::CompileFail,
        }
    }
}
