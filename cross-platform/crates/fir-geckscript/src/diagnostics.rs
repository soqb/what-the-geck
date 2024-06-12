use fir::{utils::TyPrinter, Span, Spanned};

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum LowerWarning {
    #[error("case mismatch")]
    WrongCase {
        #[label("this was expected to match `{expected}`")]
        found: Span,
        expected: String,
    },
}

impl fir::Diagnostic for LowerWarning {
    fn kind(&self) -> fir::DiagnosticKind {
        match self {
            LowerWarning::WrongCase { .. } => {
                fir::DiagnosticKind::Warning(fir::Warning::BadPractice)
            }
        }
    }
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum LowerDiagnostic {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Warning(#[from] LowerWarning),
    #[error("unknown variable on form")]
    UnknownVariable {
        #[label("this form")]
        form: Span,
        #[label("has no variable with this name")]
        variable: Span,
    },
    #[error("unknown variable")]
    UnknownVariableInGeneral {
        #[label("this was not found to be a local, form-owned or global variable")]
        variable: Span,
    },
    #[error("unknown reference")]
    Unknown1stReference {
        #[label("this was not found to be a variable of any kind, a form, or a function")]
        reference: Span,
    },
    #[error("unknown reference")]
    Unknown2ndReference {
        #[label("this was not found to be a variable of a form, or a function")]
        reference: Span,
    },
    #[error("unknown function")]
    UnknownFunction(#[label("this function was not found")] Span),
    #[error("unknown event")]
    UnknownEvent(#[label("this event was not found")] Span),
    #[error("too many reference components")]
    TooLongReference(#[label("1 or 2 components were expected")] Span),
    #[error("no matching operation")]
    BinaryOpTyMismatch {
        #[label("this operand does not support")]
        op_span: Span,
        #[label("this type {lhs}")]
        lhs: Spanned<TyPrinter>,
        #[label("and this type {rhs}")]
        rhs: Spanned<TyPrinter>,
    },
}

impl fir::Diagnostic for LowerDiagnostic {
    fn kind(&self) -> fir::DiagnosticKind {
        match self {
            LowerDiagnostic::Warning(w) => <LowerWarning as fir::Diagnostic>::kind(w),
            _ => fir::DiagnosticKind::CompileFail,
        }
        // todo: improve
    }
}

#[derive(Debug, miette::Diagnostic, thiserror::Error)]
#[error("syntax error")]
pub struct TreeSitterParseError {
    #[label("syntax error here")]
    pub span: Span,
}

impl fir::Diagnostic for TreeSitterParseError {
    fn kind(&self) -> fir::DiagnosticKind {
        fir::DiagnosticKind::Syntax
    }
}
