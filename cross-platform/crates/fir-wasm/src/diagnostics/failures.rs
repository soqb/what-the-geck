use thiserror::Error;

use fir::{utils::TyPrinter, Span, StopToken};

use super::{NotFound, Undefined};

#[derive(Debug, Error)]
pub enum IndexedKind {
    #[error("variable")]
    Variable,
    #[error("type")]
    Type,
    #[error("form")]
    Form,
}

#[derive(Debug, Error, miette::Diagnostic)]
#[diagnostic(severity(error))]
pub enum CompileFail {
    #[error("code generation produced invalid wasm")]
    InvalidWasm(#[from] wasmparser::BinaryReaderError),

    #[error("could not resolve type")]
    UnresolvableType {
        #[label(primary, "type of this value is not known")]
        span: Span,
    },

    #[error("types mismatched for operation")]
    UnaryTypeMismatch {
        inner: TyPrinter,
        #[label(primary, "this operation")]
        op: Span,
        #[label("does not support this type '{inner}'")]
        span: Span,
    },

    // fixme: provide proper tracing for this lackluster message
    #[error("operation not supported for these types")]
    UnsupportedOperation {
        #[label(primary, "operation not supported on the relevant arguments")]
        span: Span,
    },

    #[error("unsupported coersion")]
    BadCoersion {
        #[label(primary, "cannot coerce '{from}' into '{to}'")]
        span: Span,
        from: TyPrinter,
        to: TyPrinter,
    },

    #[error("undefined {kind}")]
    Undefined {
        #[label(primary, "this {kind} was not defined")]
        span: Span,
        kind: NotFound,
    },
    #[error("form type mismatch")]
    BadForm {
        #[label(primary, "did not expect a form of type {found}")]
        span: Span,
        found: String,
    },
    #[error("formid not found")]
    InvalidFormId {
        #[label(primary, "could not find this formid")]
        span: Span,
    },

    #[error("unknown blocktype")]
    UnknownBlocktype {
        #[label(primary, "this blocktype does not exist")]
        span: Span,
    },

    #[error("did not expect constant")]
    NoConst {
        #[label(primary, "a constant is not allowed here")]
        span: Span,
    },

    #[error("variable already in scope")]
    VarRedeclare { span: Span },
    #[error("cannot redefine ")]
    Redefine { span: Span },

    #[error("parameter-argument mismatch")]
    InvalidParam {
        #[label(primary, "this argument is not valid for parameter {param}")]
        span: Span,
        param: String,
    },

    #[error("too few arguments, expected no less than {count}")]
    TooFewArguments {
        #[label(primary, "expected another argument here")]
        span: Span,
        count: usize,
    },
    #[error("no default argument")]
    NoDefaultArg {
        #[label(primary, "cannot create default value for parameter {param}")]
        span: Span,
        param: String,
    },
    #[error("compilation halted by frontend")]
    StopToken,
}

#[derive(Debug, Error, miette::Diagnostic)]
pub enum CompileDiagnostic {
    #[error("types mismatched for binary operation")]
    BinaryTypeMismatch {
        #[label("this type '{lhs}'")]
        lhs_span: Span,
        lhs: TyPrinter,
        #[label("and this type '{rhs}'")]
        rhs_span: Span,
        rhs: TyPrinter,
        #[label(primary, "this operation does not support")]
        op: Span,
    },
}

impl From<StopToken> for CompileFail {
    fn from(_: StopToken) -> Self {
        Self::StopToken
    }
}

impl From<Undefined> for CompileFail {
    fn from(value: Undefined) -> Self {
        Self::Undefined {
            span: value.span,
            kind: value.kind,
        }
    }
}

impl fir::Diagnostic for CompileFail {
    fn kind(&self) -> fir::DiagnosticKind {
        use fir::DiagnosticKind as D;
        D::CompileFail
    }
}
