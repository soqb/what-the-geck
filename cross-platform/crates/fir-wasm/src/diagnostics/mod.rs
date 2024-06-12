use fir::Span;
use thiserror::Error;

pub mod failures;
pub use failures::CompileFail;
mod warnings;
pub use warnings::CompileDiagnostic;

pub type Result<T, E = CompileFail> = core::result::Result<T, E>;

#[derive(Debug, Error, Clone, Copy)]
pub enum NotFound {
    #[error("block-local variable")]
    LocalVariable,
    #[error("form-internal variable")]
    FormInternalVariable,
    #[error("form-external variable")]
    FormExternalVariable,
    #[error("event")]
    Event,
    #[error("form")]
    Form,
    #[error("function")]
    Function,
    #[error("type")]
    Type,
    #[error("variable, command or form")]
    Unknown,
}

#[derive(Debug, Clone, Copy)]
pub struct Undefined {
    pub span: Span,
    pub kind: NotFound,
}
