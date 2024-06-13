use thiserror::Error;
use tree_sitter::{Parser, Tree};

use crate::{
    reporting::{Diagnostic, DiagnosticKind},
    span::Span,
};

const ERRORS_SCM: &str = include_str!("../../tree-sitter-geckscript/queries/errors.scm");

#[derive(Debug)]
pub enum ErrorKind {
    Extraneous,
    BadTerminator,
}

#[derive(Debug, Error)]
pub enum ParseDiagnostic {
    #[error("error setting language")]
    Language(#[from] tree_sitter::LanguageError),
    #[error("")]
    PostProcessed { kind: ErrorKind, span: Span },
}

pub struct ParseOutput {
    pub tree: Tree,
    pub diagnostics: Vec<ParseDiagnostic>,
}

#[derive(Debug, Error, miette::Diagnostic)]
#[error("tree-sitter language failed to instantiate")]
struct TreeSitterError(tree_sitter::LanguageError);

impl Diagnostic for TreeSitterError {
    fn kind(&self) -> DiagnosticKind {
        DiagnosticKind::Critical
    }
}

pub fn parse(source: &str, edited_old_tree: Option<&Tree>) -> Result<Tree> {
    let mut parser = Parser::new();
    parser.set_language(tree_sitter_geckscript::language())?;
    Ok(parser.parse(source, edited_old_tree).unwrap())
}
