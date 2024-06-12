use core::fmt;
use std::fmt::Debug;

use miette::Diagnostic;
use thiserror::Error;
use winnow::stream::Stream;

use crate::{
    reporting::{CodeAction, CodeActionIter, DiagnosticKind, ListFmt, Reportable},
    span::{Span, Spanned},
};

use super::{Input, Lang, Token};

#[derive(PartialEq)]
pub struct DebugSource {
    line: u32,
    column: u32,
    file: &'static str,
}

impl fmt::Debug for DebugSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(in {} @ {}:{})", self.file, self.line, self.column)
    }
}

#[derive(Debug, Error, Diagnostic)]
pub enum ParsingFail {
    #[error("unexpected eof")]
    UnexpectedEof {
        #[label("expected {}", ListFmt::new(expected, " | "))]
        span: Span,
        expected: Vec<Token>,
    },
    #[error("unexpected eof (bad ver.)")]
    EarlyEof(usize),
    #[error("expected eof")]
    LateEof(usize),
    #[error("unexpected token (bad ver.) {0}")]
    Unexpected(#[label("did not expect this token")] Spanned<Token>),
    #[error("unexpected token {found}")]
    Mismatch {
        #[label("expected {}", ListFmt::new(expected, " | "))]
        found: Spanned<Token>,
        expected: Vec<Token>,
    },
    #[error("unexpected whitespace")]
    UnexpectedTrivia {
        #[label("expected no whitespace here")]
        span: Span,
    },
}

impl PartialEq for ParsingFail {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::UnexpectedEof { span: l_span, .. },
                Self::UnexpectedEof { span: r_span, .. },
            ) => l_span == r_span,
            (Self::EarlyEof(l0), Self::EarlyEof(r0)) => l0 == r0,
            (Self::LateEof(l0), Self::LateEof(r0)) => l0 == r0,
            (Self::Unexpected(l0), Self::Unexpected(r0)) => l0 == r0,
            (Self::Mismatch { found: l_found, .. }, Self::Mismatch { found: r_found, .. }) => {
                l_found == r_found
            }
            (Self::UnexpectedTrivia { span: l_span }, Self::UnexpectedTrivia { span: r_span }) => {
                l_span == r_span
            }
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct ParsingError {
    errors: Vec<ParsingFail>,
}

impl ParsingFail {
    pub fn from_next(input: Input<'_, '_>) -> Self {
        let span = input.next_span();
        let Some((_, (_, next))) = input.next_token() else {
            return Self::EarlyEof(span.start);
        };
        ParsingFail::Unexpected(Spanned::new(next.kind, span))
    }

    pub fn or(&mut self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Mismatch {
                    found: l_found,
                    expected: l_expected,
                },
                Self::Mismatch {
                    found: r_found,
                    expected: r_expected,
                },
            ) => {
                if l_found == r_found {
                    l_expected.extend(r_expected.iter().cloned());
                }
                l_found == r_found
            }
            (
                Self::UnexpectedEof {
                    span: l_span,
                    expected: l_expected,
                },
                Self::UnexpectedEof {
                    span: r_span,
                    expected: r_expected,
                },
            ) => {
                if l_span == r_span {
                    l_expected.extend(r_expected.iter().cloned());
                }
                l_span == r_span
            }
            (lhs, rhs) => lhs == rhs,
        }
    }
}

impl Actionables for ParsingError {
    fn actionable_iter(&self) -> ActionIter<'_> {
        Box::new(self.errors.iter().map(miette_to_actionable))
    }
}

impl From<ParsingFail> for ParsingError {
    fn from(value: ParsingFail) -> Self {
        Self {
            errors: vec![value],
        }
    }
}

impl winnow::error::ParseError<Input<'_, '_>> for ParsingError {
    fn from_error_kind(input: Input<'_, '_>, _: winnow::error::ErrorKind) -> Self {
        ParsingFail::from_next(input).into()
    }

    fn append(mut self, input: Input<'_, '_>, kind: winnow::error::ErrorKind) -> Self {
        for new in Self::from_error_kind(input, kind).errors {
            if !self.errors.iter().any(|present| *present == new) {
                self.errors.push(new)
            }
        }

        self
    }

    fn or(mut self, other: Self) -> Self {
        for new in other.errors {
            if !self.errors.iter_mut().any(|present| present.or(&new)) {
                self.errors.push(new)
            }
        }

        self
    }

    #[cfg(feature = "debug")]
    fn assert(input: Input<'_, '_>, _message: &'static str) -> Self {
        panic!("assert `{}` failed at {:#?}", _message, input);
    }
}

impl<'slice, 'src: 'slice> rowwin::ParseError<'slice, 'src, Lang> for ParsingError {
    fn from_unexpected_token(span: std::ops::Range<usize>, expected: Token, found: Token) -> Self {
        ParsingFail::Mismatch {
            expected: vec![expected],
            found: Spanned::new(found, span),
        }
        .into()
    }

    fn from_unexpected_eof(position: usize, expected: Token) -> Self {
        ParsingFail::UnexpectedEof {
            span: (position - 1..position - 1).into(),
            expected: vec![expected],
        }
        .into()
    }

    fn from_expected_eof(position: usize) -> Self {
        ParsingFail::LateEof(position).into()
    }

    fn end_offset_from(&self, position: usize) -> usize {
        self.errors
            .iter()
            .map(|error| match error {
                ParsingFail::UnexpectedEof { .. }
                | ParsingFail::EarlyEof(_)
                | ParsingFail::LateEof(_) => 0,
                ParsingFail::Unexpected(token) => token.span.end - position,
                ParsingFail::Mismatch { found, .. } => found.span.end - position,
                ParsingFail::UnexpectedTrivia { span } => span.end - position,
            })
            .min()
            .unwrap_or(0)
    }
}

#[derive(Debug, Error, Diagnostic)]
pub enum ParsingDiagnostic {
    #[error("extraneous tokens")]
    #[diagnostic(help("try removing the extra tokens or placing them in a comment"))]
    ExtraneousTokens {
        #[label("these tokens are ignored")]
        span: Span,
    },
    #[error("closing token without matching opening token")]
    #[diagnostic(help("try removing the extra closing token or placing it in a comment"))]
    MismatchedBlocks {
        #[label("this block was never opened")]
        span: Span,
    },
}

#[derive(Debug)]
pub enum ParsingReport {
    Diagnostic(ParsingDiagnostic),
    Forgiven(ParsingError),
}

impl Reportable for ParsingDiagnostic {
    fn code_actions(&self) -> CodeActionIter<'_> {
        match self {
            ParsingDiagnostic::ExtraneousTokens { .. }
            | ParsingDiagnostic::MismatchedBlocks { .. } => Some(Box::new(RemoveTokens)),
        }
    }
}

impl Reportable for ParsingError {
    fn kind(&self) -> DiagnosticKind {
        DiagnosticKind::Syntax
    }
}

struct RemoveTokens;

impl CodeAction for RemoveTokens {
    fn title(&self) -> String {
        "remove these tokens".to_owned()
    }

    fn fix(&self, syntax: crate::ast::SyntaxElement) {
        assert_eq!(
            syntax.kind(),
            Token::Error,
            "tried to remove innocent tokens",
        );
        syntax.detach();
    }
}

impl Reportable for ParsingReport {
    fn kind(&self) -> DiagnosticKind {
        DiagnosticKind::Syntax
    }
}

impl From<ParsingDiagnostic> for ParsingReport {
    fn from(value: ParsingDiagnostic) -> Self {
        ParsingReport::Diagnostic(value)
    }
}
