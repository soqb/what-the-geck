use std::fmt;

use logos::Logos;
use miette::Diagnostic;
use thiserror::Error;

use crate::{
    reporting::{DiagnosticKind, Reportable},
    span::{Span, Spanned},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Logos)]
#[repr(u16)]
pub enum Token {
    #[token("\n")]
    Newline,
    #[token(",")]
    Comma,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token(".")]
    Dot,

    #[token(">")]
    RightAngle,
    #[token("<")]
    LeftAngle,
    #[token(">>")]
    RightAngle2,
    #[token("<<")]
    LeftAngle2,

    #[token("==")]
    Equal2,
    #[token(">=")]
    RightAngleEqual,
    #[token("<=")]
    LeftAngleEqual,
    #[token("!=")]
    BangEqual,

    #[token("!")]
    Bang,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("^")]
    Caret,

    #[token("&")]
    Amp,
    #[token("|")]
    Bar,
    #[token("&&")]
    Amp2,
    #[token("||")]
    Bar2,

    #[token("$")]
    Dollar,
    #[token("#")]
    Hash,

    #[token(":=")]
    ColonEqual,
    #[token("=")]
    Equal,
    #[token("+=")]
    PlusEqual,
    #[token("-=")]
    MinusEqual,
    #[token("*=")]
    StarEqual,
    #[token("/=")]
    SlashEqual,
    #[token("%=")]
    PercentEqual,
    #[token("^=")]
    CaretEqual,
    #[token("|=")]
    BarEqual,
    #[token("&=")]
    AmpEqual,

    #[regex("scn|scriptname", ignore(ascii_case))]
    ScriptName,
    #[token("begin", ignore(ascii_case))]
    Begin,
    #[token("end", ignore(ascii_case))]
    End,
    #[token("return", ignore(ascii_case))]
    Return,
    #[token("if", ignore(ascii_case))]
    If,
    #[token("elseif", ignore(ascii_case))]
    ElseIf,
    #[token("else", ignore(ascii_case))]
    Else,
    #[token("endif", ignore(ascii_case))]
    EndIf,
    #[token("set", ignore(ascii_case))]
    Set,
    #[token("to", ignore(ascii_case))]
    To,

    #[token("int", ignore(ascii_case))]
    Int,
    #[token("short", ignore(ascii_case))]
    Short,
    #[token("long", ignore(ascii_case))]
    Long,
    #[token("float", ignore(ascii_case))]
    Float,
    #[token("ref", ignore(ascii_case))]
    Ref,

    #[regex("[a-zA-Z0-9_]+", priority = 1)]
    Identifier,
    #[regex("[0-9]+", priority = 2)]
    IntLiteral,
    #[regex("[0-9]+\\.[0-9]+")]
    FloatLiteral,
    #[regex("\"[^\"]+\"")]
    StringLiteral,

    #[regex("[\t\r ]+")]
    Whitespace,
    #[regex("===+|---+|`")]
    Gibberish,
    #[regex(";[^\n]*")]
    Comment,

    Error,

    Property,
    UnaryOp,
    BinaryOp,
    Group,
    Invoke,
    SetTo,
    IfBranch,
    IfTree,
    Variables,
    Blocktype,

    Block,
    Script,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl From<Token> for rowan::SyntaxKind {
    fn from(value: Token) -> Self {
        Self(value as u16)
    }
}

#[derive(Diagnostic, Error, Debug)]
#[error("unexpected sequence")]
pub struct TokenError {
    #[label("did not expect this sequence")]
    found: Spanned<String>,
}

impl Reportable for TokenError {
    fn kind(&self) -> DiagnosticKind {
        DiagnosticKind::Tokenization
    }
}

impl TokenError {
    pub fn new(source: &str, span: impl Into<Span>) -> Self {
        let span = span.into();
        Self {
            found: Spanned::new(source[span.start..span.end].to_owned(), span),
        }
    }
}
