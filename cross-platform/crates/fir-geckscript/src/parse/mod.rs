use rowwin::{join, opt, token, Children, Language, Parser, Search};
use winnow::{
    combinator::{alt, eof, repeat},
    error::ErrMode,
    token::take_till1,
    trace::trace,
    Parser as WinnowParser,
};

use crate::{ast::BinaryOp, span::Span};

mod error;
pub use error::*;

mod tokens;
pub use tokens::*;

pub type Input<'slice, 'src> = rowwin::Input<'slice, 'src, Lang>;

pub type IResult<'slice, 'src, O = rowwin::Children<Lang>> = rowwin::IResult<'slice, 'src, Lang, O>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}

impl Language for Lang {
    type Kind = Token;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= Token::Script as u16);
        unsafe { std::mem::transmute::<u16, Token>(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }

    fn error_kind() -> Self::Kind {
        Token::Error
    }

    type Error<'slice, 'src: 'slice> = ParsingError;

    type Diagnostic = ParsingReport;

    fn is_trivial(kind: Token) -> bool {
        matches!(kind, Token::Comment | Token::Whitespace | Token::Gibberish)
    }

    fn is_skippable(kind: Self::Kind) -> bool {
        !matches!(kind, Token::Newline)
    }

    fn convert_err(err: ParsingError) -> ParsingReport {
        ParsingReport::Forgiven(err)
    }
}

pub fn spanned<'slice, 'src: 'slice, O>(
    mut inner: impl Parser<'slice, 'src, Lang, O>,
    mut with_span: impl FnMut(O, Span) -> Result<O, ErrMode<ParsingError>>,
) -> impl Parser<'slice, 'src, Lang, O> {
    move |input: Input<'slice, 'src>| {
        let start = input.source_offset();
        let (input, output) = inner.parse_next(input)?;
        let end = input.source_offset_before_trivia();

        Ok((input, with_span(output, (start..end).into())?))
    }
}

pub fn spanned_error<'slice, 'src: 'slice>(
    inner: impl Parser<'slice, 'src, Lang>,
    mut make_error: impl FnMut(Span) -> ParsingReport,
) -> impl Parser<'slice, 'src, Lang> {
    spanned(inner, move |mut children, span| {
        children = children.into_error();
        children.report(make_error(span));
        Ok(children)
    })
}

pub fn ident<'slice, 'src: 'slice>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    trace("ident", token(Token::Identifier)).parse_next(input)
}

pub fn newline<'slice, 'src: 'slice>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    join((
        opt(spanned_error(
            take_till1(Search::new(|(_, token)| token.kind == Token::Newline)).output_into(),
            |span| ParsingDiagnostic::ExtraneousTokens { span }.into(),
        )),
        alt((repeat(1.., token(Token::Newline)), eof.output_into())),
    ))
    .node(Token::Newline)
    .parse_next(input)
}

pub fn no_trivia<'slice, 'src: 'slice, O>(
    mut inner: impl Parser<'slice, 'src, Lang, O>,
) -> impl Parser<'slice, 'src, Lang, O> {
    move |input: Input<'slice, 'src>| {
        let span = input.trivia_span();
        if span.is_empty() {
            inner.parse_next(input)
        } else {
            Err(ErrMode::Backtrack(
                ParsingFail::UnexpectedTrivia { span: span.into() }.into(),
            ))
        }
    }
}

pub fn variables<'slice, 'src: 'slice>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    join((
        alt((
            token(Token::Int),
            token(Token::Short),
            token(Token::Long),
            token(Token::Float),
            token(Token::Ref),
        )),
        ident.forgive(),
    ))
    .node(Token::Variables)
    .parse_next(input)
}

pub fn property<'slice, 'src: 'slice>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    join((
        ident,
        opt(no_trivia(join((token(Token::Dot), ident.forgive())))),
    ))
    .node(Token::Property)
    .parse_next(input)
}

pub fn literal<'slice, 'src: 'slice>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    alt((
        token(Token::IntLiteral),
        token(Token::FloatLiteral),
        token(Token::StringLiteral),
        join((token(Token::Dot), token(Token::IntLiteral).forgive())).node(Token::FloatLiteral),
    ))
    .parse_next(input)
}

pub fn invoke<'slice, 'src: 'slice>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    fn invoke_arg<'slice, 'src: 'slice>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
        join((
            opt(token(Token::Comma).map(Children::into_error)),
            alt((literal, unary_op, property)),
        ))
        .parse_next(input)
    }

    join((property, repeat(0.., invoke_arg)))
        .node(Token::Invoke)
        .parse_next(input)
}

fn const_expr<'slice, 'src: 'slice>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    alt((
        literal,
        invoke,
        unary_op,
        join((
            token(Token::LeftParen),
            expression,
            token(Token::RightParen),
        ))
        .node(Token::Group),
    ))
    .parse_next(input)
}

fn unary_op<'slice, 'src: 'slice>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    join((
        alt((
            token(Token::Minus),
            token(Token::Star),
            token(Token::Amp),
            token(Token::Dollar),
            token(Token::Hash),
            token(Token::Bang),
        )),
        const_expr,
    ))
    .node(Token::UnaryOp)
    .parse_next(input)
}

pub fn expression<'slice, 'src: 'slice>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    fn binary_op<'slice, 'src: 'slice>(
        input: Input<'slice, 'src>,
    ) -> IResult<'slice, 'src, (Children<Lang>, BinaryOp)> {
        alt((
            token(Token::Bar2).map(|ch| (ch, BinaryOp::Or)),
            token(Token::Amp2).map(|ch| (ch, BinaryOp::And)),
            token(Token::Equal2).map(|ch| (ch, BinaryOp::Eq)),
            token(Token::BangEqual).map(|ch| (ch, BinaryOp::Neq)),
            token(Token::RightAngle).map(|ch| (ch, BinaryOp::Greater)),
            token(Token::LeftAngle).map(|ch| (ch, BinaryOp::Lesser)),
            token(Token::RightAngleEqual).map(|ch| (ch, BinaryOp::GreaterOrEqual)),
            token(Token::LeftAngleEqual).map(|ch| (ch, BinaryOp::LesserOrEqual)),
            token(Token::Bar).map(|ch| (ch, BinaryOp::BitOr)),
            token(Token::Amp).map(|ch| (ch, BinaryOp::BitAnd)),
            token(Token::Plus).map(|ch| (ch, BinaryOp::Add)),
            token(Token::Minus).map(|ch| (ch, BinaryOp::Sub)),
            token(Token::Slash).map(|ch| (ch, BinaryOp::Div)),
            token(Token::Star).map(|ch| (ch, BinaryOp::Mul)),
            token(Token::Percent).map(|ch| (ch, BinaryOp::Mod)),
            token(Token::Caret).map(|ch| (ch, BinaryOp::Pow)),
        ))
        .parse_next(input)
    }

    fn climb_precedence<'slice, 'src: 'slice>(
        mut lhs: Children<Lang>,
        min_precedence: usize,
        mut input: Input<'slice, 'src>,
    ) -> IResult<'slice, 'src> {
        loop {
            let (op_children, op);
            (input, (op_children, op)) = match binary_op.parse_next(input.clone()) {
                Ok((input, (op_children, op))) if op.precedence() >= min_precedence => {
                    (input, (op_children, op))
                }
                _ => break,
            };

            let mut rhs;
            (input, rhs) = const_expr.forgive().parse_next(input)?;

            loop {
                match binary_op(input.clone()) {
                    Ok((_, (_, new_op))) if new_op.precedence() > op.precedence() => (),
                    _ => break,
                };

                (input, rhs) = climb_precedence(rhs, op.precedence() + 1, input)?;
            }

            debug_assert!(lhs.is_single(), "{lhs:?}");
            debug_assert!(rhs.is_single() || rhs.is_diagnostical(), "{rhs:?}");

            lhs.append(op_children);
            lhs.append(rhs);

            lhs = lhs.into_node(Token::BinaryOp);
        }

        Ok((input, lhs))
    }

    trace("expression", |input| {
        let (input, lhs) = const_expr(input)?;

        climb_precedence(lhs, 0, input)
    })
    .parse_next(input)
}

#[derive(Debug, Clone, Copy)]
pub enum BlockTerminator {
    If,
    Else,
    Begin,
    None,
}

impl BlockTerminator {
    pub fn terminating<'slice, 'src: 'slice>(self) -> impl Parser<'slice, 'src, Lang> {
        move |input| match self {
            BlockTerminator::If => alt((
                token(Token::Else),
                token(Token::ElseIf),
                token(Token::EndIf),
            ))
            .parse_next(input),
            BlockTerminator::Else => token(Token::EndIf).parse_next(input),
            BlockTerminator::Begin => token(Token::End).parse_next(input),
            BlockTerminator::None => eof.map(|_| Children::default()).parse_next(input),
        }
    }

    pub fn unexpected<'slice, 'src: 'slice>(self) -> impl Parser<'slice, 'src, Lang> {
        move |input| match self {
            BlockTerminator::If => token(Token::End).parse_next(input),
            BlockTerminator::Else => {
                alt((token(Token::Else), token(Token::ElseIf))).parse_next(input)
            }
            BlockTerminator::Begin => alt((
                token(Token::Else),
                token(Token::ElseIf),
                token(Token::EndIf),
            ))
            .parse_next(input),
            BlockTerminator::None => alt((
                token(Token::Else),
                token(Token::ElseIf),
                token(Token::EndIf),
                token(Token::End),
            ))
            .parse_next(input),
        }
    }
}

pub fn if_branch<'slice, 'src: 'slice>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    join((expression.forgive(), newline, block(BlockTerminator::If)))
        .node(Token::IfBranch)
        .parse_next(input)
}

pub fn statement<'slice, 'src: 'slice>(
    block_type: BlockTerminator,
) -> impl Parser<'slice, 'src, Lang> {
    trace(
        "statement",
        alt((
            join((
                spanned_error(block_type.unexpected(), |span| {
                    ParsingDiagnostic::MismatchedBlocks { span }.into()
                }),
                newline,
            )),
            join((
                alt((
                    variables,
                    join((
                        token(Token::If),
                        if_branch,
                        repeat(0.., join((token(Token::ElseIf), if_branch))),
                        opt(join((
                            token(Token::Else),
                            newline,
                            block(BlockTerminator::Else),
                        ))),
                        token(Token::EndIf).forgive(),
                    ))
                    .node(Token::IfTree),
                    join((
                        token(Token::Set),
                        property.forgive(),
                        token(Token::To).forgive(),
                        expression.forgive(),
                    ))
                    .node(Token::SetTo),
                    token(Token::Return),
                    expression,
                )),
                newline,
            )),
        )),
    )
}

pub fn block<'slice, 'src: 'slice>(terminate: BlockTerminator) -> impl Parser<'slice, 'src, Lang> {
    trace(
        match terminate {
            BlockTerminator::If => "if block",
            BlockTerminator::Else => "else block",
            BlockTerminator::Begin => "begin block",
            BlockTerminator::None => "block",
        },
        move |input| {
            repeat(0.., statement(terminate))
                .node(Token::Block)
                .parse_next(input)
        },
    )
}

fn item<'slice, 'src: 'slice>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    join((
        alt((
            variables,
            join((
                token(Token::Begin),
                ident.forgive(),
                opt(alt((token(Token::IntLiteral), property))),
                newline,
                block(BlockTerminator::Begin),
                token(Token::End).forgive(),
            ))
            .node(Token::Blocktype),
        )),
        newline,
    ))
    .parse_next(input)
}

pub fn script<'slice, 'src: 'slice>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    join((
        repeat(0.., token(Token::Newline)),
        token(Token::ScriptName).forgive(),
        ident.forgive(),
        newline,
        repeat(0.., item),
    ))
    .node(Token::Script)
    .parse_next(input)
}
