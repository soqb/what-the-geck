use nom::{
    branch::alt,
    combinator::opt,
    multi::many0,
    sequence::{delimited, preceded, terminated, tuple},
    IResult, InputTake, Parser,
};

use crate::{
    ast::{
        parse::{ident, newline, punctuated1, strip_sep},
        Block, Blocktype, Expression, Invoke, InvokeArg, Property, Script, VarTy,
    },
    span::Spanned,
};
use crate::{
    ast::{Args, Variables},
    tokens::{Keyword, Literal, Punct, Token},
};

use crate::ast::{input::Input, BinaryOp, IfBranch, Item, ParsingError, Statement, UnaryOp};

use super::spanned;

macro_rules! nvse_keyword {
    ($kw:pat) => {
        token!(Token::Identifier($kw))
    };
}

fn variables<'a>(input: Input<'a>) -> IResult<Input<'a>, Variables<'a>, ParsingError<'a>> {
    use Keyword::*;
    use Punct::*;

    alt((
        token!(Token::Keyword(Int) => VarTy::Int),
        token!(Token::Keyword(Short) => VarTy::Short),
        token!(Token::Keyword(Long) => VarTy::Long),
        token!(Token::Keyword(Float) => VarTy::Float),
        token!(Token::Keyword(Ref) => VarTy::Ref),
    ))
    .and(punctuated1(ident, token!(Token::Punct(Comma))).map(strip_sep))
    .map(|(type_, names)| Variables { names, ty: type_ })
    .parse(input)
}

pub fn property<'a>(input: Input<'a>) -> IResult<Input<'a>, Property<'a>, ParsingError<'a>> {
    punctuated1(ident, token!(Token::Punct(Punct::Dot)))
        .map(|(first, rest)| {
            let path = rest.into_iter().map(|(_, x)| x).collect();
            Property {
                referent: first,
                path,
            }
        })
        .parse(input)
}

pub fn invoke(input: Input) -> IResult<Input, Invoke, ParsingError> {
    fn invoke_arg<'a>(input: Input<'a>) -> IResult<Input<'a>, InvokeArg<'a>, ParsingError<'a>> {
        alt((
            token!(Token::Literal(ref lit) => InvokeArg::Constant(lit.clone())),
            property.map(InvokeArg::Var),
            delimited(
                token!(Token::Punct(Punct::LeftParen)),
                expression,
                token!(Token::Punct(Punct::RightParen)),
            )
            .map(InvokeArg::Inline),
        ))(input)
    }

    property
        .and(many0(invoke_arg))
        .map(|(name, args)| Invoke { name, args })
        .parse(input)
}

fn const_expr<'a>(
    input: Input<'a>,
) -> IResult<Input<'a>, Spanned<Expression<'a>>, ParsingError<'a>> {
    spanned(alt((
        token!(Token::Literal(ref lit) => Expression::Constant(lit.clone())),
        invoke.map(Expression::Invoke),
        unary_op.map(|(operator, inner)| Expression::UnaryOp {
            operator,
            inner: Box::new(inner),
        }),
        delimited(
            token!(Token::Punct(Punct::LeftParen)),
            expression,
            token!(Token::Punct(Punct::RightParen)),
        )
        .map(|expr| Expression::Group(Box::new(expr))),
    )))
    .parse(input)
}

fn unary_op(input: Input) -> IResult<Input, (UnaryOp, Spanned<Expression>), ParsingError> {
    fn unary<'a>(
        punct: Punct,
        operator: UnaryOp,
    ) -> impl Parser<Input<'a>, (UnaryOp, Spanned<Expression<'a>>), ParsingError<'a>> {
        preceded(token!(Token::Punct(p) if *p == punct), const_expr)
            .map(move |inner| (operator, inner))
    }

    alt((
        unary(Punct::Minus, UnaryOp::Neg),
        unary(Punct::Star, UnaryOp::Deref),
        unary(Punct::Amp, UnaryOp::Box),
        unary(Punct::Dollar, UnaryOp::ToString),
        unary(Punct::Hash, UnaryOp::ToNumber),
        unary(Punct::Bang, UnaryOp::Not),
    ))(input)
}

pub fn expression(input: Input) -> IResult<Input, Spanned<Expression>, ParsingError> {
    fn binary_op<'a>(input: Input<'a>) -> IResult<Input<'a>, BinaryOp, ParsingError<'a>> {
        token!(
            Token::Punct(Punct::Bar2) => BinaryOp::Or,
            Token::Punct(Punct::Amp2) => BinaryOp::And,
            Token::Punct(Punct::Equal2) => BinaryOp::Eq,
            Token::Punct(Punct::BangEqual) => BinaryOp::Neq,
            Token::Punct(Punct::RightAngle) => BinaryOp::Greater,
            Token::Punct(Punct::LeftAngle) => BinaryOp::Lesser,
            Token::Punct(Punct::RightAngleEqual) => BinaryOp::GreaterOrEqual,
            Token::Punct(Punct::LeftAngleEqual) => BinaryOp::LesserOrEqual,
            Token::Punct(Punct::Bar) => BinaryOp::BitOr,
            Token::Punct(Punct::Amp) => BinaryOp::BitAnd,
            Token::Punct(Punct::Plus) => BinaryOp::Add,
            Token::Punct(Punct::Minus) => BinaryOp::Sub,
            Token::Punct(Punct::Slash) => BinaryOp::Div,
            Token::Punct(Punct::Star) => BinaryOp::Mul,
            Token::Punct(Punct::Percent) => BinaryOp::Mod,
            Token::Punct(Punct::Caret) => BinaryOp::Pow,
        )(input)
    }

    fn climb_precedence<'a>(
        mut lhs: Spanned<Expression<'a>>,
        min_precedence: usize,
        mut input: Input<'a>,
    ) -> IResult<Input<'a>, Spanned<Expression<'a>>, ParsingError<'a>> {
        macro_rules! or_break {
                ($expr:expr $(; $bind:tt if $cond:expr)?) => {
                    match $expr {
                        Ok((input, $($bind @)? op)) $(if $cond)? => (input, op),
                        _ => break,
                    }
                };
            }

        loop {
            let op;
            (input, op) = or_break!(binary_op(input); op if op.precedence() >= min_precedence);

            let mut rhs;
            (input, rhs) = const_expr(input)?;

            loop {
                or_break!(binary_op(input); new_op if new_op.precedence() > op.precedence());

                (input, rhs) = climb_precedence(rhs, op.precedence() + 1, input)?;
            }

            lhs = Spanned {
                inner: Expression::BinaryOp {
                    operator: op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                span: Spanned::between(&lhs, &rhs),
            }
        }

        Ok((input, lhs))
    }

    let (input, lhs) = const_expr(input)?;

    climb_precedence(lhs, 0, input)
}

pub fn statement<'a>(input: Input<'a>) -> IResult<Input<'a>, Statement<'a>, ParsingError<'a>> {
    terminated(
        alt((
            variables.map(Statement::Variables),
            delimited(
                token!(Token::Keyword(Keyword::If)),
                punctuated1(
                    terminated(expression, newline)
                        .and(block)
                        .map(|(condition, code)| IfBranch { condition, code }),
                    token!(Token::Keyword(Keyword::ElseIf)),
                )
                .map(strip_sep)
                .and(opt(preceded(
                    tuple((token!(Token::Keyword(Keyword::Else)), newline)),
                    block,
                )))
                .map(|(ifs, else_)| Statement::IfTree { ifs, else_ }),
                token!(Token::Keyword(Keyword::EndIf)),
            ),
            tuple((
                token!(Token::Keyword(Keyword::Set)),
                property,
                token!(Token::Keyword(Keyword::To)),
                expression,
            ))
            .map(|(_, var, _, to)| Statement::Set { var, to }),
            tuple((
                nvse_keyword!("let"),
                property,
                token!(
                    Token::Punct(Punct::ColonEqual) => None,
                    Token::Punct(Punct::PlusEqual) => Some(BinaryOp::Add),
                    Token::Punct(Punct::MinusEqual) => Some(BinaryOp::Sub),
                    Token::Punct(Punct::StarEqual) => Some(BinaryOp::Mul),
                    Token::Punct(Punct::SlashEqual) => Some(BinaryOp::Div),
                    Token::Punct(Punct::PercentEqual) => Some(BinaryOp::Mod),
                    Token::Punct(Punct::BarEqual) => Some(BinaryOp::BitOr),
                    Token::Punct(Punct::AmpEqual) => Some(BinaryOp::BitAnd),
                ),
                expression,
            ))
            .map(|(_, var, operator, value)| Statement::Let {
                assignment: operator,
                var,
                value,
            }),
            invoke.map(Statement::Invoke),
        )),
        newline,
    )(input)
}

pub fn block(input: Input) -> IResult<Input, Block, ParsingError> {
    many0(statement).map(Block).parse(input)
}

pub fn item<'a>(input: Input<'a>) -> IResult<Input<'a>, Item<'a>, ParsingError<'a>> {
    terminated(
        alt((
            variables.map(Item::Variables),
            delimited(
                token!(Token::Keyword(Keyword::Begin)),
                tuple((
                    ident,
                    terminated(
                        opt(spanned(alt((
                            token!(
                                Token::Literal(Literal::Int {
                                    value,
                                    ..
                                }) => Args::Const(*value),
                            ),
                            property.map(Args::Var),
                            delimited(
                                token!(Token::Punct(Punct::LeftBrace)),
                                many0(ident),
                                token!(Token::Punct(Punct::RightBrace)),
                            )
                            .map(Args::Parameters),
                        )))),
                        newline,
                    ),
                    block,
                )),
                token!(Token::Keyword(Keyword::End)),
            )
            .map(|(name, parameters, code)| {
                Item::Blocktype(Blocktype {
                    name,
                    args: parameters,
                    code,
                })
            }),
        )),
        newline,
    )
    .parse(input)
}

pub fn script<'a>(input: Input<'a>) -> IResult<Input<'a>, Script<'a>, ParsingError<'a>> {
    let (input, _) = opt(newline)(input)?;

    let (input, name) =
        delimited(token!(Token::Keyword(Keyword::ScriptName)), ident, newline)(input)?;

    let (input, items) = many0(item)(input)?;

    Ok((input, Script { name, items }))
}
