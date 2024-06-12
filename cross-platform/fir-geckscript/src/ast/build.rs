use rowan::NodeOrToken;
use rowwin::{Language, RowanLang};

use crate::{
    parse::*,
    span::{Span, Spanned},
};

use super::*;

pub type SyntaxNode = rowan::SyntaxNode<RowanLang<Lang>>;
pub type SyntaxToken = rowan::SyntaxToken<RowanLang<Lang>>;
pub type SyntaxElement = rowan::SyntaxElement<RowanLang<Lang>>;

impl From<SyntaxToken> for Spanned<Token> {
    fn from(value: SyntaxToken) -> Self {
        Spanned::new(value.kind(), value.text_range())
    }
}

impl From<SyntaxNode> for Spanned<Token> {
    fn from(value: SyntaxNode) -> Self {
        Spanned::new(value.kind(), value.text_range())
    }
}

#[derive(Debug, Clone)]
struct Children {
    child: Option<SyntaxElement>,
    total_span: Span,
    total: usize,
}

impl Children {
    pub fn total_span(&self) -> Span {
        self.total_span
    }

    pub fn from_node_children(node: &SyntaxNode) -> Self {
        Self {
            child: node.first_child_or_token(),
            total_span: node.text_range().into(),
            total: node
                .children_with_tokens()
                .filter(|child| token_is_meaningful(child.kind()))
                .count(),
        }
    }

    fn is_empty(&self) -> bool {
        !self.clone().any(|t| token_is_meaningful(t.kind()))
    }

    fn next_raw(&mut self) -> Option<SyntaxElement> {
        self.child
            .as_mut()
            .and_then(|child| {
                child
                    .next_sibling_or_token()
                    .map(|sibling| std::mem::replace(child, sibling))
            })
            .or_else(|| self.child.take())
    }

    // pub fn skip_trivia(&mut self) {
    //     loop {
    //         match self.next_raw() {
    //             Some(child) if !token_is_meaningful(child.kind()) => continue,
    //             Some(child) => {
    //                 // undo consuming the meaningful token; we don't want to skip it!
    //                 self.child = Some(child);
    //                 break;
    //             }
    //             None => break,
    //         }
    //     }
    // }
}

pub fn token_is_meaningful(token: Token) -> bool {
    !Lang::is_trivial(token) && !matches!(token, Token::Newline | Token::Error)
}

impl Iterator for Children {
    type Item = SyntaxElement;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let raw = self.next_raw();
            match raw {
                Some(child) if token_is_meaningful(child.kind()) => {
                    break Some(child);
                }
                Some(_) => continue,
                None => break None,
            }
        }
    }
}

#[derive(Debug)]
struct Void<const TOKEN: u16>;

impl<const TOKEN: u16> FromChild for Void<TOKEN> {
    fn visit_token(token: SyntaxToken) -> Result<Self, AstError> {
        if token.kind() as u16 == TOKEN {
            Ok(Self)
        } else {
            Err(AstError::Mismatched {
                found: token.into(),
                expected: Lang::kind_from_raw(rowan::SyntaxKind(TOKEN)),
            })
        }
    }
}

macro_rules! void {
    ($token:expr) => {
        Void::<{ $token as u16 }>
    };
}

#[derive(Debug)]
struct Text<const TOKEN: u16>(String);

impl<const TOKEN: u16> FromChild for Text<TOKEN> {
    fn visit_token(token: SyntaxToken) -> Result<Self, AstError> {
        if token.kind() as u16 == TOKEN {
            Ok(Self(token.text().to_owned()))
        } else {
            Err(AstError::Mismatched {
                found: token.into(),
                expected: Lang::kind_from_raw(rowan::SyntaxKind(TOKEN)),
            })
        }
    }
}

macro_rules! text {
    ($ident:ident) => {
        Text($ident)
    };
    ($token:expr) => {
        Text::<{ $token as u16 }>
    };
    ($ident:ident @ $token:expr) => {
        Text::<{ $token as u16 }>($ident)
    };
}

trait FromChild: Sized {
    fn visit_token(token: SyntaxToken) -> Result<Self, AstError> {
        Err(AstError::UnexpectedKind {
            token: token.into(),
            kind: NodeOrToken::Token(()),
            note: std::any::type_name::<Self>().to_owned(),
        })
    }

    fn visit_node(node: SyntaxNode) -> Result<Self, AstError> {
        Err(AstError::UnexpectedKind {
            token: node.into(),
            kind: NodeOrToken::Node(()),
            note: std::any::type_name::<Self>().to_owned(),
        })
    }

    fn visit_child(child: SyntaxElement) -> Result<Self, AstError> {
        match child {
            NodeOrToken::Node(node) => Self::visit_node(node),
            NodeOrToken::Token(token) => Self::visit_token(token),
        }
    }
}

impl<T: FromChild> FromChild for Spanned<T> {
    fn visit_token(token: SyntaxToken) -> Result<Self, AstError> {
        let span = token.text_range();
        T::visit_token(token).map(|t| Spanned::new(t, span))
    }

    fn visit_node(node: SyntaxNode) -> Result<Self, AstError> {
        T::visit_node(node.clone()).map(|t| {
            let range = node.text_range();
            Spanned::new(t, range)
        })
    }
}

trait FromChildren: Sized {
    fn visit_children(children: &mut Children) -> Result<Self, AstError>;

    fn visit_children_exact(children: &mut Children) -> Result<Self, AstError> {
        let ret = Self::visit_children(children)?;
        if children.is_empty() {
            Ok(ret)
        } else {
            Err(AstError::TooManyChildren {
                span: children.total_span(),
                expected: children.total - 1,
                found: children.total,
            })
        }
    }

    fn visit_children_of(node: &SyntaxNode) -> Result<Self, AstError> {
        Self::visit_children_exact(&mut Children::from_node_children(node))
    }
}

impl<T: FromChild> FromChildren for T {
    fn visit_children(children: &mut Children) -> Result<Self, AstError> {
        // children.skip_trivia();
        let Some(child) = children.next() else {
            return Err(AstError::TooFewChildren {
                span: children.total_span(),
                expected: children.total + 1,
                found: children.total,
            })
        };
        T::visit_child(child)
    }
}

impl<T: FromChildren> FromChildren for Option<T> {
    fn visit_children(children: &mut Children) -> Result<Self, AstError> {
        let mut ch = children.clone();
        match T::visit_children(&mut ch) {
            Err(_) => Ok(None),
            Ok(value) => {
                *children = ch;
                Ok(Some(value))
            }
        }
    }
}

impl<T: FromChildren> FromChildren for Vec<T> {
    fn visit_children(children: &mut Children) -> Result<Self, AstError> {
        let mut vec = Vec::new();
        while let Some(next) = Option::visit_children(children)? {
            vec.push(next);
        }

        Ok(vec)
    }

    fn visit_children_exact(children: &mut Children) -> Result<Self, AstError> {
        let mut vec = Vec::new();
        loop {
            if children.is_empty() {
                break;
            }
            vec.push(T::visit_children(children)?);
        }
        Ok(vec)
    }
}

macro_rules! impl_children_tuple {
    (__impl  $pl:ident $($p:ident)*) => {
        impl<$($p: FromChildren,)* $pl: FromChildren> FromChildren for ($($p,)* $pl,) {
            fn visit_children(children: &mut Children) -> Result<Self, AstError> {
                Ok((
                    $(<$p as FromChildren>::visit_children(children)?,)*
                    <$pl as FromChildren>::visit_children(children)?
,
                ))
            }

            fn visit_children_exact(children: &mut Children) -> Result<Self, AstError> {
                Ok((
                    $(<$p as FromChildren>::visit_children(children)?,)*
                    <$pl as FromChildren>::visit_children_exact(children)?,
                ))
            }
        }
    };
    () => {};
    ($p1:ident $($pr:ident)*) => {
        impl_children_tuple!($($pr)*);
        impl_children_tuple!(__impl $p1 $($pr)*);
    };
}

impl_children_tuple!(A B C D E F G H I J K L);

macro_rules! alt {
    ($name:ident { $($pat:pat => $expr:expr),+ $(,)? }) => {
        Ok(match $name.kind() {
            $($pat => $expr,)+
            #[allow(unreachable_patterns)]
            _ => {
                return Err(AstError::Unexpected {
                    found: Spanned::new(
                        $name.kind(),
                        $name.text_range(),
                    ),
                    note: format!("{}:{}", line!(), column!()),
                })
            }
        })
    };
}

impl FromChild for BinaryOp {
    fn visit_token(token: SyntaxToken) -> Result<Self, AstError> {
        alt!(token {
            Token::Bar2 => BinaryOp::Or,
            Token::Amp2 => BinaryOp::And,
            Token::Equal2 => BinaryOp::Eq,
            Token::BangEqual => BinaryOp::Neq,
            Token::RightAngle => BinaryOp::Greater,
            Token::LeftAngle => BinaryOp::Lesser,
            Token::RightAngleEqual => BinaryOp::GreaterOrEqual,
            Token::LeftAngleEqual => BinaryOp::LesserOrEqual,
            Token::Bar => BinaryOp::BitOr,
            Token::Amp => BinaryOp::BitAnd,
            Token::Plus => BinaryOp::Add,
            Token::Minus => BinaryOp::Sub,
            Token::Slash => BinaryOp::Div,
            Token::Star => BinaryOp::Mul,
            Token::Percent => BinaryOp::Mod,
            Token::Caret => BinaryOp::Pow,
        })
    }
}

impl FromChild for Property {
    fn visit_node(node: SyntaxNode) -> Result<Self, AstError> {
        alt!(node {
            Token::Property => {
                let (referent, path): (
                    Spanned<text!(Token::Identifier)>,
                    Option<(
                        void!(Token::Dot),
                        _,
                    )>
                ) = FromChildren::visit_children_of(&node)?;
                Property {
                    referent: referent.map_inner(|Text(text)| text),
                    path: path.map(|(_, Spanned { inner: text!(text @ Token::Identifier), span })| vec![Spanned::new(text, span)]).unwrap_or_default()
                }
            },
        })
    }
}

impl FromChild for Literal {
    fn visit_token(token: SyntaxToken) -> Result<Self, AstError> {
        alt!(token {
            Token::StringLiteral => {
                let text = token.text();
                Literal::String(text[1..text.len() - 1].to_string())
            },
            Token::FloatLiteral => Literal::Float(token.text().parse()?),
            Token::IntLiteral => Literal::Int {
                value: token.text().parse()?,
                radix: IntRadix::Decimal,
            },
        })
    }

    fn visit_node(node: SyntaxNode) -> Result<Self, AstError> {
        alt!(node {
            Token::FloatLiteral => {
                let (void!(Token::Dot), text!(text @ Token::IntLiteral)) = FromChildren::visit_children_of(&node)?;
                Literal::Float(format!(".{text}").parse()?)
            }
        })
    }
}

impl FromChild for UnaryOp {
    fn visit_token(token: SyntaxToken) -> Result<Self, AstError> {
        alt!(token {
            Token::Minus => UnaryOp::Neg,
            Token::Star => UnaryOp::Deref,
            Token::Amp => UnaryOp::Box,
            Token::Dollar => UnaryOp::ToString,
            Token::Hash => UnaryOp::ToNumber,
            Token::Bang => UnaryOp::Not,
        })
    }
}

impl FromChild for InvokeArg {
    fn visit_token(token: SyntaxToken) -> Result<Self, AstError> {
        FromChild::visit_token(token).map(InvokeArg::Constant)
    }

    fn visit_node(node: SyntaxNode) -> Result<Self, AstError> {
        FromChild::visit_node(node.clone()).map(InvokeArg::Constant)
            .or_else(|_| FromChild::visit_node(node.clone()).map(InvokeArg::Var))
            .or_else(|_| alt!(node {
                Token::Group => {
                    let (void!(Token::LeftParen), expression, void!(Token::RightParen)) = FromChildren::visit_children_of(&node)?;
                    InvokeArg::Inline(expression)
                },
                Token::UnaryOp => {
                    let (operator, inner) = FromChildren::visit_children_of(&node)?;
                    InvokeArg::UnaryOp { operator, inner }
                },
            }))
    }
}

impl FromChild for Invoke {
    fn visit_node(node: SyntaxNode) -> Result<Self, AstError> {
        alt!(node {
            Token::Invoke => {
                let (name, args) = FromChildren::visit_children_of(&node)?;
                Invoke { name, args }
            },
        })
    }
}

impl FromChild for Expression {
    fn visit_token(token: SyntaxToken) -> Result<Expression, AstError> {
        FromChild::visit_token(token).map(Expression::Constant)
    }

    fn visit_node(node: SyntaxNode) -> Result<Self, AstError> {
        FromChild::visit_node(node.clone()).map(Expression::Constant)
            .or_else(|_| FromChild::visit_node(node.clone()).map(Expression::Invoke))
            .or_else(|_| alt!(node {
                Token::BinaryOp => {
                    let (lhs, operator, rhs) = FromChildren::visit_children_of(&node)?;
                    Expression::BinaryOp {
                        operator,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }
                },
                Token::UnaryOp => {
                    let (operator, inner) = FromChildren::visit_children_of(&node)?;
                    Expression::UnaryOp { operator, inner: Box::new(inner) }
                },
                Token::Group => {
                    let (void!(Token::LeftParen), expression, void!(Token::RightParen)) = FromChildren::visit_children_of(&node)?;
                    Expression::Group(Box::new(expression))
                },
            }))
    }
}

impl FromChild for VarTy {
    fn visit_token(token: SyntaxToken) -> Result<Self, AstError> {
        alt!(token {
            Token::Short => VarTy::Short,
            Token::Int => VarTy::Int,
            Token::Long => VarTy::Long,
            Token::Float => VarTy::Float,
            Token::Ref => VarTy::Ref,
        })
    }
}

impl FromChild for Variables {
    fn visit_node(node: SyntaxNode) -> Result<Self, AstError> {
        alt!(node {
            Token::Variables => {
                let (ty, Spanned { inner: text!(name @ Token::Identifier), span }) = FromChildren::visit_children_of(&node)?;
                Variables {
                    names: vec![Spanned::new(name, span)],
                    ty
                }
            }
        })
    }
}

impl FromChild for IfBranch {
    fn visit_node(node: SyntaxNode) -> Result<Self, AstError> {
        alt!(node {
            Token::IfBranch => {
                let (condition, code) = FromChildren::visit_children_of(&node)?;
                IfBranch { condition, code }
            }
        })
    }
}

impl FromChild for Statement {
    fn visit_token(token: SyntaxToken) -> Result<Self, AstError> {
        FromChild::visit_token(token.clone())
            .map(Statement::Expression)
            .or_else(|_| {
                alt!(token {
                    Token::Return => Statement::Return,
                })
            })
    }
    fn visit_node(node: SyntaxNode) -> Result<Self, AstError> {
        FromChild::visit_node(node.clone()).map(Statement::Expression)
            .or_else(|_| FromChild::visit_node(node.clone()).map(Statement::Variables))
            .or_else(|_| alt!(node {
                Token::SetTo => {
                    let (void!(Token::Set), var, void!(Token::To), to) = FromChildren::visit_children_of(&node)?;
                    Statement::Set { var, to }
                },
                Token::IfTree => {
                    let (void!(Token::If), first, ifs, else_, void!(Token::EndIf)): (
                        _,
                        _,
                        Vec<(void!(Token::ElseIf), _)>,
                        Option<(void!(Token::Else), _)>,
                        _
                    ) = FromChildren::visit_children_of(&node)?;
                    let ifs: Vec<_> = std::iter::once(first)
                        .chain(ifs.into_iter().map(|(_, branch)| branch))
                        .collect();
                    Statement::IfTree { ifs, else_: else_.map(|(_, block)| block) }
                },
            })
        )
    }
}

impl FromChild for Block {
    fn visit_node(node: SyntaxNode) -> Result<Self, AstError> {
        alt!(node {
            Token::Block => {
                let stmts = FromChildren::visit_children_of(&node)?;
                Block(stmts)
            }
        })
    }
}

impl FromChild for Args {
    fn visit_token(token: SyntaxToken) -> Result<Self, AstError> {
        alt!(token {
            Token::IntLiteral => Args::Const(token.text().parse()?),
        })
    }

    fn visit_node(node: SyntaxNode) -> Result<Self, AstError> {
        FromChild::visit_node(node).map(Self::Var)
    }
}

impl FromChild for Blocktype {
    fn visit_node(node: SyntaxNode) -> Result<Self, AstError> {
        alt!(node {
            Token::Blocktype => {
                let (
                    void!(Token::Begin),
                    Spanned { inner: text!(name @ Token::Identifier), span },
                    args,
                    block,
                    void!(Token::End),
                ): (_, _, Option<_>, _, _) = FromChildren::visit_children_of(&node)?;
                Blocktype {
                    name: Spanned::new(name, span),
                    args,
                    code: block,
                }
            }
        })
    }
}

impl FromChild for Item {
    fn visit_node(node: SyntaxNode) -> Result<Self, AstError> {
        FromChild::visit_node(node.clone())
            .map(Item::Variables)
            .or_else(|_| FromChild::visit_node(node).map(Item::Blocktype))
    }
}

impl FromChild for Script {
    fn visit_node(node: SyntaxNode) -> Result<Self, AstError> {
        alt!(node {
            Token::Script => {
                let (void!(Token::ScriptName), text!(name @ Token::Identifier), items) = FromChildren::visit_children_of(&node)?;
                Script {
                    items,
                    name,
                }
            }
        })
    }
}

pub fn build_block(node: SyntaxNode) -> Result<Block, AstError> {
    FromChildren::visit_children_of(&node)
}

pub fn build_script(node: SyntaxNode) -> Result<Script, AstError> {
    FromChild::visit_node(node)
}
