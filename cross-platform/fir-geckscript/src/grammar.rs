#[rust_sitter::grammar("geckscript")]
mod grammar {
    use rust_sitter::Spanned;

    use super::*;

    #[rust_sitter::extra]
    #[rust_sitter::leaf(pattern = r"[\r\t\f\v ]+")]
    #[derive(Debug, Clone)]
    pub struct Whitespace;

    /// Awkward tokens that appear in fnv scripts without proper care, but have no meaning.
    #[rust_sitter::extra]
    #[rust_sitter::leaf(pattern = r"===+|---+|`+")]
    #[derive(Debug, Clone)]
    pub struct Gibberish;

    #[rust_sitter::prec(1)]
    #[rust_sitter::leaf(pattern = r"(?i)if")]
    #[derive(Debug, Clone)]
    pub struct KwIf;
    #[rust_sitter::leaf(pattern = r"(?i)elseif")]
    #[rust_sitter::prec(1)]
    #[derive(Debug, Clone)]
    pub struct KwElseIf;
    #[rust_sitter::leaf(pattern = r"(?i)else")]
    #[rust_sitter::prec(1)]
    #[derive(Debug, Clone)]
    pub struct KwElse;
    #[rust_sitter::leaf(pattern = r"(?i)endif")]
    #[rust_sitter::prec(1)]
    #[derive(Debug, Clone)]
    pub struct KwEndIf;
    #[rust_sitter::leaf(pattern = r"(?i)return")]
    #[rust_sitter::prec(1)]
    #[derive(Debug, Clone)]
    pub struct KwReturn;
    #[rust_sitter::leaf(pattern = r"(?i)set")]
    #[rust_sitter::prec(1)]
    #[derive(Debug, Clone)]
    pub struct KwSet;
    #[rust_sitter::leaf(pattern = r"(?i)to")]
    #[rust_sitter::prec(1)]
    #[derive(Debug, Clone)]
    pub struct KwTo;
    #[rust_sitter::leaf(pattern = r"(?i)begin")]
    #[rust_sitter::prec(1)]
    #[derive(Debug, Clone)]
    pub struct KwBegin;
    #[rust_sitter::leaf(pattern = r"(?i)end")]
    #[rust_sitter::prec(1)]
    #[derive(Debug, Clone)]
    pub struct KwEnd;
    #[rust_sitter::leaf(pattern = r"(?i)let")]
    #[rust_sitter::prec(1)]
    #[derive(Debug, Clone)]
    pub struct KwLet;
    #[rust_sitter::leaf(pattern = r"(?i)scn|scriptname")]
    #[rust_sitter::prec(1)]
    #[derive(Debug, Clone)]
    pub struct KwScriptName;
    #[rust_sitter::leaf(pattern = r"\(")]
    #[derive(Debug, Clone)]
    pub struct LeftParen;
    #[rust_sitter::leaf(pattern = r"\)")]
    #[derive(Debug, Clone)]
    pub struct RightParen;

    #[derive(Debug, Clone)]
    pub struct Newline {
        #[rust_sitter::leaf(pattern = r";.*", transform = str::to_owned)]
        pub comment: Option<String>,
        #[rust_sitter::leaf(text = "\n")]
        pub line_feed: (),
    }

    // see `src/grammar-patch.json` for implementation details.
    #[derive(Clone)]
    #[rust_sitter::leaf(text = "~~FILLED IN BY POSTPROCESSING~~")]
    pub struct ForgivingNewline;

    #[derive(Debug, Clone)]
    pub struct Ident {
        #[rust_sitter::leaf(pattern = r"(?i)[0-9]*[a-z_][a-z0-9_]*", transform = str::to_owned)]
        pub text: Spanned<String>,
    }

    #[derive(Debug, Clone)]
    pub struct Header {
        pub kw_script_name: KwScriptName,
        pub name: Ident,
        pub newline: ForgivingNewline,
    }

    #[rust_sitter::language]
    #[derive(Debug, Clone)]
    pub struct Script {
        pub leading_newlines: Vec<Newline>,
        pub header: Option<Header>,
        pub items: Vec<Spanned<Item>>,
    }

    #[derive(Debug, Clone)]
    pub enum ItemKind {
        Variables(Variables),
        EventImpl(EventImpl),
    }

    #[derive(Debug, Clone)]
    pub struct Item {
        pub kind: ItemKind,
        pub newline: ForgivingNewline,
    }

    #[rust_sitter::prec(1)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum VarTy {
        #[rust_sitter::leaf(pattern = r"(?i)int")]
        Int,
        #[rust_sitter::leaf(pattern = r"(?i)short")]
        Short,
        #[rust_sitter::leaf(pattern = r"(?i)long")]
        Long,
        #[rust_sitter::leaf(pattern = r"(?i)float")]
        Float,
        #[rust_sitter::leaf(pattern = r"(?i)ref")]
        Ref,
    }

    #[derive(Debug, Clone)]
    pub struct Variables {
        pub ty: Spanned<VarTy>,
        #[rust_sitter::repeat(non_empty = true)]
        pub names: Vec<Ident>,
    }

    #[derive(Debug, Clone)]
    pub struct EventImpl {
        pub kw_begin: KwBegin,
        pub name: Ident,
        pub args: Option<Spanned<EventArgs>>,
        pub block: EndBlock,
    }

    #[derive(Debug, Clone)]
    pub enum InvokeArg {
        Constant(Literal),
        Reference(Reference),
        UnaryOp {
            operator: Spanned<UnaryOp>,
            inner: Spanned<Expression>,
        },
        Inline {
            left_paren: LeftParen,
            inner: Spanned<Expression>,
            right_paren: RightParen,
        },
    }

    #[rust_sitter::prec_left(1)]
    #[derive(Debug, Clone)]
    pub struct Invoke {
        pub referent: Reference,
        #[rust_sitter::prec_left(0)]
        pub args: Vec<Spanned<InvokeArg>>,
    }

    #[derive(Debug, Clone)]
    pub struct PathSegment {
        #[rust_sitter::leaf(text = ".")]
        pub dot: (),
        pub field: Ident,
    }

    #[derive(Debug, Clone)]
    pub struct Reference {
        pub head: Ident,
        pub path: Vec<PathSegment>,
    }

    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum UnaryOp {
        #[rust_sitter::leaf(text = "-")]
        Neg,
        #[rust_sitter::leaf(text = "$")]
        ToString,
        #[rust_sitter::leaf(text = "#")]
        ToNumber,
        #[rust_sitter::leaf(text = "*")]
        Deref,
        #[rust_sitter::leaf(text = "&")]
        Box,
        #[rust_sitter::leaf(text = "!")]
        Not,
    }

    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum BinaryOp {
        #[rust_sitter::leaf(text = "||")]
        #[rust_sitter::prec_left(1)]
        Or,

        #[rust_sitter::leaf(text = "&&")]
        #[rust_sitter::prec_left(2)]
        And,

        #[rust_sitter::leaf(text = "==")]
        #[rust_sitter::prec_left(4)]
        Eq,
        #[rust_sitter::leaf(text = "!=")]
        #[rust_sitter::prec_left(4)]
        Neq,

        #[rust_sitter::leaf(text = ">")]
        #[rust_sitter::prec_left(5)]
        Greater,
        #[rust_sitter::leaf(text = "<")]
        #[rust_sitter::prec_left(5)]
        Lesser,
        #[rust_sitter::leaf(text = ">=")]
        #[rust_sitter::prec_left(5)]
        GreaterOrEqual,
        #[rust_sitter::leaf(text = "<=")]
        #[rust_sitter::prec_left(5)]
        LesserOrEqual,

        #[rust_sitter::leaf(text = "|")]
        #[rust_sitter::prec_left(6)]
        BitOr,

        #[rust_sitter::leaf(text = "&")]
        #[rust_sitter::prec_left(7)]
        BitAnd,
        #[rust_sitter::leaf(text = ">>")]
        #[rust_sitter::prec_left(8)]
        LeftShift,
        #[rust_sitter::leaf(text = "<<")]
        #[rust_sitter::prec_left(8)]
        RightShift,

        #[rust_sitter::leaf(text = "+")]
        #[rust_sitter::prec_left(9)]
        Add,
        #[rust_sitter::leaf(text = "-")]
        #[rust_sitter::prec_left(9)]
        Sub,

        #[rust_sitter::leaf(text = "*")]
        #[rust_sitter::prec_left(10)]
        Mul,
        #[rust_sitter::leaf(text = "/")]
        #[rust_sitter::prec_left(10)]
        Div,
        #[rust_sitter::leaf(text = "%")]
        #[rust_sitter::prec_left(10)]
        Mod,
        #[rust_sitter::leaf(text = "^")]
        #[rust_sitter::prec_left(11)]
        Pow,
    }

    #[derive(Debug, Clone)]
    pub enum Literal {
        #[rust_sitter::prec(1)]
        DecInt(#[rust_sitter::leaf(pattern = r"\d+", transform = dec_int_parse)] IntRepr),

        #[rust_sitter::prec(2)]
        HexInt(#[rust_sitter::leaf(pattern = r"0[xX]\d+", transform = hex_int_parse)] IntRepr),

        #[rust_sitter::prec(2)]
        Float(#[rust_sitter::leaf(pattern = r"\d+\.\d+", transform = float_parse)] FloatRepr),
        String(#[rust_sitter::leaf(pattern = r#"".*?""#, transform = str::to_owned)] String),
    }

    #[derive(Debug, Clone)]
    pub enum Expression {
        Constant(Literal),
        Invoke(Invoke),
        Group {
            left_paren: LeftParen,
            inner: Box<Spanned<Expression>>,
            right_paren: RightParen,
        },
        #[rust_sitter::prec_left(1)]
        BinaryOp {
            lhs: Box<Spanned<Expression>>,
            operator: Spanned<BinaryOp>,
            rhs: Box<Spanned<Expression>>,
        },
        #[rust_sitter::prec(2)]
        UnaryOp {
            operator: Spanned<UnaryOp>,
            inner: Box<Spanned<Expression>>,
        },
    }

    #[derive(Debug, Clone)]
    pub struct Else {
        pub kw_else: Spanned<KwElse>,
        pub leading_newline: ForgivingNewline,
        pub statements: Vec<Statement>,
        pub kw_end_if: KwEndIf,
    }

    #[derive(Debug, Clone)]
    pub struct ElseIf {
        pub kw_elseif: Spanned<KwElseIf>,
        pub condition: Spanned<Expression>,
        pub leading_newline: ForgivingNewline,
        pub statements: Vec<Statement>,
        pub next: Box<NextIf>,
    }

    #[derive(Debug, Clone)]
    pub struct IfChain {
        pub kw_if: Spanned<KwIf>,
        pub condition: Spanned<Expression>,
        pub leading_newline: ForgivingNewline,
        pub statements: Vec<Statement>,
        pub next: Box<NextIf>,
    }

    #[derive(Debug, Clone)]
    pub enum NextIf {
        ElseIf(ElseIf),
        Else(Else),
        End(KwEndIf),
    }

    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum AssignOp {
        #[rust_sitter::leaf(text = ":=")]
        Set,
        #[rust_sitter::leaf(text = "+=")]
        Add,
        #[rust_sitter::leaf(text = "-=")]
        Sub,
        #[rust_sitter::leaf(text = "*=")]
        Mul,
        #[rust_sitter::leaf(text = "/=")]
        Div,
        #[rust_sitter::leaf(text = "^=")]
        Pow,
        #[rust_sitter::leaf(text = "|=")]
        Or,
        #[rust_sitter::leaf(text = "&=")]
        And,
        #[rust_sitter::leaf(text = "%=")]
        Mod,
    }

    #[derive(Debug, Clone)]
    pub enum StatementKind {
        If(IfChain),
        Set {
            kw_set: KwSet,
            var: Reference,
            kw_to: KwTo,
            value: Spanned<Expression>,
        },
        Variables(Variables),
        Let {
            kw_let: KwLet,
            var: Reference,
            assignment: Spanned<AssignOp>,
            value: Spanned<Expression>,
        },
        Return(KwReturn),
        Expression(Expression),
    }

    #[derive(Debug, Clone)]
    pub struct Statement {
        pub kind: Spanned<StatementKind>,
        pub newline: ForgivingNewline,
    }

    #[derive(Debug, Clone)]
    pub struct EndBlock {
        pub leading_newline: ForgivingNewline,
        pub statements: Vec<Statement>,
        pub end: KwEnd,
    }

    #[derive(Debug, Clone)]
    pub enum EventArgs {
        Const(#[rust_sitter::leaf(pattern = r"\d+", transform = dec_int_parse)] IntRepr),
        Var(Reference),
        // Parameters(Vec<Ident>),
    }
}
pub use grammar::*;

#[derive(Debug, Clone)]
pub enum FloatRepr {
    Good(f64),
    Bad(String),
}

#[derive(Debug, Clone)]
pub enum IntRepr {
    Good(i64),
    Bad(String),
}

fn float_parse(text: &str) -> FloatRepr {
    match fast_float::parse(text) {
        Ok(f) => FloatRepr::Good(f),
        Err(_) => FloatRepr::Bad(text.to_owned()),
    }
}
fn dec_int_parse(text: &str) -> IntRepr {
    match i64::from_str_radix(text, 10) {
        Ok(n) => IntRepr::Good(n),
        Err(_) => IntRepr::Bad(text.to_owned()),
    }
}
fn hex_int_parse(text: &str) -> IntRepr {
    match i64::from_str_radix(&text[2..text.len() - 2], 16) {
        Ok(n) => IntRepr::Good(n),
        Err(_) => IntRepr::Bad(text.to_owned()),
    }
}
