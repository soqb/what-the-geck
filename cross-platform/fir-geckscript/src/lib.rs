mod diagnostics;
mod grammar;
mod impls;

pub mod lower;

#[cfg(test)]
mod tests;

pub use grammar::*;

pub fn parse<P: rust_sitter::Extract<O>, O>(
    old_syntax_tree: Option<&rust_sitter::tree_sitter::Tree>,
    src: &str,
) -> (
    rust_sitter::tree_sitter::Tree,
    O,
    impl Iterator<Item = diagnostics::TreeSitterParseError>,
) {
    use fir::ToSpan;
    let mut p = rust_sitter::tree_sitter::Parser::new();
    p.set_language(grammar::language()).unwrap();
    let tree = p.parse(src, old_syntax_tree).unwrap();

    let parsed =
        <P as rust_sitter::Extract<O>>::extract(Some(tree.root_node()), src.as_bytes(), 0, None);

    let mut errors = Vec::new();
    if tree.root_node().has_error() {
        rust_sitter::errors::collect_parsing_errors(&tree.root_node(), src.as_bytes(), &mut errors);
    }

    (
        tree,
        parsed,
        errors
            .into_iter()
            .map(|err| diagnostics::TreeSitterParseError {
                span: (err.start..err.end).to_span(),
            }),
    )
}

#[doc(hidden)]
pub mod utils_pain {

    use std::borrow::Cow;

    use crate::*;

    use fir::{Span, ToSpan};
    use rust_sitter::{
        errors::{ParseError, ParseErrorReason},
        tree_sitter,
    };

    use tree_sitter::Node;

    use thiserror::Error;

    pub fn miette<D: fir::Diagnostic>(d: D, src: &str) {
        // let handler = &miette::DebugReportHandler;
        let handler =
            &miette::GraphicalReportHandler::new_themed(miette::GraphicalTheme::unicode());
        println!(
            "{}",
            fir::miettify(&d, &src.to_string(), fir::DiagnosticFilter::default())
                .into_display(handler)
        );
    }

    fn report_miette(deez: Vec<ParseError>, src: &str) -> ! {
        crate::grammar::language();

        #[derive(miette::Diagnostic, Error, Debug)]
        #[error("parsing failed")]
        struct Printer {
            #[label("{reason:?}")]
            span: Span,
            reason: ParseErrorReason,
            #[source_code]
            src: String,
        }

        impl fir::Diagnostic for Printer {
            fn kind(&self) -> fir::DiagnosticKind {
                fir::DiagnosticKind::Syntax
            }
        }

        for dig in deez {
            let d = Printer {
                span: (dig.start..dig.end).to_span(),
                reason: dig.reason,
                src: src.to_owned(),
            };
            miette(d, src);
        }

        std::process::exit(1);
    }

    pub fn homebrew_parse(src: &str) -> Script {
        let mut p = tree_sitter::Parser::new();
        p.set_language(grammar::language()).unwrap();
        let tree = p.parse(src, None).unwrap();

        fn seek<'a>(c: &mut tree_sitter::TreeCursor<'_>, d: usize, src: &'a str) {
            use ::colored::Colorize;
            fn change_kind(node: Node<'_>) -> Option<Cow<'_, str>> {
                node.kind()
                    .split_once("_")
                    .map(|(_, b)| match b {
                        "unit" | "text" => None,
                        _ if b.bytes().nth(0).is_some_and(|c| matches!(c, b'A'..=b'Z')) => {
                            Some(Cow::Owned(format!("_::{b}")))
                        }
                        _ => Some(Cow::Borrowed(b)),
                    })
                    .unwrap_or_else(|| match node.kind() {
                        "Whitespace" | "ForgivingNewline" => None,
                        rest => Some(Cow::Borrowed(rest)),
                    })
            }
            loop {
                'me: {
                    let me = c.node();
                    match change_kind(me) {
                        Some(kind) => print!(
                            "\n{} ({field}{kind} is {src}",
                            " ".repeat(d),
                            kind = kind.yellow(),
                            field = c
                                .field_name()
                                .map(|f| format!("{f}: "))
                                .unwrap_or(String::new())
                                .bright_black(),
                            src = format!("{:?}", &src[me.start_byte()..me.end_byte()])
                                .bright_green()
                        ),
                        None => break 'me,
                    }

                    if c.goto_first_child() {
                        seek(c, d + 1, src)
                    }

                    print!(")");
                }

                if !c.goto_next_sibling() {
                    c.goto_parent();
                    break;
                }
            }
        }

        if tree.root_node().has_error() {
            let mut errors = Vec::new();
            rust_sitter::errors::collect_parsing_errors(
                &tree.root_node(),
                src.as_bytes(),
                &mut errors,
            );
            let mut c = tree.root_node().walk();
            seek(&mut c, 0, src);
            println!();
            report_miette(errors, src);
        }

        <Script as rust_sitter::Extract<_>>::extract(
            Some(tree.root_node()),
            src.as_bytes(),
            0,
            None,
        )
    }
}
