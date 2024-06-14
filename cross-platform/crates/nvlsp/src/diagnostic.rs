use core::fmt;
use std::{fmt::Write, iter};

use fir::{miettify, Diagnostic, DiagnosticFilter};

use lsp_types::{CodeAction, CodeActionKind, NumberOrString, Position, Range, Url, WorkspaceEdit};
use miette::{LabeledSpan, SourceCode, SourceSpan};
use rand::Rng;

struct Source(pub String);

impl SourceCode for Source {
    fn read_span<'a>(
        &'a self,
        span: &SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, miette::MietteError> {
        self.0
            .read_span(span, context_lines_before, context_lines_after)
            .or_else(|_| {
                Ok(Box::new(miette::MietteSpanContents::new(
                    &[],
                    SourceSpan::new(0.into(), 0),
                    0,
                    0,
                    0,
                )))
            })
    }
}

fn range_from_span(span: &SourceSpan, source: &dyn SourceCode) -> Range {
    let start = source
        .read_span(&SourceSpan::new(span.offset().into(), 0), 0, 0)
        .unwrap();
    let end = source
        .read_span(
            &SourceSpan::new((span.offset() + span.len()).into(), 0),
            0,
            0,
        )
        .unwrap();
    Range {
        start: Position {
            line: start.line() as u32,
            character: start.column() as u32,
        },
        end: Position {
            line: end.line() as u32,
            character: end.column() as u32,
        },
    }
}

fn severity_to_miette(sev: &impl miette::Diagnostic) -> Option<lsp_types::DiagnosticSeverity> {
    match sev.severity() {
        Some(miette::Severity::Advice) => Some(lsp_types::DiagnosticSeverity::HINT),
        Some(miette::Severity::Warning) => Some(lsp_types::DiagnosticSeverity::WARNING),
        Some(miette::Severity::Error) => Some(lsp_types::DiagnosticSeverity::ERROR),
        None => None,
    }
}

fn gen_hash(rng: &mut impl Rng) -> impl fmt::Display {
    struct HashBuf([char; 4]);
    impl fmt::Display for HashBuf {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "#")?;
            for &c in &self.0 {
                f.write_char(c)?;
            }
            Ok(())
        }
    }

    let buf = std::array::from_fn(|_| {
        const TABLE: &[char] = &[
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'b', 'c', 'd', 'f', 'g', 'h', 'j',
            'k', 'l', 'm', 'n', 'p', 'q', 'r', 's', 't', 'v', 'w', 'x', 'y', 'z',
        ];

        let i = rng.gen_range(0..TABLE.len());
        TABLE[i]
    });
    HashBuf(buf)
}

fn miette_to_lsp(
    miette: impl miette::Diagnostic,
    index_offset: usize,
    rng: &mut impl Rng,
    uri: &Url,
) -> Vec<lsp_types::Diagnostic> {
    fn ellipsis(predicate: bool) -> &'static str {
        predicate.then_some("...").unwrap_or("")
    }

    let source = miette.source_code().unwrap();

    let labels = miette.labels().unwrap_or_else(|| {
        // in the case where the error has no labels, we make one to ensure the memo is seen somehow.
        let one = LabeledSpan::new_primary_with_span(None, SourceSpan::new(0.into(), 1));
        Box::new(iter::once(one))
    });

    let hash = gen_hash(rng);
    let mut labels: Vec<_> = labels
        .filter(|label| label.inner().offset() != usize::MAX)
        .collect();
    labels.sort_unstable_by(|a, b| a.offset().cmp(&b.offset()));
    labels
        .iter()
        .enumerate()
        .map(|(sorted_idx, label)| {
            let severity = label
                .primary()
                .then(|| severity_to_miette(&miette))
                .flatten()
                .unwrap_or(lsp_types::DiagnosticSeverity::HINT);
            lsp_types::Diagnostic {
                range: range_from_span(label.inner(), source),
                severity: Some(severity),
                code: miette.code().map(|x| NumberOrString::String(x.to_string())),
                message: format!(
                    "({hash}) {title}{rest}",
                    title = miette,
                    rest = label
                        .label()
                        .map(|text| {
                            let head = ellipsis(sorted_idx != 0);
                            let tail = ellipsis(sorted_idx + 1 != labels.len());
                            format!("\n{head}{text}{tail}")
                        })
                        .unwrap_or_else(|| String::new())
                ),
                data: Some(index_offset.into()),
                related_information: (label.len() != 1).then(|| {
                    labels
                        .iter()
                        .map(|label| {
                            (
                                label.label().unwrap_or(""),
                                range_from_span(label.inner(), source),
                            )
                        })
                        .map(|(label, range)| lsp_types::DiagnosticRelatedInformation {
                            location: lsp_types::Location {
                                uri: uri.clone(),
                                range,
                            },
                            message: label.to_owned(),
                        })
                        .collect()
                }),
                ..Default::default()
            }
        })
        .collect()
}

pub fn diagnostic_to_lsp<'a>(
    diagnostic: &'a dyn Diagnostic,
    index_offset: usize,
    rng: &mut impl Rng,
    source: &'a str,
    uri: &'a Url,
    filter: DiagnosticFilter,
) -> Vec<lsp_types::Diagnostic> {
    miette_to_lsp(
        miettify(diagnostic, &Source(source.to_owned()), filter),
        index_offset,
        rng,
        uri,
    )
}

pub fn diagnostic_to_lsp_code_actions<'a>(
    reportable: &'a dyn Diagnostic,
) -> impl Iterator<Item = CodeAction> + 'a {
    let changes = std::collections::HashMap::new();
    reportable.code_actions().map(|action| CodeAction {
        title: action.title().to_string(),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: todo!(),
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            ..Default::default()
        }),
        command: None,
        is_preferred: Some(false),
        disabled: None,
        data: None,
    })
}
