use core::fmt;
use std::{collections::BTreeMap, ops::Bound};

use fir::{Span, ToSpan};
use lsp_types::{Position, Range};

use crate::context::TextDocument;

pub struct SpanMap<T> {
    // todo: smallvec would be more efficient:
    inner: BTreeMap<usize, (usize, T)>,
    largest_ever_span: usize,
}

impl<T: fmt::Debug> fmt::Debug for SpanMap<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SpanMap ")?;
        f.debug_map()
            .entries(
                self.inner
                    .iter()
                    .map(|(&start, &(end, ref value))| ((start..end).to_span(), value)),
            )
            .finish()
    }
}

impl<T> Default for SpanMap<T> {
    fn default() -> Self {
        Self {
            inner: Default::default(),
            largest_ever_span: 0,
        }
    }
}

impl<T> SpanMap<T> {
    pub fn get_at(&self, idx: usize) -> Option<(Span, &T)> {
        let column = Span {
            start: idx,
            end: idx + 1,
        };
        self.iter_intersections(column).next()
    }

    pub fn iter_intersections<'a>(&'a self, span: Span) -> impl Iterator<Item = (Span, &'a T)> {
        self.inner
            .range((
                span.start
                    .checked_sub(self.largest_ever_span)
                    .map_or(Bound::Unbounded, Bound::Excluded),
                Bound::Excluded(span.end),
            ))
            .map(move |(&start, &(end, ref value))| ((start..end).to_span(), value))
            .skip_while(move |(entry_span, _)| entry_span.end <= span.start)
    }

    pub fn insert(&mut self, span: Span, value: T) -> Option<T> {
        assert_eq!(
            self.iter_intersections(span).count(),
            0,
            "spans should not overlap"
        );

        if span.len() > self.largest_ever_span {
            self.largest_ever_span = span.len();
        }

        self.inner
            .insert(span.start, (span.end, value))
            .map(|(_, value)| value)
    }

    pub fn remove_intersections(&mut self, span: Span) {
        self.inner
            .retain(move |&start, &mut (end, _)| !span.intersects((start..end).to_span()));
    }
}

impl TextDocument {
    pub fn position_to_offset(&self, position: Position) -> usize {
        self.line_lengths
            .iter()
            .take(position.line as usize)
            .sum::<usize>()
            + position.character as usize
    }

    pub fn offset_to_position(&self, mut offset: usize) -> Position {
        self.line_lengths
            .iter()
            .filter_map(|&len| match offset.checked_sub(len) {
                Some(remaining) => {
                    offset = remaining;
                    Some(offset)
                }
                None => None,
            })
            .enumerate()
            .last()
            .map_or_else(
                || Position {
                    line: 0,
                    character: offset as u32,
                },
                |(lines_passed, characters_remaining)| Position {
                    line: lines_passed as u32,
                    character: characters_remaining as u32,
                },
            )
    }

    pub fn range_to_span(&self, range: Range) -> Span {
        Span {
            start: self.position_to_offset(range.start),
            end: self.position_to_offset(range.end),
        }
    }

    pub fn span_to_range(&self, span: Span) -> Range {
        Range {
            start: self.offset_to_position(span.start),
            end: self.offset_to_position(span.end),
        }
    }
}
