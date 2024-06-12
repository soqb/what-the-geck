use core::fmt;
use core::ops::Deref;
use std::hash::Hash;
use std::iter::Sum;
use std::ops::Range;

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub const NOWHERE: Self = Self {
        start: usize::MAX,
        end: usize::MAX,
    };

    pub fn len(self) -> usize {
        self.end - self.start
    }

    pub fn of<T>(self, t: T) -> Spanned<T> {
        Spanned::new(t, self)
    }

    pub fn column(idx: usize) -> Self {
        Self {
            start: idx,
            end: idx,
        }
    }

    pub fn start_column(self) -> Self {
        Self::column(self.start)
    }

    pub fn end_column(self) -> Self {
        Self::column(self.end)
    }

    pub fn expand_to_include(self, other: Span) -> Self {
        Self {
            start: usize::min(self.start, other.start),
            end: usize::max(self.end, other.end),
        }
    }

    pub fn contains(self, other: Span) -> bool {
        self.start <= other.start && self.end >= other.end
    }

    pub fn intersects(self, other: Span) -> bool {
        self.start <= other.start || self.end >= other.end
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(value: Span) -> Self {
        miette::SourceSpan::new(value.start.into(), (value.end - value.start).into())
    }
}

impl<T> From<Spanned<T>> for miette::SourceSpan {
    fn from(value: Spanned<T>) -> Self {
        value.to_span().into()
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{start}..{end}", start = self.start, end = self.end)
    }
}

#[derive(Copy, Clone, Eq)]
pub struct Spanned<T> {
    inner: T,
    span: Span,
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl<T> Spanned<T> {
    pub fn new(inner: T, span: impl ToSpan) -> Self {
        Self {
            inner,
            span: span.to_span(),
        }
    }

    pub fn span(spanner: &Self) -> Span {
        spanner.span
    }

    pub fn inner(&self) -> &T {
        &self.inner
    }

    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    pub fn into_inner(self) -> T {
        self.inner
    }

    pub fn span_map<U>(self, mapper: impl FnOnce(T) -> U) -> Spanned<U> {
        let span = self.to_span();
        Spanned::new(mapper(self.into_inner()), span)
    }

    pub fn as_deref(&self) -> Spanned<&T::Target>
    where
        T: Deref,
    {
        Spanned::new(&self.inner, self.span)
    }

    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned::new(&self.inner, self.span)
    }
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)?;
        write!(f, " (")?;
        fmt::Debug::fmt(&self.span, f)?;
        write!(f, ")")?;
        Ok(())
    }
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.inner(), f)
    }
}

pub trait ToSpan {
    fn to_span(&self) -> Span;
}

impl ToSpan for Span {
    fn to_span(&self) -> Span {
        *self
    }
}

impl<T: ToSpan> ToSpan for &T {
    fn to_span(&self) -> Span {
        (&**self).to_span()
    }
}

impl<T> ToSpan for Spanned<T> {
    fn to_span(&self) -> Span {
        self.span
    }
}

impl ToSpan for (usize, usize) {
    fn to_span(&self) -> Span {
        Span {
            start: self.0,
            end: self.1,
        }
    }
}

impl ToSpan for Range<usize> {
    fn to_span(&self) -> Span {
        Span {
            start: self.start,
            end: self.end,
        }
    }
}

impl<const N: usize> ToSpan for [Span; N] {
    fn to_span(&self) -> Span {
        self.iter().copied().sum()
    }
}

impl Sum for Span {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(|a, b| a.expand_to_include(b))
            .unwrap_or(Span::NOWHERE)
    }
}
