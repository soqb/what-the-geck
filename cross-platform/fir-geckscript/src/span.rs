use std::{
    fmt::{self, Debug, Display},
    ops::{Deref, DerefMut, Range},
};

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub fn lengthen(lhs: Self, rhs: Self) -> Span {
        Span {
            start: lhs.start.min(rhs.start),
            end: lhs.end.max(rhs.end),
        }
    }

    pub fn shorten(lhs: Self, rhs: Self) -> Span {
        Span {
            start: lhs.start.max(rhs.start),
            end: lhs.end.min(rhs.end),
        }
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Span {
            start: value.start,
            end: value.end,
        }
    }
}

impl From<rowan::TextRange> for Span {
    fn from(value: rowan::TextRange) -> Self {
        Span {
            start: value.start().into(),
            end: value.end().into(),
        }
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(value: Span) -> Self {
        miette::SourceSpan::new(value.start.into(), (value.end - value.start).into())
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(inner: T, span: impl Into<Span>) -> Self {
        Self {
            inner,
            span: span.into(),
        }
    }

    pub fn map_inner<U>(self, map: impl FnOnce(T) -> U) -> Spanned<U> {
        let Spanned { inner, span } = self;
        Spanned {
            inner: map(inner),
            span,
        }
    }

    pub fn as_deref<U: ?Sized>(&self) -> Spanned<&U>
    where
        T: Deref<Target = U>,
    {
        let Spanned { inner, span } = self;
        Spanned {
            inner: inner.deref(),
            span: *span,
        }
    }
}

impl<T> Spanned<Option<T>> {
    pub fn transpose(Spanned { inner, span }: Self) -> Option<Spanned<T>> {
        inner.map(|inner| Spanned { inner, span })
    }
}

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.inner, f)?;
        write!(f, " @ ")?;
        Debug::fmt(&self.span, f)
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.inner, f)
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T> From<Spanned<T>> for miette::SourceSpan {
    fn from(value: Spanned<T>) -> Self {
        value.span.into()
    }
}
