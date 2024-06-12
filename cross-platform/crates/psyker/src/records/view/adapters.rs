use std::{
    io::{Read, Seek},
    marker::PhantomData,
};

use binrw::{io::TakeSeek, BinResult};

use crate::records::Span;

use super::{parsers::Parser, FieldsView};

pub trait FieldAdapter<'a, 'b, R, P>
where
    'a: 'b,
    R: Read + Seek + 'a,
{
    type ParserInput;

    fn adapt(self, view: &'b mut FieldsView<'a, R>) -> BinResult<Self::ParserInput>;
}

pub struct ReadIn<'a, R>(pub TakeSeek<&'a mut R>);

impl<'a, 'b, R, P> FieldAdapter<'a, 'b, R, P> for &'static str
where
    'a: 'b,
    R: Read + Seek + 'a,
{
    type ParserInput = ReadIn<'b, R>;

    fn adapt(self, view: &'b mut FieldsView<'a, R>) -> BinResult<Self::ParserInput> {
        view.field(self)
    }
}

pub struct JoinIn<'a, R, const N: usize> {
    reader: &'a mut R,
    span_lists: [std::vec::IntoIter<Span>; N],
}

impl<'a, R, const N: usize> JoinIn<'a, R, N>
where
    R: Read + Seek + 'a,
{
    pub fn lending_len_of(&self, index: usize) -> usize {
        self.span_lists[index].len()
    }

    pub fn borrow_next_of(&mut self, index: usize) -> Option<BinResult<ReadIn<'_, R>>> {
        self.span_lists[index]
            .next()
            .map(|span| FieldsView::read_in(self.reader, span))
    }
}

#[derive(Debug)]
pub struct Join<const N: usize>(pub [&'static str; N]);

impl<'a, 'b, R, P, const N: usize> FieldAdapter<'a, 'b, R, P> for Join<N>
where
    'a: 'b,
    R: Read + Seek + 'a,
{
    type ParserInput = JoinIn<'b, R, N>;

    fn adapt(self, view: &'b mut FieldsView<'a, R>) -> BinResult<Self::ParserInput> {
        let matches = self.0.map(|name| {
            view.iter_matches_spans(name)
                .map(|iter| iter.copied().collect::<Vec<_>>())
                .unwrap_or_default()
                .into_iter()
        });

        Ok(JoinIn {
            reader: &mut *view.reader,
            span_lists: matches,
        })
    }
}

pub struct AllIn<'a, R>(JoinIn<'a, R, 1>);

impl<'a, R> AllIn<'a, R>
where
    R: Read + Seek + 'a,
{
    pub fn lending_len(&self) -> usize {
        self.0.lending_len_of(0)
    }

    pub fn borrow_next(&mut self) -> Option<BinResult<ReadIn<'_, R>>> {
        self.0.borrow_next_of(0)
    }
}

#[derive(Debug)]
pub struct All(pub &'static str);

impl<'a, 'b, R, P> FieldAdapter<'a, 'b, R, P> for All
where
    'a: 'b,
    R: Read + Seek + 'a,
{
    type ParserInput = AllIn<'b, R>;

    fn adapt(self, view: &'b mut FieldsView<'a, R>) -> BinResult<Self::ParserInput> {
        Ok(AllIn(<Join<1> as FieldAdapter<_, P>>::adapt(
            Join([self.0]),
            view,
        )?))
    }
}

#[derive(Debug)]
pub struct Map<A, F, P1> {
    inner: A,
    func: F,
    _marker: PhantomData<P1>,
}

impl<A, F, P1> Map<A, F, P1> {
    pub fn new(inner: A, func: F) -> Self {
        Self {
            inner,
            func,
            _marker: PhantomData,
        }
    }
}

pub struct NopIn<P>(pub P);

impl<'a, 'b, R, P1, P2, A, F, I1> FieldAdapter<'a, 'b, R, P2> for Map<A, F, P1>
where
    'a: 'b,
    R: Read + Seek + 'a,
    P1: Parser<I1>,
    A: FieldAdapter<'a, 'b, R, P1, ParserInput = I1>,
    F: FnOnce(P1) -> P2,
{
    type ParserInput = NopIn<P2>;

    fn adapt(self, view: &'b mut FieldsView<'a, R>) -> BinResult<Self::ParserInput> {
        let adapted = self.inner.adapt(view)?;
        let p1 = P1::parse(adapted)?;
        let p2 = (self.func)(p1);
        Ok(NopIn(p2))
    }
}

#[derive(Debug)]
pub struct Maybe(pub &'static str);

impl<'a, 'b, R, P> FieldAdapter<'a, 'b, R, P> for Maybe
where
    'a: 'b,
    R: Read + Seek + 'a,
{
    type ParserInput = Option<ReadIn<'b, R>>;

    fn adapt(self, view: &'b mut FieldsView<'a, R>) -> BinResult<Self::ParserInput> {
        let v = view.get_field(self.0);
        v
    }
}
