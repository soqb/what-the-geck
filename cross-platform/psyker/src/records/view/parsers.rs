use std::io::{Read, Seek};

use binrw::{BinRead, BinResult};

use super::{adapters::AllIn, JoinIn, NopIn, ReadIn};

pub trait Parser<I>: 'static + Sized {
    fn parse(input: I) -> BinResult<Self>;
}

impl<P: 'static> Parser<NopIn<P>> for P {
    fn parse(input: NopIn<P>) -> BinResult<Self> {
        Ok(input.0)
    }
}

impl<I, P: Parser<I>> Parser<Option<I>> for Option<P> {
    fn parse(input: Option<I>) -> BinResult<Self> {
        input.map(P::parse).transpose()
    }
}

impl<'a, R, P> Parser<AllIn<'a, R>> for Vec<P>
where
    R: Read + Seek,
    P: for<'b> Parser<ReadIn<'b, R>>,
{
    fn parse(mut iter: AllIn<'a, R>) -> BinResult<Self> {
        let mut vec = Vec::new();

        while let Some(next) = iter.borrow_next() {
            vec.push(P::parse(next?)?);
        }

        Ok(vec)
    }
}

impl<'a, R, P1, P2> Parser<JoinIn<'a, R, 2>> for Vec<(P1, P2)>
where
    R: Read + Seek,
    P1: for<'b> Parser<ReadIn<'b, R>>,
    P2: for<'b> Parser<ReadIn<'b, R>>,
{
    fn parse(mut iter: JoinIn<'a, R, 2>) -> BinResult<Self> {
        let mut vec = Vec::new();

        loop {
            let Some(next_1) = iter.borrow_next_of(0) else { break; };
            let first = P1::parse(next_1?)?;
            let Some(next_2) = iter.borrow_next_of(1) else { break; };
            let second = P2::parse(next_2?)?;
            vec.push((first, second));
        }

        Ok(vec)
    }
}

impl<'a, B, R> Parser<ReadIn<'a, R>> for B
where
    R: Read + Seek,
    B: BinRead + 'static,
    for<'b> B::Args<'b>: Default,
{
    fn parse(mut reader: ReadIn<'a, R>) -> BinResult<Self> {
        B::read_le(&mut reader.0)
    }
}

pub trait ParsedInto<O> {
    fn parsed_into(self) -> O;
}

impl<O, I: ParsedInto<O>> ParsedInto<Option<O>> for Option<I> {
    fn parsed_into(self) -> Option<O> {
        self.map(I::parsed_into)
    }
}

impl<O, I: ParsedInto<O>> ParsedInto<Vec<O>> for Vec<I> {
    fn parsed_into(self) -> Vec<O> {
        self.into_iter().map(I::parsed_into).collect()
    }
}
