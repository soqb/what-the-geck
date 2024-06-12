use std::{
    fmt::Debug,
    io::{Read, Seek, SeekFrom},
};

use binrw::{error::CustomError, BinRead, BinResult, Endian};

use crate::records::ParsedInto;

pub enum OrEof<T> {
    Value(T),
    Eof,
}

impl<T> OrEof<T> {
    pub fn to_option(self) -> Option<T> {
        match self {
            OrEof::Value(x) => Some(x),
            OrEof::Eof => None,
        }
    }
}

impl<T: BinRead> BinRead for OrEof<T> {
    type Args<'a> = T::Args<'a>;

    fn read_options<R: Read + Seek>(
        reader: &mut R,
        endian: Endian,
        args: Self::Args<'_>,
    ) -> BinResult<Self> {
        let start_pos = reader.stream_position()?;
        let end_pos = reader.seek(SeekFrom::End(0))?;
        eprintln!("remaining bytes: {}", end_pos - start_pos);
        if start_pos == end_pos {
            return Ok(Self::Eof);
        }

        reader.seek(SeekFrom::Start(start_pos))?;
        T::read_options(reader, endian, args).map(Self::Value)
    }
}

pub trait ParseWith {
    type Args<'a>;
    type Output: Sized;
    fn parse_with<R: Read + Seek>(
        reader: &mut R,
        endian: Endian,
        args: Self::Args<'_>,
    ) -> BinResult<Self::Output>;
}

impl<T: BinRead> ParseWith for T {
    type Args<'a> = <T as BinRead>::Args<'a>;

    type Output = T;

    fn parse_with<R: Read + Seek>(
        reader: &mut R,
        endian: Endian,
        args: Self::Args<'_>,
    ) -> BinResult<Self::Output> {
        T::read_options(reader, endian, args)
    }
}

pub fn map_err_to_binrw<R: Read + Seek>(reader: &mut R, err: impl CustomError) -> binrw::Error {
    binrw::Error::Custom {
        pos: reader.stream_position().unwrap(),
        err: Box::new(format!("{:#?}", err)),
    }
}

#[repr(transparent)]
pub struct ExactBytes<T>(Vec<T>);

impl<T: Debug> BinRead for ExactBytes<T>
where
    T: BinRead + 'static,
    for<'a> T::Args<'a>: Clone,
{
    type Args<'a> = (u64, T::Args<'a>);

    fn read_options<R: Read + Seek>(
        reader: &mut R,
        endian: Endian,
        (size, args): Self::Args<'_>,
    ) -> BinResult<Self> {
        let start = reader.stream_position()?;
        let end = start + size;

        let mut read = start;

        let mut vec = Vec::new();
        while read < end {
            vec.push(T::read_options(reader, endian, args.clone())?);

            read = reader.stream_position()?;
        }

        if read != end {
            return Err(binrw::Error::Custom{
                pos: read,
                err: Box::new(format!("promised to read {size} bytes from 0x{start:x} but actually read {} bytes (ended at 0x{read:x}, but expected @{end:x})",
                read - start)),
            });
        }

        Ok(ExactBytes(vec))
    }
}

impl<T> ParsedInto<Vec<T>> for ExactBytes<T> {
    fn parsed_into(self) -> Vec<T> {
        self.0
    }
}

pub fn stream_len<R: Read + Seek>(reader: &mut R) -> std::io::Result<u64> {
    let pos = reader.stream_position()?;
    let end = reader.seek(SeekFrom::End(0))?;
    reader.seek(SeekFrom::Start(pos))?;

    Ok(end - pos)
}

pub struct Everything(pub Vec<u8>);

impl BinRead for Everything {
    type Args<'a> = ();

    fn read_options<R: Read + Seek>(
        reader: &mut R,
        _: Endian,
        _: Self::Args<'_>,
    ) -> BinResult<Self> {
        let mut buf = Vec::new();
        reader.read_to_end(&mut buf)?;
        Ok(Self(buf))
    }
}

impl ParsedInto<Vec<u8>> for Everything {
    fn parsed_into(self) -> Vec<u8> {
        self.0
    }
}

#[repr(transparent)]
pub struct UntilEof<T>(pub Vec<T>);

impl<T> BinRead for UntilEof<T>
where
    T: BinRead + 'static,
    for<'a> T::Args<'a>: Clone,
{
    type Args<'a> = T::Args<'a>;

    fn read_options<R: Read + Seek>(
        reader: &mut R,
        endian: Endian,
        args: Self::Args<'_>,
    ) -> BinResult<Self> {
        let mut vec = Vec::new();
        loop {
            let bytes_remaining = stream_len(&mut *reader)?;
            if bytes_remaining > 0 {
                match T::read_options(reader, endian, args.clone()) {
                    Ok(elem) => vec.push(elem),
                    Err(err) if err.is_eof() => break,
                    Err(err) => return Err(err),
                }
            } else {
                break;
            }
        }
        Ok(Self(vec))
    }
}

impl<T> ParsedInto<Vec<T>> for UntilEof<T> {
    fn parsed_into(self) -> Vec<T> {
        self.0
    }
}

#[macro_export]
macro_rules! parse_with {
    ($parser:ty) => {{
        // i have literally no idea why this is like this but i ain't touching it ever again.
        fn parse_hint<R: Read + Seek, T>(
            reader: &mut R,
            endian: binrw::Endian,
            args: <$parser as binrw::BinRead>::Args<'_>,
        ) -> binrw::BinResult<T>
        where
            $parser: $crate::records::ParsedInto<T>,
        {
            <$parser as binrw::BinRead>::read_options(reader, endian, args)
                .map($crate::records::ParsedInto::parsed_into)
        }
        parse_hint
    }};
}
