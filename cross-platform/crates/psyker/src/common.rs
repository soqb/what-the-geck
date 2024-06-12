use std::{
    borrow::Borrow,
    fmt::{self, Debug, Display},
    io::{Read, Seek},
    ops::Deref,
};

use arrayvec::ArrayString;
use binrw::{BinRead, BinResult, Endian, NullString};
use derive_deref::{Deref, DerefMut};
use serde::{Deserialize, Serialize};

use crate::{
    read_utils::{map_err_to_binrw, Everything},
    records::ParsedInto,
};

#[derive(BinRead, Debug, Serialize, Deserialize)]
pub struct Rgb {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
}

#[derive(
    BinRead,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Deref,
    DerefMut,
    Serialize,
    Deserialize,
)]
pub struct FormId(pub u32);

impl Display for FormId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self, f)
    }
}

impl Debug for FormId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0x{:08x}", self.0)
    }
}

#[derive(Default, Clone, Copy, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct Ident4(pub ArrayString<4>);

impl Ident4 {
    pub fn new(w: &str) -> Self {
        Ident4(ArrayString::from(w).unwrap())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Deref for Ident4 {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl Borrow<str> for Ident4 {
    fn borrow(&self) -> &str {
        self
    }
}

impl Debug for Ident4 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl Display for Ident4 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl PartialEq<str> for Ident4 {
    fn eq(&self, other: &str) -> bool {
        self.0 == *other
    }
}

impl PartialEq<&str> for Ident4 {
    fn eq(&self, other: &&str) -> bool {
        self.0 == **other
    }
}

impl BinRead for Ident4 {
    type Args<'a> = ();
    fn read_options<R: Read + Seek>(
        reader: &mut R,
        endian: Endian,
        _: Self::Args<'_>,
    ) -> BinResult<Self> {
        // stream_len(reader)?;
        let buf = <[u8; 4]>::read_options(reader, endian, ())?;
        ArrayString::from_byte_string(&buf)
            .map_err(|err| map_err_to_binrw(reader, err))
            .and_then(|string| {
                if string == *string.to_uppercase() {
                    Ok(string)
                } else {
                    Err(map_err_to_binrw(
                        reader,
                        "field and record names should be UPPERCASE".to_string(),
                    ))
                }
            })
            .map(Self)
    }
}

fn decode_from_windows_1252<R: Read + Seek>(reader: &mut R, bytes: &[u8]) -> BinResult<String> {
    match encoding_rs::WINDOWS_1252.decode_without_bom_handling_and_without_replacement(bytes) {
        Some(utf8) => Ok(utf8.into_owned()),
        None => Err(binrw::Error::Custom {
            pos: reader.stream_position()?,
            err: Box::new("string was not valid windows-1252 ascii"),
        }),
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ZString(pub String);

impl BinRead for ZString {
    type Args<'a> = ();

    fn read_options<R: Read + Seek>(reader: &mut R, endian: Endian, _: ()) -> BinResult<Self> {
        let string = NullString::read_options(reader, endian, ())?;
        decode_from_windows_1252(reader, &string.0).map(Self)
    }
}

impl ParsedInto<String> for ZString {
    fn parsed_into(self) -> String {
        self.0
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NonNullString(pub String);

impl BinRead for NonNullString {
    type Args<'a> = ();

    fn read_options<R: Read + Seek>(reader: &mut R, _: Endian, _: ()) -> BinResult<Self> {
        let Everything(string) = BinRead::read_options(reader, Endian::Little, ())?;
        decode_from_windows_1252(reader, &string).map(Self)
    }
}

impl ParsedInto<String> for NonNullString {
    fn parsed_into(self) -> String {
        self.0
    }
}
