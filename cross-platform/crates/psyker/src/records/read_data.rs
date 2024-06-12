use std::{
    collections::HashMap,
    fmt::Debug,
    io::{Cursor, Read, Seek, SeekFrom},
};

use binrw::{io::TakeSeekExt, BinRead, BinResult, Endian};
use log::{info, warn};
use serde::{Deserialize, Serialize};

use crate::{
    common::{Ident4, ZString},
    records::{FromFields, Maybe},
};

use super::{FieldsView, ParsedFields};

impl_from_fields! {
    #[derive(Default)]
    Common {
        Maybe("EDID") => editor_id: Option<String> as Option<ZString>,
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Fields {
    pub parsed: Option<ParsedFields>,
    pub unparsed: UnparsedFields,
    pub common: Common,
}

impl BinRead for Fields {
    type Args<'a> = (Ident4, u32, bool);

    fn read_options<R: Read + Seek>(
        reader: &mut R,
        endian: Endian,
        (name, total_size_raw, compressed): Self::Args<'_>,
    ) -> BinResult<Self> {
        let total_size = total_size_raw as u64;
        if compressed {
            // reader.seek(SeekFrom::Current(total_size_raw.into()))?;
            // return Ok(Self::default());

            let pos: u64 = reader.stream_position()?;
            if pos % 400 == 0 {
                info!(
                    "compressed {name} record: 0x{:x} until 0x{:x}!",
                    pos,
                    pos + total_size - 4,
                );
            }

            let decompressed_size = u32::read_options(reader, endian, ())?;
            let mut vec = Vec::with_capacity(decompressed_size as usize);
            if flate2::read::ZlibDecoder::new(reader.take_seek(total_size - 4))
                .read_to_end(&mut vec)
                .is_err()
            {
                // gibberish, but everyone makes mistakes!
                warn!("found some {name}ed gibberish!");
                reader.seek(SeekFrom::Start(pos + total_size as u64))?;
                return Ok(Self::default());
            }

            let mut cursor = Cursor::new(vec);

            Self::read_options(&mut cursor, endian, (name, decompressed_size, false))
        } else {
            let spans = FieldSpans::read_options(&mut *reader, endian, total_size)?;
            let pos = reader.stream_position()?;

            let mut view = FieldsView::new(spans, &mut *reader, name);
            let common = Common::from_fields(&mut view)?;
            #[allow(non_upper_case_globals)]
            let parsed = super::field_map!(view);
            let unparsed = view.finish()?;

            reader.seek(SeekFrom::Start(pos))?;
            Ok(Self {
                parsed,
                unparsed,
                common,
            })
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Default)]
pub struct UnparsedFields {
    pub map: HashMap<Ident4, Vec<Box<[u8]>>>,
}

impl Debug for UnparsedFields {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct BytesNum(usize);

        impl Debug for BytesNum {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "[{len} bytes]", len = self.0)
            }
        }

        f.debug_map()
            .entries(self.map.iter().flat_map(|(name, vec)| {
                vec.iter()
                    .map(|data| (name, BytesNum(data.len())))
                    .collect::<Vec<_>>()
            }))
            .finish()
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy)]
pub struct Span {
    pub offset: u64,
    pub size: u64,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FieldSpans {
    pub map: HashMap<Ident4, Vec<Span>>,
}

impl BinRead for FieldSpans {
    type Args<'a> = u64;

    fn read_options<R: Read + Seek>(
        reader: &mut R,
        endian: Endian,
        total_size: Self::Args<'_>,
    ) -> BinResult<Self> {
        {
            let mut bytes_read = reader.stream_position()?;
            let mut map = HashMap::new();

            let mut extra_size = None;
            let total = bytes_read + total_size;
            while bytes_read < total {
                let key = Ident4::read_options(reader, endian, ())?;
                if key == "XXXX" {
                    // Skip size, All XXXX fields are 4 bytes.
                    reader.seek(SeekFrom::Current(2))?;
                    extra_size = Some(u32::read_options(reader, endian, ())?);
                    continue;
                }

                let size = if let Some(size) = extra_size {
                    extra_size = None;
                    // Skip size, use size from XXXX field instead.
                    bytes_read = reader.seek(SeekFrom::Current(2))?;
                    size as u64
                } else {
                    let size = u16::read_options(reader, endian, ())?;
                    bytes_read = reader.stream_position()?;
                    size as u64
                };

                let span = Span {
                    offset: bytes_read,
                    size,
                };

                // let mut vec = vec![0; size];
                // reader.read_exact(&mut vec[..size])?;

                map.entry(key).or_insert_with(Vec::new).push(span);
                bytes_read = reader.seek(SeekFrom::Current(size as i64))?;
            }
            Ok(FieldSpans { map })
        }
    }
}

macro_rules! map_fields {
    ($view:ident; $($pat:pat => $variant:ident,)*) => {{
        match $view.record_name.as_str() {
            $(
                $pat => Some(ParsedFields::$variant(FromFields::from_fields(&mut $view)?)),
            )*
            _ => None,
        }
    }};
}

use map_fields;
