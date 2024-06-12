use std::{
    collections::{HashMap, HashSet},
    io::{Read, Seek, SeekFrom},
};

use binrw::{io::TakeSeekExt, BinResult};

use crate::common::Ident4;

mod adapters;
pub use adapters::*;

mod parsers;
pub use parsers::*;

use super::{read_data::FieldSpans, Span, UnparsedFields};

pub struct FieldsView<'a, R: Read + Seek> {
    fields: FieldSpans,
    used: HashSet<&'a str>,
    pub reader: &'a mut R,
    pub record_name: Ident4,
}

impl<'a, R: Read + Seek + 'a> FieldsView<'a, R> {
    pub fn new(fields: FieldSpans, reader: &'a mut R, record_name: Ident4) -> Self {
        Self {
            fields,
            reader,
            record_name,
            used: HashSet::new(),
        }
    }
}

impl<'a, R: Read + Seek + 'a> FieldsView<'a, R> {
    pub fn get_field(&mut self, ident: &'a str) -> Result<Option<ReadIn<'_, R>>, binrw::Error> {
        self.fields
            .map
            .get(ident)
            .map(|fields| {
                if fields.len() == 1 {
                    self.used.insert(ident);
                    Self::read_in(self.reader, fields[0])
                } else {
                    Err(binrw::Error::Custom {
                        pos: self.reader.stream_position()?,
                        err: Box::new(format!(
                            "more than one match for field {ident} in record {}",
                            self.record_name
                        )),
                    })
                }
            })
            .transpose()
    }

    pub fn field(&mut self, ident: &'a str) -> Result<ReadIn<'_, R>, binrw::Error> {
        let pos = self.reader.stream_position()?;
        self.fields
            .map
            .get(ident)
            .map(|fields| {
                if fields.len() == 1 {
                    self.used.insert(ident);
                    Self::read_in(self.reader, fields[0])
                } else {
                    Err(binrw::Error::Custom {
                        pos,
                        err: Box::new(format!(
                            "more than one match for field {ident} in record {}",
                            self.record_name
                        )),
                    })
                }
            })
            .unwrap_or_else(|| {
                Err(binrw::Error::Custom {
                    pos,
                    err: Box::new(format!(
                        "no matches for field {ident} in record {}",
                        self.record_name
                    )),
                })
            })
    }

    pub fn read_in(reader: &mut R, Span { offset, size }: Span) -> BinResult<ReadIn<'_, R>> {
        reader.seek(SeekFrom::Start(offset))?;
        Ok(ReadIn(reader.take_seek(size)))
    }

    pub fn iter_matches_spans(&mut self, ident: &'a str) -> Option<std::slice::Iter<'_, Span>> {
        self.used.insert(ident);

        #[allow(clippy::unnecessary_to_owned)]
        self.fields.map.get(ident).map(|vec| vec.iter())
    }

    pub fn finish(self) -> BinResult<UnparsedFields> {
        let mut map = HashMap::new();

        for (name, spans) in self.fields.map {
            if self.used.contains(name.as_str()) {
                continue;
            }

            let unparsed: Result<Vec<_>, binrw::Error> = spans
                .into_iter()
                .map(|span| {
                    let mut vec = vec![0; span.size as usize];
                    let mut reader = FieldsView::read_in(&mut *self.reader, span)?;
                    reader.0.read_exact(&mut vec[..])?;
                    Ok(vec.into_boxed_slice())
                })
                .collect();

            map.insert(name, unparsed?);
        }

        Ok(UnparsedFields { map })
    }
}
