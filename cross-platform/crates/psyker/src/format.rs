use std::io::{Cursor, Read, Seek, SeekFrom};

use binrw::{BinRead, BinResult, Endian};
use bitflags::bitflags;
use serde::{Deserialize, Serialize};

use crate::{
    common::{FormId, Ident4},
    parse_with,
    read_utils::{ExactBytes, UntilEof},
    records::{Fields, ParsedFields},
};

pub use crate::records::tes4::Tes4;

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum GroupLabel {
    Top(Ident4),
    WorldChildren(FormId),
    InteriorBlock(i32),
    InteriorSubBlock(i32),
    ExteriorBlock { y: i16, x: i16 },
    ExteriorSubBlock { y: i16, x: i16 },
    CellChildren(FormId),
    TopicChildren(FormId),
    CellPersistentChildren(FormId),
    CellTemporaryChildren(FormId),
    CellVisibleDistantChildren(FormId),
}

impl BinRead for GroupLabel {
    type Args<'a> = ();
    fn read_options<R: Read + Seek>(
        reader: &mut R,
        endian: Endian,
        _: Self::Args<'_>,
    ) -> BinResult<Self> {
        let four = <[u8; 4]>::read_options(reader, endian, ())?;
        let tag = u32::read_options(reader, endian, ())?;
        Ok(match tag {
            0 => Self::Top(Ident4::read_options(&mut Cursor::new(four), endian, ())?),
            1 => Self::WorldChildren(FormId(u32::from_le_bytes(four))),
            2 => Self::InteriorBlock(i32::from_le_bytes(four)),
            3 => Self::InteriorSubBlock(i32::from_le_bytes(four)),
            4 => {
                let [y1, y2, x1, x2] = four;
                Self::ExteriorBlock {
                    y: i16::from_le_bytes([y1, y2]),
                    x: i16::from_le_bytes([x1, x2]),
                }
            }
            5 => {
                let [y1, y2, x1, x2] = four;
                Self::ExteriorSubBlock {
                    y: i16::from_le_bytes([y1, y2]),
                    x: i16::from_le_bytes([x1, x2]),
                }
            }
            6 => Self::CellChildren(FormId(u32::from_le_bytes(four))),
            7 => Self::TopicChildren(FormId(u32::from_le_bytes(four))),
            8 => Self::CellPersistentChildren(FormId(u32::from_le_bytes(four))),
            9 => Self::CellTemporaryChildren(FormId(u32::from_le_bytes(four))),
            10 => Self::CellVisibleDistantChildren(FormId(u32::from_le_bytes(four))),
            _ => {
                return Err(binrw::Error::Custom {
                    pos: reader.stream_position()?,
                    err: Box::new(format!("invalid group label tag {tag}")),
                })
            }
        })
    }
}

#[derive(BinRead, Debug, Serialize, Deserialize)]
#[br(import(opts: &ParseOptions, depth: usize, size: u32))]
pub struct Group {
    pub timestamp: u16,
    pub vcs_info: u16,
    _unknown: [u8; 4],
    #[br(parse_with = parse_with!(ExactBytes<PluginEntry>), args((size - 24) as u64, (opts, depth + 1,)))]
    pub entries: Vec<PluginEntry>,
}

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
    pub struct RecordFlags: u32 {
        const MASTER_FILE = 0x00000001;
        const DELETED_GROUP = 0x00000010;
        const DELETED_RECORD = 0x00000020;
        // GLOB: Constant
        // REFR: Hidden from Local Map
        const CONSTANT = 0x00000040;
        const LOCALIZED = 0x00000080;
        // ELSE: Must Update Anims
        // REFR: Inaccessible
        const MUST_UPDATE_ANIMS = 0x00000100;
        // TES4: Light Master File
        // REFR: Hidden from Local Map
        // ACHR: Starts dead
        // REFR: MotionBlueCastsShadows
        const LIGHT_MASTER_FILE = 0x00000200;
        const PERSISTENT = 0x00000400;
        const INITIALLY_DISABLED = 0x00000800;
        const IGNORED = 0x00001000;
        const VISIBLE_AT_DISTANCE = 0x00008000;
        const RANDOM_START_ANIMATION = 0x00010000;
        const DANGEROUS_OR_OFF_LIMITS = 0x00020000;
        const COMPRESSED = 0x00040000;
        const NO_WAITING = 0x00080000;
        const IGNORE_OBJECT_INTERACTION = 0x00100000;
        const IS_MARKER = 0x00800000;
        const OBSTACLE = 0x02000000;
        const NAV_MESH_FILTER = 0x04000000;
        const NAV_MESH_BBOX = 0x08000000;
        const EXIT_TO_TALK = 0x10000000;
        const CHILD_CAN_USE = 0x20000000;
        const NAV_MESH_GROUND = 0x40000000;
        const MULTI_BOUND = 0x80000000;
    }
}

impl BinRead for RecordFlags {
    type Args<'a> = ();

    fn read_options<R: Read + Seek>(
        reader: &mut R,
        endian: Endian,
        _: Self::Args<'_>,
    ) -> BinResult<Self> {
        let num = u32::read_options(reader, endian, ())?;
        Ok(Self::from_bits_truncate(num))
    }
}

#[derive(BinRead, Debug)]
pub struct Field {
    pub name: Ident4,
    _size: u16,
    #[br(count = _size)]
    pub data: Vec<u8>,
}

#[binrw::binread]
#[derive(Debug, Serialize, Deserialize)]
#[br(import(name: Ident4, size: u32))]
pub struct Record {
    pub flags: RecordFlags,
    pub formid: FormId,
    pub timestamp: u16,
    pub vcs_info: u16,
    pub version: u16,
    _unknown: [u8; 2],
    #[br(args(name, size, flags.contains(RecordFlags::COMPRESSED)))]
    pub fields: Fields,
}

#[derive(BinRead, Debug, Serialize, Deserialize)]
pub enum EntryLabel {
    #[br(magic = b"GRUP")]
    Group {
        size: u32,
        label: GroupLabel,
    },
    Record {
        label: Ident4,
        size: u32,
    },
}

#[derive(Debug, Serialize, Deserialize)]
pub enum PluginEntry {
    Ignored(EntryLabel),
    Group { label: GroupLabel, contents: Group },
    Record { label: Ident4, contents: Record },
}

impl BinRead for PluginEntry {
    type Args<'a> = (&'a ParseOptions, usize);

    fn read_options<R: Read + Seek>(
        reader: &mut R,
        endian: Endian,
        (opts, depth): Self::Args<'_>,
    ) -> BinResult<Self> {
        let label = EntryLabel::read_options(reader, endian, ())?;
        if (opts.ignore_entry)(&label) {
            let size = match label {
                EntryLabel::Record { size, .. } => size as i64 + 16,
                EntryLabel::Group { size, .. } => size as i64 - 8,
            };
            reader.seek(SeekFrom::Current(size))?;
            return Ok(PluginEntry::Ignored(label));
        }
        match label {
            EntryLabel::Group { label, size } => {
                let contents = Group::read_options(reader, endian, (opts, depth, size))?;
                Ok(PluginEntry::Group { label, contents })
            }
            EntryLabel::Record { label, size } => {
                let contents = Record::read_options(reader, endian, (label.clone(), size))?;
                Ok(PluginEntry::Record { label, contents })
            }
        }
    }
}

fn parse_header<R: Read + Seek>(reader: &mut R, endian: binrw::Endian, (): ()) -> BinResult<Tes4> {
    let entry = PluginEntry::read_options(
        reader,
        endian,
        (
            &ParseOptions {
                ignore_entry: |_| false,
            },
            0,
        ),
    )?;

    if let PluginEntry::Record { label: _, contents } = entry {
        if let Some(ParsedFields::Tes4(header)) = contents.fields.parsed {
            return Ok(header);
        }
    }

    Err(binrw::Error::Custom {
        pos: 0,
        err: Box::new("does not start with a TES4 header!"),
    })
}

#[derive(Debug)]
pub struct ParseOptions {
    pub ignore_entry: fn(&EntryLabel) -> bool,
}

#[derive(BinRead, Debug, Serialize, Deserialize)]
#[br(import_raw(opts: &ParseOptions))]
pub struct MainFile {
    #[br(parse_with = parse_header)]
    pub header: Tes4,
    #[br(parse_with = parse_with!(UntilEof<PluginEntry>), args(opts, 0))]
    pub top_level_entries: Vec<PluginEntry>,
}

#[derive(Debug, Clone)]
pub enum Entry<P: Plugin> {
    Group { ptr: P::GroupPtr, label: GroupLabel },
    Record { ptr: P::RecordPtr, label: Ident4 },
}

#[derive(Debug, Serialize, Deserialize, BinRead)]
pub struct RecordHeader {
    pub flags: RecordFlags,
    pub formid: FormId,
    pub timestamp: u16,
    pub vcs_info: u16,
    pub version: u16,
    _unknown: [u8; 2],
}

pub trait RecordVisitor {
    type Output;

    fn visit_tes4(tes4: &crate::records::Tes4) -> Self::Output;
}

pub trait Plugin: Sized {
    type Err;
    type GroupPtr: Clone;
    type RecordPtr: Clone;

    fn root_group(&mut self) -> Result<Self::GroupPtr, Self::Err>;

    fn next_entry(&mut self, ptr: &mut Self::GroupPtr) -> Result<Entry<Self>, Self::Err>;
    fn visit_record<V: RecordVisitor>(
        &mut self,
        ptr: &mut Self::RecordPtr,
        visitor: &mut impl RecordVisitor,
    ) -> Result<V::Output, Self::Err>;
}

pub struct PluginReader<R> {
    read: R,
}

impl<R: Read + Seek> PluginReader<R> {
    pub fn new(read: R) -> BinResult<Self> {
        Ok(Self { read })
    }
}
