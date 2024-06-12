use binrw::{BinRead, BinWrite};
use serde::{Deserialize, Serialize};

use crate::{read_utils::OrEof, records::Map};

// fixme(soqb): this is for oblivion. idk about nv.
#[derive(Debug, BinRead, BinWrite, Serialize, Deserialize)]
#[brw(repr = u8)]
pub enum DialogueType {
    Topic = 0,
    Conversation = 1,
    Combat = 2,
    Persuasion = 3,
    Detection = 4,
    Service = 5,
    Miscellaneous = 6,
}

impl_from_fields! {
    Dialogue {
        // Map::new("DATA", |(_, b): (u8, OrEof<DialogueType>)| b.to_option()) => subtype: Option<DialogueType>,
    }
}
