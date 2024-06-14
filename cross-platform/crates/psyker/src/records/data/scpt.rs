use crate::{
    common::{FormId, NonNullString, ZString},
    read_utils::Everything,
    records::{All, Join},
};
use binrw::BinRead;
use serde::{Deserialize, Serialize};

#[derive(Debug, BinRead, Serialize, Deserialize)]
pub struct ScriptHeader {
    _unknown: [u8; 4],
    pub references: u32,
    pub compiled_size: u32,
    pub variable_count: u32,
    pub type_: ScriptType,
}

#[derive(Debug, BinRead, Serialize, Deserialize)]
#[repr(u32)]
#[br(stream = reader)]
pub enum ScriptType {
    #[br(magic = 0x0u32)]
    Object = 0x0,
    #[br(magic = 0x1u32)]
    Quest = 0x1,
    #[br(magic = 0x100u32)]
    MagicEffect = 0x100,
    #[br(pre_assert({
        reader.seek(std::io::SeekFrom::Current(0x4))?;
        true
    }))]
    Other,
}

#[derive(Debug, BinRead, Serialize, Deserialize)]
#[repr(u32)]
#[br(stream = reader)]
pub enum VarType {
    #[br(magic = 0x0u8)]
    FloatOrRef = 0x0,
    #[br(magic = 0x1u8)]
    SomeInt = 0x1,
    #[br(pre_assert({
        reader.seek(std::io::SeekFrom::Current(0x4))?;
        true
    }))]
    Other,
}

#[derive(Debug, BinRead, Serialize, Deserialize)]
pub struct VarData {
    pub index: u32,
    _unknown: [u8; 12],
    pub type_: VarType,
    _unknown2: [u8; 4],
}

impl_from_fields! {
    Script {
        "SCHR" => header: ScriptHeader,
        "SCDA" => compiled: Vec<u8> as Everything,
        "SCTX" => source_text: String as NonNullString,
        Join(["SCVR", "SLSD"]) => variables: Vec<(ZString, VarData)>, // TODO allow String rather than ZString here.
        All("SCRO") => object_references: Vec<FormId>,
        All("SCRV") => external_local_variables: Vec<u32>,
    }
}
