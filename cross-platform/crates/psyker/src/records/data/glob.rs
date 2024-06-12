use binrw::BinRead;
use serde::{Deserialize, Serialize};

#[derive(Debug, BinRead, Serialize, Deserialize)]
pub enum GlobalType {
    #[br(magic = b's')]
    Short,
    #[br(magic = b'l')]
    Long,
    #[br(magic = b'f')]
    Float,
}

impl_from_fields! {
    Global {
        "FNAM" => type_: GlobalType,
        "FLTV" => value: f32,
    }
}
