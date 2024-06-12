use binrw::BinRead;
use serde::{Deserialize, Serialize};

use crate::{common::ZString, records::Maybe};

#[derive(Debug, BinRead, Serialize, Deserialize)]
pub enum Specialization {
    #[br(magic = 0u8)]
    Combat,
    #[br(magic = 1u8)]
    Magic,
    #[br(magic = 2u8)]
    Stealth,
}

#[derive(Debug, BinRead, Serialize, Deserialize)]
pub struct ClassData {
    // _unknown: [u8; 4],
    // pub training_skill: u8,
    // pub training_level: u8,
    // pub skill_weights: [u8; 18],
    // pub bleedout: f32,
    // pub voice_points: u32,
    // pub health_weight: u8,
    // pub magicka_weight: u8,
    // pub stamina_weight: u8,
    // pub flags: u8,
    // pub primary_attributes: [FormId; 2],
    // pub specialization: Specialization,
    // pub manjor_skills: [FormId; 7],
    // // TODO bitflags
    // pub flags: u32,
    // // TODO bitflags
    // pub vendor_flags: u32,
    // pub trained_skill: u8,
    // _unknown: [u8; 2],
}

impl_from_fields! {
    Class {
        "FULL" => full_name: String as ZString,
        "DESC" => description: String as ZString,
        "DATA" => data: ClassData,
        Maybe("ICON") => icon: Option<String> as Option<ZString>,
    }
}
