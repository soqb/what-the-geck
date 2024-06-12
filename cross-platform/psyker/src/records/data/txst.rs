use binrw::BinRead;
use serde::{Deserialize, Serialize};

use crate::{
    common::{Rgb, ZString},
    records::view::Maybe,
};

#[derive(BinRead, Debug, Serialize, Deserialize)]
pub struct DecalData {
    pub min_width: f32,
    pub max_width: f32,
    pub min_height: f32,
    pub max_height: f32,
    pub depth: f32,
    pub shininess: f32,
    pub parallax: f32,
    pub parallax_passes: u8,
    // TODO bitflags
    pub flags: u8,
    _unkown: u16,
    pub color: Rgb,
}

impl_from_fields! {
    TextureSet {
        Maybe("TX00") => color_map: Option<String> as Option<ZString>,
        Maybe("TX01") => normal_map: Option<String> as Option<ZString>,
        Maybe("TX02") => mask: Option<String> as Option<ZString>,
        Maybe("TX03") => tone_map: Option<String> as Option<ZString>,
        Maybe("TX04") => detail_map: Option<String> as Option<ZString>,
        Maybe("TX05") => environment_map: Option<String> as Option<ZString>,
        Maybe("TX06") => multilayer: Option<String> as Option<ZString>,
        Maybe("TX07") => specularity_map: Option<String> as Option<ZString>,
        Maybe("DODT") => decal: Option<DecalData>,
        // TODO bitflags
        Maybe("DNAM") => flags: Option<u16>,
    }
}
