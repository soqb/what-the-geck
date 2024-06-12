use crate::{
    common::ZString,
    records::{
        view::{All, Map},
        Maybe,
    },
};

impl_from_fields! {
    Tes4 {
        Map::new(
            "HEDR",
            |tuple: (_, u32, u32)| tuple.0
        ) => version: f32,
        Map::new(
            "HEDR",
            |tuple: (f32, _, u32)| tuple.1
        ) => record_count: u32,
        Map::new(
            "HEDR",
            |tuple: (f32, u32, _)| tuple.2
        ) => next_object_id: u32,
        "CNAM" => author: String as ZString,
        Maybe("SNAM") => description: Option<String> as Option<ZString>,
        All("MAST") => masters: Vec<String> as Vec<ZString>,
    }
}
