use crate::{
    common::{FormId, NonNullString},
    records::{All, Maybe},
};

impl_from_fields! {
    Quest {
        Maybe("SCRI") => script: Option<FormId>,
        All("SCTX") => result_source_texts: Vec<String> as Vec<NonNullString>,
    }
}
