use binrw::BinResult;
use std::io::{Read, Seek};

#[macro_use]
mod define_data;

mod read_data;
pub use read_data::*;

mod data;
pub use data::*;
mod view;
pub use view::*;

mod experiment;

pub trait FromFields: Sized {
    fn from_fields<'a, 'b, R>(view: &'b mut FieldsView<'a, R>) -> BinResult<Self>
    where
        'a: 'b,
        R: Read + Seek + 'a;
}
