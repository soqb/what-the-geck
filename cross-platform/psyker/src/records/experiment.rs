use std::io::{self, Read};

use crate::common::Ident4;

struct FieldsView {}

impl FieldsView {
    fn next_field(&mut self) -> Result<Option<(Ident4, impl Read + '_)>, ()> {
        Ok(Some((Ident4::new("AAAA"), io::empty())))
    }
}
