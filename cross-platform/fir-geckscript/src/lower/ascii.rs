use std::{borrow::Borrow, hash};

use derive_deref::{Deref, DerefMut};

#[derive(Deref, DerefMut, Debug)]
#[repr(transparent)]
pub struct CaselessString(String);

#[derive(Deref, DerefMut, ref_cast::RefCast, Debug)]
#[repr(transparent)]
pub struct CaselessStr(str);

impl hash::Hash for CaselessStr {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.to_lowercase().hash(state)
    }
}

impl hash::Hash for CaselessString {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        Borrow::<CaselessStr>::borrow(self).hash(state)
    }
}

impl CaselessString {
    pub fn new(string: String) -> Self {
        Self(string)
    }
}

impl CaselessStr {
    pub fn new(string: &str) -> &CaselessStr {
        ref_cast::RefCast::ref_cast(string)
    }
}

impl Borrow<CaselessStr> for CaselessString {
    fn borrow(&self) -> &CaselessStr {
        CaselessStr::new(self)
    }
}

impl AsRef<str> for CaselessStr {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for CaselessString {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl<T: AsRef<str> + ?Sized> PartialEq<T> for CaselessString
where
    Self: Borrow<T>,
{
    fn eq(&self, other: &T) -> bool {
        unicase::eq_ascii(self.as_ref(), other.as_ref())
    }
}

impl<T: AsRef<str> + ?Sized> PartialEq<T> for CaselessStr
where
    Self: Borrow<T>,
{
    fn eq(&self, other: &T) -> bool {
        unicase::eq_ascii(self.as_ref(), other.as_ref())
    }
}

impl Eq for CaselessStr {}
impl Eq for CaselessString {}
