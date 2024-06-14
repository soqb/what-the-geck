use core::fmt;
use std::borrow::Cow;

use crate::*;

impl fmt::Display for Ident4 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl fmt::Display for FormTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FormTy::Any => write!(f, "any"),
            FormTy::Single(kind) => write!(f, "{kind}"),
        }
    }
}

impl Ty {
    pub fn to_ident(self) -> impl fmt::Display {
        match self {
            Ty::Unit => Cow::Borrowed("unit"),
            Ty::Bool => Cow::Borrowed("bool"),
            Ty::String => Cow::Borrowed("string"),
            Ty::Integer => Cow::Borrowed("integer"),
            Ty::Float => Cow::Borrowed("float"),
            Ty::Ref(RefTy::Unknown) => Cow::Borrowed("ref_any"),
            Ty::Ref(RefTy::ANY_OBJECT) => Cow::Borrowed("ref_object_any"),
            Ty::Ref(RefTy::ANY_FORM) => Cow::Borrowed("ref_form_any"),
            Ty::Ref(RefTy::Object(FormTy::Single(kind))) => {
                Cow::Owned(format!("object_ref_{}", kind.as_str().to_lowercase()))
            }
            Ty::Ref(RefTy::Form(FormTy::Single(kind))) => {
                Cow::Owned(format!("form_ref_{}", kind.as_str().to_lowercase()))
            }
            // TODO: this is /terrible/ for caching
            Ty::Adt(idx) => Cow::Owned(format!("adt_{idx}")),
        }
    }
}

impl FunctionDefinition {
    pub fn is_length_variable(&self) -> bool {
        self.params.iter().any(|param| param.optional)
    }
}

macro_rules! impl_idx_extras {
    ($name: ident) => {
        impl From<slotmap::KeyData> for $name {
            fn from(value: slotmap::KeyData) -> Self {
                Self(value.as_ffi())
            }
        }

        unsafe impl slotmap::Key for $name {
            fn data(&self) -> slotmap::KeyData {
                slotmap::KeyData::from_ffi(self.0)
            }
        }
    };
}

impl_idx_extras!(ComponentIdx);
impl_idx_extras!(FormIdx);
impl_idx_extras!(TypeIdx);
impl_idx_extras!(EventIdx);
impl_idx_extras!(ExternalVariableIdx);
impl_idx_extras!(FunctionIdx);

impl fmt::Display for StopToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "compilation was cancelled")
    }
}

impl std::error::Error for StopToken {}

pub struct PrintSlotMap<'a, K: slotmap::Key, V>(pub &'a slotmap::SlotMap<K, V>);

impl<'a, K: fmt::Debug + slotmap::Key, V: fmt::Debug> fmt::Debug for PrintSlotMap<'a, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut m = f.debug_map();
        for (k, v) in self.0 {
            m.entry(&k, v);
        }
        m.finish()
    }
}

impl fmt::Debug for TheResources {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TheResources")
            .field("components", &PrintSlotMap(&self.components))
            .field("functions", &PrintSlotMap(&self.functions))
            .field("forms", &PrintSlotMap(&self.forms))
            .field("events", &PrintSlotMap(&self.events))
            .field(
                "external_variables",
                &PrintSlotMap(&self.external_variables),
            )
            .finish_non_exhaustive()
    }
}
