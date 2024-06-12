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

impl<C: Component> Resources<C> {
    pub fn get_function_definition(&self, func_idx: FunctionIdx) -> Option<&FunctionDefinition> {
        match &self.get_function(func_idx)?.reference {
            FunctionReference::Defined(def) => Some(def),
            &FunctionReference::Alias { aliased_function } => {
                self.get_function_definition(aliased_function)
            }
        }
    }
}

impl FunctionDefinition {
    pub fn is_length_variable(&self) -> bool {
        self.params.iter().any(|param| param.optional)
    }
}

impl From<FormIdx> for ComponentIdx {
    fn from(value: FormIdx) -> Self {
        ComponentIdx((u64::from(value) >> 12) as u16)
    }
}

impl From<slotmap::KeyData> for FormIdx {
    fn from(value: slotmap::KeyData) -> Self {
        // we strip the most significant 12 bits off the version since those are used for component indices.
        let mask: u64 = (1 << 52) - 1;
        Self(value.as_ffi() & mask)
    }
}

unsafe impl slotmap::Key for FormIdx {
    fn data(&self) -> slotmap::KeyData {
        Self(slotmap::KeyData::from_ffi(self.0))
    }
}

impl From<TypeIdx> for ComponentIdx {
    fn from(value: TypeIdx) -> Self {
        ComponentIdx((u64::from(value) >> 32) as u32)
    }
}
impl From<EventIdx> for ComponentIdx {
    fn from(value: EventIdx) -> Self {
        ComponentIdx((u64::from(value) >> 32) as u32)
    }
}
impl From<ExternalVariableIdx> for ComponentIdx {
    fn from(value: ExternalVariableIdx) -> Self {
        ComponentIdx((u64::from(value) >> 32) as u32)
    }
}
impl From<FunctionIdx> for ComponentIdx {
    fn from(value: FunctionIdx) -> Self {
        ComponentIdx((u64::from(value) >> 32) as u32)
    }
}

        impl slotmap::Key for $name {
            fn data(&self)
        }
