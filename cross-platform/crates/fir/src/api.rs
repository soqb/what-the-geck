use core::fmt;
use std::{marker::PhantomData, path::PathBuf};

use hashbrown::HashMap;

use crate::*;

pub enum ProjectLocation {
    OneFile(PathBuf),
    Directory(PathBuf),
    Inline(String),
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident4(pub(self) [ascii::AsciiChar; 4]);

impl fmt::Debug for Ident4 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // NB: don't use `f.debug_tuple` because we want a single line of output.
        write!(f, "Ident4({s})", s = self.as_str())
    }
}

impl Ident4 {
    pub const fn from_str(string: &str) -> Self {
        const fn is_valid(c: &u8) -> bool {
            c.is_ascii_uppercase() || matches!(c, b'_')
        }

        const fn map(b: &u8) -> ascii::AsciiChar {
            ascii::AsciiChar::new(*b as char)
        }

        let [a, b, c, d] = string.as_bytes() else {
            panic!("expected a string of length 4");
        };
        if is_valid(a) && is_valid(b) && is_valid(c) && is_valid(d) {
            Self([map(a), map(b), map(c), map(d)])
        } else {
            panic!("expected the string to contain only uppercase ascii letters")
        }
    }

    pub fn as_str(&self) -> &str {
        let x: &[ascii::AsciiChar] = &self.0;
        // SAFETY: `AsciiChar` has highest bit always 0
        // => each byte in `x` is a utf-8 ascii character.
        let y: &str = unsafe { std::mem::transmute(x) };
        y
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormTy {
    Any,
    Single(Ident4),
}

/// Describes the relationship between the sets of possible values between two types.
///
/// Notionally, these types are called the "A" and "B" types.
/// # Semantics
///
/// Importantly, this type is allowed to represent both objective and semantic relationships.
///
/// For example, `TyRelationship::Identical` should imply that all valid values of type A
/// are valid values of type B, but `SemanticTyRelationship::Analagous` additionally enforces that each value of A
/// maps to an *analagous* value of B (floats and ints of the same size would not be indentical under this model).
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TyRelationship {
    /// A *is* B.
    ///
    /// Every value of A is a value of B *and* vice versa.
    Identical,
    /// A and B have no overlap.
    ///
    /// Every value of A *is not* a value of B *and* vice versa.
    Distinct,
    /// A is a subtype of B; B is a supertype of A.
    ///
    /// Every value of A is a value of B *but not* vice-versa.
    Specialisation,
    /// A is a supertype of B; B is a subtype of A.
    ///
    /// Every value of B is a value of A *but not* vice-versa.
    Generalisation,
}

impl TyRelationship {
    pub fn all_or_nothing(value: bool) -> Self {
        if value {
            Self::Identical
        } else {
            Self::Distinct
        }
    }

    pub fn reverse(self) -> Self {
        match self {
            TyRelationship::Identical | TyRelationship::Distinct => self,
            TyRelationship::Specialisation => TyRelationship::Generalisation,
            TyRelationship::Generalisation => TyRelationship::Specialisation,
        }
    }
}

impl FormTy {
    pub fn relationship_to(self, rhs: Self) -> TyRelationship {
        match (self, rhs) {
            (Self::Any, Self::Any) => TyRelationship::Identical,
            (Self::Any, _) => TyRelationship::Generalisation,
            (_, Self::Any) => TyRelationship::Specialisation,
            (Self::Single(a), Self::Single(b)) => TyRelationship::all_or_nothing(a == b),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Const {
    Integer(i64),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NumericTy {
    Integer,
    Float,
}

impl From<NumericTy> for Ty {
    fn from(value: NumericTy) -> Ty {
        match value {
            NumericTy::Integer => Ty::Integer,
            NumericTy::Float => Ty::Float,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RefTy {
    Object(FormTy),
    Form(FormTy),
    Unknown,
}

impl RefTy {
    pub const ANY_OBJECT: Self = RefTy::Object(FormTy::Any);
    pub const ANY_FORM: Self = RefTy::Form(FormTy::Any);

    pub const fn object(single: &str) -> Self {
        RefTy::Object(FormTy::Single(Ident4::from_str(single)))
    }
    pub const fn form(single: &str) -> Self {
        RefTy::Form(FormTy::Single(Ident4::from_str(single)))
    }

    pub fn relationship_to(self, rhs: Self) -> TyRelationship {
        match (self, rhs) {
            (RefTy::Unknown, RefTy::Unknown) => TyRelationship::Identical,
            (RefTy::Object(a), RefTy::Object(b)) | (RefTy::Form(a), RefTy::Form(b)) => {
                a.relationship_to(b)
            }
            (RefTy::Unknown, RefTy::Object(_) | RefTy::Form(_)) => TyRelationship::Generalisation,
            (RefTy::Object(_) | RefTy::Form(_), RefTy::Unknown) => TyRelationship::Specialisation,
            (RefTy::Object(_), RefTy::Form(_)) | (RefTy::Form(_), RefTy::Object(_)) => {
                TyRelationship::Distinct
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Ty {
    Unit,
    String,
    Bool,
    Integer,
    Float,
    Ref(RefTy),
    Adt(TypeIdx),
}

impl Ty {
    pub const fn form_ref(single: &str) -> Self {
        Self::Ref(RefTy::form(single))
    }

    pub const fn object_ref(single: &str) -> Self {
        Self::Ref(RefTy::object(single))
    }

    pub fn relationship_to(self, rhs: Self) -> TyRelationship {
        if self == rhs {
            return TyRelationship::Identical;
        }
        match (self, rhs) {
            (Self::Ref(a), Self::Ref(b)) => a.relationship_to(b),
            _ => TyRelationship::Distinct,
        }
    }

    /// The subtyping relationship of the shapes of two types.
    ///
    /// For example, integers and floats of the same size have the same shape.
    pub fn morphic_relationship_to(self, rhs: Self) -> TyRelationship {
        if self == rhs {
            return TyRelationship::Identical;
        }
        match (self, rhs) {
            (Self::Integer, Self::Float)
            | (Self::Float, Self::Integer)
            | (Self::Ref(_), Self::Ref(_)) => TyRelationship::Identical,
            _ => TyRelationship::Distinct,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParamInfo {
    pub name: Option<String>,
    pub ty: Ty,
    pub optional: bool,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub self_param: Option<Ty>,
    pub params: Vec<ParamInfo>,
    pub return_ty: Ty,
}

#[derive(Debug)]
pub enum FunctionReference {
    Defined(FunctionDefinition),
    Alias { aliased_function: FunctionIdx },
}

#[derive(Debug)]
pub struct EventInfo {
    pub name: Name,
    pub parameters: Vec<ParamInfo>,
}

#[derive(Debug)]
pub struct FunctionInfo {
    pub name: Name,
    pub reference: FunctionReference,
}

#[derive(Debug)]
pub struct FormInfo {
    pub name: Name,
    pub kind: Ident4,
    pub script: Option<Script>,
}

#[derive(Debug)]
pub enum TypeDefinition {
    Enum {
        const_ty: Ty,
        map: HashMap<String, Const>,
    },
    FormUnion {
        is_object_ref: bool,
        fields: Vec<Ident4>,
    },
}

#[derive(Debug)]
pub struct TypeInfo {
    pub name: Name,
    pub definition: TypeDefinition,
}

#[derive(Debug)]
pub struct VariableInfo {
    pub name: Name,
    pub owning_form: Option<FormIdx>,
    pub ty: Ty,
}

/// Returned from error reporting to signify compilation should not continue.
///
/// Compliant implementations do not have to respect this signal,
/// though they can choose to.
#[derive(Debug, Clone, Copy)]
pub struct StopToken;

pub trait Frontend {
    fn report(&mut self, diagnostic: impl Diagnostic + 'static) -> Result<(), StopToken>;

    fn report_all<D: Diagnostic + 'static>(
        &mut self,
        list: impl IntoIterator<Item = D>,
    ) -> Result<(), StopToken> {
        for diagnostic in list {
            self.report(diagnostic)?;
        }

        Ok(())
    }
}

pub trait TargetContext {
    type Component: Component;

    fn build_component(self, idx: ComponentIdx) -> anyhow::Result<Self::Component>;
}

pub trait Component {
    fn identifier(&self) -> &str;
    fn get_idx(&self) -> ComponentIdx;

    fn get_function(&self, key: FunctionIdx) -> Option<&FunctionInfo>;
    fn get_form(&self, key: FormIdx) -> Option<&FormInfo>;
    fn get_event(&self, key: EventIdx) -> Option<&EventInfo>;
    fn get_type(&self, key: TypeIdx) -> Option<&TypeInfo>;
    fn get_external_variable(&self, key: ExternalVariableIdx) -> Option<&VariableInfo>;

    fn iter_functions(&self) -> impl Iterator<Item = (FunctionIdx, &FunctionInfo)>;
    fn iter_forms(&self) -> impl Iterator<Item = (FormIdx, &FormInfo)>;
    fn iter_events(&self) -> impl Iterator<Item = (EventIdx, &EventInfo)>;
    fn iter_types(&self) -> impl Iterator<Item = (TypeIdx, &TypeInfo)>;
    fn iter_variables(&self) -> impl Iterator<Item = (ExternalVariableIdx, &VariableInfo)>;
}

#[derive(Debug)]
pub struct Resources<C> {
    components: HashMap<ComponentIdx, C>,
    awaiting_components: u32,
}

impl<C> Default for Resources<C> {
    fn default() -> Self {
        Self {
            components: Default::default(),
            awaiting_components: 0,
        }
    }
}

impl<C: Component> Resources<C> {
    fn expected_component_idx(&self) -> u32 {
        self.components.len() as u32
    }

    pub fn next_component_idx(&mut self) -> ComponentIdx {
        let idx = ComponentIdx(self.expected_component_idx() + self.awaiting_components);
        self.awaiting_components += 1;
        idx
    }

    pub fn install_component(&mut self, component: C) {
        if component.get_idx().0 != self.expected_component_idx() {
            panic!(
                "expected a component with {a}, but received {b}",
                a = self.expected_component_idx(),
                b = component.get_idx(),
            );
        }

        self.awaiting_components -= 1;
        self.components.insert(component.get_idx(), component);
    }

    pub fn component(&self, key: ComponentIdx) -> Option<&C> {
        self.components.get(&key)
    }

    pub fn component_mut(&mut self, key: ComponentIdx) -> Option<&mut C> {
        self.components.get_mut(&key)
    }

    fn fetch<I: Into<ComponentIdx> + Copy, D>(
        &self,
        key: I,
        fetcher: impl FnOnce(&C, I) -> Option<&D>,
    ) -> Option<&D> {
        self.component(key.into())
            .and_then(move |comp| fetcher(comp, key))
    }

    pub fn get_function(&self, key: FunctionIdx) -> Option<&FunctionInfo> {
        self.fetch(key, C::get_function)
    }
    pub fn get_form(&self, key: FormIdx) -> Option<&FormInfo> {
        self.fetch(key, C::get_form)
    }
    pub fn get_event(&self, key: EventIdx) -> Option<&EventInfo> {
        self.fetch(key, C::get_event)
    }
    pub fn get_type(&self, key: TypeIdx) -> Option<&TypeInfo> {
        self.fetch(key, C::get_type)
    }
    pub fn get_external_variable(&self, key: ExternalVariableIdx) -> Option<&VariableInfo> {
        self.fetch(key, C::get_external_variable)
    }

    pub fn iter_functions(&self) -> impl Iterator<Item = (FunctionIdx, &FunctionInfo)> {
        self.components
            .iter()
            .flat_map(|(_, info)| info.iter_functions())
    }
    pub fn iter_forms(&self) -> impl Iterator<Item = (FormIdx, &FormInfo)> {
        self.components
            .iter()
            .flat_map(|(_, info)| info.iter_forms())
    }
    pub fn iter_events(&self) -> impl Iterator<Item = (EventIdx, &EventInfo)> {
        self.components
            .iter()
            .flat_map(|(_, info)| info.iter_events())
    }
    pub fn iter_types(&self) -> impl Iterator<Item = (TypeIdx, &TypeInfo)> {
        self.components
            .iter()
            .flat_map(|(_, info)| info.iter_types())
    }
    pub fn iter_variables(&self) -> impl Iterator<Item = (ExternalVariableIdx, &VariableInfo)> {
        self.components
            .iter()
            .flat_map(|(_, info)| info.iter_variables())
    }
}

pub struct DynamicComponent {
    this_idx: ComponentIdx,
    identifier: String,

    functions: HashMap<FunctionIdx, FunctionInfo>,
    forms: HashMap<FormIdx, FormInfo>,
    events: HashMap<EventIdx, EventInfo>,
    types: HashMap<TypeIdx, TypeInfo>,
    variables: HashMap<ExternalVariableIdx, VariableInfo>,
}

impl DynamicComponent {
    pub fn new(idx: ComponentIdx, identifier: impl Into<String>) -> Self {
        Self {
            this_idx: idx,
            identifier: identifier.into(),

            functions: Default::default(),
            forms: Default::default(),
            events: Default::default(),
            types: Default::default(),
            variables: Default::default(),
        }
    }

    fn next_idx<I, D>(&self, collection: &HashMap<I, D>) -> u64 {
        let len: u64 = collection.len() as u32 as u64;
        let comp_idx = u32::from(self.this_idx) as u64;
        (comp_idx << 32) + len
    }

    pub fn insert_function(&mut self, func: FunctionInfo) -> FunctionIdx {
        let key = FunctionIdx(self.next_idx(&self.functions));
        self.functions.insert(key, func);
        key
    }
    pub fn insert_form(&mut self, form: FormInfo) -> FormIdx {
        let key = FormIdx(self.next_idx(&self.forms));
        self.forms.insert(key, form);
        key
    }
    pub fn insert_event(&mut self, event: EventInfo) -> EventIdx {
        let key = EventIdx(self.next_idx(&self.events));
        self.events.insert(key, event);
        key
    }
    pub fn insert_type(&mut self, type_: TypeInfo) -> TypeIdx {
        let key = TypeIdx(self.next_idx(&self.types));
        self.types.insert(key, type_);
        key
    }
    pub fn insert_variable(&mut self, var: VariableInfo) -> ExternalVariableIdx {
        let key = ExternalVariableIdx(self.next_idx(&self.variables));
        if let Some(form_idx) = var.owning_form {
            if let Some(form) = self.forms.get_mut(&form_idx) {
                let variables = &mut form.script.get_or_insert_with(Default::default).variables;
                let idx = InternalVariableIdx(variables.len() as u32);
                variables.insert(idx, key);
            }
        }
        self.variables.insert(key, var);
        key
    }
}

impl Component for DynamicComponent {
    fn get_idx(&self) -> ComponentIdx {
        self.this_idx
    }

    fn identifier(&self) -> &str {
        &self.identifier
    }

    fn get_function(&self, key: FunctionIdx) -> Option<&FunctionInfo> {
        self.functions.get(&key)
    }
    fn get_form(&self, key: FormIdx) -> Option<&FormInfo> {
        self.forms.get(&key)
    }
    fn get_event(&self, key: EventIdx) -> Option<&EventInfo> {
        self.events.get(&key)
    }
    fn get_type(&self, key: TypeIdx) -> Option<&TypeInfo> {
        self.types.get(&key)
    }
    fn get_external_variable(&self, key: ExternalVariableIdx) -> Option<&VariableInfo> {
        self.variables.get(&key)
    }

    fn iter_functions(&self) -> impl Iterator<Item = (FunctionIdx, &FunctionInfo)> {
        self.functions.iter().map(|(&idx, info)| (idx, info))
    }
    fn iter_forms(&self) -> impl Iterator<Item = (FormIdx, &FormInfo)> {
        self.forms.iter().map(|(&idx, info)| (idx, info))
    }
    fn iter_events(&self) -> impl Iterator<Item = (EventIdx, &EventInfo)> {
        self.events.iter().map(|(&idx, info)| (idx, info))
    }
    fn iter_types(&self) -> impl Iterator<Item = (TypeIdx, &TypeInfo)> {
        self.types.iter().map(|(&idx, info)| (idx, info))
    }
    fn iter_variables(&self) -> impl Iterator<Item = (ExternalVariableIdx, &VariableInfo)> {
        self.variables.iter().map(|(&idx, info)| (idx, info))
    }
}

pub trait Backend {
    type Context: TargetContext;
    fn cx_provider(&mut self) -> &mut Self::Context;
}

pub trait Sources<I> {
    fn project_name(&self) -> &str;

    fn iter_sources(&self) -> impl Iterator<Item = (SourceIdx, I)>;
    fn get_source(&self, idx: SourceIdx) -> Option<I>;
}

pub struct Project<'a, I, S, F> {
    pub sources: &'a S,
    pub frontend: &'a mut F,
    _marker: PhantomData<fn() -> I>,
}

impl<'a, I, S: Sources<I>, F: Frontend> Project<'a, I, S, F> {
    pub fn new(sources: &'a S, frontend: &'a mut F) -> Self {
        Self {
            sources,
            frontend,
            _marker: PhantomData,
        }
    }
}

pub trait LowerProject<'a, C: Component>: Sized {
    type Input;

    fn make_component_for<S: Sources<Self::Input>, F: Frontend>(
        &mut self,
        project: Project<'_, Self::Input, S, F>,
        component_idx: ComponentIdx,
    ) -> Result<C, StopToken>;

    fn lower_scripts<S: Sources<Self::Input>, F: Frontend>(
        &mut self,
        project: Project<'_, Self::Input, S, F>,
        resources: &Resources<C>,
        take_script: impl FnMut(Result<Script, StopToken>) -> Result<(), StopToken>,
    ) -> Result<(), StopToken>;
}
