use core::fmt;
use core::hash::Hash;
use std::borrow::Cow;

use fir::*;
use hashbrown::{hash_map::Entry, HashMap};

use indexmap::IndexMap;

use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, EntityType, ExportKind, ExportSection, Function,
    FunctionSection, GlobalSection, GlobalType, ImportSection, Instruction, MemorySection,
    MemoryType, Module, TypeSection, ValType,
};

use crate::{
    context::{idx_undefined_adapter, LowerCx},
    generate::const_expr_zero,
    NotFound, Undefined,
};

use super::{CompileDiagnostic, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ImportNamespace {
    Runtime,
    Functions,
}

impl ImportNamespace {
    pub fn as_str(self) -> &'static str {
        match self {
            ImportNamespace::Runtime => "runtime",
            ImportNamespace::Functions => "functions",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Import<'a> {
    pub namespace: ImportNamespace,
    pub name: Cow<'a, str>,
}

impl<'a> Import<'a> {
    fn import(&self, imports: &mut ImportSection, f: impl FnOnce() -> EntityType) {
        imports.import(self.namespace.as_str(), self.name.as_ref(), f());
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WasmPath<'a> {
    Defined(Cow<'a, str>),
    Imported(Import<'a>),
}

impl<'a> fmt::Display for WasmPath<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WasmPath::Defined(name) => write!(f, "{name}"),
            WasmPath::Imported(Import { namespace, name }) => {
                write!(f, "{}:{name}", namespace.as_str())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct WasmStore<'a, T> {
    elements: IndexMap<WasmPath<'a>, T>,
}

impl<'a, T> WasmStore<'a, T> {
    pub fn iter<'b>(&'b self) -> impl Iterator<Item = (&'b WasmPath<'a>, &'b T)> {
        self.elements.iter()
    }

    pub fn len(&self) -> usize {
        self.elements.len()
    }
}
impl<'a, T> Default for WasmStore<'a, T> {
    fn default() -> Self {
        Self {
            elements: Default::default(),
        }
    }
}

impl<'a, T> WasmStore<'a, T> {
    pub fn define(&mut self, path: Cow<'a, str>, value: T) -> u32 {
        self.elements.insert_full(WasmPath::Defined(path), value).0 as u32
    }

    pub fn import(&mut self, path: Import<'a>, value: T) -> u32 {
        self.elements.insert_full(WasmPath::Imported(path), value).0 as u32
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct WasmTypeDef {
    pub params: Vec<ValType>,
    pub results: Vec<ValType>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WasmFunc {
    pub type_index: u32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TyRepr {
    Wasm(ValType),
    Nothing,
}

impl TyRepr {
    pub fn into_option(self) -> Option<ValType> {
        match self {
            TyRepr::Wasm(val_type) => Some(val_type),
            TyRepr::Nothing => None,
        }
    }
}

pub enum InternalVariableSource {
    ResourceInterned,
    Manual(HashMap<InternalVariableIdx, VariableInfo>),
}

pub struct ModuleBuilder<'a, F, R> {
    pub bg: LowerCx<'a, F, R>,
    /// WASM global variable indices for form-internal fir variables.
    pub global_var_indices: HashMap<InternalVariableIdx, (u32, ValType)>,
    pub funcs: WasmStore<'a, WasmTypeDef>,
    pub codes: CodeSection,
    pub internal_source: InternalVariableSource,
    string_idx_map: HashMap<String, u32>,
    strings_len: u32,
}

pub(crate) fn collect_variables<'a, I: Copy, V: Eq + Hash, T>(
    mut get_key: impl FnMut(I, &'a T) -> V,
    mut get_ty: impl FnMut(&'a T) -> Option<TyRepr>,
    repository: impl IntoIterator<Item = (I, &'a T)>,
) -> HashMap<V, (u32, ValType)>
where
    T: 'a,
    I: 'a,
{
    repository
        .into_iter()
        .enumerate()
        .filter_map(|(i, (idx, var))| {
            get_ty(var).and_then(|repr| match repr {
                TyRepr::Wasm(val_type) => Some((get_key(idx, var), (i as u32, val_type))),
                TyRepr::Nothing => None,
            })
        })
        .collect()
}

impl<'a, F: Frontend, R: Resources> ModuleBuilder<'a, F, R> {
    pub fn new(bg: LowerCx<'a, F, R>, internal_source: InternalVariableSource) -> Self {
        let mut cx = Self {
            bg,
            internal_source,
            global_var_indices: HashMap::new(),
            funcs: WasmStore::default(),
            codes: CodeSection::new(),
            string_idx_map: HashMap::new(),
            strings_len: 0,
        };
        cx.global_var_indices = collect_variables(
            |var_idx, _| var_idx,
            |&var_idx| {
                cx.lower_ty_to_repr(Span::NOWHERE.of(cx.bg.res.get_external_variable(var_idx)?.ty))
                    .ok()
            },
            cx.bg.source.variables.iter().map(|(&i, v)| (i, v)),
        );
        cx
    }

    pub fn get_internal_variable(&self, in_idx: InternalVariableIdx) -> Option<&VariableInfo> {
        match &self.internal_source {
            InternalVariableSource::ResourceInterned => self
                .bg
                .source
                .variables
                .get(&in_idx)
                .and_then(|&ex_idx| self.bg.res.get_external_variable(ex_idx)),
            InternalVariableSource::Manual(map) => map.get(&in_idx),
        }
    }

    pub fn string_idx(&mut self, string: String) -> i32 {
        match self.string_idx_map.entry(string) {
            Entry::Occupied(entry) => *entry.get() as i32,
            Entry::Vacant(entry) => {
                let idx = self.strings_len;
                entry.insert(idx);
                self.strings_len = idx + 4;
                idx as i32
            }
        }
    }

    pub fn get_function_definition(
        &self,
        idx: Spanned<FunctionIdx>,
    ) -> Result<(&Name, &FunctionDefinition), Undefined> {
        let func = self.bg.get_function(idx)?;
        match &func.reference {
            FunctionReference::Defined(def) => Ok((&func.name, def)),
            &FunctionReference::Alias { aliased_function } => {
                self.get_function_definition(idx.span_map(|_| aliased_function))
            }
        }
    }

    pub fn lower_ty_to_repr(&self, ty: Spanned<Ty>) -> Result<TyRepr, Undefined> {
        let span = ty.to_span();
        let val_type = match ty.into_inner() {
            Ty::Unit => return Ok(TyRepr::Nothing),
            Ty::Integer | Ty::Bool => ValType::I32,
            Ty::Float => ValType::F32,
            Ty::String => ValType::I32,
            Ty::Ref(_) => ValType::I32,
            Ty::Adt(idx) => match &self.bg.get_type(span.of(idx))?.definition {
                TypeDefinition::FormUnion { .. } => ValType::I32,
                &TypeDefinition::Enum { const_ty, .. } => {
                    return self.lower_ty_to_repr(ty.to_span().of(const_ty))
                }
            },
        };
        Ok(TyRepr::Wasm(val_type))
    }

    pub fn into_module(self) -> Result<Module> {
        let mut module = Module::new();

        let mut memory_section = MemorySection::new();
        let mut export_section = ExportSection::new();
        let mut type_section = TypeSection::new();
        let mut func_section = FunctionSection::new();
        let mut import_section = ImportSection::new();
        let mut global_section = GlobalSection::new();

        for (i, (&var_idx, &(wasm_global_idx, val_type))) in
            self.global_var_indices.iter().enumerate()
        {
            assert_eq!(i, wasm_global_idx as usize);
            let global = self.get_internal_variable(var_idx).unwrap();
            global_section.global(
                GlobalType {
                    mutable: true,
                    val_type,
                },
                &const_expr_zero(val_type),
            );

            export_section.export(
                &format!("variable:{name}", name = global.name.ident),
                ExportKind::Global,
                wasm_global_idx,
            );
        }

        let mut ty_idx = 0;
        for (idx, (path, func)) in self.funcs.iter().enumerate() {
            type_section.function(func.params.iter().copied(), func.results.iter().copied());

            match path {
                WasmPath::Defined(def) => {
                    func_section.function(ty_idx);
                    export_section.export(def.as_ref(), ExportKind::Func, idx as u32);
                }
                WasmPath::Imported(imp) => {
                    imp.import(&mut import_section, || EntityType::Function(idx as u32));
                }
            }

            ty_idx += 1;
        }

        let strings_idx = memory_section.len();
        let mut total_string_len = 0;
        let mut string_data_section = DataSection::new();
        for (value, _) in self.string_idx_map {
            let mut raw = Vec::with_capacity(value.len() + 4);
            raw.extend_from_slice(&(value.len() as u32).to_le_bytes());
            raw.extend_from_slice(value.as_bytes());
            let len = raw.len();
            string_data_section.active(
                strings_idx,
                &ConstExpr::i32_const(total_string_len as i32),
                raw.into_iter(),
            );
            total_string_len += len as u32;
        }

        if total_string_len > 0x80000000 {
            panic!("too many strings!");
        }

        memory_section.memory(MemoryType {
            minimum: (total_string_len / 64000 + 1) as u64,
            maximum: None,
            memory64: false,
            shared: false,
        });

        export_section.export("strings", ExportKind::Memory, strings_idx);

        module.section(&type_section);
        module.section(&import_section);
        module.section(&func_section);
        module.section(&memory_section);
        module.section(&global_section);
        module.section(&export_section);
        module.section(&self.codes);

        module.section(&string_data_section);

        Ok(module)
    }
}

type LocalVarIndices = HashMap<(u32, BodyVariableIdx), (u32, ValType)>;

pub(crate) struct CodeBuilder<'ctx, 's, F, R> {
    pub cx: &'s mut ModuleBuilder<'ctx, F, R>,
    pub local_var_indices: LocalVarIndices,
    pub body: &'s FunctionBody,
    // necessary to fiddle with `local_var_indices`
    // since multiple builders are used to form the same WASM function,
    // so we keep an index to match up the variables in the fir scope.
    local_var_key: u32,
    target_function: Option<&'s mut Function>,
}

impl<'s, 'ctx: 's, F: Frontend, R: Resources> typeck::LocalResources
    for CodeBuilder<'ctx, 's, F, R>
{
    fn get_form(&self, idx: fir::FormIdx) -> Option<&fir::FormInfo> {
        self.cx.bg.res.get_form(idx)
    }

    fn get_external_variable(&self, idx: fir::ExternalVariableIdx) -> Option<&fir::VariableInfo> {
        self.cx.bg.res.get_external_variable(idx)
    }

    fn get_function_def(&self, idx: FunctionIdx) -> Option<&FunctionDefinition> {
        let (_, def) = self
            .cx
            .get_function_definition(Span::NOWHERE.of(idx))
            .ok()?;
        Some(def)
    }

    fn get_body_variable(&self, idx: fir::BodyVariableIdx) -> Option<&BodyVariableInfo> {
        self.body.variables.get(&idx)
    }

    fn get_internal_variable(&self, idx: InternalVariableIdx) -> Option<&fir::VariableInfo> {
        self.cx.get_internal_variable(idx)
    }
}

impl<'s, 'ctx: 's, F: Frontend, R: Resources> CodeBuilder<'ctx, 's, F, R> {
    pub(self) fn new(
        cx: &'s mut ModuleBuilder<'ctx, F, R>,
        body: &'s FunctionBody,
        local_var_key: u32,
    ) -> Self {
        CodeBuilder {
            cx,
            body,
            local_var_indices: HashMap::new(),
            target_function: None,
            local_var_key,
        }
    }

    pub fn get_local_variable(
        &mut self,
        local_variable: Spanned<BodyVariableIdx>,
    ) -> Result<u32, Undefined> {
        idx_undefined_adapter(
            NotFound::LocalVariable,
            |idx| {
                self.local_var_indices
                    .get(&(self.local_var_key, idx))
                    .map(|&(idx, _)| idx)
            },
            local_variable,
        )
    }

    pub fn instruction(&mut self, inst: Instruction<'static>) {
        if let Some(target) = &mut self.target_function {
            target.instruction(&inst);
        }
    }

    /// Signals that the program is contractually unsound.
    ///
    /// This stops the collection of instructions into functions, saving on allocation.
    ///
    /// **NB:** A WASM module can still be derived from an unsound program,
    /// but function bodies are not emitted.
    pub fn silently_fail(&mut self) {
        self.target_function = None;
    }

    pub fn has_failed(&self) -> bool {
        self.target_function.is_none()
    }

    pub fn report_failing(&mut self, diagnostic: impl Into<CompileDiagnostic>) -> Result<()> {
        self.silently_fail();
        self.report(diagnostic)?;
        Ok(())
    }

    pub fn report(&mut self, diagnostic: impl Into<CompileDiagnostic>) -> Result<()> {
        self.cx.bg.report(diagnostic.into())
    }
}

impl<'s, 'ctx: 's, F, C> Extend<Instruction<'static>> for CodeBuilder<'ctx, 's, F, C> {
    fn extend<T: IntoIterator<Item = Instruction<'static>>>(&mut self, iter: T) {
        if let Some(target) = &mut self.target_function {
            for inst in iter {
                target.instruction(&inst);
            }
        }
    }
}

pub(crate) fn function_from_body<F: Frontend, R: Resources>(
    cx: &mut ModuleBuilder<'_, F, R>,
    body: &FunctionBody,
) -> Result<Option<Function>> {
    let mut builder = CodeBuilder::new(cx, body, 0);
    builder.local_var_indices = collect_variables(
        |idx, _| idx,
        |var| builder.cx.lower_ty_to_repr(var.ty).ok(),
        builder.body.variables.iter().map(|(&i, v)| ((0, i), v)),
    );
    let mut func =
        Function::new_with_locals_types(builder.local_var_indices.values().map(|&(_, ty)| ty));
    builder.target_function = Some(&mut func);
    let success = builder.lower_body(body.entrypoint.inner())?;

    Ok(success.then_some(func))
}

pub(crate) fn function_from_single_event<F: Frontend, R: Resources>(
    cx: &mut ModuleBuilder<'_, F, R>,
    event: &'_ EventImpl,
) -> Result<Option<Function>> {
    function_from_body(cx, &event.body)
}

pub(crate) fn function_from_multiple_events<
    'a,
    F: Frontend,
    R: Resources,
    I: Iterator<Item = &'a EventImpl>,
>(
    cx: &mut ModuleBuilder<'_, F, R>,
    events: impl Fn() -> I,
) -> Result<Option<Function>> {
    let mut block_idx_offset = 0;
    let local_var_indices = collect_variables(
        |idx, _| idx,
        |var| cx.lower_ty_to_repr(var.ty).ok(),
        events().flat_map(|event| {
            // this is gooey and awful. :(
            let iter = event
                .body
                .variables
                .iter()
                .map(move |(v_idx, v)| ((block_idx_offset as u32, v_idx), v));
            block_idx_offset += 1;
            return iter;
        }),
    );
    let mut func =
        Function::new_with_locals_types(local_var_indices.iter().map(|(_, &(_, ty))| ty));

    let mut block_idx_offset = 0;
    let mut success = true;
    for event in events() {
        let mut builder = CodeBuilder::new(cx, &event.body, block_idx_offset as u32);
        block_idx_offset += 1;
        builder.target_function = Some(&mut func);

        success &= builder.lower_body(event.body.entrypoint.inner())?;
    }

    Ok(success.then_some(func))
}
