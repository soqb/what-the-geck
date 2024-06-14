use core::fmt;
use std::collections::HashMap;
use std::convert::identity;

use indexmap::IndexMap;

use crate::diagnostics::{LowerDiagnostic, LowerWarning};
use crate::lower::iter_lowered_variables;
use crate::Variables;

use super::ascii::{CaselessStr, CaselessString};
use super::{Respan, Result};

use fir::{
    typeck, utils::ResourcesExt as _, Const, Diagnostic, EventIdx, ExternalVariableIdx, FormIdx,
    Frontend, FunctionDefinition, FunctionIdx, Resources, Spanned, ToSpan, TypeDefinition, TypeIdx,
};

#[derive(Debug)]
struct Scope<'a> {
    variable_name_map: IndexMap<CaselessString, fir::BodyVariableIdx>,
    variable_list: Vec<(fir::BodyVariableIdx, fir::BodyVariableInfo)>,
    parent_scope: Option<&'a Scope<'a>>,
    block: fir::Block,
}

impl Default for Scope<'_> {
    fn default() -> Self {
        Self {
            variable_name_map: Default::default(),
            variable_list: Default::default(),
            parent_scope: Default::default(),
            block: fir::Block {
                statements: Vec::new(),
                termination: fir::BranchTarget::Return,
            },
        }
    }
}

impl<'a> Scope<'a> {
    fn resolve_variable(&self, name: &str) -> Option<fir::BodyVariableIdx> {
        self.variable_name_map
            .get(CaselessStr::new(&name))
            .copied()
            .or_else(|| {
                self.parent_scope
                    .and_then(|scope| scope.resolve_variable(name))
            })
    }

    fn iter_lineage(&self) -> impl Iterator<Item = &Scope<'a>> {
        struct Iter<'a, 'b> {
            next: Option<&'b Scope<'a>>,
        }

        impl<'a, 'b> Iterator for Iter<'a, 'b> {
            type Item = &'b Scope<'a>;

            fn next(&mut self) -> Option<Self::Item> {
                match self.next {
                    Some(slot) => {
                        let parent = slot.parent_scope;
                        self.next = parent;
                        Some(slot)
                    }
                    None => None,
                }
            }
        }

        Iter { next: Some(self) }
    }

    fn variable_count(&self) -> usize {
        self.iter_lineage()
            .map(|scope| scope.variable_list.len())
            .sum()
    }
}

#[derive(Default)]
pub(crate) struct ScriptMetadata {
    pub(crate) internal_variable_name_map: IndexMap<CaselessString, fir::ExternalVariableIdx>,
}

pub struct LowerContext<'a, F, R> {
    resources: &'a LowerResources<'a, R>,
    scope: Scope<'a>,
    frontend: &'a mut F,
    meta: &'a ScriptMetadata,
    src: &'a str,
}

impl<'a, F, R: Resources> typeck::LocalResources for LowerContext<'a, F, R> {
    fn get_form(&self, idx: fir::FormIdx) -> Option<&fir::FormInfo> {
        self.resources.raw.get_form(idx)
    }

    fn get_external_variable(&self, idx: fir::ExternalVariableIdx) -> Option<&fir::VariableInfo> {
        self.resources.raw.get_external_variable(idx)
    }

    fn get_internal_variable(&self, idx: fir::InternalVariableIdx) -> Option<&fir::VariableInfo> {
        let i = u32::from(idx) as usize;
        let (_, &external_idx) = self.meta.internal_variable_name_map.get_index(i)?;
        self.get_external_variable(external_idx)
    }

    fn get_body_variable(&self, idx: fir::BodyVariableIdx) -> Option<&fir::BodyVariableInfo> {
        self.scope
            .variable_list
            .get(u32::from(idx) as usize - self.scope.variable_count() as usize)
            .map(|(_, var)| var)
    }

    fn get_function_def(&self, idx: fir::FunctionIdx) -> Option<&FunctionDefinition> {
        match &self.resources.raw.get_function(idx)?.reference {
            fir::FunctionReference::Defined(def) => Some(def),
            &fir::FunctionReference::Alias { aliased_function } => {
                self.get_function_def(aliased_function)
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Case {
    Lowercase,
}

impl Case {
    #[inline(always)]
    pub fn ck_byte(self, byte: u8) -> bool {
        match self {
            Case::Lowercase => !byte.is_ascii_uppercase(),
        }
    }
}

pub(crate) fn ck_token_case<T>(
    f: &mut impl Frontend,
    src: &str,
    kw: &impl Respan<T>,
    case: Case,
) -> Result<()> {
    let span = kw.span();
    let text = &src[span];
    if text.bytes().any(move |byte| !case.ck_byte(byte)) {
        let expected = text.to_ascii_lowercase();
        f.report(LowerWarning::WrongCase {
            found: span,
            expected,
        })
    } else {
        Ok(())
    }
}

impl<'a, F: Frontend + 'a, R: Resources> LowerContext<'a, F, R> {
    pub(crate) fn ck_token_case<T>(&mut self, kw: &impl Respan<T>) -> Result<()> {
        ck_token_case(self.frontend, self.src, kw, Case::Lowercase)
    }

    fn cmp_case(found: Spanned<&str>, expected: &CaselessStr) -> Option<LowerDiagnostic> {
        if *found.inner() != expected.as_ref() {
            Some(
                LowerWarning::WrongCase {
                    found: found.to_span(),
                    expected: expected.to_string(),
                }
                .into(),
            )
        } else {
            None
        }
    }

    fn get_casewise<'b, I: Copy, T: 'b, U>(
        collection: &'b StrIndex<'b, I, T>,
        name: Spanned<&str>,
        map_data: impl FnOnce(&'b T) -> U,
        error: impl FnOnce(fir::Span) -> LowerDiagnostic,
    ) -> Result<(I, U, Option<LowerDiagnostic>), LowerDiagnostic> {
        collection
            .get_key_value(CaselessStr::new(&name.into_inner()))
            .ok_or_else(|| error(name.to_span()))
            .map(|(&k, &(idx, ref t))| (idx, map_data(t), Self::cmp_case(name, k)))
    }

    pub(crate) fn new(
        resources: &'a LowerResources<'a, R>,
        frontend: &'a mut F,
        meta: &'a ScriptMetadata,
        src: &'a str,
    ) -> Self {
        Self {
            scope: Scope::default(),
            resources,
            meta,
            frontend,
            src,
        }
    }

    pub fn resources(&self) -> &'a R {
        self.resources.raw
    }

    pub fn report(&mut self, d: impl Diagnostic + 'static) -> Result<()> {
        self.frontend.report(d)
    }

    pub fn report_all<D: Diagnostic + 'static>(
        &mut self,
        d: impl IntoIterator<Item = D>,
    ) -> Result<()> {
        self.frontend.report_all(d)
    }

    pub fn in_block<U>(
        &mut self,
        block_termination: fir::BranchTarget,
        f: impl FnOnce(&mut LowerContext<'_, F, R>) -> U,
    ) -> (fir::Block, U) {
        let mut new_scope = LowerContext {
            scope: Scope {
                variable_name_map: Default::default(),
                variable_list: Vec::new(),
                parent_scope: Some(&self.scope),
                block: fir::Block {
                    statements: Vec::new(),
                    termination: block_termination,
                },
            },
            resources: self.resources,
            frontend: self.frontend,
            meta: self.meta,
            src: self.src,
        };
        let return_value = f(&mut new_scope);
        let Scope {
            variable_list,
            block,
            variable_name_map: _,
            parent_scope: _,
        } = new_scope.scope;
        {
            self.scope.variable_list.extend(variable_list);
        }
        (block, return_value)
    }

    pub fn form(
        &self,
        name: Spanned<&str>,
    ) -> Result<(fir::FormIdx, Option<LowerDiagnostic>), LowerDiagnostic> {
        Self::get_casewise(
            &self.resources.forms,
            name,
            identity,
            LowerDiagnostic::UnknownFunction,
        )
        .map(|(idx, _, d)| (idx, d))
    }

    pub fn event(&self, name: Spanned<&str>) -> Result<EventIdx, LowerDiagnostic> {
        self.resources
            .events
            .get(CaselessStr::new(&name.into_inner()))
            .map(|&(id, _)| id)
            .ok_or_else(|| LowerDiagnostic::UnknownEvent(name.to_span()))
    }

    pub fn enum_variant(
        &self,
        name: Spanned<&str>,
        type_idx: TypeIdx,
    ) -> Option<(&'a Const, Option<LowerDiagnostic>)> {
        self.resources
            .enums
            .get_key_value(&(type_idx, CaselessStr::new(name.inner())))
            .map(|(&(_, ref k), &t)| (t, Self::cmp_case(name, k)))
    }

    pub fn function(
        &self,
        name: Spanned<&str>,
    ) -> Result<(FunctionIdx, &FunctionDefinition, Option<LowerDiagnostic>), LowerDiagnostic> {
        Self::get_casewise(
            &self.resources.functions,
            name,
            Clone::clone,
            LowerDiagnostic::UnknownFunction,
        )
    }

    pub fn local_variable(&self, name: &str) -> Option<fir::BodyVariableIdx> {
        self.scope.resolve_variable(name)
    }

    fn get_variable_internally(
        &self,
        name: Spanned<&str>,
    ) -> Option<(fir::VariableIdx, Option<LowerDiagnostic>)> {
        let key = CaselessStr::new(name.into_inner());

        let map = &self.meta.internal_variable_name_map;
        let Some(&idx) = map.get(key) else {
            return None;
        };

        Some((
            fir::VariableIdx::FormExternal(idx),
            Self::cmp_case(name, key),
        ))
    }

    fn get_variable_externally(
        &self,
        owning_form: Option<FormIdx>,
        name: Spanned<&str>,
    ) -> Option<(fir::VariableIdx, Option<LowerDiagnostic>)> {
        if let Ok(((_, str), &idx)) = self
            .resources
            .variables
            .get_key_value(&(owning_form, CaselessStr::new(&name.into_inner())))
            .ok_or_else(|| LowerDiagnostic::UnknownFunction(name.to_span()))
        // .map(|(, &idx)| (idx, Self::cmp_case(name, str)))
        {
            let idx = fir::VariableIdx::FormExternal(idx);
            return Some((idx, Self::cmp_case(name, str)));
        }

        None
    }

    pub fn variable(
        &self,
        owning_form: Option<FormIdx>,
        name: Spanned<&str>,
    ) -> Option<(fir::VariableIdx, Option<LowerDiagnostic>)> {
        if owning_form.is_none() {
            match self.get_variable_internally(name) {
                r @ Some(_) => return r,
                None => (),
            }
        }

        self.get_variable_externally(owning_form, name)

        // let key = CaselessStr::new(name.into_inner());

        // let map = &self.meta.internal_variable_name_map;
        // let Some(idx) = map.get_index_of(key) else {
        //     return None;
        // };
        // let (key, _) = map.get_index(idx).unwrap();
        // Some((
        //     fir::VariableIdx::FormInternal(fir::InternalVariableIdx(idx as u32)),
        //     Self::cmp_case(name, key.borrow()),
        // ))

        // None
    }

    pub fn insert_body_variables(&mut self, variables: Variables) {
        let var_count = self.scope.variable_count();

        let Self {
            scope:
                Scope {
                    variable_name_map,
                    variable_list,
                    ..
                },
            ..
        } = self;

        let iter = iter_lowered_variables(&variables)
            .enumerate()
            .map(|(idx, var)| (fir::BodyVariableIdx((var_count + idx) as u32), var))
            .inspect(|&(idx, ref var)| {
                variable_name_map.insert(CaselessString::new(var.name.inner().clone()), idx);
            });
        variable_list.extend(iter);
    }

    pub fn push_stmt(&mut self, stmt: Spanned<fir::Statement>) {
        self.scope.block.statements.push(stmt);
    }

    pub fn flush_function_body(&mut self, block: Spanned<fir::Block>) -> fir::FunctionBody {
        assert!(self.scope.parent_scope.is_none());
        fir::FunctionBody {
            variables: self.scope.variable_list.drain(..).collect(),
            entrypoint: block,
        }
    }
}

type StrIndex<'a, I, T = ()> = HashMap<&'a CaselessStr, (I, T)>;

pub struct LowerResources<'a, R> {
    raw: &'a R,
    functions: StrIndex<'a, FunctionIdx, &'a FunctionDefinition>,
    forms: StrIndex<'a, FormIdx>,
    events: StrIndex<'a, EventIdx>,
    enums: HashMap<(TypeIdx, &'a CaselessStr), &'a Const>,
    variables: HashMap<(Option<FormIdx>, &'a CaselessStr), ExternalVariableIdx>,
}

impl<'a, R: Resources> LowerResources<'a, R> {
    fn reformulate_index<I: Copy, V: 'a, T>(
        index: impl Iterator<Item = (I, &'a V)>,
        mut name: impl FnMut(&'a V) -> Option<&'a str>,
        mut info_map: impl FnMut(&'a V) -> T,
    ) -> StrIndex<'a, I, T> {
        index
            .filter_map(|(id, info)| {
                name(info).map(|name| (CaselessStr::new(name), (id, info_map(info))))
            })
            .collect()
    }

    pub fn build_from_resources(raw: &'a R) -> Self {
        let enums = raw
            .iter_types()
            .filter_map(|(idx, info)| {
                if let TypeDefinition::Enum { map, .. } = &info.definition {
                    Some((idx, map))
                } else {
                    None
                }
            })
            .flat_map(|(idx, map)| {
                map.iter()
                    .map(move |(key, tag)| ((idx, CaselessStr::new(key.as_str())), tag))
            })
            .collect();

        Self {
            functions: Self::reformulate_index(
                raw.iter_functions(),
                |i| Some(&i.name.ident),
                |f| match &f.reference {
                    fir::FunctionReference::Defined(def) => def,
                    &fir::FunctionReference::Alias { aliased_function } => raw
                        .get_function_definition(aliased_function)
                        .expect("alias cycle!"),
                },
            ),
            events: Self::reformulate_index(raw.iter_events(), |i| Some(&i.name.ident), drop),
            // global_variables: Self::reformulate_index(
            //     raw.iter_variables()
            //         .filter(|(_, info)| info.owning_form.is_none()),
            //     |i| Some(&i.name.ident),
            //     drop,
            // ),
            forms: Self::reformulate_index(
                raw.iter_forms(),
                |i| Some(&i.name.ident),
                drop,
                // |form| {
                //     Self::reformulate_index(
                //         form.script
                //             .iter()
                //             .flat_map(|script| &script.variables)
                //             .map(|(&int, ext)| (int, ext)),
                //         |&var| {
                //             raw.get_external_variable(var)
                //                 .map(|var| var.name.ident.as_str())
                //         },
                //         Clone::clone,
                //     )
                // },
            ),
            variables: raw
                .iter_variables()
                .map(|(id, info)| {
                    (
                        (info.owning_form.clone(), CaselessStr::new(&info.name.ident)),
                        id,
                    )
                })
                .collect(),
            raw,
            enums,
        }
    }
}
