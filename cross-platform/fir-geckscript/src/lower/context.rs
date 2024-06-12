use std::borrow::Borrow;
use std::collections::HashMap;
use std::convert::identity;

use indexmap::IndexMap;

use crate::diagnostics::{LowerDiagnostic, LowerWarning};
use crate::lower::iter_lowered_variables;
use crate::Variables;

use super::ascii::{CaselessStr, CaselessString};
use super::Result;

use fir::{
    typeck, Const, Diagnostic, DynamicComponent, EventIdx, ExternalVariableIdx, FormIdx, Frontend,
    FunctionDefinition, FunctionIdx, InternalVariableIdx, Resources, Spanned, ToSpan,
    TypeDefinition, TypeIdx,
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

pub struct LowerContext<'a, F> {
    resources: &'a LowerResources<'a>,
    scope: Scope<'a>,
    frontend: &'a mut F,
    meta: &'a ScriptMetadata,
}

impl<'a, F> typeck::LocalResources for LowerContext<'a, F> {
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

impl<'a, F: Frontend + 'a> LowerContext<'a, F> {
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

    fn get_casewise<'b, I: Copy, T: 'b, R>(
        collection: &'b Index<'b, I, T>,
        name: Spanned<&str>,
        map_data: impl FnOnce(&'b T) -> R,
        error: impl FnOnce(fir::Span) -> LowerDiagnostic,
    ) -> Result<(I, R, Option<LowerDiagnostic>), LowerDiagnostic> {
        collection
            .get_key_value(CaselessStr::new(&name.into_inner()))
            .ok_or_else(|| error(name.to_span()))
            .map(|(&k, &(idx, ref t))| (idx, map_data(t), Self::cmp_case(name, k)))
    }

    pub fn new(
        resources: &'a LowerResources<'a>,
        frontend: &'a mut F,
        meta: &'a ScriptMetadata,
    ) -> Self {
        Self {
            scope: Scope::default(),
            resources,
            meta,
            frontend,
        }
    }

    pub fn resources(&self) -> &'a Resources<DynamicComponent> {
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

    pub fn in_block<R>(
        &mut self,
        block_termination: fir::BranchTarget,
        f: impl FnOnce(&mut LowerContext<'_, F>) -> R,
    ) -> (fir::Block, R) {
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
    ) -> Result<
        (
            fir::FormIdx,
            &Index<InternalVariableIdx, ExternalVariableIdx>,
            Option<LowerDiagnostic>,
        ),
        LowerDiagnostic,
    > {
        Self::get_casewise(
            &self.resources.forms,
            name,
            identity,
            LowerDiagnostic::UnknownFunction,
        )
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

    pub fn field_of(
        variables: Spanned<&Index<InternalVariableIdx, ExternalVariableIdx>>,
        field: Spanned<&str>,
    ) -> Result<
        (
            fir::InternalVariableIdx,
            fir::ExternalVariableIdx,
            Option<LowerDiagnostic>,
        ),
        LowerDiagnostic,
    > {
        Self::get_casewise(variables.inner(), field, Clone::clone, |variable| {
            LowerDiagnostic::UnknownVariable {
                form: variables.to_span(),
                variable,
            }
        })
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

    pub fn global_variable(
        &self,
        name: Spanned<&str>,
    ) -> Option<(fir::VariableIdx, Option<LowerDiagnostic>)> {
        if let Ok((idx, _, warn)) = Self::get_casewise(
            &self.resources.global_variables,
            name,
            Clone::clone,
            |reference| LowerDiagnostic::Unknown1stReference { reference },
        ) {
            return Some((fir::VariableIdx::FormExternal(idx), warn));
        }

        let key = CaselessStr::new(name.into_inner());

        let map = &self.meta.internal_variable_name_map;
        let Some(idx) = map.get_index_of(key) else {
            return None;
        };
        let (key, _) = map.get_index(idx).unwrap();
        Some((
            fir::VariableIdx::FormInternal(fir::InternalVariableIdx(idx as u32)),
            Self::cmp_case(name, key.borrow()),
        ))
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

type Index<'a, I, T = ()> = HashMap<&'a CaselessStr, (I, T)>;

pub struct LowerResources<'a> {
    raw: &'a Resources<DynamicComponent>,
    functions: Index<'a, FunctionIdx, &'a FunctionDefinition>,
    forms: Index<'a, FormIdx, Index<'a, InternalVariableIdx, ExternalVariableIdx>>,
    events: Index<'a, EventIdx>,
    enums: HashMap<(TypeIdx, &'a CaselessStr), &'a Const>,
    global_variables: Index<'a, ExternalVariableIdx>,
}

impl<'a> LowerResources<'a> {
    fn reformulate_index<I: Copy, V: 'a, T>(
        index: impl Iterator<Item = (I, &'a V)>,
        mut name: impl FnMut(&'a V) -> Option<&'a str>,
        mut info_map: impl FnMut(&'a V) -> T,
    ) -> Index<'a, I, T> {
        index
            .filter_map(|(id, info)| {
                name(info).map(|name| (CaselessStr::new(name), (id, info_map(info))))
            })
            .collect()
    }

    pub fn build_from_resources(raw: &'a Resources<DynamicComponent>) -> Self {
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
            global_variables: Self::reformulate_index(
                raw.iter_variables()
                    .filter(|(_, info)| info.owning_form.is_none()),
                |i| Some(&i.name.ident),
                drop,
            ),
            forms: Self::reformulate_index(
                raw.iter_forms(),
                |i| Some(&i.name.ident),
                |form| {
                    Self::reformulate_index(
                        form.script
                            .iter()
                            .flat_map(|script| &script.variables)
                            .map(|(&int, ext)| (int, ext)),
                        |&var| {
                            raw.get_external_variable(var)
                                .map(|var| var.name.ident.as_str())
                        },
                        Clone::clone,
                    )
                },
            ),
            raw,
            enums,
        }
    }
}
