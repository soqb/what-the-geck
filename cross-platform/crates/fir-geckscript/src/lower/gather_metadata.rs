use fir::{Insert, ResourcesMut};
use hashbrown::HashMap;

use crate::{ItemKind, Script};

use super::{
    ascii::CaselessString,
    context::{Case, ScriptMetadata},
    LowerContext, LowerResources,
};

struct ScriptCache {
    pub tree: rust_sitter::tree_sitter::Tree,
    pub ast: crate::Script,
    pub meta: ScriptMetadata,
}

#[derive(Default)]
pub struct Geckscript {
    script_meta: HashMap<fir::SourceIdx, ScriptCache>,
}

impl<R: ResourcesMut> fir::LowerProject<R> for Geckscript {
    type Input<'a> = &'a str;

    fn make_component_for<'a, S: 'a, F>(
        &mut self,
        project: fir::Project<'a, S, F>,
        component: &mut R::InsertCx<'a>,
    ) -> Result<(), fir::StopToken>
    where
        S: fir::Sources<Input<'a> = Self::Input<'a>>,
        F: fir::Frontend,
    {
        // let mut component = DynamicComponent::new(component_idx, project.sources.project_name());
        for (idx, source) in project.sources.iter_sources() {
            let (tree, ast, errors) = crate::parse::<Script, Script>(None, source);
            project.frontend.report_all(errors)?;

            let mut meta = ScriptMetadata::default();
            for item in &ast.items {
                let ItemKind::Variables(vars) = &item.value.kind else {
                    continue;
                };

                super::context::ck_token_case(project.frontend, source, &vars.ty, Case::Lowercase)?;

                super::iter_lowered_variables(vars)
                    .map(|var| fir::VariableInfo {
                        name: fir::Name {
                            ident: var.name.into_inner(),
                        },
                        owning_form: None,
                        ty: var.ty.into_inner(),
                    })
                    .for_each(|var| {
                        let key = CaselessString::new(var.name.ident.clone());
                        let idx = component.insert_external_variable(var);
                        meta.internal_variable_name_map.insert(key, idx);
                    });
            }

            let cache = ScriptCache { ast, meta, tree };
            self.script_meta.insert(idx, cache);
        }
        Ok(())
    }

    fn lower_scripts<'a, S: 'a, F>(
        &mut self,
        project: fir::Project<'a, S, F>,
        resources: &'a R,
        mut take_script: impl FnMut(Result<fir::Script, fir::StopToken>) -> Result<(), fir::StopToken>,
    ) -> Result<(), fir::StopToken>
    where
        S: fir::Sources<Input<'a> = Self::Input<'a>>,
        F: fir::Frontend,
    {
        let lower_resources = LowerResources::build_from_resources(resources);
        for (source, src) in project.sources.iter_sources() {
            let ScriptCache { tree: _, ast, meta } = self.script_meta.remove(&source).unwrap();
            let cx = LowerContext::new(&lower_resources, project.frontend, &meta, src);
            let val = cx.lower_script(ast);
            take_script(val)?;
        }

        Ok(())
    }
}
