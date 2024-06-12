use fir::DynamicComponent;
use hashbrown::HashMap;

use crate::{ItemKind, Script};

use super::{ascii::CaselessString, context::ScriptMetadata, LowerContext, LowerResources};

struct ScriptCache {
    pub tree: rust_sitter::tree_sitter::Tree,
    pub ast: crate::Script,
    pub meta: ScriptMetadata,
}

#[derive(Default)]
pub struct Geckscript {
    script_meta: HashMap<fir::SourceIdx, ScriptCache>,
}

impl<'a> fir::LowerProject<'a, DynamicComponent> for Geckscript {
    type Input = &'a str;

    fn make_component_for<S: fir::Sources<Self::Input>, F: fir::Frontend>(
        &mut self,
        project: fir::Project<'_, Self::Input, S, F>,
        component_idx: fir::ComponentIdx,
    ) -> fir::Result<DynamicComponent, fir::StopToken> {
        let mut component = DynamicComponent::new(component_idx, project.sources.project_name());
        for (idx, source) in project.sources.iter_sources() {
            let (tree, ast, errors) = crate::parse::<Script, Script>(None, source);
            project.frontend.report_all(errors)?;

            let mut meta = ScriptMetadata::default();
            for item in &ast.items {
                let ItemKind::Variables(vars) = &item.value.kind else {
                    continue;
                };

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
                        let idx = component.insert_variable(var);
                        meta.internal_variable_name_map.insert(key, idx);
                    });
            }

            let cache = ScriptCache { ast, meta, tree };
            self.script_meta.insert(idx, cache);
        }
        Ok(component)
    }

    fn lower_scripts<S: fir::Sources<Self::Input>, F: fir::Frontend>(
        &mut self,
        project: fir::Project<'_, Self::Input, S, F>,
        resources: &fir::Resources<DynamicComponent>,
        mut take_script: impl FnMut(
            fir::Result<fir::Script, fir::StopToken>,
        ) -> Result<(), fir::StopToken>,
    ) -> Result<(), fir::StopToken> {
        let lower_resources = LowerResources::build_from_resources(resources);
        for (source, _) in project.sources.iter_sources() {
            let ScriptCache { tree, ast, meta } = self.script_meta.remove(&source).unwrap();
            let cx = LowerContext::new(&lower_resources, project.frontend, &meta);
            let val = cx.lower_script(ast);
            take_script(val)?;
        }

        Ok(())
    }
}
