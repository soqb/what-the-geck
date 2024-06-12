pub mod context;
pub mod data;
pub mod generate;

mod diagnostics;
pub use diagnostics::*;

#[cfg(test)]
mod tests {
    use fir::{DynamicComponent, Frontend, LowerProject};
    use fir_geckscript::lower::Geckscript;

    use crate::{
        context::{EventStrategy, LowerCx, LowerOpts},
        data::{InternalVariableSource, ModuleBuilder},
    };

    #[test]
    fn test_lowering() -> Result<(), fir::StopToken> {
        let src = "\
scn 556mmSurplusAmmoAddScript

int killme

BEGIN OnAdd Player
	Set killme to 1
END

begin GameMode
	RemoveMe
	int countofstuff;
	int numberToAdd;
	
	if (killme == 1)
		Player.AddItem Ammo556mmSurplus 250
		RemoveMe
	endif

end
";

        struct DummyF<'a>(&'a str);

        impl<'a> Frontend for DummyF<'a> {
            fn report(&mut self, diagnostic: impl fir::Diagnostic) -> Result<(), fir::StopToken> {
                fir_geckscript::utils_pain::miette(diagnostic, &self.0);
                Ok(())
            }
        }

        let mut f = DummyF(src);

        struct Sources {
            one_source: &'static str,
        }

        impl fir::Sources<&'static str> for Sources {
            fn project_name(&self) -> &str {
                "joe's project!"
            }

            fn iter_sources(&self) -> impl Iterator<Item = (fir::SourceIdx, &'static str)> {
                Some((fir::SourceIdx(0), self.one_source)).into_iter()
            }

            fn get_source(&self, idx: fir::SourceIdx) -> Option<&'static str> {
                if idx == fir::SourceIdx(0) {
                    Some(self.one_source)
                } else {
                    None
                }
            }
        }

        let sources = Sources { one_source: src };
        let project = fir::Project::new(&sources, &mut f);

        let mut resources = fir::Resources::default();
        let mut instance = {
            let idx = resources.next_component_idx();
            let (instance, component) = Geckscript::make_component(project, idx)?;
            resources.install_component(component);
            instance
        };

        fn create_component(idx: fir::ComponentIdx) -> DynamicComponent {
            fn name(n: &str) -> fir::Name {
                fir::Name {
                    ident: n.to_owned(),
                }
            }

            let mut component = DynamicComponent::new(idx, "test-comp");
            component.insert_variable(fir::VariableInfo {
                name: name("player"),
                owning_form: None,
                ty: fir::Ty::object_ref("ACHR"),
            });
            component.insert_event(fir::EventInfo {
                name: name("OnAdd"),
                parameters: vec![fir::ParamInfo {
                    ty: fir::Ty::object_ref("ACHR"),
                    optional: true,
                    name: None,
                }],
            });
            component.insert_event(fir::EventInfo {
                name: name("GameMode"),
                parameters: vec![fir::ParamInfo {
                    ty: fir::Ty::Integer,
                    optional: true,
                    name: None,
                }],
            });
            component.insert_function(fir::FunctionInfo {
                name: name("AddItem"),
                reference: fir::FunctionReference::Defined(fir::FunctionDefinition {
                    self_param: Some(fir::Ty::Ref(fir::RefTy::Unknown)),
                    params: vec![fir::ParamInfo {
                        ty: fir::Ty::Ref(fir::RefTy::ANY_FORM),
                        optional: false,
                        name: None,
                    }],
                    return_ty: fir::Ty::Float,
                }),
            });
            component.insert_form(fir::FormInfo {
                name: name("Ammo556mmSurplus"),
                kind: fir::Ident4::from_str("AMMO"),
                script: None,
            });
            component.insert_function(fir::FunctionInfo {
                name: name("RemoveMe"),
                reference: fir::FunctionReference::Defined(fir::FunctionDefinition {
                    self_param: Some(fir::Ty::Ref(fir::RefTy::ANY_FORM)),
                    params: Vec::new(),
                    return_ty: fir::Ty::Float,
                }),
            });
            component
        }

        {
            let idx = resources.next_component_idx();
            resources.install_component(create_component(idx));
        }

        let mut script = None;
        let project = fir::Project::new(&sources, &mut f);
        instance
            .lower_scripts(project, &resources, |res| {
                res.map(|yes| script.insert(yes)).map(|_| ())
            })
            .unwrap();

        let cx = LowerCx {
            res: &resources,
            opts: LowerOpts {
                event_strategy: EventStrategy::EventLoop,
            },
            source: script.as_ref().unwrap(),
            frontend: &mut f,
        };

        let mut builder = ModuleBuilder::new(cx, InternalVariableSource::ResourceInterned);
        builder.flush().unwrap_or_else(|e| {
            fir_geckscript::utils_pain::miette(e, src);
        });
        let module = builder.into_module().unwrap();
        std::fs::write("out.wasm", module.as_slice()).unwrap();

        Ok(())
    }
}
