pub mod context;
pub mod data;
pub mod generate;

mod diagnostics;
pub use diagnostics::*;

#[cfg(test)]
mod tests {
    use std::io;

    use fir::{Frontend, Insert, LowerProject, ResourcesMut};
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

begin OnAdd player
	set killme to 1
end

begin GameMode
	RemoveMe
	int countofstuff;
	int numberToAdd;
	
	if (killme == 1)
		player.AddItem Ammo556mmSurplus 250
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

        impl fir::Sources for Sources {
            type Input<'a> = &'a str;

            fn project_name(&self) -> &str {
                "joe's project!"
            }

            fn iter_sources(&self) -> impl Iterator<Item = (fir::SourceIdx, &str)> {
                Some((fir::SourceIdx(0), self.one_source)).into_iter()
            }

            fn get_source(&self, idx: fir::SourceIdx) -> Option<&str> {
                if idx == fir::SourceIdx(0) {
                    Some(self.one_source)
                } else {
                    None
                }
            }
        }

        let sources = Sources { one_source: src };
        let project = fir::Project::new(&sources, &mut f);

        let mut resources = fir::TheResources::default();
        let mut instance = Geckscript::default();
        let info = fir::ComponentInfo {
            identifier: "nvse compiled scripts".to_owned(),
        };

        {
            let mut component = resources.new_component_cx(info);
            <_ as LowerProject<fir::TheResources>>::make_component_for(
                &mut instance,
                project,
                &mut component,
            )?;
            component.install();
        }

        fn create_component(res: &mut impl ResourcesMut) {
            fn name(n: &str) -> fir::Name {
                fir::Name {
                    ident: n.to_owned(),
                }
            }

            let mut component = res.new_component_cx(fir::ComponentInfo {
                identifier: "test-dependencies".to_owned(),
            });
            component.insert_external_variable(fir::VariableInfo {
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
            });
            component.insert_function(fir::FunctionInfo {
                name: name("RemoveMe"),
                reference: fir::FunctionReference::Defined(fir::FunctionDefinition {
                    self_param: Some(fir::Ty::Ref(fir::RefTy::ANY_FORM)),
                    params: Vec::new(),
                    return_ty: fir::Ty::Float,
                }),
            });

            component.install();
        }

        create_component(&mut resources);

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

        let bytes = module.finish();
        let mut outstream = std::fs::OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open("out.wasm")
            .unwrap();
        io::copy(&mut &bytes[..], &mut outstream).unwrap();

        Ok(())
    }
}
