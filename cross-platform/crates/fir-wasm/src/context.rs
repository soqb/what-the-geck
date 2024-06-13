use fir::{
    Diagnostic, EventIdx, EventInfo, ExternalVariableIdx, FormIdx, FormInfo, Frontend, FunctionIdx,
    FunctionInfo, Resources, Spanned, ToSpan, TypeIdx, TypeInfo, VariableInfo,
};

use crate::{NotFound, Result, Undefined};

/// How events should be lowered into WASM functions.
#[derive(Debug, Clone, Default)]
pub enum EventStrategy {
    /// Sequentially checks each condition and runs events in order.
    ///
    /// This is what the vanilla engine does.
    #[default]
    EventLoop,
    /// Runs each event listener when the event is triggered.
    ///
    /// This is best practice, but less compatible with vanilla scripting.
    AdHoc,
}

#[derive(Debug, Clone, Default)]
pub struct LowerOpts {
    pub event_strategy: EventStrategy,
}

pub struct LowerCx<'a, F, R> {
    pub res: &'a R,
    pub opts: LowerOpts,
    pub source: &'a fir::Script,
    pub frontend: &'a mut F,
}

pub(crate) fn idx_undefined_adapter<T, I>(
    kind: NotFound,
    f: impl FnOnce(I) -> Option<T>,
    idx: Spanned<I>,
) -> Result<T, Undefined> {
    let span = idx.to_span();
    f(idx.into_inner()).ok_or_else(|| Undefined { span, kind })
}

impl<'a, F: Frontend, R: Resources> LowerCx<'a, F, R> {
    pub fn get_function(&self, idx: Spanned<FunctionIdx>) -> Result<&'a FunctionInfo, Undefined> {
        idx_undefined_adapter(NotFound::Function, |idx| self.res.get_function(idx), idx)
    }
    pub fn get_form(&self, idx: Spanned<FormIdx>) -> Result<&'a FormInfo, Undefined> {
        idx_undefined_adapter(NotFound::Form, |idx| self.res.get_form(idx), idx)
    }
    pub fn get_event(&self, idx: Spanned<EventIdx>) -> Result<&'a EventInfo, Undefined> {
        idx_undefined_adapter(NotFound::Event, |idx| self.res.get_event(idx), idx)
    }
    pub fn get_type(&self, idx: Spanned<TypeIdx>) -> Result<&'a TypeInfo, Undefined> {
        idx_undefined_adapter(NotFound::Type, |idx| self.res.get_type(idx), idx)
    }

    pub fn get_external_variable(
        &self,
        idx: Spanned<ExternalVariableIdx>,
    ) -> Result<&'a VariableInfo, Undefined> {
        idx_undefined_adapter(
            NotFound::FormExternalVariable,
            |idx| self.res.get_external_variable(idx),
            idx,
        )
    }

    pub fn report(&mut self, diagnostic: impl Diagnostic + 'static) -> Result<()> {
        Ok(self.frontend.report(diagnostic)?)
    }

    // pub fn form(&self, type_hint: Option<ExprTy>, name: Spanned<&str>) -> Result<(FormId, FormTy)> {
    //     match self.index.editor_id_map.get(&Ascii::new(*name)) {
    //         Some(record) => match (record.name.as_str(), &record.fields.parsed, type_hint) {
    //             ("DIAL", _, _) => Ok((
    //                 record.formid,
    //                 FormTy::Ref(RefTy::BaseForm(Some(FormHint::Topic))),
    //             )),
    //             ("IMAD", _, _) => Ok((
    //                 record.formid,
    //                 FormTy::Ref(RefTy::BaseForm(Some(FormHint::ImageSpaceModifier))),
    //             )),
    //             ("IMGS", _, _) => Ok((
    //                 record.formid,
    //                 FormTy::Ref(RefTy::BaseForm(Some(FormHint::ImageSpace))),
    //             )),
    //             ("PACK", _, _) => Ok((
    //                 record.formid,
    //                 FormTy::Ref(RefTy::BaseForm(Some(FormHint::AIPackage))),
    //             )),
    //             ("EFSH", _, _) => Ok((
    //                 record.formid,
    //                 FormTy::Ref(RefTy::BaseForm(Some(FormHint::EffectShader))),
    //             )),
    //             ("SOUN", _, _) => Ok((
    //                 record.formid,
    //                 FormTy::Ref(RefTy::BaseForm(Some(FormHint::Sound))),
    //             )),
    //             ("FACT", _, _) => Ok((
    //                 record.formid,
    //                 FormTy::Ref(RefTy::BaseForm(Some(FormHint::Faction))),
    //             )),
    //             ("PERK", _, _) => Ok((
    //                 record.formid,
    //                 FormTy::Ref(RefTy::BaseForm(Some(FormHint::Perk))),
    //             )),
    //             ("FLST", _, _) => Ok((
    //                 record.formid,
    //                 FormTy::Ref(RefTy::BaseForm(Some(FormHint::FormList))),
    //             )),
    //             ("MESG", _, _) => Ok((
    //                 record.formid,
    //                 FormTy::Ref(RefTy::BaseForm(Some(FormHint::Message))),
    //             )),
    //             ("BOOK", _, _) => Ok((
    //                 record.formid,
    //                 FormTy::Ref(RefTy::BaseForm(Some(FormHint::SpellItem))),
    //             )),

    //             ("ACRE", _, _) => Ok((
    //                 record.formid,
    //                 FormTy::Ref(RefTy::ObjectRef(Some(ObjectRefHint::Creature))),
    //             )),
    //             ("ACHR", _, _) => Ok((
    //                 record.formid,
    //                 FormTy::Ref(RefTy::ObjectRef(Some(ObjectRefHint::Actor))),
    //             )),
    //             (
    //                 "REFR",
    //                 _,
    //                 Some(ExprTy::Concret, Constant, ExprTy, FormHint, GeckTy, ObjectRefHint, ParamTy,
    // ParamUnion, RefTy,, Constant, ExprTy, FormHint, GeckTy, ObjectRefHint, ParamTy,
    // ParamUnion, RefTy,e(GeckTy::Ref(
    //                     ty @ RefTy::BaseForm(Some(FormHint::MapMarker)),
    //                 ))),
    //             ) => Ok((record.formid, FormTy::Ref(ty))),
    //             ("REFR", _, _) => Ok((record.formid, FormTy::Ref(RefTy::ObjectRef(None)))),
    //             (
    //                 "GLOB",
    //                 _,
    //                 Some(ExprTy::Concrete(GeckTy::Ref(
    //                     ty @ RefTy::BaseForm(Some(FormHint::Global)),
    //                 ))),
    //             ) => Ok((record.formid, FormTy::Ref(ty))),
    //             ("GLOB", Some(ParsedFields::Global(glob)), _) => Ok((
    //                 record.formid,
    //                 match glob.type_ {
    //                     records::glob::GlobalType::Float => FormTy::Global(GeckTy::Float),
    //                     records::glob::GlobalType::Short | records::glob::GlobalType::Long => {
    //                         FormTy::Global(GeckTy::Int)
    //                     }
    //                 },
    //             )),
    //             ("SCPT", Some(ParsedFields::Script(_)), _) => {
    //                 Ok((record.formid, FormTy::Ref(RefTy::Unknown)))
    //             }
    //             ("QUST", Some(ParsedFields::Quest(quest)), _) => {
    //                 let Some(script_id) = quest.script else {
    //                     return Ok((record.formid, FormTy::Ref(RefTy::Unknown)));
    //                 };
    //                 match self.index.formid_map.get(&script_id) {
    //                     Some(record) => {
    //                         if let Some(ParsedFields::Script(_)) = &record.fields.parsed {
    //                             Ok((record.formid, FormTy::Ref(RefTy::Unknown)))
    //                         } else {
    //                             Err(CompileFail::BadForm {
    //                                 span: name.span,
    //                                 found: record.name,
    //                             })
    //                         }
    //                     }
    //                     None => Err(CompileFail::InvalidFormId { span: name.span }),
    //                 }
    //             }
    //             _ => Ok((record.formid, FormTy::Ref(RefTy::BaseForm(None)))),
    //         },
    //         None => Err(CompileFail::Undefined {
    //             span: name.span,
    //             kind: NotFound::Form,
    //         }),
    //     }
    // }
}
