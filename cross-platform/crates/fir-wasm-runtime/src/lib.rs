use fir::Ident4;
use wasmtime::Caller;

pub trait Runtime {
    type Err: std::error::Error + Send + Sync + 'static;

    fn get_variable_integer(&mut self, idx: fir::ExternalVariableIdx) -> Result<u32, Self::Err>;
    fn get_variable_form_ref(
        &mut self,
        idx: fir::ExternalVariableIdx,
        ty: Ident4,
    ) -> Result<u32, Self::Err>;
    fn get_variable_object_ref(
        &mut self,
        idx: fir::ExternalVariableIdx,
        ty: Ident4,
    ) -> Result<u32, Self::Err>;
    fn resolve_formid(&mut self, idx: fir::FormIdx) -> Result<u32, Self::Err>;
}

pub fn create_instance<R: Runtime>(mut r: R) -> wasmtime::Result<()> {
    use wasmtime::{Engine, Linker};

    let engine = Engine::default();
    let mut linker = Linker::<R>::new(&engine);

    linker.func_wrap(
        "runtime",
        "resolve_formid",
        |mut caller: Caller<'_, R>, idx: u64| {
            let id = caller.data_mut().resolve_formid(fir::FormIdx(idx))?;
            Ok(id)
        },
    )?;

    Ok(())
}
