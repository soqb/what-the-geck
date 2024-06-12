use std::{any::TypeId, marker::PhantomData};

use thiserror::Error;

use crate::{
    ast::{Block, Script},
    parse::Lang,
    reporting::{DiagnosticFilter, Reportable},
};

pub struct PipelineContext<'ctx> {
    config: &'ctx PipelineConfig,
    diagnostics: Vec<Box<dyn Reportable>>,
}

impl<'ctx> PipelineContext<'ctx> {
    pub fn report<S: for<'a> PipelineStage<'a>>(
        &mut self,
        diagnostic: impl Reportable + 'static,
    ) -> PipelineError {
        self.diagnostics.push(Box::new(diagnostic));
        S::make_error()
    }
}

#[derive(Debug, Clone, Error)]
#[error("failed during {type_name}")]
pub struct PipelineError {
    type_id: TypeId,
    type_name: &'static str,
}

impl PipelineError {
    pub fn during<S: for<'a> PipelineStage<'a>>(&self) -> bool {
        TypeId::of::<S>() == self.type_id
    }
}

pub type PipelineResult<T> = Result<T, PipelineError>;

pub trait PipelineStage<'ctx>: 'static {
    type Input<'src>
    where
        'ctx: 'src;
    type Output<'src>
    where
        'ctx: 'src;

    fn execute_stage<'src>(
        &self,
        context: &mut PipelineContext,
        input: Self::Input<'src>,
    ) -> PipelineResult<Self::Output<'src>>;

    fn make_error() -> PipelineError {
        PipelineError {
            type_id: TypeId::of::<Self>(),
            type_name: std::any::type_name::<Self>(),
        }
    }
}

pub trait PipelineCons<'src, 'ctx: 'src, A: 'ctx> {
    type Input;
    type Output;

    fn process(
        &'src self,
        context: &mut PipelineContext,
        input: Self::Input,
        args: &'ctx A,
    ) -> PipelineResult<Self::Output>;
}

impl<'src, 'ctx: 'src, C, S, A: 'ctx> PipelineCons<'src, 'ctx, A> for (C, S)
where
    C: PipelineCons<'src, 'ctx, A>,
    S: PipelineStage<'ctx, Input<'src> = C::Output>,
{
    type Input = C::Input;

    type Output = S::Output<'src>;

    fn process(
        &'src self,
        parent_context: &mut PipelineContext,
        input: Self::Input,
        args: &'ctx A,
    ) -> PipelineResult<Self::Output>
    where
        'ctx: 'src,
    {
        let output = self.0.process(parent_context, input, args)?;
        let mut context = PipelineContext {
            diagnostics: Vec::new(),
            config: parent_context.config,
        };
        self.1
            .execute_stage(&mut context, output)
            .and_then(move |output| {
                let all_diagnostics_ok = context
                    .diagnostics
                    .iter()
                    .all(|e| e.kind() < parent_context.config.diagnostic_filter.kind);
                parent_context.diagnostics.extend(context.diagnostics);
                if all_diagnostics_ok {
                    Ok(output)
                } else {
                    Err(S::make_error())
                }
            })
    }
}

pub mod stages {
    #[derive(Default)]
    pub struct Tokenize(());
    #[derive(Default)]
    pub struct Parse(());
    #[derive(Default)]
    pub struct ScriptAst(());
    #[derive(Default)]
    pub struct BlockAst(());
    #[derive(Default)]
    pub struct UnknownAst(());
    #[derive(Default)]
    pub struct Compile(());
}

impl<'ctx> PipelineStage<'ctx> for stages::Tokenize {
    type Input<'src> = &'src str where 'ctx: 'src;
    type Output<'src> = Vec<rowwin::RichToken<'src, Lang>> where 'ctx: 'src;

    fn execute_stage<'src>(
        &self,
        context: &mut PipelineContext,
        source: Self::Input<'src>,
    ) -> PipelineResult<Self::Output<'src>>
    where
        'ctx: 'src,
    {
        use crate::parse::{Token, TokenError};
        <Token as logos::Logos>::lexer(source)
            .spanned()
            .map(|(result, span)| {
                result
                    .map(|token| rowwin::RichToken {
                        kind: token,
                        source: &source[span.clone()],
                    })
                    .map_err(|()| TokenError::new(source, span))
            })
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| context.report::<Self>(e))
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub enum SourceContext {
    Script,
    Block,
    #[default]
    Unknown,
}

impl<'ctx> PipelineStage<'ctx> for stages::Parse {
    type Input<'src> = (Vec<rowwin::RichToken<'src, Lang>>, SourceContext) where 'ctx: 'src;
    type Output<'src> = rowan::SyntaxNode<rowwin::RowanLang<Lang>> where 'ctx: 'src;

    fn execute_stage<'src>(
        &self,
        context: &mut PipelineContext,
        (input, kind): Self::Input<'src>,
    ) -> PipelineResult<Self::Output<'src>>
    where
        'ctx: 'src,
    {
        use crate::parse::{block, script, BlockTerminator, Input};
        use rowwin::finish;
        use winnow::{combinator::alt, Parser};

        let input = Input::new(&input);
        let result = match kind {
            SourceContext::Script => finish(script).parse(input),
            SourceContext::Block => finish(block(BlockTerminator::None)).parse(input),
            SourceContext::Unknown => {
                finish(alt((script, block(BlockTerminator::None)))).parse(input)
            }
        };
        result
            .map(|(node, diagnostics)| {
                for diagnostic in diagnostics {
                    context.report::<Self>(diagnostic);
                }
                node
            })
            .map_err(|e| context.report::<Self>(e))
    }
}

impl<'ctx> PipelineStage<'ctx> for stages::ScriptAst {
    type Input<'src> = rowan::SyntaxNode<rowwin::RowanLang<Lang>> where 'ctx: 'src;
    type Output<'src> = Script where 'ctx: 'src;

    fn execute_stage<'src>(
        &self,
        context: &mut PipelineContext,
        input: Self::Input<'src>,
    ) -> PipelineResult<Self::Output<'src>>
    where
        'ctx: 'src,
    {
        use crate::ast::build_script;

        build_script(input).map_err(|e| context.report::<Self>(e))
    }
}

impl<'ctx> PipelineStage<'ctx> for stages::BlockAst {
    type Input<'src> = rowan::SyntaxNode<rowwin::RowanLang<Lang>> where 'ctx: 'src;
    type Output<'src> = Block where 'ctx: 'src;

    fn execute_stage<'src>(
        &self,
        context: &mut PipelineContext,
        input: Self::Input<'src>,
    ) -> PipelineResult<Self::Output<'src>>
    where
        'ctx: 'src,
    {
        use crate::ast::build_block;

        build_block(input).map_err(|e| context.report::<Self>(e))
    }
}

impl<'ctx> PipelineStage<'ctx> for stages::UnknownAst {
    type Input<'src> = rowan::SyntaxNode<rowwin::RowanLang<Lang>> where 'ctx: 'src;
    type Output<'src> = crate::ast::AnyAst where 'ctx: 'src;

    fn execute_stage<'src>(
        &self,
        context: &mut PipelineContext,
        input: Self::Input<'src>,
    ) -> PipelineResult<Self::Output<'src>>
    where
        'ctx: 'src,
    {
        use crate::ast::{build_block, build_script, AnyAst};

        build_script(input.clone())
            .map(AnyAst::Script)
            .or_else(|_| build_block(input.clone()).map(AnyAst::Block))
            .map_err(|e| context.report::<Self>(e))
    }
}

impl<'ctx> PipelineStage<'ctx> for stages::Compile {
    type Output<'src> = wasm_encoder::Module where 'ctx: 'src;
    type Input<'src> = (Script, &'ctx crate::wasm::Context<'ctx>) where 'ctx: 'src;

    fn execute_stage<'src>(
        &self,
        context: &mut PipelineContext,
        (input, wasm_context): Self::Input<'src>,
    ) -> PipelineResult<Self::Output<'src>>
    where
        'ctx: 'src,
    {
        crate::wasm::generate_wasm_for_script(&input, wasm_context)
            .map(|output| {
                for diagnostic in output.diagnostics {
                    context.report::<Self>(diagnostic);
                }
                output.module
            })
            .map_err(|e| context.report::<Self>(e))
    }
}

pub mod private {
    use std::marker::PhantomData;

    pub struct TakeInput<T> {
        pub(super) _marker: PhantomData<T>,
    }

    pub struct Map<C, P, S> {
        pub(super) inner: C,
        pub(super) picker: P,
        pub(super) _marker: PhantomData<S>,
    }
}

impl<'src, 'ctx: 'src, T, A: 'ctx> PipelineCons<'src, 'ctx, A> for private::TakeInput<T> {
    type Input = T;
    type Output = T;

    fn process(
        &'src self,
        _: &mut PipelineContext,
        input: Self::Input,
        _: &'ctx A,
    ) -> PipelineResult<Self::Output>
    where
        'ctx: 'src,
    {
        Ok(input)
    }
}

impl<
        'src,
        'ctx: 'src,
        A: 'ctx,
        C: PipelineCons<'src, 'ctx, A>,
        S: PipelineStage<'ctx>,
        F: Picker<'src, 'ctx, A, C, S>,
    > PipelineCons<'src, 'ctx, A> for private::Map<C, F, S>
{
    type Input = C::Input;
    type Output = S::Input<'src>;

    fn process(
        &'src self,
        context: &mut PipelineContext,
        input: Self::Input,
        args: &'ctx A,
    ) -> PipelineResult<Self::Output>
    where
        'ctx: 'src,
    {
        let output = self.inner.process(context, input, args)?;
        Ok(self.picker.pick(output, args))
    }
}

pub trait Picker<'src, 'ctx, A, C, S>
where
    'ctx: 'src,
    A: 'ctx,
    C: PipelineCons<'src, 'ctx, A>,
    S: PipelineStage<'ctx>,
{
    fn pick(&'src self, input: C::Output, args: &'ctx A, stage: impl) -> S::Input<'src>;
}

impl<'src, 'ctx, A, C, S, F: Fn(C::Output, &'ctx A) -> S::Input<'src>> Picker<'src, 'ctx, A, C, S>
    for F
where
    'ctx: 'src,
    A: 'ctx,
    C: PipelineCons<'src, 'ctx, A>,
    S: PipelineStage<'ctx>,
{
    fn pick(&'src self, input: C::Output, args: &'ctx A) -> S::Input<'src> {
        self(input, args)
    }
}

#[non_exhaustive]
#[derive(Debug, Clone, Default)]
pub struct PipelineConfig {
    pub diagnostic_filter: DiagnosticFilter,
}

impl PipelineConfig {
    pub fn with_filter(self, filter: DiagnosticFilter) -> Self {
        Self {
            diagnostic_filter: filter,
            ..self
        }
    }
}

#[derive(Clone)]
pub struct Pipeline<'ctx, C, A> {
    config: PipelineConfig,
    cons: C,
    args: &'ctx A,
    _marker: PhantomData<(&'ctx (), A)>,
}

impl<'ctx, T, A> Pipeline<'ctx, private::TakeInput<T>, A> {
    pub fn new(config: PipelineConfig, args: &'ctx A) -> Self {
        Pipeline {
            cons: private::TakeInput {
                _marker: PhantomData,
            },
            config,
            args,
            _marker: PhantomData,
        }
    }
}

impl<'ctx, A: 'ctx, C> Pipeline<'ctx, C, A> {
    pub fn wrap<'src, C2, F>(self, f: F) -> Pipeline<'ctx, C2, A>
    where
        'ctx: 'src,
        C: PipelineCons<'src, 'ctx, A>,
        F: FnOnce(C) -> C2,
    {
        Pipeline {
            _marker: PhantomData,
            cons: f(self.cons),
            args: self.args,
            config: self.config,
        }
    }

    pub fn then_value<'src, S>(self, stage: S) -> Pipeline<'ctx, (C, S), A>
    where
        'ctx: 'src,
        S: PipelineStage<'ctx>,
        C: PipelineCons<'src, 'ctx, A, Output = S::Input<'src>>,
    {
        self.wrap(|previous| (previous, stage))
    }

    pub fn then<'src, S>(self) -> Pipeline<'ctx, (C, S), A>
    where
        'ctx: 'src,
        S: PipelineStage<'ctx> + Default,
        C: PipelineCons<'src, 'ctx, A, Output = S::Input<'src>>,
    {
        self.then_value(S::default())
    }

    pub fn then_pick<'src, S, P>(self, picker: P) -> Pipeline<'ctx, (private::Map<C, P, S>, S), A>
    where
        'ctx: 'src,
        C: PipelineCons<'src, 'ctx, A>,
        P: Picker<'src, 'ctx, A, C, S>,
        S: PipelineStage<'ctx> + Default,
    {
        self.wrap(|inner| private::Map {
            picker,
            inner,
            _marker: PhantomData,
        })
        .then::<S>()
    }

    pub fn run<'src>(
        &'src self,
        input: C::Input,
    ) -> PipelineOutput<'ctx, <C as PipelineCons<'src, 'ctx, A>>::Output>
    where
        'ctx: 'src,
        C: PipelineCons<'src, 'ctx, A>,
    {
        let mut context = PipelineContext {
            diagnostics: Vec::new(),
            config: &self.config,
        };
        let output = self.cons.process(&mut context, input, self.args);
        PipelineOutput {
            output,
            diagnostics: context.diagnostics,
        }
    }
}

pub struct PipelineOutput<'a, O> {
    pub output: PipelineResult<O>,
    pub diagnostics: Vec<Box<dyn Reportable + 'a>>,
}

// fn a() {
//     struct Ditto<T> {
//         _marker: PhantomData<T>,
//     }

//     impl<'ctx, T: 'ctx> PipelineStage<'ctx> for Ditto<T> {
//         type Output<'src> = T;
//         type Input<'src> = T;

//         fn process<'src>(
//             &self,
//             context: &mut PipelineContext,
//             input: Self::Input<'src>,
//         ) -> PipelineResult<Self::Output<'src>>
//         where
//             'ctx: 'src,
//         {
//             Ok(input)
//         }
//     }
//     let s = String::new();
//     Pipeline::new(ErrorKind::Nothing)
//         .then_value(Ditto {
//             _marker: PhantomData,
//         })
//         .run(&s, &());
// }
