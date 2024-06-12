use std::{any::TypeId, convert::Infallible, marker::PhantomData};

use tree_sitter::Tree;
use winnow::Parser;

use crate::{
    reporting::{Diagnostic, DiagnosticFilter, DiagnosticKind},
    wasm::{self, generate_wasm_for_script},
};

// pub type Tokens<'src> = Vec<rowwin::RichToken<'src, Lang>>;
// pub type Syntax = rowan::SyntaxNode<rowwin::RowanLang<Lang>>;

pub trait Driver: Sized {
    type Error;

    fn reset(&mut self);

    fn fail(diagnostic: &(impl Diagnostic + 'static)) -> Self::Error;

    fn report(&mut self, diagnostic: impl Diagnostic + 'static);

    fn visit<S: PipelineStage<Self>>(&mut self, output: &S::Output<'_>) -> Result<(), Self::Error>;

    fn wasm_context(&self) -> &crate::wasm::Context;
}

macro_rules! fail {
    ($driver:ident) => {
        |err| {
            let failure = D::fail(&err);
            let _ = $driver.report(err);
            return Err(failure);
        }
    };
}

// pub fn tokenize<'src, D: Driver>(
//     driver: &mut D,
//     source: &'src str,
// ) -> Result<Tokens<'src>, D::Error> {
//     <Token as logos::Logos>::lexer(source)
//         .spanned()
//         .map(|(result, span)| {
//             result
//                 .map(|token| rowwin::RichToken {
//                     kind: token,
//                     source: &source[span.clone()],
//                 })
//                 .map_err(|()| TokenError::new(source, span))
//         })
//         .collect::<Result<Vec<_>, _>>()
//         .or_else(fail!(driver))
// }

// pub fn parse<D: Driver>(driver: &mut D, tokens: Tokens<'_>) -> Result<Syntax, D::Error> {
//     let input = Input::new(&tokens);
//     let result =
//         rowwin::finish(winnow::branch::alt((script, block(BlockTerminator::None)))).parse(input);
//     result.or_else(fail!(driver)).map(|(node, diagnostics)| {
//         for diagnostic in diagnostics {
//             driver.report(diagnostic);
//         }
//         node
//     })
// }

pub fn parse<D: Driver>(
    driver: &mut D,
    source: &str,
    old_edited_tree: Option<Tree>,
) -> Result<Tree, D::Error> {
    crate::tree_sitter::parse(source, old_edited_tree)
}

// pub fn build_ast<D: Driver>(driver: &mut D, syntax: Syntax) -> Result<AnyAst, D::Error> {
//     build_script(syntax.clone())
//         .map(AnyAst::Script)
//         .or_else(|_| build_block(syntax).map(AnyAst::Block))
//         .or_else(fail!(driver))
// }

// pub fn compile<D: Driver>(driver: &mut D, ast: &AnyAst) -> Result<wasm_encoder::Module, D::Error> {
//     let AnyAst::Script(script) = ast else {
//         panic!("expected a script!");
//     };
//     generate_wasm_for_script(script, driver.wasm_context())
//         .or_else(fail!(driver))
//         .map(|output| {
//             for diagnostic in output.diagnostics {
//                 driver.report(diagnostic);
//             }
//             output.module
//         })
// }

pub trait PipelineStage<D: Driver>: Sized + 'static {
    type Input<'src>;
    type Output<'src>;

    fn run<'src>(driver: &mut D, input: Self::Input<'src>) -> Result<Self::Output<'src>, D::Error>;
}

pub fn downcast_stage_output<'a, 'b, S2: PipelineStage<D>, S1: PipelineStage<D>, D: Driver>(
    ty: &'a S1::Output<'b>,
) -> Option<&'a S2::Output<'b>> {
    (TypeId::of::<S2>() == TypeId::of::<S1>())
        .then_some(ty)
        .map(|a| {
            // SAFETY: typeids are the same therefore output types are the same!
            unsafe { std::mem::transmute::<_, &S2::Output<'_>>(a) }
        })
}

pub trait Pipeline<D: Driver> {
    type Input<'src>;
    type Output<'src>;

    fn run<'src>(driver: &mut D, input: Self::Input<'src>) -> Result<Self::Output<'src>, D::Error>;
}

pub struct Tail<S> {
    _marker: (PhantomData<S>, Infallible),
}

impl<D: Driver, S: PipelineStage<D>> Pipeline<D> for Tail<S> {
    type Input<'src> = S::Input<'src>;

    type Output<'src> = S::Output<'src>;

    fn run<'src>(
        driver: &mut D,
        input: Self::Input<'src>,
    ) -> Result<Self::Output<'src>, <D as Driver>::Error> {
        S::run(driver, input).and_then(|output| {
            driver.visit::<S>(&output)?;
            Ok(output)
        })
    }
}

impl<D: Driver, P: Pipeline<D>, S> Pipeline<D> for (S, P)
where
    for<'src> S: PipelineStage<D, Output<'src> = P::Input<'src>>,
{
    type Input<'src> = S::Input<'src>;

    type Output<'src> = P::Output<'src>;

    fn run<'src>(driver: &mut D, input: Self::Input<'src>) -> Result<Self::Output<'src>, D::Error> {
        S::run(driver, input).and_then(|inter| {
            driver.visit::<S>(&inter)?;
            P::run(driver, inter)
        })
    }
}

pub mod stages {
    use super::*;

    pub enum Tokenize {}

    pub enum Parse {}
    impl<D: Driver> PipelineStage<D> for Parse {
        type Input<'src> = (&'src str, Option<&'src Tree>);
        type Output<'src> = Tree;

        fn run<'src>(
            driver: &mut D,
            (source, tree): Self::Input<'src>,
        ) -> Result<Self::Output<'src>, D::Error> {
            parse(driver, source, tree)
        }
    }

    // pub enum BuildAst {}
    // impl<D: Driver> PipelineStage<D> for BuildAst {
    //     type Input<'src> = Syntax;
    //     type Output<'src> = Arc<AnyAst>;

    //     fn run<'src>(
    //         driver: &mut D,
    //         input: Self::Input<'src>,
    //     ) -> Result<Self::Output<'src>, D::Error> {
    //         build_ast(driver, input).map(Arc::new)
    //     }
    // }

    // pub enum Compile {}
    // impl<D: Driver> PipelineStage<D> for Compile {
    //     type Input<'src> = Arc<AnyAst>;
    //     type Output<'src> = wasm_encoder::Module;

    //     fn run<'src>(
    //         driver: &mut D,
    //         input: Self::Input<'src>,
    //     ) -> Result<Self::Output<'src>, D::Error> {
    //         compile(driver, input.as_ref())
    //     }
    // }
}

#[macro_export]
macro_rules! pipeline {
    ($first:ty) => {
        $crate::Tail<$first>
    };
    ($first:ty, $($rest:ty),+ $(,)?) => {
        (
            $first,
            $crate::pipeline!($($rest),+),
        )
    };
}

pub fn drive<'src, D: Driver, P: Pipeline<D>>(
    driver: &'src mut D,
    source: P::Input<'src>,
) -> Result<P::Output<'src>, D::Error> {
    driver.reset();
    P::run(driver, source)
}

#[non_exhaustive]
#[derive(Debug, Clone, Default)]
pub struct CompilerDriverConfig {
    pub diagnostic_filter: DiagnosticFilter,
}

impl CompilerDriverConfig {
    pub fn with_filter(self, filter: DiagnosticFilter) -> Self {
        Self {
            diagnostic_filter: filter,
            ..self
        }
    }
}

pub fn find_unfiltered_diagnostic<'a, E, I: IntoIterator<Item = &'a (impl Diagnostic + 'a)>>(
    filter: DiagnosticFilter,
    diagnostics: I,
    make_error: impl FnOnce(&dyn Diagnostic) -> E,
) -> Result<(), E> {
    let ohno = diagnostics
        .into_iter()
        .find(move |err| filter.is_failing(err.kind()))
        .map(|err| make_error(err as &dyn Diagnostic));

    match ohno {
        Some(kind) => Err(kind),
        None => Ok(()),
    }
}

pub struct CompilerDriver<'ctx> {
    pub config: CompilerDriverConfig,
    pub wasm_context: wasm::Context<'ctx>,
    pub diagnostics: Vec<Box<dyn Diagnostic>>,
    pub diagnostics_buffer: Vec<Box<dyn Diagnostic>>,
}

impl<'ctx> Driver for CompilerDriver<'ctx> {
    type Error = DiagnosticKind;

    fn reset(&mut self) {
        self.diagnostics.clear();
    }

    fn fail(diagnostic: &(impl Diagnostic + 'static)) -> Self::Error {
        diagnostic.kind()
    }

    fn report(&mut self, diagnostics: impl Diagnostic + 'static) {
        self.diagnostics_buffer.push(Box::new(diagnostics));
    }

    fn wasm_context(&self) -> &crate::wasm::Context {
        &self.wasm_context
    }

    fn visit<S: PipelineStage<Self>>(&mut self, _: &S::Output<'_>) -> Result<(), Self::Error> {
        let res = find_unfiltered_diagnostic(
            self.config.diagnostic_filter,
            &self.diagnostics_buffer,
            |err| err.kind(),
        );
        self.diagnostics.append(&mut self.diagnostics_buffer);
        res
    }
}
