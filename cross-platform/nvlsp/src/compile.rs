use std::{borrow::Cow, cell::RefCell, pin::Pin};

use fir::{
    typeck::{self, LocalResources, TyHint, TypeckEngine, TypeckResult},
    Block, BranchKind, ConversionReason, Diagnostic, DiagnosticKind, DynamicComponent, EventImpl,
    Expression, Frontend, FunctionBody, InternalVariableIdx, LowerProject, Operator, Resources,
    Span, Spanned, Statement, TargetContext, ToSpan,
    Tried::{self, Resolved, Unresolvable},
    Ty, UserFunctionDefinition, VariableInfo, Warning,
};
use fir_geckscript::lower::{Geckscript, LowerResources};
use hashbrown::{HashMap, HashSet};
use lsp_types::Url;

use crate::{
    context::{Symbol, SymbolKind, TextDocument},
    tree_sitter,
    utils::SpanMap,
};

#[derive(Default)]
struct NvlspFrontend {
    pub diagnostics: Vec<Box<dyn Diagnostic>>,
}

impl Frontend for NvlspFrontend {
    fn report(&mut self, diagnostic: impl Diagnostic + 'static) -> Result<(), fir::StopToken> {
        self.diagnostics
            .push(Box::new(diagnostic) as Box<dyn Diagnostic>);
        Ok(())
    }
}

struct Sources<'a> {
    unchanged_documents: Vec<&'a TextDocument>,
    changed_documents: Vec<&'a mut TextDocument>,
}

impl<'a> fir::Sources<&'a str> for Sources<'a> {
    fn project_name(&self) -> &str {
        "idk bro"
    }

    fn iter_sources(&self) -> impl Iterator<Item = (fir::SourceIdx, &'a str)> {
        self.unchanged_documents
            .iter()
            .copied()
            .chain(self.changed_documents.iter().map(|t| &**t))
            .enumerate()
            .map(|(i, d)| (fir::SourceIdx(i as u64), d.text.as_ref()))
    }

    fn get_source(&self, idx: fir::SourceIdx) -> Option<&'a str> {
        todo!()
    }
}

pub struct CompilerState {
    resources: Resources<DynamicComponent>,
}

impl CompilerState {
    pub fn new(tcx: impl TargetContext<Component = DynamicComponent>) -> anyhow::Result<Self> {
        let mut resources = Resources::default();
        let idx = resources.next_component_idx();
        resources.install_component(tcx.build_component(idx)?);
        Ok(Self { resources })
    }
}

pub struct Compiler<'a> {
    state: &'a mut CompilerState,
    sources: Sources<'a>,
    instance: Geckscript,
    frontend: NvlspFrontend,
}

fn seek<'a>(c: &mut tree_sitter::TreeCursor<'_>, d: usize, src: &'a str) {
    fn change_kind(node: tree_sitter::Node<'_>) -> Option<Cow<'_, str>> {
        node.kind()
            .split_once("_")
            .map(|(_, b)| match b {
                "unit" | "text" => None,
                _ if b.bytes().nth(0).is_some_and(|c| matches!(c, b'A'..=b'Z')) => {
                    Some(Cow::Owned(format!("_::{b}")))
                }
                _ => Some(Cow::Borrowed(b)),
            })
            .unwrap_or_else(|| match node.kind() {
                "Whitespace" => None,
                rest => Some(Cow::Borrowed(rest)),
            })
    }
    loop {
        'me: {
            let me = c.node();
            match change_kind(me) {
                Some(kind) => eprint!(
                    "\n{} ({field}{kind}{src}",
                    " ".repeat(d),
                    field = c
                        .field_name()
                        .map(|f| format!("{f}: "))
                        .unwrap_or(String::new()),
                    src = src
                        .get(me.start_byte()..me.end_byte())
                        .map(|s| format!(" is {s:?}"))
                        .unwrap_or(String::new()),
                ),
                None => {
                    if me.has_error() {
                        eprint!(
                            "\n{} ({field}{kind}{src}",
                            " ".repeat(d),
                            kind = me.kind(),
                            field = c
                                .field_name()
                                .map(|f| format!("{f}: "))
                                .unwrap_or(String::new()),
                            src = src
                                .get(me.start_byte()..me.end_byte())
                                .map(|s| format!(" is {s:?}"))
                                .unwrap_or(String::new()),
                        )
                    } else {
                        break 'me;
                    }
                }
            }

            if c.goto_first_child() {
                seek(c, d + 1, src)
            }

            eprint!(")");
        }

        if !c.goto_next_sibling() {
            c.goto_parent();
            break;
        }
    }
}

impl<'a> Compiler<'a> {
    pub fn new(state: &'a mut CompilerState, sources: Sources<'a>) -> Self {
        Compiler {
            state,
            sources,
            instance: Default::default(),
            frontend: NvlspFrontend::default(),
        }
    }

    pub fn preprocess(&mut self) {
        let project = fir::Project::new(&self.sources, &mut self.frontend);
        self.instance.make_component_for();
    }

    pub fn compile(
        &self,
        old_syntax_tree: Option<&tree_sitter::Tree>,
        source: &str,
    ) -> (
        tree_sitter::Tree,
        Vec<Box<dyn Diagnostic>>,
        Option<(fir::Script, HashMap<InternalVariableIdx, VariableInfo>)>,
    ) {
        let mut frontend = NvlspFrontend {
            diagnostics: Vec::new(),
        };

        let (syntax_tree, ast, parse_errors) = fir_geckscript::parse::<
            fir_geckscript::Script,
            fir_geckscript::Script,
        >(old_syntax_tree, source);

        if syntax_tree.root_node().has_error() {
            let mut c = syntax_tree.walk();
            seek(&mut c, 0, source);
        }

        // ignore `StopToken`s since our frontend never produces them.
        let _ = frontend.report_all(parse_errors);

        let script_result = self.state.with_lower_resources(|r| {
            fir_geckscript::lower::lower_script(r, ast, &mut frontend).ok()
        });
        (
            syntax_tree,
            frontend.diagnostics,
            script_result.map(|(script, vars)| (script, vars.collect())),
        )
    }

    pub fn analyze(
        &mut self,
        script: &fir::Script,
        internal_variables: &HashMap<fir::InternalVariableIdx, fir::VariableInfo>,
        mut report: impl FnMut(Box<dyn Diagnostic>),
    ) -> SpanMap<Symbol> {
        let mut cker = Cker {
            script,
            resources: &self.state.resources,
            internal_variables,
            report: &mut report,
            current_body: None,
            symbols: Default::default(),
        };
        cker.ck_script();
        cker.symbols
    }
}

#[derive(Debug, miette::Diagnostic, thiserror::Error)]
pub enum CkDiagnostic {
    #[error("this expression has no side effects")]
    NoSideEffects(#[label("this operation's return value is never observed")] Span),
    #[error("unreachable code found")]
    DeadCode(#[label("this code cannot be reached")] Span),
}

impl Diagnostic for CkDiagnostic {
    fn kind(&self) -> DiagnosticKind {
        match self {
            CkDiagnostic::NoSideEffects(_) | CkDiagnostic::DeadCode(_) => {
                DiagnosticKind::Warning(Warning::Superfluous)
            }
        }
    }
}

pub(crate) struct Cker<'a, F> {
    pub script: &'a fir::Script,
    pub current_body: Option<&'a fir::FunctionBody>,
    pub resources: &'a Resources<DynamicComponent>,
    pub internal_variables: &'a HashMap<InternalVariableIdx, VariableInfo>,
    pub symbols: SpanMap<Symbol>,
    pub report: F,
}

impl<'a, F: FnMut(Box<dyn Diagnostic>)> Cker<'a, F> {
    fn report(&mut self, d: impl Diagnostic + 'static) {
        (self.report)(Box::new(d) as Box<dyn Diagnostic>)
    }

    pub fn report_typeck_failures(&mut self, failures: Vec<typeck::TypeckFailure>) {
        for failure in failures {
            self.report(failure.into_diagnostic(&self.resources));
        }
    }

    pub fn ck_expression(&mut self, expr: Spanned<&Expression>) -> TypeckResult {
        let span = expr.to_span();
        match expr.into_inner() {
            Expression::Operation(op) => {
                if let &Operator::Function { idx: Resolved(idx) } = op.operator.inner() {
                    self.symbols.insert(
                        op.operator.to_span(),
                        Symbol {
                            kind: SymbolKind::Function(idx),
                        },
                    );
                }
            }
            &Expression::FormRef(form_idx) => {
                self.symbols.insert(
                    span,
                    Symbol {
                        kind: SymbolKind::Form(form_idx),
                    },
                );
            }
            &Expression::GetVariable(Resolved(var_idx)) => {
                eprintln!("get of {var_idx:?}");
                self.symbols.insert(
                    span,
                    Symbol {
                        kind: SymbolKind::Variable(var_idx),
                    },
                );
            }
            _ => (),
        }
        TypeckEngine::new()
            .expression_typeck(&mut *self, expr, |this, nested| this.ck_expression(nested))
    }

    pub fn ck_expression_is(
        &mut self,
        expr: Spanned<&Expression>,
        expected: Tried<Ty>,
        _expected_span: impl ToSpan,
    ) {
        let TypeckResult {
            failures,
            return_ty,
        } = self.ck_expression(expr);
        self.report_typeck_failures(failures);
        match (return_ty, expected) {
            (Resolved(found), Resolved(expected_ty)) => match expected_ty.relationship_to(found) {
                fir::TyRelationship::Identical | fir::TyRelationship::Generalisation => (),
                fir::TyRelationship::Distinct | fir::TyRelationship::Specialisation => {
                    self.report(typeck::TypeckFailureDiagnostic::WrongTy {
                        found: expr.to_span().of(self.resources.print_ty(found)),
                        expected: self.resources.print_ty_hint(TyHint::Exactly(expected_ty)),
                    })
                }
            },
            _ => (),
        }
    }

    pub fn span_without_side_effects(&mut self, expr: Spanned<&Expression>) -> Option<Span> {
        match expr.into_inner() {
            Expression::Convert { conversion, value } => {
                if conversion.provenance.into_inner() == ConversionReason::Explicit {
                    self.span_without_side_effects(value.as_ref().as_ref())
                } else {
                    None
                }
            }
            Expression::Operation(operation) => {
                if matches!(operation.operator.inner(), Operator::Function { .. }) {
                    None
                } else {
                    let (has_side_effects, no_effects_span) = operation
                        .operands
                        .iter()
                        .filter_map(|arg| self.span_without_side_effects(arg.as_ref()))
                        .enumerate()
                        .reduce(|(_, a), (i, b)| (i, a.expand_to_include(b)))
                        .map_or_else(
                            || (!operation.operands.is_empty(), operation.operator.to_span()),
                            |(i, span)| {
                                (
                                    i < operation.operands.len(),
                                    operation.operator.to_span().expand_to_include(span),
                                )
                            },
                        );
                    if has_side_effects {
                        self.report(CkDiagnostic::NoSideEffects(no_effects_span));
                        None
                    } else {
                        Some(no_effects_span)
                    }
                }
            }
            _ => Some(expr.to_span()),
        }
    }

    pub fn ck_statement(&mut self, stmt: &Statement, span: Span) {
        match stmt {
            Statement::Express(expr) => {
                if let Some(span) = self.span_without_side_effects(span.of(expr)) {
                    self.report(CkDiagnostic::NoSideEffects(span));
                }

                self.ck_expression(span.of(expr));
            }
            Statement::SetVariable { variable, value } => {
                if let &Resolved(var_idx) = variable.inner() {
                    self.ck_expression_is(
                        value.as_ref(),
                        self.get_variable_ty(var_idx),
                        variable.to_span(),
                    );
                }
            }
            Statement::Branch {
                kind: BranchKind::IfTrue(expr),
                ..
            } => {
                self.ck_expression_is(expr.as_ref(), Tried::Resolved(Ty::Bool), span);
            }
            Statement::Branch {
                kind: BranchKind::Unconditional,
                ..
            } => (),
            Statement::Block(block) => self.ck_block(block),
        }
    }

    pub fn ck_script(&mut self) {
        for event_impl in &self.script.event_impls {
            let EventImpl {
                event,
                arguments,
                body,
            } = event_impl.inner();
            match event.inner() {
                &Resolved(event_idx) => {
                    if let Some(event_info) = self.resources.get_event(event_idx) {
                        let iter = typeck::param_arg_match(
                            event_info
                                .parameters
                                .iter()
                                .map(|param| (param.optional, param.ty)),
                            arguments.iter().map(|arg| {
                                let TypeckResult {
                                    return_ty,
                                    failures,
                                } = self.ck_expression(arg.as_ref());
                                self.report_typeck_failures(failures);
                                arg.to_span().of(return_ty)
                            }),
                            event.to_span(),
                        );
                        self.report_typeck_failures(iter);
                    }
                }
                Unresolvable => (),
            }
            self.ck_body(body);
            // match event_impl.inner() {
            //     Item::EventImpl(EventImpl {
            //         body,
            //         arguments,
            //         event,
            //     }) => {
            //         match event.inner() {
            //             &Resolved(event_idx) => {
            //                 if let Some(event_info) = self.resources.get_event(event_idx) {
            //                     let iter = typeck::param_arg_match(
            //                         event_info
            //                             .parameters
            //                             .iter()
            //                             .map(|param| (param.optional, param.ty)),
            //                         arguments.iter().map(|arg| {
            //                             let TypeckResult {
            //                                 return_ty,
            //                                 failures,
            //                             } = self.ck_expression(arg.as_ref());
            //                             self.report_typeck_failures(failures);
            //                             arg.to_span().of(return_ty)
            //                         }),
            //                         event.to_span(),
            //                     );
            //                     self.report_typeck_failures(iter);
            //                 }
            //             }
            //             Unresolvable => (),
            //         }
            //         self.ck_body(body)
            //     }
            //     Item::Function(UserFunctionDefinition { body, .. }) => {
            //         self.ck_body(body);
            //     }
            // }
        }
    }

    pub fn ck_block(&mut self, block: &'a Block) {
        for stmt in &block.statements {
            self.ck_statement(stmt.inner(), stmt.to_span());
        }
    }

    pub fn ck_body(&mut self, body: &'a FunctionBody) {
        self.current_body = Some(body);
        self.ck_block(body.entrypoint.inner());
        // let mut visited_blocks = HashSet::new();

        // for (idx, block) in &body{
        //     if !visited_blocks.contains(idx) {
        //         self.report(CkDiagnostic::DeadCode(
        //             block.statements.iter().map(|s| s.to_span()).sum(),
        //         ));
        //     }

        //     for stmt in &block.statements {
        //         self.ck_statement(stmt.inner(), stmt.to_span())
        //     }
        // }
    }
}

impl<'a, F> typeck::LocalResources for Cker<'a, F> {
    fn get_form(&self, idx: fir::FormIdx) -> Option<&fir::FormInfo> {
        self.resources.get_form(idx)
    }

    fn get_external_variable(&self, idx: fir::ExternalVariableIdx) -> Option<&VariableInfo> {
        self.resources.get_external_variable(idx)
    }

    fn get_internal_variable(&self, idx: InternalVariableIdx) -> Option<&VariableInfo> {
        self.internal_variables.get(&idx)
    }

    fn get_body_variable(&self, idx: fir::BodyVariableIdx) -> Option<&fir::BodyVariableInfo> {
        self.current_body.and_then(|body| body.variables.get(&idx))
    }

    fn get_function_def(&self, idx: fir::FunctionIdx) -> Option<&fir::FunctionDefinition> {
        self.resources.get_function_definition(idx)
    }
}
