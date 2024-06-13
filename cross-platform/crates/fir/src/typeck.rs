use std::{iter, marker::PhantomData};

use arrayvec::ArrayVec;

use crate::{
    utils::{ResourcesExt, TyHintPrinter, TyPrinter},
    *,
};

#[derive(Debug, Clone, Copy)]
pub enum TyHint {
    Never,
    Exactly(Ty),
    Union(&'static [TyHint]),
}
impl TyHint {
    pub fn relationship_to_ty(self, ty: Ty) -> TyRelationship {
        match self {
            Self::Exactly(hint_ty) => hint_ty.relationship_to(ty),
            Self::Union(hints) => {
                let mut relationship = TyRelationship::Distinct;
                for hint in hints {
                    relationship = match (hint.relationship_to_ty(ty), relationship) {
                        (b @ TyRelationship::Identical, _) => b,
                        (
                            b @ TyRelationship::Generalisation,
                            TyRelationship::Distinct | TyRelationship::Specialisation,
                        ) => b,
                        (b @ TyRelationship::Specialisation, TyRelationship::Distinct) => b,
                        (_, existing) => existing,
                    };
                }
                relationship
            }
            _ => TyRelationship::Distinct,
        }
    }
}

#[derive(Debug)]
pub enum TypeckFailure {
    TooFewArgs(Spanned<Vec<TyHint>>),
    UncheckedConversion {
        generic: Spanned<Ty>,
        specific: Ty,
        provenance: Spanned<ConversionReason>,
    },
    TooManyArgs(Spanned<usize>),
    WrongTy {
        found: Spanned<Ty>,
        expected: TyHint,
    },
}

impl TypeckFailure {
    pub fn collect_overflowing_args(
        args: impl Iterator<Item = Spanned<Tried<Ty>>>,
    ) -> Option<TypeckFailure> {
        let mut total_span = Span::NOWHERE;
        let extra_arg_count = args
            .inspect(|arg| {
                let span = arg.to_span();
                if total_span == Span::NOWHERE {
                    total_span = span;
                } else {
                    total_span = total_span.expand_to_include(span);
                }
            })
            .count();
        (extra_arg_count > 0).then(|| TypeckFailure::TooManyArgs(total_span.of(extra_arg_count)))
    }

    pub fn maybe_unchecked_conversion(
        operand_span: impl ToSpan,
        conversion: &Conversion,
    ) -> Option<TypeckFailure> {
        if conversion.method != ConversionMethod::NarrowingTransmute {
            // conversion cannot fail, no error to report.
            return None;
        }
        Some(Self::UncheckedConversion {
            generic: operand_span.to_span().of(conversion.from),
            specific: conversion.to,
            provenance: conversion.provenance,
        })
    }

    pub fn into_diagnostic<R: Resources>(self, res: &R) -> TypeckFailureDiagnostic {
        match self {
            TypeckFailure::TooFewArgs(hints) => {
                TypeckFailureDiagnostic::TooFewArgs(hints.span_map(|h| h.len()))
            }
            TypeckFailure::UncheckedConversion {
                generic,
                specific,
                provenance,
            } => TypeckFailureDiagnostic::UncheckedConversion {
                generic: generic.span_map(|ty| res.print_ty(ty)),
                specific: res.print_ty(specific),
                provenance,
            },
            TypeckFailure::TooManyArgs(n) => TypeckFailureDiagnostic::TooManyArgs(n),
            TypeckFailure::WrongTy { found, expected } => TypeckFailureDiagnostic::WrongTy {
                found: found.span_map(|ty| res.print_ty(ty)),
                expected: res.print_ty_hint(expected),
            },
        }
    }
}

#[derive(Debug, miette::Diagnostic, thiserror::Error)]
pub enum TypeckFailureDiagnostic {
    #[error("too few args")]
    TooFewArgs(#[label("expected {0} more arguments")] Spanned<usize>),
    #[error("unchecked transmute")]
    UncheckedConversion {
        #[label("this is type {generic}, converted to type {specific}")]
        generic: Spanned<TyPrinter>,
        specific: TyPrinter,
        #[label("due to {p}", p = Self::provenance_to_str(.provenance.into_inner()))]
        provenance: Spanned<ConversionReason>,
    },
    #[error("too many args")]
    TooManyArgs(#[label("found {0} more args than expected")] Spanned<usize>),
    #[error("type mismatch")]
    WrongTy {
        #[label("this is type {found} which is not applicable to {expected}")]
        found: Spanned<TyPrinter>,
        expected: TyHintPrinter,
    },
}

impl TypeckFailureDiagnostic {
    fn provenance_to_str(p: ConversionReason) -> &'static str {
        match p {
            ConversionReason::VariableSet => "this variable being set",
            ConversionReason::OperatorParam => "the types expected by this operation",
            ConversionReason::FuncParam => "the types of this function's parameters",
            ConversionReason::SelfParam => "the type of the self parameter on this function",
            ConversionReason::Explicit => "this transmute",
            ConversionReason::Other => "the usage here",
        }
    }
}

impl Diagnostic for TypeckFailureDiagnostic {
    fn kind(&self) -> DiagnosticKind {
        match self {
            TypeckFailureDiagnostic::UncheckedConversion { .. } => {
                DiagnosticKind::Warning(Warning::BadPractice)
            }
            TypeckFailureDiagnostic::WrongTy { .. }
            | TypeckFailureDiagnostic::TooFewArgs { .. }
            | TypeckFailureDiagnostic::TooManyArgs { .. } => DiagnosticKind::CompileFail,
        }
    }
}

pub struct TypeckResult {
    pub return_ty: Tried<Ty>,
    pub failures: Vec<TypeckFailure>,
}

impl TypeckResult {
    pub const UNRESOLVED: Self = Self {
        return_ty: Unresolvable,
        failures: Vec::new(),
    };

    pub const fn new(ty: Tried<Ty>) -> Self {
        Self {
            return_ty: ty,
            failures: Vec::new(),
        }
    }

    pub const fn new_resolved(ty: Ty) -> Self {
        Self::new(Resolved(ty))
    }

    pub fn with_failures(mut self, failures: impl IntoIterator<Item = TypeckFailure>) -> Self {
        self.failures.extend(failures);
        self
    }
}

pub struct TypeckEngine<C>(PhantomData<fn() -> C>);

pub trait LocalResources {
    fn get_form(&self, idx: FormIdx) -> Option<&FormInfo>;
    fn get_external_variable(&self, idx: ExternalVariableIdx) -> Option<&VariableInfo>;
    fn get_internal_variable(&self, idx: InternalVariableIdx) -> Option<&VariableInfo>;
    fn get_body_variable(&self, idx: BodyVariableIdx) -> Option<&BodyVariableInfo>;
    fn get_function_def(&self, idx: FunctionIdx) -> Option<&FunctionDefinition>;

    fn get_variable_ty(&self, var_idx: impl Into<Tried<VariableIdx>>) -> Tried<Ty> {
        let Resolved(var_idx) = var_idx.into() else {
            return Unresolvable;
        };
        let opt = match var_idx {
            VariableIdx::BodyLocal(idx) => {
                self.get_body_variable(idx).map(|var| var.ty.into_inner())
            }
            VariableIdx::FormInternal(idx) => self.get_internal_variable(idx).map(|var| var.ty),
            VariableIdx::FormExternal(idx) => self.get_external_variable(idx).map(|var| var.ty),
        };
        opt.map_or(Unresolvable, Resolved)
    }
}

impl<C: LocalResources> TypeckEngine<C> {
    pub fn new() -> Self {
        TypeckEngine(PhantomData)
    }
}

impl<C: LocalResources> TypeckEngine<C> {
    pub fn type_of(&self, cx: &C, expression: &Expression) -> Tried<Ty> {
        let ty = match expression {
            Expression::PrimitiveValue(PrimitiveValue::NullReference) => Ty::Ref(RefTy::Unknown),
            Expression::PrimitiveValue(PrimitiveValue::Unit) => Ty::Unit,
            Expression::PrimitiveValue(PrimitiveValue::Bool(_)) => Ty::Bool,
            Expression::PrimitiveValue(PrimitiveValue::Int(_)) => Ty::Integer,
            Expression::PrimitiveValue(PrimitiveValue::String(_)) => Ty::String,
            Expression::PrimitiveValue(PrimitiveValue::Float(_)) => Ty::Float,
            Expression::Convert { conversion, .. } => conversion.to,
            &Expression::FormRef(form_idx) => {
                // todo: maybe consider "shape" since we know this is a form ref.
                match cx.get_form(form_idx) {
                    Some(form) => Ty::Ref(RefTy::Form(FormTy::Single(form.kind))),
                    None => return Unresolvable,
                }
            }
            &Expression::GetVariable(Tried::Resolved(VariableIdx::BodyLocal(var_idx))) => {
                match cx.get_body_variable(var_idx) {
                    Some(var) => var.ty.into_inner(),
                    None => return Unresolvable,
                }
            }
            &Expression::GetVariable(Tried::Resolved(VariableIdx::FormExternal(var_idx))) => {
                match cx.get_external_variable(var_idx) {
                    Some(var) => var.ty,
                    None => return Unresolvable,
                }
            }
            &Expression::GetVariable(Tried::Resolved(VariableIdx::FormInternal(var_idx))) => {
                match cx.get_internal_variable(var_idx) {
                    Some(var) => var.ty,
                    None => return Unresolvable,
                }
            }
            &Expression::GetVariable(Tried::Unresolvable) => {
                return Unresolvable;
            }
            Expression::Operation(op) => {
                return self.operator_return_ty(cx, op.operator.inner());
            }
        };
        Resolved(ty)
    }

    pub fn operator_return_ty(&self, cx: &C, operator: &Operator) -> Tried<Ty> {
        match operator {
            Operator::Core(op) => {
                let ty = match *op {
                    CoreOperator::Greater(_)
                    | CoreOperator::Lesser(_)
                    | CoreOperator::GreaterOrEqual(_)
                    | CoreOperator::LesserOrEqual(_)
                    | CoreOperator::Or
                    | CoreOperator::And
                    | CoreOperator::Eq(_)
                    | CoreOperator::Neq(_) => Ty::Bool,
                    CoreOperator::BitOr => Ty::Integer,
                    CoreOperator::BitAnd => Ty::Integer,
                    CoreOperator::BitXor => Ty::Integer,
                    CoreOperator::LeftShift => Ty::Integer,
                    CoreOperator::RightShift => Ty::Integer,
                    CoreOperator::Add(num_ty)
                    | CoreOperator::Sub(num_ty)
                    | CoreOperator::Mul(num_ty)
                    | CoreOperator::Div(num_ty)
                    | CoreOperator::Rem(num_ty)
                    | CoreOperator::Neg(num_ty) => num_ty.into(),
                    CoreOperator::Not => Ty::Bool,
                    CoreOperator::BitNot => Ty::Integer,
                    CoreOperator::Pow(PowKind::IntegerBase) => Ty::Integer,
                    CoreOperator::Pow(PowKind::FloatBase { .. }) => Ty::Float,
                };
                Resolved(ty)
            }
            &Operator::Function { idx: Unresolvable } => Unresolvable,
            &Operator::Function {
                idx: Resolved(func_idx),
            } => match cx.get_function_def(func_idx) {
                Some(func) => Resolved(func.return_ty),
                None => Unresolvable,
            },
            Operator::TypeError => Unresolvable,
        }
    }

    pub fn expression_typeck(
        &self,
        cx: &mut C,
        expression: Spanned<&Expression>,
        mut map_expr: impl FnMut(&mut C, Spanned<&Expression>) -> TypeckResult,
    ) -> TypeckResult {
        match expression.into_inner() {
            Expression::PrimitiveValue(_) | Expression::FormRef(_) | Expression::GetVariable(_) => {
                TypeckResult::new(self.type_of(cx, expression.inner()))
            }
            Expression::Convert { value, conversion } => {
                let TypeckResult {
                    return_ty: inner_ty,
                    mut failures,
                } = map_expr(cx, value.as_ref().as_ref());

                match value.inner() {
                    Expression::PrimitiveValue(PrimitiveValue::NullReference) => (),
                    _ => failures.extend(TypeckFailure::maybe_unchecked_conversion(
                        value.as_ref().as_ref(),
                        conversion,
                    )),
                }

                if let Resolved(ty) = inner_ty {
                    match ty.relationship_to(conversion.from) {
                        TyRelationship::Identical | TyRelationship::Specialisation => (),
                        TyRelationship::Distinct | TyRelationship::Generalisation => {
                            failures.push(TypeckFailure::WrongTy {
                                found: value.to_span().of(ty),
                                expected: TyHint::Exactly(conversion.from),
                            });
                        }
                    }
                }

                TypeckResult {
                    return_ty: Resolved(conversion.to),
                    failures,
                }
            }
            Expression::Operation(op) => {
                let mut result = TypeckResult::new(self.type_of(cx, expression.inner()));
                let mut operands = op.operands.iter();
                self.operation_typeck(cx, op.operator.as_ref(), |cx| {
                    operands.next().map(|expr| {
                        let TypeckResult {
                            return_ty,
                            failures,
                        } = map_expr(cx, expr.as_ref());
                        result.failures.extend(failures);
                        expr.to_span().of(return_ty)
                    })
                });
                result
            }
        }
    }

    pub fn operation_typeck(
        &self,
        cx: &mut C,
        operator: Spanned<&Operator>,
        mut next_arg: impl FnMut(&mut C) -> Option<Spanned<Tried<Ty>>>,
    ) -> TypeckResult {
        match operator.into_inner() {
            Operator::Core(op) => {
                macro_rules! avec {
                    ($($tt:tt)*) => {
                        [$($tt)*].into_iter().collect()
                    };
                }
                let param_map: ArrayVec<Ty, 2> = match *op {
                    CoreOperator::Or => avec![Ty::Bool; 2],
                    CoreOperator::And => avec![Ty::Bool; 2],
                    CoreOperator::Eq(prim) => avec![prim.into(); 2],
                    CoreOperator::Neq(prim) => avec![prim.into(); 2],
                    CoreOperator::Greater(num_ty)
                    | CoreOperator::Lesser(num_ty)
                    | CoreOperator::GreaterOrEqual(num_ty)
                    | CoreOperator::LesserOrEqual(num_ty) => avec![num_ty.into(); 2],
                    CoreOperator::BitOr
                    | CoreOperator::BitAnd
                    | CoreOperator::BitXor
                    | CoreOperator::LeftShift
                    | CoreOperator::RightShift => avec![Ty::Integer; 2],
                    CoreOperator::Add(num_ty)
                    | CoreOperator::Sub(num_ty)
                    | CoreOperator::Mul(num_ty)
                    | CoreOperator::Div(num_ty)
                    | CoreOperator::Rem(num_ty) => avec![num_ty.into(); 2],
                    CoreOperator::Neg(num_ty) => avec![num_ty.into()],
                    CoreOperator::Not => avec![Ty::Bool],
                    CoreOperator::BitNot => avec![Ty::Integer],
                    CoreOperator::Pow(PowKind::IntegerBase) => avec![Ty::Integer; 2],
                    CoreOperator::Pow(PowKind::FloatBase { exponent: num_ty }) => {
                        avec![Ty::Float, num_ty.into()]
                    }
                };

                let mut failures = Vec::new();
                for i in 0.. {
                    match (next_arg(&mut *cx), param_map.get(i)) {
                        (Some(pragmatic), Some(&param)) => match pragmatic.into_inner() {
                            Resolved(arg_ty) => match param.relationship_to(arg_ty) {
                                TyRelationship::Identical | TyRelationship::Generalisation => (),
                                TyRelationship::Distinct | TyRelationship::Specialisation => {
                                    failures.push(TypeckFailure::WrongTy {
                                        found: pragmatic.to_span().of(arg_ty),
                                        expected: TyHint::Exactly(param),
                                    });
                                }
                            },
                            Unresolvable => (),
                        },
                        (None, Some(_)) => {
                            let tys = param_map
                                .iter()
                                .copied()
                                .map(|param| TyHint::Exactly(param))
                                .collect();
                            failures.push(TypeckFailure::TooFewArgs(operator.to_span().of(tys)));
                            break;
                        }
                        (_, None) => {
                            if let Some(too_many) =
                                TypeckFailure::collect_overflowing_args(core::iter::from_fn(|| {
                                    next_arg(&mut *cx)
                                }))
                            {
                                failures.push(too_many);
                            }
                            break;
                        }
                    }
                }

                TypeckResult::new(self.operator_return_ty(cx, operator.inner()))
                    .with_failures(failures)
            }
            // fixme: find somewhere to move this check to.

            // Operator::Convert(conversion) => {
            //     let hint = match conversion.to {
            //         Ty::Bool => TyHint::Exactly(Ty::Bool),
            //         Ty::Unit => TyHint::Exactly(Ty::Unit),
            //         Ty::String => TyHint::Exactly(Ty::String),
            //         Ty::Integer => TyHint::Union(&[
            //             TyHint::Exactly(Ty::Integer),
            //             TyHint::Exactly(Ty::Bool),
            //             TyHint::Exactly(Ty::Float),
            //             TyHint::Exactly(Ty::String),
            //             TyHint::Exactly(Ty::Ref(RefTy::Unknown)),
            //         ]),
            //         Ty::Float => {
            //             TyHint::Union(&[TyHint::Exactly(Ty::Float), TyHint::Exactly(Ty::Integer)])
            //         }
            //         Ty::Ref(_) => TyHint::Union(&[
            //             TyHint::Exactly(Ty::Ref(RefTy::Unknown)),
            //             TyHint::Exactly(Ty::Integer),
            //         ]),
            //         Ty::Adt(_) => TyHint::Never,
            //     };

            //     let Some(arg) = next_arg(&mut *cx) else {
            //         return TypeckResult {
            //             return_ty: Resolved(conversion.to),
            //             failures: vec![TypeckFailure::TooFewArgs(
            //                 operator.to_span().of(vec![hint]),
            //             )],
            //         };
            //     };

            //     if let Some(too_many) =
            //         TypeckFailure::collect_overflowing_args(core::iter::from_fn(|| {
            //             next_arg(&mut *cx)
            //         }))
            //     {
            //         return TypeckResult {
            //             return_ty: Resolved(conversion.to),
            //             failures: vec![too_many],
            //         };
            //     }

            //     let danger =
            //         TypeckFailure::unchecked_conversion(&arg, conversion, operator.to_span());
            //     hint.typeck(arg, || Resolved(conversion.to))
            //         .with_failures(danger)
            // }
            &Operator::Function {
                idx: Resolved(func_idx),
            } => {
                let (params, return_ty) = {
                    let Some(func) = cx.get_function_def(func_idx) else {
                        // fixme: technically a correctness error.
                        return TypeckResult::UNRESOLVED;
                    };
                    (
                        func.self_param
                            .as_ref()
                            .map(|&ty| (false, ty))
                            .into_iter()
                            .chain(func.params.iter().map(|param| (param.optional, param.ty)))
                            .collect::<Vec<_>>(),
                        func.return_ty,
                    )
                };

                TypeckResult::new_resolved(return_ty).with_failures(param_arg_match(
                    params,
                    iter::from_fn(|| next_arg(&mut *cx)),
                    operator.to_span(),
                ))
            }
            Operator::Function { idx: Unresolvable } | Operator::TypeError => {
                iter::from_fn(|| next_arg(&mut *cx)).for_each(drop);
                TypeckResult::UNRESOLVED
            }
        }
    }
}

pub fn param_arg_match(
    params: impl IntoIterator<Item = (bool, Ty)>,
    args: impl IntoIterator<Item = Spanned<Tried<Ty>>>,
    no_arg_span: Span,
) -> Vec<TypeckFailure> {
    let mut args = args.into_iter();
    let mut params = params.into_iter();
    let mut failures = Vec::new();
    let mut last_arg_span = no_arg_span;
    loop {
        let arg = args.next();
        match (arg, params.next()) {
            (Some(arg_ty_spanned), Some((_, param_ty))) => {
                last_arg_span = arg_ty_spanned.to_span();
                match arg_ty_spanned.into_inner() {
                    Resolved(arg_ty) => match param_ty.relationship_to(arg_ty) {
                        TyRelationship::Identical | TyRelationship::Generalisation => (),
                        TyRelationship::Distinct | TyRelationship::Specialisation => {
                            failures.push(TypeckFailure::WrongTy {
                                found: arg_ty_spanned.to_span().of(arg_ty),
                                expected: TyHint::Exactly(param_ty),
                            });
                        }
                    },
                    Unresolvable => (),
                }
            }
            (None, None) => break,
            (None, Some((optional, param_ty))) => {
                if !optional {
                    let tys = iter::once(TyHint::Exactly(param_ty))
                        .chain(
                            params
                                .take_while(|(optional, _)| !optional)
                                .map(|(_, param_ty)| TyHint::Exactly(param_ty)),
                        )
                        .collect();
                    failures.push(TypeckFailure::TooFewArgs(
                        last_arg_span.end_column().of(tys),
                    ));
                }
                break;
            }
            (Some(_), None) => {
                if let Some(too_many) = TypeckFailure::collect_overflowing_args(args) {
                    failures.push(too_many);
                }
                break;
            }
        }
    }

    failures
}
