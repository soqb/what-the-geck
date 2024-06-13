use fir::typeck::LocalResources;
use fir::Tried::{self, Resolved, Unresolvable};
use fir::{typeck, utils::ResourcesExt as _, ConversionReason, Frontend, Resources, Ty};
use fir::{Span, Spanned, ToSpan};

use rust_sitter::Spanned as AstSpanned;

use crate::diagnostics::LowerDiagnostic;
use crate::*;

pub use context::{LowerContext, LowerResources};

mod ascii;
mod context;
mod gather_metadata;

pub use gather_metadata::Geckscript;

pub type Result<T, E = fir::StopToken> = core::result::Result<T, E>;

enum Coercibility {
    ViaConversion {
        from: Ty,
        to: Ty,
        method: fir::ConversionMethod,
    },
    NotNecessary,
    NotPossible,
}

impl Coercibility {
    pub fn between(from: Tried<Ty>, to: Tried<Ty>) -> Coercibility {
        let (Resolved(from), Resolved(to)) = (from, to) else {
            return Coercibility::NotPossible;
        };

        match (from, to) {
            (Ty::Float, Ty::Integer) => Coercibility::ViaConversion {
                from,
                to,
                method: fir::ConversionMethod::Translate,
            },
            (Ty::Integer, Ty::Float) => Coercibility::ViaConversion {
                from,
                to,
                method: fir::ConversionMethod::Translate,
            },
            // fixme: this works, but should probably be explicit.
            _ => match from.relationship_to(to) {
                fir::TyRelationship::Specialisation | fir::TyRelationship::Identical => {
                    Coercibility::NotNecessary
                }
                _ => match from.morphic_relationship_to(to) {
                    fir::TyRelationship::Identical | fir::TyRelationship::Specialisation => {
                        Coercibility::ViaConversion {
                            from,
                            to,
                            method: fir::ConversionMethod::WideningTransmute,
                        }
                    }
                    fir::TyRelationship::Generalisation => Coercibility::ViaConversion {
                        from,
                        to,
                        method: fir::ConversionMethod::NarrowingTransmute,
                    },
                    fir::TyRelationship::Distinct => Coercibility::NotPossible,
                },
            },
        }
    }
}

impl<'a, F: Frontend + 'a, R: Resources> LowerContext<'a, F, R> {
    fn lower_literal(&mut self, literal: Literal, span: Span) -> fir::Expression {
        let pv = match literal {
            Literal::DecInt(IntRepr::Good(x)) | Literal::HexInt(IntRepr::Good(x)) => {
                fir::PrimitiveValue::Int(x)
            }
            Literal::Float(FloatRepr::Good(f)) => fir::PrimitiveValue::Float(f),
            Literal::String(s) => fir::PrimitiveValue::String(s),
            Literal::DecInt(IntRepr::Bad(_))
            | Literal::HexInt(IntRepr::Bad(_))
            | Literal::Float(FloatRepr::Bad(_)) => {
                return fir::Expression::Operation(fir::Operation {
                    operands: Vec::new(),
                    operator: span.of(fir::Operator::TypeError),
                })
            }
        };
        fir::Expression::PrimitiveValue(pv)
    }

    fn lower_unary_op(
        &mut self,
        op: AstSpanned<UnaryOp>,
        inner: AstSpanned<Expression>,
    ) -> Result<fir::Operation> {
        let expr = self.lower_expression(inner)?;
        let ty = typeck::TypeckEngine::new().type_of(&*self, expr.inner());
        use fir::CoreOperator as Op;
        use fir::Operator::Core;
        let operator = match (ty, op.value) {
            (Unresolvable, _) => fir::Operator::TypeError,
            (Resolved(Ty::Integer), UnaryOp::Neg) => Core(Op::Neg(fir::NumericTy::Integer)),
            (Resolved(Ty::Float), UnaryOp::Neg) => Core(Op::Neg(fir::NumericTy::Float)),
            (Resolved(Ty::Integer), UnaryOp::Not) => Core(Op::BitNot),
            (Resolved(Ty::Bool), UnaryOp::Not) => Core(Op::Not),
            (Resolved(_), UnaryOp::ToString) => todo!("lower UnaryOp::ToString"),
            (Resolved(_), UnaryOp::ToNumber) => todo!("lower UnaryOp::ToNumber"),
            (Resolved(_), UnaryOp::Deref) => todo!("lower UnaryOp::Deref"),
            (Resolved(_), UnaryOp::Box) => todo!("lower UnaryOp::Box"),
            _ => todo!("unary op type mismatch"),
        };
        Ok(fir::Operation {
            operands: vec![expr],
            operator: fir::Spanned::new(operator, op.span),
        })
    }

    fn reference_iter(reference: Reference) -> impl DoubleEndedIterator<Item = Ident> {
        core::iter::once(reference.head)
            .chain(reference.path.into_iter().map(|segment| segment.field))
    }

    fn reference_pair(
        &mut self,
        reference: Reference,
    ) -> Result<(Spanned<String>, Option<Spanned<String>>)> {
        let mut iter = Self::reference_iter(reference);
        let first_name = iter
            .next()
            .unwrap_or_else(|| panic!("reference should have at least 1 element"))
            .respan();
        let field_name = iter.next().map(Respan::respan);

        if let Some(extra_span) = iter
            .map(|ident| ident.span())
            .reduce(|a, b| [a, b].to_span())
        {
            self.report(LowerDiagnostic::TooLongReference(extra_span))?;
        }

        Ok((first_name, field_name))
    }

    fn lower_possible_enum(
        &mut self,
        ident: Spanned<&str>,
        type_idx: fir::TypeIdx,
    ) -> Result<Option<fir::Spanned<fir::Expression>>> {
        let Some((const_, warn)) = self.enum_variant(ident.as_deref(), type_idx) else {
            return Ok(None);
        };
        self.report_all(warn)?;
        Ok(Some(
            ident
                .to_span()
                .of(fir::Expression::PrimitiveValue(const_.into())),
        ))
    }

    fn lower_invoke_arg(
        &mut self,
        arg: AstSpanned<InvokeArg>,
        param_ty: Tried<Ty>,
    ) -> Result<fir::Spanned<fir::Expression>> {
        let possible_enum = match (&arg.value, param_ty) {
            (InvokeArg::Constant(Literal::String(string)), Resolved(Ty::Adt(idx))) => {
                self.lower_possible_enum(arg.span.to_span().of(string), idx)?
            }
            (InvokeArg::Reference(reference), Resolved(Ty::Adt(idx)))
                if reference.path.is_empty() =>
            {
                self.lower_possible_enum(arg.span.to_span().of(&reference.head), idx)?
            }
            _ => None,
        };
        if let Some(expr) = possible_enum {
            return Ok(expr);
        }
        let expression = 'expr: {
            let expr = match arg.value {
                InvokeArg::Constant(lit) => self.lower_literal(lit, arg.span.to_span()),
                InvokeArg::Reference(reference) => {
                    return self.lower_invoke(
                        Invoke {
                            referent: reference,
                            args: Vec::new(),
                        },
                        arg.span,
                    )
                }
                InvokeArg::UnaryOp { operator, inner } => {
                    fir::Expression::Operation(self.lower_unary_op(operator, inner)?)
                }
                InvokeArg::Inline { inner, .. } => break 'expr self.lower_expression(inner)?,
            };
            fir::Spanned::new(expr, arg.span)
        };
        Ok(expression)
    }

    fn coerce_expr(
        &self,
        provenance: ConversionReason,
        operand: Spanned<fir::Expression>,
        param_ty: Tried<Ty>,
        provenance_span: Span,
    ) -> Spanned<fir::Expression> {
        let arg_ty = typeck::TypeckEngine::new().type_of(&*self, operand.inner());
        match Coercibility::between(arg_ty, param_ty) {
            Coercibility::ViaConversion { from, to, method } => {
                operand.to_span().of(fir::Expression::Convert {
                    value: Box::new(operand),
                    conversion: fir::Conversion {
                        from,
                        to,
                        provenance: provenance_span.of(provenance),
                        method,
                    },
                })
            }
            // NB: even though the `NotPossible` case is an error, we don't handle it in this branch.
            Coercibility::NotNecessary | Coercibility::NotPossible => operand,
        }
    }

    fn lower_function_call_chain(
        &mut self,
        chain_index: usize,
        self_expr: Option<fir::Spanned<fir::Expression>>,
        func_name: fir::Spanned<String>,
        mut rest: impl Iterator<Item = Ident>,
        args: Vec<AstSpanned<InvokeArg>>,
        span_start: usize,
    ) -> Result<fir::Spanned<fir::Expression>> {
        let next_ident = rest.next();
        let func = match self.function(func_name.as_deref()) {
            Ok(func) => Some(func),
            Err(_) if chain_index == 0 && (next_ident.is_some() || args.is_empty()) => {
                self.report(LowerDiagnostic::Unknown1stReference {
                    reference: func_name.to_span(),
                })?;
                None
            }
            Err(_) if chain_index == 1 && (next_ident.is_some() || args.is_empty()) => {
                self.report(LowerDiagnostic::Unknown2ndReference {
                    reference: func_name.to_span(),
                })?;
                None
            }
            Err(err) => {
                self.report(err)?;
                None
            }
        };

        let self_expr = match (&func, self_expr) {
            (Some((_, func_def, _)), None) if func_def.self_param.is_some() => {
                let span = Span {
                    start: span_start,
                    end: span_start,
                };
                Some(span.of(fir::Expression::PrimitiveValue(
                    fir::PrimitiveValue::NullReference,
                )))
            }
            (_, expr) => expr,
        };
        let mut operands = self_expr
            .map(|op| {
                self.coerce_expr(
                    ConversionReason::SelfParam,
                    op,
                    func.as_ref()
                        .and_then(|(_, def, _)| def.self_param)
                        .map_or(Unresolvable, Resolved),
                    func_name.to_span(),
                )
            })
            .into_iter()
            .collect();

        let expr_span = (span_start..func_name.to_span().end).to_span();
        match next_ident {
            Some(next_func) => {
                let (idx, case_err) = func
                    .map(|(idx, _, err)| (Resolved(idx), err))
                    .unwrap_or_else(|| (Unresolvable, None));
                let expr = expr_span.of(fir::Expression::Operation(fir::Operation {
                    operands,
                    operator: func_name.to_span().of(fir::Operator::Function { idx }),
                }));
                if let Some(err) = case_err {
                    self.report(err)?;
                }
                self.lower_function_call_chain(
                    chain_index + 1,
                    Some(expr),
                    next_func.respan(),
                    rest,
                    args,
                    span_start,
                )
            }
            None => {
                let func_idx = func.as_ref().map(|&(idx, ..)| idx);
                if let Some((_, func_def, case_err)) = func {
                    let mut params = func_def
                        .params
                        .iter()
                        .map(|param| (param.ty, param.optional))
                        .collect::<Vec<_>>()
                        .into_iter();
                    let mut args = args.into_iter();
                    loop {
                        let op = match (args.next(), params.next()) {
                            (Some(arg), Some((param_ty, _))) => {
                                let op = self.lower_invoke_arg(arg, param_ty.into())?;
                                self.coerce_expr(
                                    ConversionReason::FuncParam,
                                    op,
                                    param_ty.into(),
                                    func_name.to_span(),
                                )
                            }
                            (Some(arg), None) => self.lower_invoke_arg(arg, Unresolvable)?,
                            (None, _) => break,
                        };
                        operands.push(op);
                    }

                    if let Some(err) = case_err {
                        self.report(err)?;
                    }
                } else {
                    operands.extend(
                        args.into_iter()
                            .map(|arg| self.lower_invoke_arg(arg, Unresolvable))
                            .scan(false, |cancel_flag, arg_res| {
                                match (*cancel_flag, arg_res) {
                                    (true, _) => None,
                                    (false, Err(fir::StopToken { .. })) => {
                                        *cancel_flag = true;
                                        None
                                    }
                                    (false, Ok(t)) => Some(t),
                                }
                            }),
                    );
                }
                Ok(expr_span.of(fir::Expression::Operation(fir::Operation {
                    operands,
                    operator: func_name.to_span().of(fir::Operator::Function {
                        idx: func_idx.into(),
                    }),
                })))
            }
        }
    }

    fn lower_invoke(
        &mut self,
        invoke: Invoke,
        span: impl ToSpan + Copy,
    ) -> Result<fir::Spanned<fir::Expression>> {
        let mut idents = Self::reference_iter(invoke.referent);
        let first = idents
            .next()
            .unwrap_or_else(|| panic!("reference should have at least 1 element"))
            .respan();

        let (self_expr, next_ident) = if let Some(local_idx) = self.local_variable(first.inner()) {
            (
                Some(first.to_span().of(fir::Expression::GetVariable(Resolved(
                    fir::VariableIdx::BodyLocal(local_idx),
                )))),
                None,
            )
        } else if let Some((global_idx, warn)) = self.global_variable(first.as_deref()) {
            self.report_all(warn)?;
            (
                Some(
                    first
                        .to_span()
                        .of(fir::Expression::GetVariable(Resolved(global_idx))),
                ),
                None,
            )
        } else if let Ok((form_idx, form_variables, warn)) = self.form(first.as_deref()) {
            let pair = match (idents.next(), invoke.args.is_empty()) {
                (Some(field_name), true) => {
                    let field_name = field_name.respan();
                    match Self::field_of(first.to_span().of(form_variables), field_name.as_deref())
                    {
                        Ok((_, var_idx, warn)) => {
                            self.report_all(warn)?;
                            let span = [first.to_span(), field_name.to_span()].to_span();
                            (
                                Some(span.of(fir::Expression::GetVariable(Resolved(
                                    fir::VariableIdx::FormExternal(var_idx),
                                )))),
                                None,
                            )
                        }
                        Err(_) => (
                            Some(first.to_span().of(fir::Expression::FormRef(form_idx))),
                            Some(field_name),
                        ),
                    }
                }
                (next, _) => (
                    Some(first.to_span().of(fir::Expression::FormRef(form_idx))),
                    next.map(Respan::respan),
                ),
            };
            self.report_all(warn)?;
            pair
        } else {
            (None, Some(first))
        };

        match next_ident.map_or_else(
            || (false, idents.next().map(Respan::respan)),
            |ident| (self_expr.is_none(), Some(ident)),
        ) {
            (is_first, Some(next)) => self.lower_function_call_chain(
                is_first.then_some(0).unwrap_or(1),
                self_expr,
                next,
                idents,
                invoke.args,
                span.to_span().start,
            ),
            (_, None) => Ok(self_expr.unwrap_or_else(|| {
                Span::NOWHERE.of(fir::Expression::Operation(fir::Operation {
                    operands: vec![],
                    operator: Span::NOWHERE.of(fir::Operator::TypeError),
                }))
            })),
        }
    }

    fn lower_binary_operator(
        &mut self,
        op: Spanned<BinaryOp>,
        mut lhs: Spanned<fir::Expression>,
        mut rhs: Spanned<fir::Expression>,
    ) -> Result<fir::Operation> {
        let typeck = typeck::TypeckEngine::new();
        let lhs_ty = typeck.type_of(self, lhs.inner());
        let rhs_ty = typeck.type_of(self, rhs.inner());

        let op_span = op.to_span();

        let (Resolved(lhs_ty), Resolved(rhs_ty)) = (lhs_ty, rhs_ty) else {
            return Ok(fir::Operation {
                operands: vec![lhs, rhs],
                operator: op_span.of(fir::Operator::TypeError),
            });
        };

        fn conv_using(expr: &mut Spanned<fir::Expression>, conversion: fir::Conversion) {
            *expr = expr.to_span().of({
                fir::Expression::Convert {
                    value: Box::new(std::mem::replace(
                        expr,
                        // dummy value
                        Span::NOWHERE.of(fir::Expression::PrimitiveValue(
                            fir::PrimitiveValue::NullReference,
                        )),
                    )),
                    conversion,
                }
            })
        }

        let op = match op.into_inner() {
            BinaryOp::Greater
            | BinaryOp::Lesser
            | BinaryOp::GreaterOrEqual
            | BinaryOp::LesserOrEqual
            | BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Mod => 'operator: {
                let numeric = match (lhs_ty, rhs_ty) {
                    (Ty::Integer, Ty::Integer) => fir::NumericTy::Integer,
                    (Ty::Float, Ty::Float) => fir::NumericTy::Float,
                    (Ty::Integer, Ty::Float) => {
                        conv_using(
                            &mut lhs,
                            fir::Conversion {
                                from: fir::Ty::Integer,
                                to: fir::Ty::Float,
                                provenance: op_span.of(ConversionReason::OperatorParam),
                                method: fir::ConversionMethod::Translate,
                            },
                        );
                        fir::NumericTy::Float
                    }
                    (Ty::Float, Ty::Integer) => {
                        conv_using(
                            &mut rhs,
                            fir::Conversion {
                                from: fir::Ty::Integer,
                                to: fir::Ty::Float,
                                provenance: op_span.of(ConversionReason::OperatorParam),
                                method: fir::ConversionMethod::Translate,
                            },
                        );
                        fir::NumericTy::Float
                    }
                    _ => {
                        self.report(LowerDiagnostic::BinaryOpTyMismatch {
                            lhs: lhs.to_span().of(self.resources().print_ty(lhs_ty)),
                            rhs: rhs.to_span().of(self.resources().print_ty(rhs_ty)),
                            op_span,
                        })?;
                        break 'operator fir::Operator::TypeError;
                    }
                };
                let op = match op.into_inner() {
                    BinaryOp::Greater => fir::CoreOperator::Greater(numeric),
                    BinaryOp::Lesser => fir::CoreOperator::Lesser(numeric),
                    BinaryOp::GreaterOrEqual => fir::CoreOperator::GreaterOrEqual(numeric),
                    BinaryOp::LesserOrEqual => fir::CoreOperator::GreaterOrEqual(numeric),
                    BinaryOp::Add => fir::CoreOperator::Add(numeric),
                    BinaryOp::Sub => fir::CoreOperator::Sub(numeric),
                    BinaryOp::Mul => fir::CoreOperator::Mul(numeric),
                    BinaryOp::Div => fir::CoreOperator::Div(numeric),
                    BinaryOp::Mod => fir::CoreOperator::Rem(numeric),
                    _ => unreachable!(),
                };
                fir::Operator::Core(op)
            }
            // fixme: this doesn't yet respect control flow, should be lazily evaluated.
            BinaryOp::Or | BinaryOp::And => 'operator: {
                if let from @ Ty::Integer = lhs_ty {
                    conv_using(
                        &mut lhs,
                        fir::Conversion {
                            from,
                            to: Ty::Float,
                            provenance: op_span.of(ConversionReason::OperatorParam),
                            method: fir::ConversionMethod::NarrowingTransmute,
                        },
                    );
                }
                if let from @ Ty::Integer = rhs_ty {
                    conv_using(
                        &mut rhs,
                        fir::Conversion {
                            from,
                            to: Ty::Float,
                            provenance: op_span.of(ConversionReason::OperatorParam),
                            method: fir::ConversionMethod::NarrowingTransmute,
                        },
                    );
                }
                match (lhs_ty, rhs_ty) {
                    (Ty::Bool | Ty::Integer, Ty::Bool | Ty::Integer) => (),
                    _ => {
                        self.report(LowerDiagnostic::BinaryOpTyMismatch {
                            lhs: lhs.to_span().of(self.resources().print_ty(lhs_ty)),
                            rhs: rhs.to_span().of(self.resources().print_ty(rhs_ty)),
                            op_span,
                        })?;
                        break 'operator fir::Operator::TypeError;
                    }
                };

                match op.into_inner() {
                    BinaryOp::And => fir::Operator::Core(fir::CoreOperator::And),
                    BinaryOp::Or => fir::Operator::Core(fir::CoreOperator::Or),
                    _ => unreachable!(),
                }
            }
            BinaryOp::Eq | BinaryOp::Neq => 'operator: {
                let ty = match lhs_ty.relationship_to(rhs_ty) {
                    fir::TyRelationship::Identical | fir::TyRelationship::Generalisation => lhs_ty,
                    fir::TyRelationship::Specialisation => rhs_ty,
                    fir::TyRelationship::Distinct => match (lhs_ty, rhs_ty) {
                        (from @ Ty::Integer, to @ Ty::Float) => {
                            conv_using(
                                &mut lhs,
                                fir::Conversion {
                                    from,
                                    to,
                                    provenance: op_span.of(ConversionReason::OperatorParam),
                                    method: fir::ConversionMethod::Translate,
                                },
                            );
                            Ty::Float
                        }
                        (to @ Ty::Float, from @ Ty::Integer) => {
                            conv_using(
                                &mut rhs,
                                fir::Conversion {
                                    from,
                                    to,
                                    provenance: op_span.of(ConversionReason::OperatorParam),
                                    method: fir::ConversionMethod::Translate,
                                },
                            );
                            Ty::Float
                        }
                        // fixme: best to restrict to only `0` somewhere.
                        (from @ Ty::Ref(_), to @ Ty::Integer) => {
                            conv_using(
                                &mut lhs,
                                fir::Conversion {
                                    from,
                                    to,
                                    provenance: op_span.of(ConversionReason::OperatorParam),
                                    method: fir::ConversionMethod::NarrowingTransmute,
                                },
                            );
                            Ty::Integer
                        }
                        (to @ Ty::Integer, from @ Ty::Ref(_)) => {
                            conv_using(
                                &mut rhs,
                                fir::Conversion {
                                    from,
                                    to,
                                    provenance: op_span.of(ConversionReason::OperatorParam),
                                    method: fir::ConversionMethod::NarrowingTransmute,
                                },
                            );
                            Ty::Integer
                        }
                        _ => {
                            self.report(LowerDiagnostic::BinaryOpTyMismatch {
                                lhs: lhs.to_span().of(self.resources().print_ty(lhs_ty)),
                                rhs: rhs.to_span().of(self.resources().print_ty(rhs_ty)),
                                op_span,
                            })?;
                            break 'operator fir::Operator::TypeError;
                        }
                    },
                };

                match op.into_inner() {
                    BinaryOp::Eq => fir::Operator::Core(fir::CoreOperator::Eq(ty)),
                    BinaryOp::Neq => fir::Operator::Core(fir::CoreOperator::Neq(ty)),
                    _ => unreachable!(),
                }
            }
            BinaryOp::BitOr | BinaryOp::BitAnd => 'operator: {
                let boolwise = match (lhs_ty, rhs_ty) {
                    (Ty::Bool, Ty::Bool) => true,
                    (from @ Ty::Integer, to @ Ty::Bool) => {
                        conv_using(
                            &mut lhs,
                            fir::Conversion {
                                from,
                                to,
                                provenance: op_span.of(ConversionReason::OperatorParam),
                                method: fir::ConversionMethod::NarrowingTransmute,
                            },
                        );
                        true
                    }
                    (to @ Ty::Bool, from @ Ty::Integer) => {
                        conv_using(
                            &mut rhs,
                            fir::Conversion {
                                from,
                                to,
                                provenance: op_span.of(ConversionReason::OperatorParam),
                                method: fir::ConversionMethod::NarrowingTransmute,
                            },
                        );
                        true
                    }
                    (Ty::Integer, Ty::Integer) => false,
                    _ => {
                        self.report(LowerDiagnostic::BinaryOpTyMismatch {
                            lhs: lhs.to_span().of(self.resources().print_ty(lhs_ty)),
                            rhs: rhs.to_span().of(self.resources().print_ty(rhs_ty)),
                            op_span,
                        })?;
                        break 'operator fir::Operator::TypeError;
                    }
                };

                match (op.into_inner(), boolwise) {
                    (BinaryOp::BitAnd, true) => fir::Operator::Core(fir::CoreOperator::And),
                    (BinaryOp::BitOr, true) => fir::Operator::Core(fir::CoreOperator::Or),
                    (BinaryOp::BitAnd, false) => fir::Operator::Core(fir::CoreOperator::BitAnd),
                    (BinaryOp::BitOr, false) => fir::Operator::Core(fir::CoreOperator::BitOr),
                    _ => unreachable!(),
                }
            }
            BinaryOp::LeftShift => fir::Operator::Core(fir::CoreOperator::LeftShift),
            BinaryOp::RightShift => fir::Operator::Core(fir::CoreOperator::RightShift),
            BinaryOp::Pow => 'operator: {
                let pow_kind = match (lhs_ty, rhs_ty) {
                    (Ty::Integer, Ty::Integer) => fir::PowKind::IntegerBase,
                    (Ty::Float, Ty::Integer) => fir::PowKind::FloatBase {
                        exponent: fir::NumericTy::Integer,
                    },
                    (Ty::Float, Ty::Float) => fir::PowKind::FloatBase {
                        exponent: fir::NumericTy::Float,
                    },
                    _ => {
                        self.report(LowerDiagnostic::BinaryOpTyMismatch {
                            lhs: lhs.to_span().of(self.resources().print_ty(lhs_ty)),
                            rhs: rhs.to_span().of(self.resources().print_ty(rhs_ty)),
                            op_span,
                        })?;
                        break 'operator fir::Operator::TypeError;
                    }
                };

                fir::Operator::Core(fir::CoreOperator::Pow(pow_kind))
            }
        };

        Ok(fir::Operation {
            operands: vec![lhs, rhs],
            operator: op_span.of(op),
        })
    }

    fn lower_binary_op(
        &mut self,
        operator: AstSpanned<BinaryOp>,
        lhs: AstSpanned<Expression>,
        rhs: AstSpanned<Expression>,
    ) -> Result<fir::Expression> {
        let lhs = self.lower_expression(lhs)?;
        let rhs = self.lower_expression(rhs)?;
        self.lower_binary_operator(operator.respan(), lhs, rhs)
            .map(fir::Expression::Operation)
    }

    fn lower_expression(
        &mut self,
        expression: AstSpanned<Expression>,
    ) -> Result<fir::Spanned<fir::Expression>> {
        let span = expression.span;
        let expr = match expression.value {
            Expression::Constant(lit) => self.lower_literal(lit, span.to_span()),
            Expression::Invoke(invoke) => return self.lower_invoke(invoke, span),
            Expression::Group { inner, .. } => return self.lower_expression(*inner),
            Expression::BinaryOp { lhs, operator, rhs } => {
                self.lower_binary_op(operator, *lhs, *rhs)?
            }
            Expression::UnaryOp { operator, inner } => {
                fir::Expression::Operation(self.lower_unary_op(operator, *inner)?)
            }
        };
        Ok(fir::Spanned::new(expr, span))
    }

    fn lower_external_variable_idx(
        &mut self,
        first: fir::Spanned<String>,
        second: fir::Spanned<String>,
    ) -> Result<Tried<fir::VariableIdx>> {
        let var_idx = match self.form(first.as_deref()) {
            Ok((_, variables, warn)) => {
                let idx = match Self::field_of(first.to_span().of(variables), second.as_deref()) {
                    Ok((_, idx, warn)) => {
                        self.report_all(warn)?;
                        Resolved(fir::VariableIdx::FormExternal(idx))
                    }
                    Err(err) => {
                        self.report(err)?;
                        Unresolvable
                    }
                };
                self.report_all(warn)?;
                idx
            }
            Err(err) => {
                self.report(err)?;
                Unresolvable
            }
        };
        Ok(var_idx)
    }

    fn lower_set(
        &mut self,
        reference: Reference,
        expression: AstSpanned<Expression>,
        map_expression: impl FnOnce(
            &mut Self,
            fir::Spanned<fir::Expression>,
            fir::Spanned<Tried<fir::VariableIdx>>,
        ) -> Result<fir::Expression>,
    ) -> Result<fir::Statement> {
        let span = reference.to_span();
        let (first_ident, second_ident) = self.reference_pair(reference)?;
        let variable = if let Some(field_name) = second_ident {
            self.lower_external_variable_idx(first_ident, field_name)?
        } else if let Some(var_idx) = self.local_variable(first_ident.inner()) {
            Resolved(fir::VariableIdx::BodyLocal(var_idx))
        } else if let Some((var_idx, warn)) = self.global_variable(first_ident.as_deref()) {
            self.report_all(warn)?;
            Resolved(var_idx)
        } else {
            self.report(LowerDiagnostic::UnknownVariableInGeneral { variable: span })?;
            Unresolvable
        };

        let unmapped_value = self.lower_expression(expression)?;
        let value =
            unmapped_value
                .to_span()
                .of(map_expression(self, unmapped_value, span.of(variable))?);

        Ok(fir::Statement::SetVariable {
            variable: span.of(variable),
            value: self.coerce_expr(
                ConversionReason::VariableSet,
                value,
                self.get_variable_ty(variable),
                span,
            ),
        })
    }

    fn map_let_expression(
        assignment: AstSpanned<AssignOp>,
    ) -> impl FnOnce(
        &mut Self,
        fir::Spanned<fir::Expression>,
        fir::Spanned<Tried<fir::VariableIdx>>,
    ) -> Result<fir::Expression> {
        let binary_op = match assignment.value {
            AssignOp::Set => None,
            AssignOp::Add => Some(BinaryOp::Add),
            AssignOp::Sub => Some(BinaryOp::Sub),
            AssignOp::Mul => Some(BinaryOp::Mul),
            AssignOp::Div => Some(BinaryOp::Div),
            AssignOp::Pow => Some(BinaryOp::Pow),
            AssignOp::Or => Some(BinaryOp::Or),
            AssignOp::And => Some(BinaryOp::And),
            AssignOp::Mod => Some(BinaryOp::Mod),
        };
        let binary_op = binary_op.map(|op| assignment.span().of(op));
        move |this, expression, var_idx| {
            if let Some(op) = binary_op {
                let get_expr = fir::Expression::GetVariable(var_idx.into_inner());

                this.lower_binary_operator(op, var_idx.to_span().of(get_expr), expression)
                    .map(fir::Expression::Operation)
            } else {
                Ok(expression.into_inner())
            }
        }
    }

    fn lower_block_inline(&mut self, in_statements: Vec<Statement>) -> Result<()> {
        for stmt in in_statements {
            let stmt_span = stmt.kind.span();
            match stmt.kind.value {
                StatementKind::Variables(variables) => {
                    self.insert_body_variables(variables);
                }
                other => {
                    let stmt = self.lower_stmt(other, stmt_span)?;
                    self.push_stmt(stmt);
                }
            }
        }

        Ok(())
    }

    fn lower_conditional(
        &mut self,
        reason_span: Span,
        condition: AstSpanned<Expression>,
        code: Vec<Statement>,
    ) -> Result<Spanned<fir::Block>> {
        let span_sum = reason_span
            .expand_to_include(condition.span())
            .expand_to_include(code.iter().map(|stmt| stmt.kind.span()).sum());

        let (block, res) = self.in_block(fir::BranchTarget::Break { depth: 0 }, move |cx| {
            let mut expr = cx.lower_expression(condition)?;
            match typeck::TypeckEngine::new().type_of(&*cx, expr.inner()) {
                Resolved(Ty::Bool) | Unresolvable => (),
                Resolved(from) => {
                    expr = expr.to_span().of(fir::Expression::Convert {
                        value: Box::new(expr),
                        conversion: fir::Conversion {
                            from,
                            to: Ty::Bool,
                            provenance: reason_span.of(ConversionReason::Other),
                            method: fir::ConversionMethod::NarrowingTransmute,
                        },
                    });
                }
            }
            expr = expr
                .to_span()
                .of(fir::Expression::Operation(fir::Operation {
                    operands: vec![expr],
                    operator: reason_span.of(fir::Operator::Core(fir::CoreOperator::Not)),
                }));

            cx.push_stmt(reason_span.of(fir::Statement::Branch {
                target: fir::BranchTarget::Break { depth: 1 },
                kind: fir::BranchKind::IfTrue(expr),
            }));

            cx.lower_block_inline(code)
        });
        res?;

        Ok(span_sum.of(block))
    }

    fn lower_stmt(
        &mut self,
        stmt: StatementKind,
        span: impl ToSpan,
    ) -> Result<Spanned<fir::Statement>> {
        let stmt = match stmt {
            StatementKind::If(chain) => {
                self.ck_token_case(&chain.kw_if.span)?;

                let mut containing_block = fir::Block {
                    statements: Vec::new(),
                    termination: fir::BranchTarget::Break { depth: 0 },
                };

                let mut next_block = *chain.next;
                loop {
                    match next_block {
                        NextIf::ElseIf(ElseIf {
                            kw_elseif,
                            condition,
                            leading_newline: _,
                            statements,
                            next,
                        }) => {
                            self.ck_token_case(&kw_elseif.span)?;

                            let inner_block = self.lower_conditional(
                                kw_elseif.span.span(),
                                condition,
                                statements,
                            )?;
                            containing_block
                                .statements
                                .push(inner_block.span_map(fir::Statement::Block));

                            next_block = *next;
                        }
                        NextIf::Else(Else {
                            kw_else,
                            leading_newline: _,
                            statements,
                            kw_end_if,
                        }) => {
                            self.ck_token_case(&kw_else.span)?;
                            self.ck_token_case(&kw_end_if.span)?;

                            self.lower_block_inline(statements)?;

                            break;
                        }
                        NextIf::End(kw_end_if) => {
                            self.ck_token_case(&kw_end_if.span)?;
                            break;
                        }
                    }
                }

                fir::Statement::Block(containing_block)

                // let mut next_block = self.reserve_block();
                // let mut branch_blocks = Vec::new();
                // let mut iter = chain.into_iter();
                // for (span, condition, code) in iter.by_ref() {
                //     // let target = self.reserve_block();
                //     // stmt_buffer.push(span.of(fir::Statement::Branch(fir::Branch {

                //     // }) {
                //     //     target: target.idx,
                //     //     kind: fir::BranchKind::IfTrue(expr),
                //     // }));
                //     // branch_blocks.push((code, target));
                // }

                // let mut termination = fir::Branch::Jump(next_block.idx);
                // if let Some(Else {
                //     statements,
                //     kw_else,
                //     ..
                // }) = iter.else_branch()
                // {
                //     let else_block = mem::replace(&mut next_block, self.reserve_block());
                //     stmt_buffer.push(kw_else.span().of(fir::Statement::Branch {
                //         target: else_block.idx,
                //         kind: fir::BranchKind::Unconditional,
                //     }));
                //     self.lower_block(
                //         statements,
                //         fir::Branch::Jump(next_block.idx),
                //         Some(else_block),
                //     )?;
                //     termination = fir::Branch::Unreachable;
                // }

                // for (code, block_idx) in branch_blocks {
                //     self.lower_block(
                //         code,
                //         fir::Branch::Jump(next_block.idx),
                //         Some(block_idx),
                //     )?;
                // }

                // return Ok(ControlFlow::Break((termination, next_block)));
            }
            StatementKind::Set {
                var,
                value,
                kw_set,
                kw_to,
            } => {
                self.ck_token_case(&kw_set.span)?;
                self.ck_token_case(&kw_to.span)?;

                self.lower_set(var, value, |_, x, _| Ok(x.into_inner()))?
            }
            StatementKind::Variables(_) => {
                unreachable!("variables have been previously processed")
            }
            StatementKind::Let {
                var,
                assignment,
                value,
                kw_let,
            } => {
                self.ck_token_case(&kw_let.span)?;
                self.lower_set(var, value, Self::map_let_expression(assignment))?
            }
            StatementKind::Return(kw_return) => {
                self.ck_token_case(&kw_return.span)?;
                fir::Statement::Branch {
                    kind: fir::BranchKind::Unconditional,
                    target: fir::BranchTarget::Return,
                }
            }
            StatementKind::Expression(e) => fir::Statement::Express(
                self.lower_expression(AstSpanned {
                    value: e,
                    span: (span.to_span().start, span.to_span().end),
                })?
                .into_inner(),
            ),
        };
        Ok(span.to_span().of(stmt))
    }

    fn lower_block(
        &mut self,
        code: Vec<Statement>,
        termination: fir::BranchTarget,
    ) -> Result<Spanned<fir::Block>> {
        let span: Span = code.iter().map(|stmt| stmt.kind.span()).sum();
        let (block, res) = self.in_block(termination, move |cx| cx.lower_block_inline(code));
        res?;
        Ok(span.of(block))
    }

    fn lower_body(&mut self, statements: Vec<Statement>) -> Result<fir::FunctionBody> {
        let block = self.lower_block(statements, fir::BranchTarget::Break { depth: 0 })?;
        Ok(self.flush_function_body(block))
    }

    fn lower_item(
        &mut self,
        item: EventImpl,
        span: impl ToSpan,
        mut consume_event_impl: impl FnMut(fir::Spanned<fir::EventImpl>),
        mut consume_ufd: impl FnMut(fir::Spanned<fir::UserFunctionDefinition>),
    ) -> Result<()> {
        self.ck_token_case(&item.kw_begin.span)?;
        self.ck_token_case(&item.block.end.span)?;

        let name_span = item.name.span();
        let item = match item.args {
            // todo: function defintiions
            args => consume_event_impl(
                span.to_span().of(fir::EventImpl {
                    arguments: args
                        .map(|args| {
                            let args = args.respan();
                            let span = args.to_span();
                            let arg_expr = match args.into_inner() {
                                EventArgs::Const(int) => {
                                    self.lower_literal(Literal::DecInt(int), span)
                                }
                                EventArgs::Var(reference) => {
                                    return self.lower_invoke(
                                        Invoke {
                                            referent: reference,
                                            args: Vec::new(),
                                        },
                                        span,
                                    )
                                }
                            };
                            Ok(span.of(arg_expr))
                        })
                        .transpose()?
                        .into_iter()
                        .collect(),
                    event: match self.event(item.name.respan().as_deref()) {
                        Ok(event) => name_span.of(Resolved(event)),
                        Err(d) => {
                            self.report(d)?;
                            name_span.of(Unresolvable)
                        }
                    },
                    body: self.lower_body(item.block.statements)?,
                }),
            ),
        };
        Ok(())
    }

    fn lower_script(mut self, script: Script) -> Result<fir::Script> {
        let mut event_impls = Vec::new();
        let mut ufds = Vec::new();
        for item in script.items {
            match item.value.kind {
                ItemKind::Variables(_) => (),
                ItemKind::EventImpl(event) => {
                    self.lower_item(
                        event,
                        item.span,
                        |event| event_impls.push(event),
                        |ufd| ufds.push(ufd),
                    )?;
                }
            }
        }

        Ok(fir::Script {
            event_impls,
            ufds,
            variables: Default::default(),
        })
    }
}

pub(crate) trait Respan<T> {
    fn respan(self) -> Spanned<T>;
    fn span(&self) -> Span;
}

impl<T> Respan<T> for AstSpanned<T> {
    fn respan(self) -> Spanned<T> {
        Spanned::new(self.value, self.span)
    }

    fn span(&self) -> Span {
        self.span.to_span()
    }
}

impl Respan<String> for Ident {
    fn respan(self) -> Spanned<String> {
        self.text.respan()
    }

    fn span(&self) -> Span {
        self.text.span()
    }
}

pub(crate) fn iter_lowered_variables(
    variables: &Variables,
) -> impl Iterator<Item = fir::BodyVariableInfo> + '_ {
    let span = variables.ty.span();
    let ty = match &variables.ty.value {
        VarTy::Short | VarTy::Int | VarTy::Long => Ty::Integer,
        VarTy::Float => Ty::Float,
        VarTy::Ref => Ty::Ref(fir::RefTy::Unknown),
    };

    let ty = span.of(ty);
    variables
        .names
        .iter()
        .map(move |name| fir::BodyVariableInfo {
            name: name.text.clone().respan(),
            ty,
        })
}
