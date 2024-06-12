use std::borrow::Cow;

use fir::*;

use wasm_encoder::{BlockType, ConstExpr, Instruction, RefType, ValType};

use super::Result;

use crate::{
    data::{
        function_from_multiple_events, function_from_single_event, CodeBuilder, Import,
        ImportNamespace, ModuleBuilder, TyRepr, WasmTypeDef,
    },
    CompileFail, NotFound, Undefined,
};

pub fn const_expr_zero(val: ValType) -> ConstExpr {
    match val {
        ValType::I32 => ConstExpr::i32_const(0),
        ValType::I64 => ConstExpr::i64_const(0),
        ValType::F32 => ConstExpr::f32_const(0.),
        ValType::F64 => ConstExpr::f64_const(0.),
        ValType::V128 => ConstExpr::v128_const(0),
        ValType::Ref(RefType {
            nullable: true,
            heap_type,
        }) => ConstExpr::ref_null(heap_type),
        ValType::Ref(_) => {
            panic!("cannot create a const expression for a non-nullable `ValType::Ref`")
        }
    }
}

impl<'s, 'ctx: 's, F: Frontend, C: Component> CodeBuilder<'ctx, 's, F, C> {
    fn lower_pv(&mut self, p: &PrimitiveValue) -> Result<()> {
        match p {
            PrimitiveValue::NullReference => self.instruction(Instruction::I32Const(0)),
            PrimitiveValue::Unit => (),
            &PrimitiveValue::Bool(b) => self.instruction(Instruction::I32Const(b as i32)),
            &PrimitiveValue::Int(i) => self.instruction(Instruction::I32Const(i as i32)),
            &PrimitiveValue::Float(f) => self.instruction(Instruction::F32Const(f as f32)),
            PrimitiveValue::String(s) => {
                let idx = self.cx.string_idx(s.clone());
                self.instruction(Instruction::I32Const(idx));
            }
        };

        Ok(())
    }

    fn lower_expr(&mut self, expr: Spanned<&Expression>) -> Result<()> {
        let span = expr.to_span();
        match expr.into_inner() {
            Expression::PrimitiveValue(p) => return self.lower_pv(p),
            &Expression::FormRef(form_idx) => {
                let func_idx = self.cx.funcs.import(
                    Import {
                        namespace: ImportNamespace::Runtime,
                        name: Cow::Borrowed("resolve_formid"),
                    },
                    WasmTypeDef {
                        params: vec![ValType::I32; 2],
                        results: vec![ValType::I32],
                    },
                );

                let cmp_i = ComponentIdx::from(form_idx).0;
                let form_i = form_idx.0 as u32;

                self.instruction(Instruction::I32Const(cmp_i as i32));
                self.instruction(Instruction::I32Const(form_i as i32));
                self.instruction(Instruction::Call(func_idx));
            }
            Expression::GetVariable(Unresolvable) => {
                self.silently_fail();
            }
            &Expression::GetVariable(Resolved(fir::VariableIdx::BodyLocal(var_idx))) => {
                let local_idx = self.get_local_variable(span.of(var_idx))?;
                self.instruction(Instruction::LocalGet(local_idx));
            }
            &Expression::GetVariable(Resolved(fir::VariableIdx::FormInternal(var_idx))) => {
                match self.cx.global_var_indices.get(&var_idx) {
                    Some(&(global_idx, _)) => self.instruction(Instruction::GlobalGet(global_idx)),
                    None => {
                        self.report_failing(Undefined {
                            span,
                            kind: NotFound::FormExternalVariable,
                        })?;
                        return Ok(());
                    }
                }
            }
            &Expression::GetVariable(Resolved(fir::VariableIdx::FormExternal(var_idx))) => {
                let Ok((var, TyRepr::Wasm(return_ty))) = self
                    .cx
                    .bg
                    .get_external_variable(span.of(var_idx))
                    .map_err(|d| self.report_failing(d))
                    .and_then(|var| {
                        self.cx
                            .lower_ty_to_repr(span.of(var.ty))
                            .map(|repr| (var, repr))
                            .map_err(|d| self.report_failing(d))
                    })
                else {
                    return Ok(());
                };
                let func_idx = self.cx.funcs.import(
                    Import {
                        namespace: ImportNamespace::Runtime,
                        name: Cow::Owned(format!("get_variable_{}", var.ty.to_ident())),
                    },
                    WasmTypeDef {
                        params: vec![ValType::I32],
                        results: vec![return_ty],
                    },
                );
                self.instruction(Instruction::I32Const(var_idx.0 as i32));
                self.instruction(Instruction::Call(func_idx));
            }
            Expression::Operation(operation) => match operation.operator.into_inner() {
                Operator::Core(op) => {
                    let inst = match op {
                        CoreOperator::BitOr | CoreOperator::Or => Instruction::I32Or,
                        CoreOperator::BitAnd | CoreOperator::And => Instruction::I32And,
                        CoreOperator::BitXor => Instruction::I32Xor,
                        CoreOperator::Neq(Ty::Unit) | CoreOperator::Eq(Ty::Unit) => {
                            for arg in &operation.operands {
                                self.lower_expr(arg.as_ref())?;
                            }
                            return Ok(());
                        }
                        CoreOperator::Eq(Ty::Integer | Ty::String | Ty::Bool | Ty::Ref(_)) => {
                            Instruction::I32Eq
                        }
                        CoreOperator::Eq(Ty::Float) => Instruction::F32Eq,
                        CoreOperator::Neq(Ty::Integer | Ty::String | Ty::Bool | Ty::Ref(_)) => {
                            Instruction::I32Ne
                        }
                        CoreOperator::Neq(Ty::Float) => Instruction::F32Ne,
                        CoreOperator::Eq(_) | CoreOperator::Neq(_) => {
                            return Err(CompileFail::UnsupportedOperation {
                                span: operation.operator.to_span(),
                            })
                        }
                        CoreOperator::GreaterOrEqual(NumericTy::Integer) => Instruction::I32GeS,
                        CoreOperator::LesserOrEqual(NumericTy::Integer) => Instruction::I32LeS,
                        CoreOperator::GreaterOrEqual(NumericTy::Float) => Instruction::F32Ge,
                        CoreOperator::LesserOrEqual(NumericTy::Float) => Instruction::F32Le,
                        CoreOperator::Greater(NumericTy::Integer) => Instruction::I32GtS,
                        CoreOperator::Lesser(NumericTy::Integer) => Instruction::I32LtS,
                        CoreOperator::Greater(NumericTy::Float) => Instruction::F32Gt,
                        CoreOperator::Lesser(NumericTy::Float) => Instruction::F32Lt,
                        CoreOperator::LeftShift => Instruction::I32Shl,
                        CoreOperator::RightShift => Instruction::I32ShrS,
                        CoreOperator::Add(NumericTy::Integer) => Instruction::I32Add,
                        CoreOperator::Sub(NumericTy::Integer) => Instruction::I32Sub,
                        CoreOperator::Mul(NumericTy::Integer) => Instruction::I32Mul,
                        CoreOperator::Div(NumericTy::Integer) => Instruction::I32DivS,
                        CoreOperator::Add(NumericTy::Float) => Instruction::F32Add,
                        CoreOperator::Sub(NumericTy::Float) => Instruction::F32Sub,
                        CoreOperator::Mul(NumericTy::Float) => Instruction::F32Mul,
                        CoreOperator::Div(NumericTy::Float) => Instruction::F32Div,
                        CoreOperator::Rem(NumericTy::Integer) => Instruction::I32RemS,
                        CoreOperator::Neg(NumericTy::Integer) => {
                            self.instruction(Instruction::I32Const(0));
                            for arg in &operation.operands {
                                self.lower_expr(arg.as_ref())?;
                            }
                            self.instruction(Instruction::I32Sub);
                            return Ok(());
                        }
                        CoreOperator::Neg(NumericTy::Float) => Instruction::F32Neg,
                        CoreOperator::Rem(NumericTy::Float) | CoreOperator::Pow(_) => {
                            todo!("nvse i hate you (probably defer to runtime? who knows.)")
                        }
                        CoreOperator::Not => {
                            for arg in &operation.operands {
                                self.lower_expr(arg.as_ref())?;
                            }
                            self.instruction(Instruction::I32Const(1));
                            self.instruction(Instruction::I32Xor);
                            return Ok(());
                        }
                        CoreOperator::BitNot => {
                            for arg in &operation.operands {
                                self.lower_expr(arg.as_ref())?;
                            }
                            self.instruction(Instruction::I32Const(-1));
                            self.instruction(Instruction::I32Xor);
                            return Ok(());
                        }
                    };
                    for arg in &operation.operands {
                        self.lower_expr(arg.as_ref())?;
                    }
                    self.instruction(inst);
                }
                Operator::Function { idx: Unresolvable } => {
                    self.silently_fail();
                    for arg in &operation.operands {
                        self.lower_expr(arg.as_ref())?;
                    }
                }
                Operator::Function {
                    idx: Resolved(func_idx),
                } => {
                    let mut things_to_report = Vec::new();
                    match self.cx.get_function_definition(span.of(func_idx)) {
                        Err(d) => {
                            self.report_failing(d)?;
                        }
                        Ok((name, func)) => {
                            let name =
                                format!("{};{}", name.ident, operation.operands.len()).into();
                            let return_vals = match self
                                .cx
                                .lower_ty_to_repr(span.of(func.return_ty))
                            {
                                Ok(return_val) => return_val.into_option().into_iter().collect(),
                                Err(d) => {
                                    things_to_report.push(d);
                                    Vec::new()
                                }
                            };
                            println!("{func:?}");
                            let params = func
                                .self_param
                                .as_ref()
                                .map_or(&[] as &[Ty], |p| core::slice::from_ref(p))
                                .iter()
                                .copied()
                                .chain(func.params.iter().map(|param| param.ty))
                                .flat_map(|param| {
                                    self.cx
                                        .lower_ty_to_repr(span.of(param))
                                        .map_err(|d| {
                                            things_to_report.push(d);
                                        })
                                        .ok()
                                        .and_then(TyRepr::into_option)
                                })
                                .take(operation.operands.len())
                                .collect();
                            if things_to_report.is_empty() {
                                let call_idx = self.cx.funcs.import(
                                    Import {
                                        namespace: ImportNamespace::Functions,
                                        name,
                                    },
                                    WasmTypeDef {
                                        params,
                                        results: return_vals,
                                    },
                                );

                                for arg in &operation.operands {
                                    self.lower_expr(arg.as_ref())?;
                                }

                                self.instruction(Instruction::Call(call_idx));
                            } else {
                                for d in things_to_report {
                                    self.report_failing(d)?;
                                }
                            }
                        }
                    }
                }
                Operator::TypeError => {}
            },
            Expression::Convert { value, conversion } => {
                self.lower_expr(value.as_ref().as_ref())?;
                self.lower_conversion(conversion)?;
            }
        };

        Ok(())
    }

    fn lower_conversion(&mut self, conversion: &Conversion) -> Result<()> {
        if conversion.from == conversion.to {
            return Ok(());
        }
        let inst = match (conversion.from, conversion.to, conversion.method) {
            (_, _, ConversionMethod::NarrowingTransmute | ConversionMethod::WideningTransmute) => {
                return Ok(())
            }
            (Ty::Integer, Ty::Float, ConversionMethod::Translate) => Instruction::I32TruncF32S,
            (Ty::Float, Ty::Integer, ConversionMethod::Translate) => Instruction::F32ConvertI32S,
            _ => todo!("these conversions are never generated by fir-geckscript, but we should probably still support them"),
        };
        self.instruction(inst);
        Ok(())
    }

    fn lower_stmt(&mut self, stmt: Spanned<&Statement>) -> Result<()> {
        let span = stmt.to_span();
        match stmt.into_inner() {
            Statement::Express(expr) => {
                let ty = typeck::TypeckEngine::new().type_of(&*self, expr);
                self.lower_expr(Spanned::new(expr, span))?;
                if ty != Resolved(Ty::Unit) {
                    self.instruction(Instruction::Drop);
                }
            }
            Statement::SetVariable { variable, value } => match variable.into_inner() {
                Unresolvable => {
                    self.silently_fail();
                    self.lower_expr(value.as_ref())?;
                }
                Resolved(VariableIdx::BodyLocal(var_idx)) => {
                    self.lower_expr(value.as_ref())?;
                    let idx = self.get_local_variable(variable.to_span().of(var_idx))?;
                    self.instruction(Instruction::LocalSet(idx));
                }
                Resolved(VariableIdx::FormInternal(var_idx)) => {
                    if let Some(&(global_idx, _)) = self.cx.global_var_indices.get(&var_idx) {
                        self.lower_expr(value.as_ref())?;
                        self.instruction(Instruction::GlobalSet(global_idx));
                    } else {
                        return Err(CompileFail::Undefined {
                            span: variable.to_span(),
                            kind: NotFound::LocalVariable,
                        });
                    }
                }
                Resolved(VariableIdx::FormExternal(var_idx)) => {
                    let Ok((var, TyRepr::Wasm(return_ty))) = self
                        .cx
                        .bg
                        .get_external_variable(variable.to_span().of(var_idx))
                        .map_err(|d| self.report_failing(d))
                        .and_then(|var| {
                            self.cx
                                .lower_ty_to_repr(variable.to_span().of(var.ty))
                                .map(|repr| (var, repr))
                                .map_err(|d| self.report_failing(d))
                        })
                    else {
                        return Ok(());
                    };
                    let func_idx = self.cx.funcs.import(
                        Import {
                            namespace: ImportNamespace::Runtime,
                            name: Cow::Owned(format!("set_variable_{}", var.ty.to_ident())),
                        },
                        WasmTypeDef {
                            params: vec![ValType::I32, return_ty],
                            results: vec![],
                        },
                    );
                    self.instruction(Instruction::I32Const(var_idx.0 as i32));
                    self.lower_expr(value.as_ref())?;
                    self.instruction(Instruction::Call(func_idx));
                }
            },
            &Statement::Branch { target, ref kind } => match kind {
                BranchKind::Unconditional => self.lower_branch(target),
                BranchKind::IfTrue(expr) => {
                    self.lower_expr(expr.as_ref())?;
                    if let BranchTarget::Break { depth } = target {
                        self.instruction(Instruction::BrIf(depth as u32));
                    } else {
                        self.instruction(Instruction::If(BlockType::Empty));
                        self.lower_branch(target);
                        self.instruction(Instruction::End);
                    }
                }
            },
            Statement::Block(block) => {
                self.lower_block(block)?;
            }
        };

        Ok(())
    }

    fn lower_branch(&mut self, termination: fir::BranchTarget) {
        match termination {
            BranchTarget::Loop | BranchTarget::Break { depth: 0 } => (),
            BranchTarget::Return => self.instruction(Instruction::Return),
            BranchTarget::Unreachable => self.instruction(Instruction::Unreachable),
            BranchTarget::Break { depth } => self.instruction(Instruction::Br(depth as u32)),
        }
    }

    fn lower_block(&mut self, block: &fir::Block) -> Result<()> {
        match block.termination {
            fir::BranchTarget::Break { .. }
            | fir::BranchTarget::Return
            | fir::BranchTarget::Unreachable => {
                println!("yahaha");
                self.instruction(Instruction::Block(BlockType::Empty))
            }
            fir::BranchTarget::Loop => self.instruction(Instruction::Loop(BlockType::Empty)),
        }

        for stmt in &block.statements {
            self.lower_stmt(stmt.as_ref())?;
        }

        self.lower_branch(block.termination);
        self.instruction(Instruction::End);

        Ok(())
    }

    pub(crate) fn lower_body(&mut self, block: &fir::Block) -> Result<bool> {
        self.lower_block(block)?;
        Ok(!self.has_failed())
    }
}

impl<'a, F: Frontend, C: Component> ModuleBuilder<'a, F, C> {
    fn push_and_export_function(
        &mut self,
        function: Option<wasm_encoder::Function>,
        name: Cow<'a, str>,
        params: Vec<ValType>,
        results: Vec<ValType>,
    ) {
        self.funcs.define(name, WasmTypeDef { params, results });
        if let Some(mut f) = function {
            f.instruction(&Instruction::End);
            self.codes.function(&f);
        }
    }

    pub fn flush(&mut self) -> Result<()> {
        use crate::context::EventStrategy;
        match self.bg.opts.event_strategy {
            EventStrategy::EventLoop => {
                let func = function_from_multiple_events(self, || {
                    self.bg.source.event_impls.iter().map(Spanned::inner)
                })?;
                self.push_and_export_function(
                    func,
                    Cow::Borrowed("flush_events"),
                    Vec::new(),
                    Vec::new(),
                );
            }
            EventStrategy::AdHoc => {
                for event in &self.bg.source.event_impls {
                    let Resolved(event_idx) = event.inner().event.into_inner() else {
                        continue;
                    };
                    let Ok(event_def) = self
                        .bg
                        .get_event(event.inner().event.to_span().of(event_idx))
                    else {
                        continue;
                    };

                    let func = function_from_single_event(self, event.inner())?;
                    if let Some(func) = func {
                        self.push_and_export_function(
                            Some(func),
                            Cow::Owned(format!(
                                "on:{}",
                                self.bg.res.print_name(&event_def.name, event_idx)
                            )),
                            Vec::new(),
                            Vec::new(),
                        );
                    }
                }
            }
        }

        // todo: ufds

        Ok(())
    }
}
