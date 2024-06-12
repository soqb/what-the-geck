use core::fmt;
use std::borrow::Cow;

use fir::{Expression, PrimitiveValue, Span, Spanned, Statement, VarTy};
use hashbrown::HashMap;
use thiserror::Error;
use wasm_encoder::{BlockType, ConstExpr, ExportKind, Function, Instruction, Module, ValType};

use crate::{
    data::{CodeBuilder, Import, VarKind, WasmTy, WasmVarScope},
    diagnostics::Result,
    CompileDiagnostic, CompileFail,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FormHint {
    Sound,
    Topic,
    Quest,
    Race,
    Class,
    Faction,
    Global,
    Furniture,
    WorldSpace,
    AIPackage,
    CombatStyle,
    MagicEffect,
    Weather,
    Npc,
    EffectShader,
    FormList,
    MenuIcon,
    Perk,
    Note,
    ImageSpaceModifier,
    ImageSpace,
    EncounterZone,
    Message,
    SoundFile,
    LeveledCharacter,
    LeveledCreature,
    LeveledItem,
    Reputation,
    Casino,
    CasinoChip,
    Challenge,
    CaravanMoney,
    CaravanCard,
    CaravanDeck,
    Region,
    MusicType,

    Owner,
    SpellItem,
    InvObjOrFormList,
    ActorBase,
    Spell,
    MapMarker,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ObjectRefHint {
    Actor,
    Creature,
    MapMarker,
    Container,
    Cell,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RefTy {
    BaseForm(Option<FormHint>),
    ObjectRef(Option<ObjectRefHint>),
    Unknown,
}

impl fmt::Display for RefTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                RefTy::Unknown => "ref",
                RefTy::ObjectRef(None) => "ref (object)",
                RefTy::BaseForm(None) => "ref (form)",
                RefTy::ObjectRef(Some(form)) => return write!(f, "ref (object {form:?})"),
                RefTy::BaseForm(Some(form)) => return write!(f, "ref (form {form:?})"),
            }
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StringTy {
    Literal,
    Var,
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum GeckTy {
    Int,
    Float,
    Ref(RefTy),
    String,
    Bool,
    Array,
}

impl From<GeckTy> for ValType {
    fn from(value: GeckTy) -> Self {
        value.into_wasm().into_val()
    }
}

impl From<VarTy> for GeckTy {
    fn from(value: VarTy) -> Self {
        match value {
            VarTy::Int => GeckTy::Int,
            VarTy::Short => GeckTy::Int,
            VarTy::Long => GeckTy::Int,
            VarTy::Float => GeckTy::Float,
            VarTy::Ref => GeckTy::Ref(RefTy::Unknown),
            VarTy::String => GeckTy::String(StringTy::Var),
            VarTy::Array => GeckTy::Array,
        }
    }
}

impl fmt::Display for GeckTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                GeckTy::Int => "int",
                GeckTy::Float => "float",
                GeckTy::Ref(ty) => return fmt::Display::fmt(&ty, f),
                GeckTy::String => "string",
                GeckTy::Bool => "bool",
                GeckTy::Array => "array",
            }
        )
    }
}

impl GeckTy {
    pub const fn form_ref(value: FormHint) -> Self {
        Self::Ref(RefTy::BaseForm(Some(value)))
    }

    pub const fn object_ref(value: ObjectRefHint) -> Self {
        Self::Ref(RefTy::ObjectRef(Some(value)))
    }

    pub fn is_eq(self, other: Self) -> bool {
        self == other
    }

    pub fn is_comparable_with(self, other: Self) -> bool {
        match (self, other) {
            (GeckTy::Ref(RefTy::BaseForm(_)), GeckTy::Ref(RefTy::ObjectRef(_)))
            | (GeckTy::Ref(RefTy::ObjectRef(_)), GeckTy::Ref(RefTy::BaseForm(_))) => false,
            (GeckTy::Ref(_), GeckTy::Ref(_)) => true,
            (GeckTy::String, GeckTy::String) => true,
            (x, y) => x == y,
        }
    }

    pub fn is_arithmetic_with(self, other: Self) -> bool {
        matches!(
            (self, other),
            (GeckTy::Int, GeckTy::Int) | (GeckTy::Float, GeckTy::Float)
        )
    }

    pub fn as_ident(&self) -> &str {
        match self {
            GeckTy::Int => "int",
            GeckTy::Float => "float",
            GeckTy::Ref(_) => "ref",
            GeckTy::Bool => "bool",
            GeckTy::String => "string",
            GeckTy::Array => "array",
        }
    }

    pub fn number_or_else(self, f: impl FnOnce() -> CompileFail) -> Result<WasmTy> {
        match self {
            GeckTy::Int => Ok(WasmTy::SignedI32),
            GeckTy::Float => Ok(WasmTy::F64),
            _ => Err(f()),
        }
    }

    pub fn into_wasm(self) -> WasmTy {
        match self {
            GeckTy::Int => WasmTy::SignedI32,
            GeckTy::Float => WasmTy::F64,
            GeckTy::Ref(_) | GeckTy::String | GeckTy::Bool | GeckTy::Array => WasmTy::UnsignedI32,
        }
    }

    pub fn const_expr_zero(self) -> ConstExpr {
        match self {
            GeckTy::Float => ConstExpr::f64_const(0.),
            GeckTy::Int | GeckTy::Ref(_) => ConstExpr::i32_const(0),
            _ => todo!(),
        }
    }

    pub fn expr_zero(self) -> Instruction<'static> {
        match self {
            GeckTy::Float => Instruction::F64Const(0.),
            GeckTy::Int | GeckTy::Bool | GeckTy::Ref(_) => Instruction::I32Const(0),
            _ => todo!(),
        }
    }
}

const NULL_REF: Instruction = Instruction::I32Const(0);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExprTy {
    Concrete(GeckTy),
    CmdReturn,
}

impl From<ExprTy> for WasmTy {
    fn from(value: ExprTy) -> Self {
        match value {
            ExprTy::Concrete(ty) => ty.into_wasm(),
            ExprTy::CmdReturn => WasmTy::F64,
        }
    }
}

impl From<ExprTy> for ValType {
    fn from(value: ExprTy) -> Self {
        WasmTy::from(value).into_val()
    }
}

impl From<VarTy> for ExprTy {
    fn from(value: VarTy) -> Self {
        Self::Concrete(value.into())
    }
}

impl fmt::Display for ExprTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprTy::CmdReturn => write!(f, "command result"),
            ExprTy::Concrete(ty) => fmt::Display::fmt(ty, f),
        }
    }
}

fn coersion(
    expr_span: Span,
    from: ExprTy,
    to: ExprTy,
    builder: &mut CodeBuilder<'_, '_, '_>,
) -> Result<()> {
    let to = match to {
        ExprTy::Concrete(to) => to,
        ExprTy::CmdReturn => {
            let inst = match from {
                ExprTy::Concrete(ty) => match ty.into_wasm() {
                    WasmTy::UnsignedI32 => Instruction::F64ConvertI32U,
                    WasmTy::SignedI32 => Instruction::F64ConvertI32S,
                    WasmTy::F64 => return Ok(()),
                },
                ExprTy::CmdReturn => return Ok(()),
            };

            builder.instruction(inst);

            return Ok(());
        }
    };

    if from == ExprTy::Concrete(to) {
        return Ok(());
    }

    let inst = match from {
        ExprTy::Concrete(from) => match (from, to) {
            (GeckTy::Int, GeckTy::Bool) => None,
            (GeckTy::Bool, GeckTy::Int) => None,
            (GeckTy::Ref(_), GeckTy::Bool) => None,
            (GeckTy::String, GeckTy::String) => None,
            (GeckTy::Float, GeckTy::Bool) => Some(Instruction::I32TruncF64S),

            (GeckTy::Int, GeckTy::Float) => Some(Instruction::F64ConvertI32S),
            (GeckTy::Float, GeckTy::Int) => Some(Instruction::I32TruncF64S),

            (GeckTy::Ref(from_kind), GeckTy::Ref(to_kind)) => {
                if let (RefTy::BaseForm(Some(_)), RefTy::BaseForm(Some(_)))
                | (RefTy::ObjectRef(Some(_)), RefTy::ObjectRef(Some(_)))
                | (RefTy::ObjectRef(_), RefTy::BaseForm(_))
                | (RefTy::BaseForm(_), RefTy::ObjectRef(_)) = (from_kind, to_kind)
                {
                    builder.warn(CompileDiagnostic::BadRefCoersion {
                        span: expr_span,
                        from: from_kind,
                        to: to_kind,
                    })
                }

                None
            }

            _ => {
                return Err(CompileFail::BadCoersion {
                    span: expr_span,
                    from: ExprTy::Concrete(from),
                    to,
                });
            }
        },
        ExprTy::CmdReturn => match to.into_wasm() {
            WasmTy::UnsignedI32 => Some(Instruction::I32TruncF64S),
            WasmTy::SignedI32 => Some(Instruction::I32TruncF64U),
            WasmTy::F64 => None,
        },
    };

    builder.extend(inst);

    Ok(())
}

fn coerce_expr(
    expr: &Spanned<Expression>,
    to: ExprTy,
    builder: &mut CodeBuilder<'_, '_, '_>,
) -> Result<()> {
    let ty = expression(expr, Some(to), builder)?;
    coersion(expr.span, ty, to, builder)
}

fn binary_operator(
    lhs_expr: &Spanned<Expression>,
    rhs_expr: &Spanned<Expression>,
    type_hint: Option<ExprTy>,
    op: Spanned<BinaryOp>,
    builder: &mut CodeBuilder<'_, '_, '_>,
) -> Result<ExprTy> {
    // let lhs_span = lhs_expr.span;
    // let rhs_span = rhs_expr.span;

    // let (lhs, lhs_insts);
    // let (rhs, rhs_insts);

    // match builder.defer(|builder| {
    //     let ty = expression(lhs_expr, None, builder)?;
    //     Ok(ty)
    // })? {
    //     (ExprTy::CmdReturn, _) => {
    //         (rhs, rhs_insts) = builder.defer(|builder| {
    //             let ty = expression(rhs_expr, None, builder)?;
    //             Ok(ty)
    //         })?;

    //         (lhs, lhs_insts) = builder.defer(|builder| {
    //             let ty = expression(lhs_expr, Some(rhs), builder)?;
    //             Ok(ty)
    //         })?;
    //     }
    //     lhs_both => {
    //         (lhs, lhs_insts) = lhs_both;
    //         (rhs, rhs_insts) = builder.defer(|builder| {
    //             let ty = expression(rhs_expr, Some(lhs), builder)?;
    //             Ok(ty)
    //         })?
    //     }
    // }

    // let mut lhs_map = Vec::new();
    // let mut rhs_map = Vec::new();

    // macro_rules! matching_operands {
    //     ($(try $pre_pat:pat => $pre_expr:expr;)? if $comp:expr; $($pat:pat $(=> $dsg:expr)?),* $(,)?) => {
    //         'ok: {
    //             'err: {
    //                 #[allow(unreachable_patterns)]
    //                 let mut lhs_ = match lhs {
    //                     $($pre_pat => $pre_expr,)?
    //                     ExprTy::Concrete(ty) => ty,
    //                     $($pre_pat => $pre_expr,)?
    //                     ExprTy::CmdReturn => break 'err,
    //                 };

    //                 #[allow(unreachable_patterns)]
    //                 let mut rhs_ = match rhs {
    //                     $($pre_pat => $pre_expr,)?
    //                     ExprTy::Concrete(ty) => ty,
    //                     $($pre_pat => $pre_expr,)?
    //                     ExprTy::CmdReturn => break 'err,
    //                 };

    //                 loop {
    //                     if $comp(lhs_, rhs_) {
    //                         break 'ok lhs_;
    //                     }

    //                     #[allow(path_statements, unreachable_patterns, clippy::redundant_pattern)]
    //                     let lhs_new = match lhs_ {
    //                         $(ty @ $pat => {
    //                             ty
    //                             $(;$dsg)?
    //                         })*
    //                         _ => break 'err,
    //                     };

    //                     #[allow(path_statements, unreachable_patterns, clippy::redundant_pattern)]
    //                     let rhs_new = match rhs_ {
    //                         $(ty @ $pat => {
    //                             ty
    //                             $(;$dsg)?
    //                         })*
    //                         _ => break 'err,
    //                     };

    //                     if lhs_new == lhs_ && rhs_new == rhs_ {
    //                         break 'err;
    //                     }

    //                     lhs_map.extend(builder.defer(|builder| {
    //                         coersion(
    //                             lhs_span,
    //                             ExprTy::Concrete(lhs_),
    //                             ExprTy::Concrete(lhs_new),
    //                             builder
    //                         )
    //                     })?.1);

    //                     rhs_map.extend(builder.defer(|builder| {
    //                         coersion(
    //                             rhs_span,
    //                             ExprTy::Concrete(rhs_),
    //                             ExprTy::Concrete(rhs_new),
    //                             builder
    //                         )
    //                     })?.1);

    //                     lhs_ = lhs_new;
    //                     rhs_ = rhs_new;
    //                 }
    //             }

    //             return Err(CompileFail::BinaryTypeMismatch { lhs, lhs_span, rhs, rhs_span, op: op.span });
    //         }
    //     };

    //     (short_circuit) => {
    //         if let Some(ExprTy::Concrete(GeckTy::Bool)) = type_hint {
    //             matching_operands! {
    //                 try ExprTy::CmdReturn => GeckTy::Float;
    //                 if GeckTy::is_eq;
    //                 _ => GeckTy::Bool,
    //             }
    //         } else {
    //             matching_operands! {
    //                 try ExprTy::CmdReturn => GeckTy::Float;
    //                 if GeckTy::is_eq;
    //                 GeckTy::Int => GeckTy::Float,
    //                 _,
    //             }
    //         }
    //     };

    //     (equality) => {
    //         matching_operands! {
    //             try ExprTy::CmdReturn => GeckTy::Float;
    //             if GeckTy::is_comparable_with;
    //             GeckTy::Int => GeckTy::Float,
    //             GeckTy::Float, GeckTy::Ref(_), GeckTy::String,
    //         }
    //     };

    //     (comparison) => {
    //         matching_operands! {
    //             try ExprTy::CmdReturn => GeckTy::Float;
    //             if GeckTy::is_comparable_with;
    //             GeckTy::Int => GeckTy::Float,
    //             GeckTy::Float,
    //         }
    //     };

    //     (arithmetic) => {
    //         matching_operands! {
    //             try ExprTy::CmdReturn => GeckTy::Float;
    //             if GeckTy::is_arithmetic_with;
    //             GeckTy::Int => GeckTy::Float,
    //             GeckTy::Float,
    //         }
    //     };
    // }

    // let (insts, result) = 'insts: {
    //     let (inst, result) = match *op {
    //         BinaryOp::Or => {
    //             let ty = matching_operands!(short_circuit);

    //             let juggle_idx = builder.scope.juggler(ty.into(), 0);
    //             lhs_map.push(Instruction::LocalTee(juggle_idx));
    //             lhs_map.extend(
    //                 builder
    //                     .defer(|builder| {
    //                         coersion(
    //                             lhs_span,
    //                             ExprTy::Concrete(ty),
    //                             ExprTy::Concrete(GeckTy::Bool),
    //                             builder,
    //                         )
    //                     })?
    //                     .1,
    //             );
    //             lhs_map.extend([
    //                 Instruction::If(BlockType::Result(ty.into())),
    //                 Instruction::LocalGet(juggle_idx),
    //                 Instruction::Else,
    //             ]);

    //             rhs_map.push(Instruction::End);
    //             break 'insts (vec![], ExprTy::Concrete(ty));
    //         }
    //         BinaryOp::And => {
    //             let ty = matching_operands!(short_circuit);

    //             lhs_map.extend(
    //                 builder
    //                     .defer(|builder| {
    //                         coersion(
    //                             lhs_span,
    //                             ExprTy::Concrete(ty),
    //                             ExprTy::Concrete(GeckTy::Bool),
    //                             builder,
    //                         )
    //                     })?
    //                     .1,
    //             );
    //             lhs_map.push(Instruction::If(BlockType::Result(ty.into())));
    //             rhs_map.push(Instruction::Else);
    //             rhs_map.push(ty.expr_zero());
    //             rhs_map.push(Instruction::End);
    //             break 'insts (vec![], ExprTy::Concrete(ty));
    //         }
    //         BinaryOp::Eq => (
    //             match matching_operands!(equality).into_wasm() {
    //                 WasmTy::UnsignedI32 | WasmTy::SignedI32 => Instruction::I32Eq,
    //                 WasmTy::F64 => Instruction::F64Eq,
    //             },
    //             GeckTy::Int,
    //         ),
    //         BinaryOp::Neq => (
    //             match matching_operands!(equality).into_wasm() {
    //                 WasmTy::UnsignedI32 | WasmTy::SignedI32 => Instruction::I32Ne,
    //                 WasmTy::F64 => Instruction::F64Ne,
    //             },
    //             GeckTy::Int,
    //         ),
    //         BinaryOp::Greater => (
    //             match matching_operands!(comparison).into_wasm() {
    //                 WasmTy::UnsignedI32 => Instruction::I32GtU,
    //                 WasmTy::SignedI32 => Instruction::I32GtS,
    //                 WasmTy::F64 => Instruction::F64Gt,
    //             },
    //             GeckTy::Int,
    //         ),
    //         BinaryOp::Lesser => (
    //             match matching_operands!(comparison).into_wasm() {
    //                 WasmTy::UnsignedI32 => Instruction::I32LtU,
    //                 WasmTy::SignedI32 => Instruction::I32LtS,
    //                 WasmTy::F64 => Instruction::F64Lt,
    //             },
    //             GeckTy::Int,
    //         ),
    //         BinaryOp::GreaterOrEqual => (
    //             match matching_operands!(comparison).into_wasm() {
    //                 WasmTy::UnsignedI32 => Instruction::I32GeU,
    //                 WasmTy::SignedI32 => Instruction::I32GeS,
    //                 WasmTy::F64 => Instruction::F64Ge,
    //             },
    //             GeckTy::Int,
    //         ),
    //         BinaryOp::LesserOrEqual => (
    //             match matching_operands!(comparison).into_wasm() {
    //                 WasmTy::UnsignedI32 => Instruction::I32LeU,
    //                 WasmTy::SignedI32 => Instruction::I32LeS,
    //                 WasmTy::F64 => Instruction::F64Le,
    //             },
    //             GeckTy::Int,
    //         ),
    //         BinaryOp::Add => {
    //             let ty = matching_operands!(arithmetic);
    //             (
    //                 match ty.into_wasm() {
    //                     WasmTy::SignedI32 | WasmTy::UnsignedI32 => Instruction::I32Add,
    //                     WasmTy::F64 => Instruction::F64Add,
    //                 },
    //                 ty,
    //             )
    //         }
    //         BinaryOp::Sub => {
    //             let ty = matching_operands!(arithmetic);
    //             (
    //                 match ty.into_wasm() {
    //                     WasmTy::SignedI32 | WasmTy::UnsignedI32 => Instruction::I32Sub,
    //                     WasmTy::F64 => Instruction::F64Sub,
    //                 },
    //                 ty,
    //             )
    //         }
    //         BinaryOp::Mul => {
    //             let ty = matching_operands!(arithmetic);
    //             (
    //                 match ty.into_wasm() {
    //                     WasmTy::SignedI32 | WasmTy::UnsignedI32 => Instruction::I32Mul,
    //                     WasmTy::F64 => Instruction::F64Mul,
    //                 },
    //                 ty,
    //             )
    //         }
    //         BinaryOp::Div => {
    //             let ty = matching_operands!(arithmetic);
    //             (
    //                 match ty.into_wasm() {
    //                     WasmTy::SignedI32 => Instruction::I32DivS,
    //                     WasmTy::UnsignedI32 => Instruction::I32DivU,
    //                     WasmTy::F64 => Instruction::F64Mul,
    //                 },
    //                 ty,
    //             )
    //         }
    //         BinaryOp::Mod => {
    //             let ty = matching_operands!(arithmetic);
    //             (
    //                 match ty.into_wasm() {
    //                     WasmTy::SignedI32 => Instruction::I32RemS,
    //                     WasmTy::UnsignedI32 => Instruction::I32RemU,
    //                     WasmTy::F64 => {
    //                         let lhs_var = builder.scope.juggler(ty.into(), 0);
    //                         let rhs_var = builder.scope.juggler(ty.into(), 1);

    //                         let insts = vec![
    //                             Instruction::LocalSet(rhs_var),
    //                             Instruction::LocalTee(lhs_var),
    //                             Instruction::LocalGet(lhs_var),
    //                             Instruction::LocalGet(rhs_var),
    //                             Instruction::F64Div,
    //                             Instruction::F64Floor,
    //                             Instruction::LocalGet(rhs_var),
    //                             Instruction::F64Mul,
    //                             Instruction::F64Sub,
    //                         ];

    //                         break 'insts (insts, ExprTy::Concrete(ty));
    //                     }
    //                 },
    //                 ty,
    //             )
    //         }
    //         BinaryOp::LeftShift => {
    //             matching_operands! {
    //                 if GeckTy::is_eq;
    //                 GeckTy::Int,
    //             };
    //             (Instruction::I32Shl, GeckTy::Int)
    //         }
    //         BinaryOp::RightShift => {
    //             matching_operands! {
    //                 if GeckTy::is_eq;
    //                 GeckTy::Int,
    //             };
    //             (Instruction::I32ShrS, GeckTy::Int)
    //         }
    //         BinaryOp::BitAnd => {
    //             matching_operands! {
    //                 if GeckTy::is_eq;
    //                 GeckTy::Float => GeckTy::Int,
    //                 GeckTy::Int,
    //             };
    //             (Instruction::I32And, GeckTy::Int)
    //         }
    //         BinaryOp::BitOr => {
    //             matching_operands! {
    //                 if GeckTy::is_eq;
    //                 GeckTy::Float => GeckTy::Int,
    //                 GeckTy::Int,
    //             };
    //             (Instruction::I32Or, GeckTy::Int)
    //         }
    //         BinaryOp::Pow => todo!(),
    //     };
    //     (vec![inst], ExprTy::Concrete(result))

    // };

    // builder.extend(lhs_insts);
    // builder.extend(lhs_map);

    // builder.extend(rhs_insts);
    // builder.extend(rhs_map);

    // builder.extend(insts);

    // Ok(result)
    todo!()
}

fn unary_operator(
    inner: &Spanned<Expression>,
    op: Spanned<UnaryOp>,
    builder: &mut CodeBuilder<'_, '_, '_>,
) -> Result<GeckTy> {
    let inner_ty = expression(inner, None, builder)?;
    let ty = if let ExprTy::Concrete(ty) = inner_ty {
        ty
    } else {
        coersion(
            inner.span,
            inner_ty,
            ExprTy::Concrete(GeckTy::Float),
            builder,
        )?;
        GeckTy::Float
    };
    match *op {
        UnaryOp::Neg => {
            match ty.number_or_else(|| CompileFail::UnaryTypeMismatch {
                inner: inner_ty,
                span: inner.span,
                op: op.span,
            })? {
                WasmTy::SignedI32 => {
                    builder.instruction(Instruction::I32Const(i32::MIN));
                    builder.instruction(Instruction::I32Xor);
                    Ok(ty)
                }
                WasmTy::UnsignedI32 => Err(CompileFail::UnaryTypeMismatch {
                    inner: inner_ty,
                    span: inner.span,
                    op: op.span,
                }),
                WasmTy::F64 => {
                    builder.instruction(Instruction::F64Neg);
                    Ok(ty)
                }
            }
        }
        UnaryOp::ToString => todo!(),
        UnaryOp::ToNumber => todo!(),
        UnaryOp::Deref => todo!(),
        UnaryOp::Box => todo!(),
        UnaryOp::Not => {
            #[allow(clippy::single_match)]
            match ty.number_or_else(|| CompileFail::UnaryTypeMismatch {
                inner: inner_ty,
                span: inner.span,
                op: op.span,
            })? {
                WasmTy::F64 => {
                    builder.instruction(Instruction::F64Trunc);
                }
                _ => (),
            };
            builder.instruction(Instruction::I32Eqz);
            Ok(GeckTy::Int)
        }
    }
}

fn constant(
    value: &Spanned<PrimitiveValue>,
    type_hint: Option<ExprTy>,
    builder: &mut CodeBuilder<'_, '_, '_>,
) -> Result<ExprTy> {
    match (&value.inner, type_hint) {
        (
            PrimitiveValue::Int(0) | PrimitiveValue::NullReference,
            Some(ty @ ExprTy::Concrete(GeckTy::Ref(_))),
        ) => {
            builder.instruction(NULL_REF);
            Ok(ty)
        }
        (PrimitiveValue::NullReference, None) => {
            builder.instruction(NULL_REF);
            Ok(ExprTy::Concrete(GeckTy::Ref()))
        }
        (PrimitiveValue::Int(num), Some(ty @ ExprTy::Concrete(GeckTy::String))) => {
            let str_idx = builder.data.string(num.to_string());
            builder.instruction(Instruction::I32Const(str_idx as i32));
            Ok(ty)
        }
        (PrimitiveValue::Float(value), Some(ty @ ExprTy::Concrete(GeckTy::String))) => {
            let str_idx = builder.data.string(value.to_string());
            builder.instruction(Instruction::I32Const(str_idx as i32));
            Ok(ty)
        }
        (Literal::Int { value, .. }, _) => {
            builder.instruction(Instruction::I32Const(*value));
            Ok(ExprTy::Concrete(GeckTy::Int))
        }
        (Literal::Float(value), _) => {
            builder.instruction(Instruction::F64Const(*value));
            Ok(ExprTy::Concrete(GeckTy::Float))
        }
        (Literal::String(string), Some(ExprTy::Concrete(GeckTy::Ref(ty)))) => {
            let (formid, _) = builder.context.form(
                type_hint,
                Spanned {
                    inner: string,
                    span: lit.span,
                },
            )?;
            builder.instruction(Instruction::I32Const(formid.0 as i32));
            Ok(ExprTy::Concrete(GeckTy::Ref(ty)))
        }
        (Literal::String(string), _) => {
            let str_idx = builder.data.string(string.to_string());
            builder.instruction(Instruction::I32Const(str_idx as i32));
            Ok(ExprTy::Concrete(GeckTy::String(StringTy::Literal)))
        }
    }
}

fn expression(
    expr: &Expression,
    type_hint: Option<ExprTy>,
    builder: &mut CodeBuilder<'_, '_, '_>,
) -> Result<ExprTy> {
    let ty = match expr {
        Expression::Constant(lit) => constant(lit, type_hint, builder)?,
        Expression::Invoke(invocation) => invoke(invocation, type_hint, builder)?,
        Expression::Group(group) => expression(group, type_hint, builder)?,
        Expression::BinaryOp { operator, lhs, rhs } => {
            binary_operator(lhs, rhs, type_hint, *operator, builder)?
        }
        Expression::UnaryOp { operator, inner } => {
            unary_operator(inner, *operator, builder).map(ExprTy::Concrete)?
        }
    };

    Ok(ty)
}

fn property_path_segment(
    referent_ty: Spanned<ExprTy>,
    segment: Spanned<&str>,
    builder: &mut CodeBuilder<'_, '_, '_>,
) -> Result<ExprTy> {
    if builder.context.has_command(&segment) {
        coersion(
            referent_ty.span,
            *referent_ty,
            ExprTy::Concrete(GeckTy::Ref(RefTy::Unknown)),
            builder,
        )?;
        call_command(segment.span, segment, &[], None, builder)
    } else {
        let func_idx = builder.data.funcs.import(
            Import {
                namespace: ImportNamespace::Runtime,
                name: Cow::Borrowed("get_ref_local"),
            },
            WasmTypeDef {
                params: vec![
                    GeckTy::Ref(RefTy::ObjectRef(None)).into_wasm().into_val(),
                    GeckTy::String(StringTy::Var).into_wasm().into_val(),
                ],
                results: vec![ExprTy::CmdReturn.into()],
            },
        );

        coersion(
            segment.span,
            *referent_ty,
            ExprTy::Concrete(GeckTy::Ref(RefTy::ObjectRef(None))),
            builder,
        )?;

        let str_idx = builder.data.string(segment.inner.to_string());
        builder.instruction(Instruction::I32Const(str_idx as i32));
        builder.instruction(Instruction::Call(func_idx));

        Ok(ExprTy::CmdReturn)
    }
}

fn property_first(
    first: Spanned<&str>,
    type_hint: Option<ExprTy>,
    builder: &mut CodeBuilder<'_, '_, '_>,
) -> Result<ExprTy> {
    if let Some(global) = builder
        .context
        .defs
        .absolute_constants
        .get(&Ascii::new(*first))
    {
        builder.instruction((*global).into());
        Ok((*global).into())
    } else if let Ok(var) = builder.scope.local(first) {
        builder.instruction(var.instruction_get());
        Ok(ExprTy::Concrete(var.ty.into()))
    } else if builder.context.has_form(&first) {
        let (formid, ty) = builder.context.form(type_hint, first)?;
        builder.instruction(Instruction::I32Const(formid.0 as i32));

        if let FormTy::Global(ty) = ty {
            let func_idx = builder.data.funcs.import(
                Import {
                    namespace: ImportNamespace::Runtime,
                    name: Cow::Owned(format!("get_global_{}", ty.as_ident())),
                },
                WasmTypeDef {
                    params: vec![ValType::I32],
                    results: vec![ty.into()],
                },
            );
            builder.instruction(Instruction::Call(func_idx));
        }

        Ok(ty.into())
    } else if builder.context.has_command(&first) {
        builder.instruction(NULL_REF);
        call_command(first.span, first, &[], type_hint, builder)
    } else {
        Err(CompileFail::Undefined {
            span: first.span,
            kind: NotFound::Unknown,
        })
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Constant {
    Int(i32),
    Ref(u32),
}

impl From<Constant> for ExprTy {
    fn from(value: Constant) -> Self {
        match value {
            Constant::Int(_) => ExprTy::Concrete(GeckTy::Int),
            Constant::Ref(_) => ExprTy::Concrete(GeckTy::Ref(RefTy::Unknown)),
        }
    }
}

impl From<Constant> for Instruction<'static> {
    fn from(value: Constant) -> Self {
        match value {
            Constant::Ref(int) => Instruction::I32Const(int as i32),
            Constant::Int(int) => Instruction::I32Const(int),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ParamLiteral {
    pub canon: &'static str,
    pub value: Constant,
}

#[derive(Debug, Clone, Copy, Error)]
pub enum ParamTy<'a> {
    Expr(ExprTy),
    FormatArg,
    VarName,
    Union(&'a ParamUnion<'a>),
}

impl<'a> fmt::Display for ParamTy<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParamTy::Expr(expr) => fmt::Display::fmt(expr, f),
            ParamTy::VarName => write!(f, "variable"),
            ParamTy::FormatArg => write!(f, "format_arg"),
            ParamTy::Union(union) => {
                for (i, key) in union.keys().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{key}")?;
                }
                Ok(())
            }
        }
    }
}

// fn operation(op: &Spanned<Operation>)

fn invoke_arg(
    ty: ParamTy,
    arg: &Spanned<InvokeArg>,
    builder: &mut CodeBuilder<'_, '_, '_>,
) -> Result<()> {
    fn as_expr(
        expected_ty: ExprTy,
        arg: &Spanned<InvokeArg>,
        builder: &mut CodeBuilder<'_, '_, '_>,
    ) -> Result<()> {
        let from = match &arg.inner {
            InvokeArg::Constant(lit) => constant(lit, Some(expected_ty), builder)?,
            InvokeArg::Var(prop) => {
                if let ExprTy::Concrete(GeckTy::String(_)) = expected_ty {
                    if prop.path.is_empty() {
                        let str_idx = builder.data.string(prop.referent.inner.to_string());
                        builder.instruction(Instruction::I32Const(str_idx as i32));
                        return Ok(());
                    }
                }

                property_get(prop, Some(expected_ty), builder)?
            }
            InvokeArg::UnaryOp { operator, inner } => {
                unary_operator(inner, *operator, builder).map(ExprTy::Concrete)?
            }
            InvokeArg::Inline(expr) => expression(expr, Some(expected_ty), builder)?,
        };

        coersion(arg.span, from, expected_ty, builder)?;

        Ok(())
    }

    match ty {
        ParamTy::Expr(expected_ty) => {
            as_expr(expected_ty, arg, builder)?;
        }
        ParamTy::FormatArg => {
            as_expr(
                ExprTy::Concrete(GeckTy::String(StringTy::Unknown)),
                arg,
                builder,
            )?;
        }
        ParamTy::VarName => match &arg.inner {
            InvokeArg::Var(prop) if prop.path.is_empty() => {
                let var = builder.scope.local(prop.referent.as_deref())?;
                builder.instruction(var.instruction_get());
                coersion(arg.span, var.ty.into(), ExprTy::CmdReturn, builder)?;
            }
            _ => {
                return Err(CompileFail::InvalidParam {
                    span: arg.span,
                    param: ty.to_string(),
                })
            }
        },
        ParamTy::Union(map) => match &arg.inner {
            InvokeArg::Var(prop) if prop.path.is_empty() => {
                let constant = map.get(&Ascii::new(*prop.referent.as_deref())).ok_or(
                    CompileFail::InvalidParam {
                        span: arg.span,
                        param: ty.to_string(),
                    },
                )?;
                builder.instruction((*constant).into());
            }
            _ => {
                return Err(CompileFail::InvalidParam {
                    span: arg.span,
                    param: ty.to_string(),
                })
            }
        },
    }

    Ok(())
}

fn call_command(
    span: Span,
    name: Spanned<&str>,
    args: &[Spanned<InvokeArg>],
    type_hint: Option<ExprTy>,
    builder: &mut CodeBuilder<'_, '_, '_>,
) -> Result<ExprTy> {
    let (command, mut params) = builder.context.command(name)?;

    let mut param_val_tys = Vec::with_capacity(args.len() + 1);
    param_val_tys.push(GeckTy::Ref(RefTy::ObjectRef(None)).into_wasm().into_val());

    let mut args_iter = args.iter();
    let mut i = 0;
    loop {
        i += 1;
        match (args_iter.next(), params.next()) {
            (_, None) => break,
            (None, Some((_, optional))) => {
                let last_span = args.last().map(|arg| arg.span).unwrap_or(name.span);
                let missing_span = (last_span.end..last_span.end + 1).into();
                if optional {
                    break;
                } else {
                    return Err(CompileFail::TooFewArguments {
                        span: missing_span,
                        count: command.params.iter().filter(|x| !x.optional).count(),
                    });
                }
            }
            (Some(arg), Some((ty, _))) => {
                invoke_arg(ty, arg, builder)?;
                param_val_tys.push(ty.into());
            }
        }
    }

    let func_idx = builder.data.funcs.import(
        Import {
            namespace: ImportNamespace::Commands,
            name: Cow::Owned(format!("{}_{}", command.long_name, i - 1)),
        },
        WasmTypeDef {
            params: param_val_tys,
            results: vec![ValType::F64],
        },
    );

    builder.instruction(Instruction::Call(func_idx));

    let hint = match type_hint {
        Some(hint) => {
            coersion(span, ExprTy::CmdReturn, hint, builder)?;
            hint
        }
        None => ExprTy::CmdReturn,
    };

    Ok(hint)
}

fn property_get_truncated<'a>(
    prop: &'a Property,
    builder: &mut CodeBuilder<'_, '_, '_>,
) -> Option<Result<(Spanned<ExprTy>, Spanned<&'a str>)>> {
    prop.path.split_last().map(|(last, args)| {
        let mut last_ty = Spanned {
            inner: property_first(prop.referent.as_deref(), None, builder)?,
            span: prop.referent.span,
        };
        for segment in args {
            last_ty = Spanned {
                inner: property_path_segment(last_ty, segment.as_deref(), builder)?,
                span: Span::between(last_ty.span, segment.span),
            };
        }
        Ok((last_ty, last.as_deref()))
    })
}

fn property_get(
    prop: &Property,
    type_hint: Option<ExprTy>,
    builder: &mut CodeBuilder<'_, '_, '_>,
) -> Result<ExprTy> {
    match property_get_truncated(prop, builder) {
        Some(Err(e)) => Err(e),
        None => {
            if let Some(ty @ ExprTy::Concrete(GeckTy::String(_))) = type_hint {
                if prop.path.is_empty() {
                    builder.data.string(prop.referent.inner.to_string());
                    return Ok(ty);
                }
            }
            property_first(prop.referent.as_deref(), type_hint, builder)
        }
        Some(Ok((ref_ty, last))) => {
            let ty = property_path_segment(ref_ty, last, builder)?;

            if let Some(hint) = type_hint {
                coersion(ref_ty.span, ty, hint, builder)?;
                Ok(hint)
            } else {
                Ok(ty)
            }
        }
    }
}

fn invoke(
    invoke: &Spanned<Invoke>,
    type_hint: Option<ExprTy>,
    builder: &mut CodeBuilder<'_, '_, '_>,
) -> Result<ExprTy> {
    if invoke.args.is_empty() {
        property_get(&invoke.name, type_hint, builder)
    } else {
        let (command_name, insts) =
            builder.defer(
                |builder| match property_get_truncated(&invoke.name, builder) {
                    Some(Err(e)) => Err(e),
                    None => {
                        builder.instruction(NULL_REF);
                        Ok(invoke.name.referent.as_deref())
                    }
                    Some(Ok((ty, last))) => {
                        coersion(
                            ty.span,
                            *ty,
                            ExprTy::Concrete(GeckTy::Ref(RefTy::Unknown)),
                            builder,
                        )?;
                        Ok(last)
                    }
                },
            )?;

        let (can_command, remainder_args) =
            if let Ok((_, params)) = builder.context.command(command_name) {
                if let Some(remainder) = invoke.args.get(params.count()..) {
                    (true, remainder)
                } else {
                    (true, &[][..])
                }
            } else {
                (false, &invoke.args[..])
            };

        if remainder_args.is_empty() {
            if can_command {
                builder.extend(insts);
                return call_command(invoke.span, command_name, &invoke.args, type_hint, builder);
            } else {
                return Err(CompileFail::Undefined {
                    span: invoke.span,
                    kind: NotFound::Unknown,
                });
            }
        }

        // Annoyingly, there is no difference between the following syntaxes and we need to try both.
        // This unfortunately means we need to recompute binary op order as `&`, `-`, `*` are all ambiguous.
        // 1. MyCommand -1 => MyCommand (-1)
        // 2. MyVar -1 => (MyVar) - (1)

        fn climb_precedence(
            mut lhs: Spanned<Expression>,
            min_precedence: usize,
            remaining: &mut &[(Spanned<BinaryOp>, &Spanned<Expression>)],
        ) -> Spanned<Expression> {
            loop {
                let (op, mut rhs) = match remaining.split_first() {
                    Some(((op, rhs), rest)) if op.precedence() >= min_precedence => {
                        *remaining = rest;
                        (op, (*rhs).clone())
                    }
                    _ => break,
                };

                loop {
                    match remaining.get(0) {
                        Some((new_op, _)) if new_op.precedence() > op.precedence() => (),
                        _ => break,
                    };

                    rhs = climb_precedence(rhs, op.precedence() + 1, &mut *remaining);
                }

                lhs = Spanned {
                    span: Span::between(lhs.span, rhs.span),
                    inner: Expression::BinaryOp {
                        operator: *op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                }
            }

            lhs
        }

        let mut ops = Vec::with_capacity(remainder_args.len());
        for arg in remainder_args {
            if let InvokeArg::UnaryOp { operator, inner } = &arg.inner {
                let op = match **operator {
                    UnaryOp::Neg => Some(BinaryOp::Sub),
                    UnaryOp::Deref => Some(BinaryOp::Mul),
                    UnaryOp::Box => Some(BinaryOp::BitAnd),
                    _ => None,
                };

                if let Some(op) = op {
                    ops.push((
                        Spanned {
                            inner: op,
                            span: operator.span,
                        },
                        inner,
                    ));
                    continue;
                }
            }

            builder.warn(CompileDiagnostic::TooManyArguments {
                span: invoke.span,
                count: invoke.args.len() - remainder_args.len(),
            });
            ops.clear();
            break;
        }

        let truncated_invoke = Invoke {
            args: invoke.args[..invoke.args.len() - remainder_args.len()].to_vec(),
            name: invoke.name.clone(),
        };

        // todo: correct span information
        let truncated_invoke = Spanned {
            inner: truncated_invoke,
            span: invoke.span,
        };

        let combined_expr = climb_precedence(
            Spanned {
                inner: Expression::Invoke(truncated_invoke),
                span: invoke.span,
            },
            0,
            &mut &ops[..],
        );

        expression(&combined_expr, type_hint, builder)
    }
}

fn statement<'a>(stmt: &'a Statement, builder: &mut CodeBuilder<'a, '_, '_>) -> Result<()> {
    fn set_property(
        var: &Property,
        value: &Spanned<Expression>,
        builder: &mut CodeBuilder<'_, '_, '_>,
    ) -> Result<()> {
        if let Some(result) = property_get_truncated(var, builder) {
            let (_, last) = result?;

            let set_idx = {
                builder.data.funcs.import(
                    Import {
                        namespace: ImportNamespace::Runtime,
                        name: Cow::Borrowed("set_ref_local"),
                    },
                    WasmTypeDef {
                        params: vec![
                            GeckTy::Ref(RefTy::ObjectRef(None)).into(),
                            GeckTy::String(StringTy::Unknown).into(),
                            ExprTy::CmdReturn.into(),
                        ],
                        results: vec![],
                    },
                )
            };

            let str_idx = builder.data.string(last.inner.to_string());
            builder.instruction(Instruction::I32Const(str_idx as i32));

            coerce_expr(value, ExprTy::CmdReturn, builder)?;

            builder.instruction(Instruction::Call(set_idx));
        } else if let Ok(local) = builder.scope.local(var.referent.as_deref()) {
            coerce_expr(value, local.ty.into(), builder)?;
            builder.instruction(local.instruction_set());
        } else if builder.context.has_form(&var.referent) {
            let (formid, ty) = builder.context.form(None, var.referent.as_deref())?;
            builder.instruction(Instruction::I32Const(formid.0 as i32));

            if let FormTy::Global(ty) = ty {
                let func_idx = builder.data.funcs.import(
                    Import {
                        namespace: ImportNamespace::Runtime,
                        name: Cow::Owned(format!("set_global_{}", ty.as_ident())),
                    },
                    WasmTypeDef {
                        params: vec![ValType::I32, ty.into()],
                        results: vec![],
                    },
                );

                coerce_expr(value, ExprTy::Concrete(ty), builder)?;
                builder.instruction(Instruction::Call(func_idx));
            }
        } else {
            return Err(CompileFail::Undefined {
                span: var.referent.span,
                kind: NotFound::Unknown,
            });
        }

        Ok(())
    }

    match stmt {
        Statement::IfTree { ifs, else_ } => {
            assert!(!ifs.is_empty(), "should have at least one if branch!");

            for (i, branch) in ifs.iter().enumerate() {
                if i > 0 {
                    builder.instruction(Instruction::Else);
                }

                coerce_expr(&branch.condition, ExprTy::Concrete(GeckTy::Bool), builder)?;
                builder.instruction(Instruction::If(BlockType::Empty));

                block(&branch.code, builder)?;
            }

            if let Some(else_) = else_ {
                builder.instruction(Instruction::Else);
                block(else_, builder)?;
            }

            for _ in ifs {
                builder.instruction(Instruction::End);
            }
        }
        Statement::Expression(invocation) => {
            expression(invocation, None, builder)?;
            builder.instruction(Instruction::Drop);
        }
        Statement::SetVariable { var, to } => {
            set_property(var, to, builder)?;
        }
        Statement::Let {
            assignment,
            var,
            value,
        } => {
            if let Some(operator) = *assignment {
                set_property(
                    var,
                    &Spanned {
                        inner: Expression::BinaryOp {
                            operator,
                            lhs: Box::new(Spanned {
                                inner: Expression::Invoke(Spanned {
                                    inner: Invoke {
                                        name: var.clone(),
                                        args: Vec::new(),
                                    },
                                    span: var.referent.span,
                                }),
                                span: var.referent.span,
                            }),
                            rhs: Box::new(value.clone()),
                        },
                        span: var.referent.span,
                    },
                    builder,
                )?;
            } else {
                set_property(var, value, builder)?;
            };
            todo!()
        }
        Statement::Variables(vars) => {
            for local in &vars.names {
                builder.insert_local(local.as_deref(), vars.ty)?;
            }
        }
        Statement::Return => builder.instruction(Instruction::Return),
    }

    Ok(())
}

fn block<'a>(block: &'a Block, builder: &mut CodeBuilder<'a, '_, '_>) -> Result<()> {
    for stmt in &block.0 {
        statement(stmt, builder)?;
    }

    Ok(())
}

fn blocktype_code<'a>(
    blocktype: &'a Blocktype,
    param_ty: Option<&GeckTy>,
    builder: &mut CodeBuilder<'a, '_, '_>,
) -> Result<()> {
    if let Some(args) = &blocktype.args {
        match (&args.inner, param_ty) {
            (_, None) => return block(&blocktype.code, builder),
            (Args::Const(i), Some(GeckTy::Int)) => {
                builder.instruction(Instruction::I32Const(*i));
            }
            (Args::Const(_), Some(_)) => return Err(CompileFail::NoConst { span: args.span }),
            (Args::Var(var), Some(&param_ty)) => {
                let ty = property_first(
                    var.referent.as_deref(),
                    Some(ExprTy::Concrete(param_ty)),
                    builder,
                )?;
                coersion(args.span, ty, ExprTy::Concrete(param_ty), builder)?;
            }
            (Args::Parameters(_), _) => todo!(),
        }

        builder.instruction(Instruction::LocalGet(0));
        builder.instruction(Instruction::I32Eq);
        builder.instruction(Instruction::If(BlockType::Empty));
        block(&blocktype.code, builder)?;
        builder.instruction(Instruction::End);
    } else {
        block(&blocktype.code, builder)?;
    }

    Ok(())
}

pub struct CompileOutput {
    pub module: Module,
    pub diagnostics: Vec<CompileDiagnostic>,
}

pub fn generate_wasm_for_script<'a>(
    script: &'a Script,
    context: &'a Context,
) -> Result<CompileOutput> {
    let mut diagnostics = Vec::new();

    let mut globals = VariableStore::default();

    let mut data = WasmData::default();

    let mut vars = Vec::new();
    let mut blocktypes: HashMap<Ascii<&str>, (Span, Vec<&Blocktype>)> = HashMap::new();

    for item in &script.items {
        match item {
            Item::Blocktype(blocktype) => blocktypes
                .entry(Ascii::new(&blocktype.name))
                .or_insert_with(|| (blocktype.name.span, Vec::new()))
                .1
                .push(blocktype),
            Item::Variables(var) => vars.push(var),
        }
    }

    for var in vars {
        for name in &var.names {
            globals.insert(name, var.ty, WasmVarScope::Global);
        }
    }

    for (name, (span, bt_func)) in blocktypes {
        let Some((canon_name, param)) = context.defs.blocktypes.get_key_value(&Ascii::new(*name))
        else {
            return Err(CompileFail::UnknownBlocktype { span });
        };

        let mut locals_used = 0;

        let export_name = format!("blocktype:{}", canon_name);

        data.exports.export(
            &export_name,
            ExportKind::Func,
            data.funcs.defined.len() as u32,
        );

        let (type_def, param_ty) = match param {
            BlocktypeParam::None => (WasmTypeDef::default(), None),
            BlocktypeParam::Maybe(param) | BlocktypeParam::One(param) => {
                locals_used += 1;
                (
                    WasmTypeDef {
                        params: vec![param.into_wasm().into_val()],
                        results: vec![],
                    },
                    Some(param),
                )
            }
        };

        data.funcs.define(Cow::Owned(export_name), type_def);

        let (mut func, mut insts) = {
            let mut scope = Scope::new_root(&globals, locals_used);
            let mut builder = CodeBuilder {
                context,
                scope: &mut scope,
                data: &mut data,
                diagnostics: &mut diagnostics,
                instructions: Vec::new(),
            };

            for blocktype in bt_func {
                if let Some(Args::Parameters(_)) = blocktype.args.as_ref().map(|args| &args.inner) {
                    todo!("UDF");
                };

                match (param, &blocktype.args) {
                    (BlocktypeParam::None, Some(args)) => {
                        builder.warn(CompileDiagnostic::TooManyArguments {
                            span: args.span,
                            count: 0,
                        });
                    }
                    (BlocktypeParam::One(_), None) => {
                        return Err(CompileFail::TooFewArguments {
                            count: 1,
                            span: blocktype.name.span,
                        })
                    }
                    _ => (),
                }

                blocktype_code(blocktype, param_ty, &mut builder)?;
            }

            builder.instruction(Instruction::End);

            (
                Function::new_with_locals_types(
                    builder
                        .scope
                        .take_definitions()
                        .into_iter()
                        .map(|var| match var {
                            VarKind::Typed(def) => {
                                (def.kind, GeckTy::from(def.ty).into_wasm().into_val())
                            }
                            VarKind::Wasm { ty, .. } => (WasmVarScope::Local, ty),
                        })
                        .filter_map(|(scope, ty)| {
                            if let WasmVarScope::Local = scope {
                                Some(ty)
                            } else {
                                None
                            }
                        }),
                ),
                builder.instructions,
            )
        };

        for inst in insts.drain(..) {
            func.instruction(&inst);
        }

        data.codes.function(&func);
    }

    let module = data.into_module(&globals)?;

    wasmparser::validate(module.as_slice()).map_err(CompileFail::InvalidWasm)?;

    #[cfg(feature = "opt")]
    println!(
        "opt: {}",
        wasmprinter::print_bytes(optimize(&script.name, &module)?)?
    );

    Ok(CompileOutput {
        module,
        diagnostics,
    })
}

#[cfg(feature = "opt")]
fn optimize(name: &str, module: &Module) -> anyhow::Result<Vec<u8>> {
    let file = format!("./.cache/{name}");
    std::fs::write(&file, module.as_slice())?;
    let opt_opts = wasm_opt::OptimizationOptions::new_opt_level_4();
    opt_opts.run(&file, &file)?;
    Ok(std::fs::read(&file)?)
}
