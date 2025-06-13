use crate::generating_function::{GenFun, GenFunKind};
use crate::id_manager::IDManager;
use crate::ir::type_check_arithmetic;
use crate::ir::{BoolExpr, CompoundDistribution, Distribution, Expr, SimpleDistribution};
use crate::lang::{Arithmetic, ProjectionExpr, Type, TypingContext};
use crate::numbers::{Exp, Number};
use crate::transformers::{Event, Projection, Transform};

use num_traits::{One, Pow, Zero};
use std::collections::HashMap;

pub static ID: IDManager = IDManager::new("_");

pub trait Semantics<T> {
    fn semantics(&self, ret: &str) -> Result<(Type, GenFun<T>), String>;
}

impl<T: Number> Semantics<T> for Expr {
    fn semantics(&self, ret: &str) -> Result<(Type, GenFun<T>), String> {
        semantics(self, ret)
    }
}

pub fn semantics<T: Number>(expr: &Expr, ret: &str) -> Result<(Type, GenFun<T>), String> {
    let mut context = HashMap::new();
    semantics_expr(&mut context, expr, ret, GenFun::one())
}

pub fn semantics_projection(
    context: &mut TypingContext,
    p: &ProjectionExpr,
) -> Result<(Type, String), String> {
    match p {
        ProjectionExpr::Var(var) => {
            let Some(var_type) = context.get(var) else {
                return Err(format!("Type error: variable {} not found", var));
            };
            Ok((var_type.clone(), var.clone()))
        }
        ProjectionExpr::Fst(sub_p) => match semantics_projection(context, sub_p)? {
            (Type::Tuple(t, _), x) => Ok((*t, x.fst())),
            _ => Err(format!(
                "Type error: `fst` requires a Tuple operand: {:?}",
                p
            )),
        },
        ProjectionExpr::Snd(sub_p) => match semantics_projection(context, sub_p)? {
            (Type::Tuple(_, t), x) => Ok((*t, x.snd())),
            _ => Err(format!(
                "Type error: `snd` requires a Tuple operand: {:?}",
                p
            )),
        },
    }
}
pub fn semantics_bool_expr<T: Number>(
    context: &mut TypingContext,
    bool_expr: &BoolExpr,
    gf: GenFun<T>,
) -> Result<(Type, Event<T>), String> {
    pub fn semantics_bool_expr_impl<T: Number>(
        context: &mut TypingContext,
        bool_expr: &BoolExpr,
        gf: GenFun<T>,
    ) -> Result<(Type, Event<T>), String> {
        match bool_expr {
            BoolExpr::BoolLiteral(true) => Ok((
                Type::Bool,
                Event {
                    pos: gf.clone(),
                    neg: GenFun::zero(),
                },
            )),
            BoolExpr::BoolLiteral(false) => Ok((
                Type::Bool,
                Event {
                    pos: GenFun::zero(),
                    neg: gf.clone(),
                },
            )),
            BoolExpr::Lookup(p) => match semantics_projection(context, p)? {
                (ty @ Type::Bool, x) | (ty @ Type::Nat, x) => Ok((Type::Bool, gf.test(&x, &ty)?)),
                _ => Err("Type error: `Lookup` should resolve to a Bool or Nat".to_string()),
            },
            BoolExpr::Test(expr) => {
                let fresh = ID.next();
                match semantics_expr(context, expr, &fresh.clone(), gf)? {
                    (Type::Bool, expr_gf) | (Type::Nat, expr_gf) => {
                        let Event { pos, neg } = expr_gf.test_fresh(&fresh);
                        Ok((Type::Bool, Event { pos, neg }))
                    }
                    (ty, _) => Err(format!(
                        "Type error: `Test` expect a Bool or Nat operand, got {:?}",
                        ty
                    )),
                }
            }
            BoolExpr::Not(sub_expr) => {
                let (Type::Bool, Event { pos, neg }) = semantics_bool_expr(context, sub_expr, gf)?
                else {
                    return Err("Type error: `Not` requires a Bool operand".to_string());
                };
                Ok((Type::Bool, Event { pos: neg, neg: pos }))
            }
            BoolExpr::And(lhs, rhs) => {
                let (Type::Bool, Event { pos: lhs_pos, .. }) =
                    semantics_bool_expr(context, lhs, gf.clone())?
                else {
                    return Err("Type error: `And` requires Bool operands".to_string());
                };
                let (Type::Bool, Event { pos: rhs_pos, .. }) =
                    semantics_bool_expr(context, rhs, lhs_pos)?
                else {
                    return Err("Type error: `And` requires Bool operands".to_string());
                };
                Ok((
                    Type::Bool,
                    Event {
                        pos: rhs_pos.clone(),
                        neg: gf.clone() - rhs_pos,
                    },
                ))
            }
        }
    }
    let r = semantics_bool_expr_impl(context, bool_expr, gf)?;
    if cfg!(debug_assertions) {
        println!("[semantics_bool_expr] {:?}:\n[semantics_bool_expr] pos:{}\n[semantics_bool_expr] neg:{}\n", bool_expr, r.1.pos, r.1.neg);
    }
    Ok(r)
}

pub fn semantics_arithmetic<T: Number>(
    context: &mut TypingContext,
    arithmetic @ Arithmetic { terms, offset }: &Arithmetic,
    ret: &str,
    gf: GenFun<T>,
) -> Result<(Type, GenFun<T>), String> {
    if let Some(e) = Option::<Expr>::from(arithmetic.clone()) {
        semantics_expr(context, &e, ret, gf)
    } else {
        match type_check_arithmetic(context, arithmetic)? {
            Type::Nat => {
                let linear_gf =
                    terms.iter().try_fold(gf.clone(), |acc, (coeff, var)| {
                        match context.get(var) {
                            Some(Type::Bool | Type::Nat) => {
                                let gf_var: GenFun<T> = var.clone().into();
                                let gf_ret: GenFun<T> = ret.to_string().into();
                                Ok(GenFunKind::Let(
                                    var.to_string(),
                                    gf_var * gf_ret.pow(coeff.unwrap_or(1)),
                                    acc,
                                )
                                .into())
                            }
                            _ => Err(format!("Type error: variable {} must be Bool or Nat", var)),
                        }
                    })?;
                let gf_var: GenFun<T> = ret.to_string().into();
                Ok((Type::Nat, gf_var.pow(offset.unwrap_or(0)) * linear_gf))
            }
            Type::Real => {
                let linear_gf =
                    terms.iter().try_fold(gf.clone(), |acc, (coeff, var)| {
                        match context.get(var) {
                            Some(Type::Bool | Type::Nat) => {
                                let gf_var: GenFun<T> = var.clone().into();
                                let gf_ret: GenFun<T> = ret.to_string().into();
                                Ok(GenFunKind::Let(
                                    var.to_string(),
                                    gf_var * (gf_ret * coeff.unwrap_or(1).into()).exp(),
                                    acc,
                                )
                                .into())
                            }
                            Some(Type::Real) => {
                                let gf_var: GenFun<T> = var.clone().into();
                                let gf_ret: GenFun<T> = ret.to_string().into();
                                Ok(GenFunKind::Let(
                                    var.to_string(),
                                    gf_var + gf_ret * coeff.unwrap_or(1).into(),
                                    acc,
                                )
                                .into())
                            }
                            _ => Err(format!(
                                "Type error: variable {} must be Bool, Nat or Real",
                                var
                            )),
                        }
                    })?;
                let gf_var: GenFun<T> = ret.to_string().into();
                let offset: GenFun<T> = offset.unwrap_or(0).into();
                Ok((Type::Real, (gf_var * offset).exp() * linear_gf))
            }
            _ => Err("Type error: arithmetic expression must be Nat or Real".to_string()),
        }
    }
}

pub fn semantics_expr<T: Number>(
    context: &mut TypingContext,
    expr: &Expr,
    ret: &str,
    gf: GenFun<T>,
) -> Result<(Type, GenFun<T>), String> {
    pub fn semantics_expr_imp<T: Number>(
        context: &mut TypingContext,
        expr: &Expr,
        ret: &str,
        gf: GenFun<T>,
    ) -> Result<(Type, GenFun<T>), String> {
        let gf = if context.get(ret).is_none() {
            Ok(gf)
        } else {
            Err(format!("Type error: variable {} already defined", ret))
        }?;
        match expr {
            Expr::Annotated(e, _) => semantics_expr(context, e, ret, gf),
            Expr::Sample(
                dist @ Distribution::Simple(
                    SimpleDistribution::Dirac(0)
                    | SimpleDistribution::Dirac(1)
                    | SimpleDistribution::Bernoulli(_)
                    | SimpleDistribution::Biased(..),
                ),
            ) => Ok((Type::Bool, gf.extend(context, ret, dist)?)),
            Expr::Sample(
                dist @ Distribution::Simple(
                    SimpleDistribution::Geometric(..)
                    | SimpleDistribution::Poisson(_)
                    | SimpleDistribution::Binomial(..)
                    | SimpleDistribution::NegBinomial(..)
                    | SimpleDistribution::Dirac(..),
                ),
            ) => Ok((Type::Nat, gf.extend(context, ret, dist)?)),
            Expr::Sample(
                dist @ Distribution::Simple(
                    SimpleDistribution::Exponential(..) | SimpleDistribution::Gamma(..),
                ),
            ) => Ok((Type::Real, gf.extend(context, ret, dist)?)),
            Expr::Sample(dist @ Distribution::Simple(SimpleDistribution::Uniform(a, b)))
                if a.is_zero() && b.is_one() =>
            {
                Ok((Type::UnitInterval, gf.extend(context, ret, dist)?))
            }
            Expr::Sample(dist @ Distribution::Simple(SimpleDistribution::Uniform(..))) => {
                Ok((Type::Real, gf.extend(context, ret, dist)?))
            }
            Expr::Sample(dist @ Distribution::Compound(CompoundDistribution::Bernoulli(_))) => {
                Ok((Type::Bool, gf.extend(context, ret, dist)?))
            }
            Expr::Sample(dist @ Distribution::Compound(CompoundDistribution::Poisson(..))) => {
                Ok((Type::Nat, gf.extend(context, ret, dist)?))
            }
            Expr::Observe(b) => {
                let (Type::Bool, Event { pos, .. }) = semantics_bool_expr(context, b, gf)? else {
                    return Err("Type error: `Observe` requires a Bool operand".to_string());
                };
                Ok((Type::Bool, pos))
            }
            Expr::Var(var) => {
                let Some(var_type) = context.get(var) else {
                    return Err(format!("Type error: variable {} not found", var));
                };
                Ok((var_type.clone(), gf.copy(var, ret, var_type)?))
            }
            Expr::Const(c) => Ok((
                if c <= &1 { Type::Bool } else { Type::Nat },
                gf.extend(
                    context,
                    ret,
                    &Distribution::Simple(SimpleDistribution::Dirac(*c)),
                )?,
            )),
            Expr::Fst(e) => {
                let fresh = ID.next();
                let (e_type, e_gf) = semantics_expr(context, e, &fresh, gf)?;
                match e_type {
                    Type::Tuple(ref t, _) => Ok((
                        *t.clone(),
                        e_gf.copy(&fresh.fst(), ret, t)?
                            .marginalize(&fresh, &e_type),
                    )),
                    _ => Err(format!(
                        "Type error: `fst` requires a Tuple operand: {:?}",
                        expr
                    )),
                }
            }
            Expr::Snd(e) => {
                let fresh = ID.next();
                let (e_type, e_gf) = semantics_expr(context, e, &fresh, gf)?;
                match e_type {
                    Type::Tuple(_, ref t) => Ok((
                        *t.clone(),
                        e_gf.copy(&fresh.snd(), ret, t)?
                            .marginalize(&fresh, &e_type),
                    )),
                    _ => Err(format!(
                        "Type error: `snd` requires a Tuple operand: {:?}",
                        expr
                    )),
                }
            }
            Expr::Lookup(p) => match semantics_projection(context, p)? {
                (ty @ Type::Bool, x) | (ty @ Type::Nat, x) => {
                    let gf = gf.copy(&x, ret, &ty)?;
                    Ok((ty, gf))
                }
                _ => Err("Type error: `Lookup` requires a Bool or Nat operand".to_string()),
            },
            Expr::Tuple(lhs, rhs) => {
                let (l_type, l_gf) = semantics_expr(context, lhs, ret.fst(), gf)?;
                let (r_type, r_gf) = semantics_expr(context, rhs, ret.snd(), l_gf)?;
                Ok((Type::Tuple(l_type.into(), r_type.into()), r_gf))
            }
            Expr::Arithmetic(arithmetic) => semantics_arithmetic(context, arithmetic, ret, gf),
            Expr::LoopSum(bound, body) => {
                let fresh = ID.next();
                let (Type::Nat | Type::Bool, body_gf) =
                    semantics_expr(&mut HashMap::new(), body, ret, GenFun::one())?
                else {
                    return Err("Type error: `LoopSum` requires a Nat body".to_string());
                };
                let (Type::Nat | Type::Bool, bound_gf) =
                    semantics_expr(context, bound, &fresh, gf)?
                else {
                    return Err("Type error: `LoopSum` requires a Nat bound".to_string());
                };
                Ok((
                    Type::Nat,
                    GenFunKind::Let(fresh.to_string(), body_gf, bound_gf).into(),
                ))
            }
            Expr::IfThenElse(cond, then_branch, else_branch) => {
                let (cond_type, Event { pos, neg }) = semantics_bool_expr(context, cond, gf)?;
                let Type::Bool = cond_type else {
                    return Err("Type error: `IfThenElse` condition must be Bool".to_string());
                };
                match (
                    semantics_expr(context, then_branch, ret, pos)?,
                    semantics_expr(context, else_branch, ret, neg)?,
                ) {
                    ((then_type, then_gf), (else_type, else_gf)) if then_type == else_type => {
                        Ok((then_type, then_gf + else_gf))
                    }
                    _ => {
                        Err("Type error: `IfThenElse` branches must have the same type".to_string())
                    }
                }
            }
            Expr::Let(var, value, body) => {
                let (value_type, value_gf) = semantics_expr(context, value, var, gf)?;
                let None = context.insert(var.clone(), value_type.clone()) else {
                    return Err(format!("Type error: variable {} already defined", var));
                };
                let (body_type, body_gf) = semantics_expr(context, body, ret, value_gf)?;
                context.remove(var);
                let let_gf = body_gf.marginalize(var, &value_type);
                Ok((body_type, let_gf))
            }
        }
    }
    if cfg!(debug_assertions) {
        let (expr_type, expr_gf) = semantics_expr_imp(context, expr, ret, gf.clone())?;
        println!(
            "[semantics_expr] [[ {:?} |- {:?} : {:?} | {} ]]({:})\n[semantics_expr] {}\n",
            context, expr, expr_type, ret, gf, expr_gf
        );
        Ok((expr_type, expr_gf))
    } else {
        semantics_expr_imp(context, expr, ret, gf)
    }
}
