use std::collections::HashMap;

use num_traits::{One, Zero};

use crate::ir::{BoolExpr, CompoundDistribution, Distribution, Expr, SimpleDistribution};
use crate::lang::{Arithmetic, ProjectionExpr, Type};

pub fn type_check(expr: &Expr) -> Result<Type, String> {
    let mut context = HashMap::new();
    type_check_expr(&mut context, expr)
}

pub fn type_check_projection(
    context: &mut HashMap<String, Type>,
    p: &ProjectionExpr,
) -> Result<Type, String> {
    match p {
        ProjectionExpr::Var(var) => context.get(var).map_or(
            Err(format!("Type error: variable {} not found", var)),
            |t: &Type| Ok(t.clone()),
        ),
        ProjectionExpr::Fst(sub_p) => match type_check_projection(context, sub_p)? {
            Type::Tuple(t, _) => Ok(*t),
            _ => Err(format!(
                "Type error: `fst` requires a Tuple operand: {:?}",
                p
            )),
        },
        ProjectionExpr::Snd(sub_p) => match type_check_projection(context, sub_p)? {
            Type::Tuple(_, t) => Ok(*t),
            _ => Err(format!(
                "Type error: `snd` requires a Tuple operand: {:?}",
                p
            )),
        },
    }
}

pub fn type_check_bool_expr(
    context: &mut HashMap<String, Type>,
    bool_expr: &BoolExpr,
) -> Result<Type, String> {
    match bool_expr {
        BoolExpr::BoolLiteral(_) => Ok(Type::Bool),
        BoolExpr::Lookup(p) => match type_check_projection(context, p)? {
            Type::Bool | Type::Nat => Ok(Type::Bool),
            _ => Err("Type error: `Lookup` should resolve to a Bool or Nat".to_string()),
        },
        BoolExpr::Test(expr) => match type_check_expr(context, expr)? {
            Type::Bool | Type::Nat => Ok(Type::Bool),
            t => Err(format!(
                "Type error: `Test` expect a Bool or Nat operand, got {:?}",
                t
            )),
        },
        BoolExpr::Not(sub_expr) => match type_check_bool_expr(context, sub_expr)? {
            Type::Bool => Ok(Type::Bool),
            _ => Err("Type error: `Not` requires a Bool operand".to_string()),
        },
        BoolExpr::And(lhs, rhs) => {
            match (
                type_check_bool_expr(context, lhs)?,
                type_check_bool_expr(context, rhs)?,
            ) {
                (Type::Bool, Type::Bool) => Ok(Type::Bool),
                _ => Err(format!(
                    "Type error: `And` require Bool operands, got {:?} and {:?}",
                    lhs, rhs
                )),
            }
        }
    }
}

pub fn type_check_arithmetic(
    context: &mut HashMap<String, Type>,
    arithmetic @ Arithmetic { terms, .. }: &Arithmetic,
) -> Result<Type, String> {
    if let Some(e) = Option::<Expr>::from(arithmetic.clone()) {
        type_check_expr(context, &e)
    } else {
        terms
            .iter()
            .try_fold(Type::Nat, |acc, (_, var)| match context.get(var) {
                Some(Type::Bool) => Ok(acc),
                Some(Type::Nat) => Ok(acc),
                Some(Type::Real | Type::UnitInterval) => Ok(Type::Real),
                Some(Type::Tuple(_, _)) => {
                    Err(format!("Type error: variable {} must not be Tuple", var))
                }
                None => Err(format!("Type error: variable {} not found", var)),
            })
    }
}

pub fn type_check_expr(context: &mut HashMap<String, Type>, expr: &Expr) -> Result<Type, String> {
    pub fn type_check_expr_imp(
        context: &mut HashMap<String, Type>,
        expr: &Expr,
    ) -> Result<Type, String> {
        match expr {
            Expr::Annotated(e, _) => type_check_expr(context, e),
            Expr::Var(var) => context.get(var).map_or(
                Err(format!("Type error: variable {} not found", var)),
                |t: &Type| Ok(t.clone()),
            ),
            Expr::Const(n) => Ok(if n == &0 || n == &1 {
                Type::Bool
            } else {
                Type::Nat
            }),
            Expr::Fst(sub_expr) => match type_check_expr(context, sub_expr)? {
                Type::Tuple(t, _) => Ok(*t),
                _ => Err(format!(
                    "Type error: `fst` requires a Tuple operand: {:?}",
                    expr
                )),
            },
            Expr::Snd(sub_expr) => match type_check_expr(context, sub_expr)? {
                Type::Tuple(_, t) => Ok(*t),
                _ => Err(format!(
                    "Type error: `nd` requires a Tuple operand: {:?}",
                    expr
                )),
            },
            Expr::Lookup(p) => match type_check_projection(context, p)? {
                ty @ Type::Bool | ty @ Type::Nat => Ok(ty),
                _ => Err("Type error: `Lookup` requires a Bool or Nat operand".to_string()),
            },
            Expr::Tuple(lhs, rhs) => {
                let lhs_type = type_check_expr(context, lhs)?;
                let rhs_type = type_check_expr(context, rhs)?;
                Ok(Type::Tuple(Box::new(lhs_type), Box::new(rhs_type)))
            }
            Expr::Sample(Distribution::Simple(
                SimpleDistribution::Dirac(0)
                | SimpleDistribution::Dirac(1)
                | SimpleDistribution::Bernoulli(_)
                | SimpleDistribution::Biased(..),
            )) => Ok(Type::Bool),
            Expr::Sample(Distribution::Simple(
                SimpleDistribution::Geometric(..)
                | SimpleDistribution::Poisson(_)
                | SimpleDistribution::Binomial(..)
                | SimpleDistribution::NegBinomial(..)
                | SimpleDistribution::Dirac(..),
            )) => Ok(Type::Nat),
            Expr::Sample(Distribution::Simple(
                SimpleDistribution::Exponential(_) | SimpleDistribution::Gamma(..),
            )) => Ok(Type::Real),
            Expr::Sample(Distribution::Simple(SimpleDistribution::Uniform(a, b))) => {
                if a.is_zero() && b.is_one() {
                    Ok(Type::UnitInterval)
                } else {
                    Ok(Type::Real)
                }
            }
            Expr::Sample(Distribution::Compound(CompoundDistribution::Bernoulli(x))) => {
                match context
                    .get(x)
                    .ok_or("Type error: variable not found".to_string())?
                {
                    Type::UnitInterval | Type::Bool => Ok(Type::Bool),
                    ty => Err(format!(
                        "Type error: `Bernoulli` expect a UnitInterval or Bool operand, got {ty}"
                    )),
                }
            }
            Expr::Sample(Distribution::Compound(CompoundDistribution::Poisson(..))) => {
                Ok(Type::Nat)
            }
            Expr::Observe(bool_expr) => match type_check_bool_expr(context, bool_expr)? {
                Type::Bool => Ok(Type::Bool),
                _ => Err("Type error: `Observe` requires a Bool operand".to_string()),
            },
            Expr::IfThenElse(cond, then_branch, else_branch) => {
                match (
                    type_check_bool_expr(context, cond)?,
                    type_check_expr(context, then_branch)?,
                    type_check_expr(context, else_branch)?,
                ) {
                    (Type::Bool, then_type, else_type) if then_type == else_type => Ok(then_type),
                    (Type::Bool, _, _) => {
                        Err("Type error: `IfThenElse` branches must have the same type".to_string())
                    }
                    _ => Err("Type error: `IfThenElse` condition must be Bool".to_string()),
                }
            }
            Expr::LoopSum(bound, body) => {
                match (
                    type_check_expr(&mut HashMap::new(), body)?,
                    type_check_expr(context, bound)?,
                ) {
                    (Type::Nat | Type::Bool, Type::Nat | Type::Bool) => Ok(Type::Nat),
                    (Type::Nat, _) => Err("Type error: `LoopSum` requires a Nat bound".to_string()),
                    (_, Type::Nat) => Err("Type error: `LoopSum` requires a Nat body".to_string()),
                    _ => Err("Type error: `LoopSum` requires Nat or Bool types".to_string()),
                }
            }
            Expr::Let(var, value, body) => {
                let value_type = type_check_expr(context, value)?;
                let None = context.insert(var.clone(), value_type) else {
                    return Err(format!("Type error: variable {} already defined", var));
                };
                let result = type_check_expr(context, body)?;
                context.remove(var);
                Ok(result)
            }
            Expr::Arithmetic(arithmetic) => type_check_arithmetic(context, arithmetic),
        }
    }

    if cfg!(debug_assertions) {
        println!("[type_check_expr] {:?} |- {:?} : ?\n", context, expr);
    }
    let result = type_check_expr_imp(context, expr)?;
    if cfg!(debug_assertions) {
        println!(
            "[type_check_expr] {:?} |- {:?} : {:?}\n",
            context, expr, result
        );
    }
    Ok(result)
}
