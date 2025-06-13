use im::{ordset, OrdSet};

use crate::ir::{self, CompoundDistribution, Distribution, SimpleDistribution};
use crate::lang::{self, Arithmetic, TypingContext};

type Annotated<T> = (T, VariableSet);

// FIXME: Code duplication in semantics.rs and live_variable_analysis.rs
#[derive(Debug, Clone)]
pub enum Expr {
    Var(String),
    Const(Natural),
    Arithmetic(Arithmetic),
    Tuple(Box<Annotated<Expr>>, Box<Annotated<Expr>>),
    Fst(Box<Annotated<Expr>>),
    Snd(Box<Annotated<Expr>>),
    Lookup(Box<ProjectionExpr>),
    Sample(Distribution),
    LoopSum(Box<Annotated<Expr>>, Box<Annotated<Expr>>),
    Observe(Box<Annotated<BoolExpr>>),
    IfThenElse(
        Box<Annotated<BoolExpr>>,
        Box<Annotated<Expr>>,
        Box<Annotated<Expr>>,
    ),
    Let(String, Box<Annotated<Expr>>, Box<Annotated<Expr>>),
}

// impl Expr {
//     pub fn as_var(&self) -> Option<Expr> {
//         match self {
//             Expr::Var(_) => Some((*self).clone()),
//             Expr::Arithmetic(term, constant) => {
//                 if let ([(None, x)], None) = (term.as_slice(), constant) {
//                     Some(Expr::Var(x.clone()))
//                 } else {
//                     None
//                 }
//             }
//             _ => None,
//         }
//     }
//     pub fn as_const(&self) -> Option<Expr> {
//         match self {
//             Expr::Const(_) => Some((*self).clone()),
//             Expr::Arithmetic(term, constant) => {
//                 if let ([], Some(c)) = (term.as_slice(), constant) {
//                     Some(Expr::Const(*c))
//                 } else {
//                     None
//                 }
//             }
//             _ => None,
//         }
//     }
// }

impl From<Arithmetic> for Option<Expr> {
    fn from(Arithmetic { terms, offset }: Arithmetic) -> Self {
        if let ([], Some(c)) = (terms.as_slice(), offset) {
            Some(Expr::Const(c))
        } else if let ([(None, x)], None) = (terms.as_slice(), offset) {
            Some(Expr::Var(x.clone()))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub enum BoolExpr {
    BoolLiteral(bool),
    Test(Box<Annotated<Expr>>),
    Lookup(Box<ProjectionExpr>),
    Not(Box<Annotated<BoolExpr>>),
    And(Box<Annotated<BoolExpr>>, Box<Annotated<BoolExpr>>),
}

#[derive(Debug, Clone)]
pub enum ProjectionExpr {
    Var(String),
    Fst(Box<ProjectionExpr>),
    Snd(Box<ProjectionExpr>),
}

pub trait Info {
    fn info(&self) -> usize;
}

impl Info for Annotated<Expr> {
    fn info(&self) -> usize {
        let current = self.1.len();
        match &self.0 {
            Expr::Var(_) => current,
            Expr::Const(_) => current,
            Expr::Arithmetic(..) => current,
            Expr::Tuple(lhs, rhs) => [current, lhs.info(), rhs.info()].into_iter().max().unwrap(),
            Expr::Fst(e) => cmp::max(current, e.info()),
            Expr::Snd(e) => cmp::max(current, e.info()),
            Expr::Sample(_) => current,
            Expr::Observe(b) => cmp::max(current, b.info()),
            Expr::IfThenElse(cond, then_branch, else_branch) => {
                [current, cond.info(), then_branch.info(), else_branch.info()]
                    .into_iter()
                    .max()
                    .unwrap()
            }
            Expr::LoopSum(..) => current,
            Expr::Lookup(_) => current,
            Expr::Let(_, a, b) => [current, a.info(), b.info()].into_iter().max().unwrap(),
        }
    }
}

impl Info for Annotated<BoolExpr> {
    fn info(&self) -> usize {
        let current = self.1.len();
        match &self.0 {
            BoolExpr::BoolLiteral(_) => current,
            BoolExpr::Test(e) => cmp::max(current, e.info()),
            BoolExpr::Lookup(_) => current,
            BoolExpr::Not(e) => cmp::max(current, e.info()),
            BoolExpr::And(lhs, rhs) => [current, lhs.info(), rhs.info()].into_iter().max().unwrap(),
        }
    }
}

pub trait LiveVariableAnalysis<T> {
    fn apply(&self, input: VariableSet) -> Annotated<T>;
}

impl LiveVariableAnalysis<ProjectionExpr> for lang::ProjectionExpr {
    fn apply(&self, input: VariableSet) -> Annotated<ProjectionExpr> {
        match self {
            lang::ProjectionExpr::Var(v) => {
                (ProjectionExpr::Var(v.clone()), input.update(v.clone()))
            }
            lang::ProjectionExpr::Fst(e) => {
                let (e, vars_e) = e.apply(input);
                (ProjectionExpr::Fst(Box::new(e)), vars_e)
            }
            lang::ProjectionExpr::Snd(e) => {
                let (e, vars_e) = e.apply(input);
                (ProjectionExpr::Snd(Box::new(e)), vars_e)
            }
        }
    }
}

impl LiveVariableAnalysis<BoolExpr> for ir::BoolExpr {
    fn apply(&self, input: VariableSet) -> Annotated<BoolExpr> {
        match self {
            ir::BoolExpr::BoolLiteral(b) => (BoolExpr::BoolLiteral(*b), input),
            ir::BoolExpr::Test(e) => {
                let (e, vars_e) = e.apply(input);
                (BoolExpr::Test(Box::new((e, vars_e.clone()))), vars_e)
            }
            ir::BoolExpr::Lookup(p) => {
                let (p, vars_p) = p.apply(input);
                (BoolExpr::Lookup(Box::new(p)), vars_p)
            }
            ir::BoolExpr::Not(e) => {
                let (e, vars_e) = e.apply(input);
                (BoolExpr::Not(Box::new((e, vars_e.clone()))), vars_e)
            }
            ir::BoolExpr::And(lhs, rhs) => {
                let (rhs, vars_rhs) = rhs.apply(input);
                let (lhs, vars_lhs) = lhs.apply(vars_rhs.clone());
                (
                    BoolExpr::And(
                        Box::new((lhs, vars_lhs.clone())),
                        Box::new((rhs, vars_rhs.clone())),
                    ),
                    OrdSet::unions([vars_lhs, vars_rhs]),
                )
            }
        }
    }
}

impl LiveVariableAnalysis<Expr> for ir::Expr {
    fn apply(&self, input: VariableSet) -> Annotated<Expr> {
        fn apply_imp(expr: &ir::Expr, input: VariableSet) -> Annotated<Expr> {
            match expr {
                ir::Expr::Annotated(e, _) => apply_imp(e, input),
                ir::Expr::Const(n) => (Expr::Const(*n), input.clone()),
                ir::Expr::Sample(d @ Distribution::Simple(_)) => {
                    (Expr::Sample(d.clone()), input.clone())
                }
                ir::Expr::Sample(
                    d @ Distribution::Compound(CompoundDistribution::Poisson(_, y)),
                ) => (Expr::Sample(d.clone()), input.update(y.clone())),
                ir::Expr::Sample(
                    d @ Distribution::Compound(CompoundDistribution::Bernoulli(y)),
                ) => (Expr::Sample(d.clone()), input.update(y.clone())),
                ir::Expr::Observe(b) => {
                    let (b, s) = b.apply(input);
                    (Expr::Observe(Box::new((b, s.clone()))), s)
                }
                ir::Expr::Var(v) => (Expr::Var(v.clone()), input.update(v.clone())),
                ir::Expr::Arithmetic(Arithmetic { terms, offset }) => {
                    let vars: OrdSet<String> = terms.iter().map(|(_, v)| v).collect();
                    (
                        Expr::Arithmetic(Arithmetic {
                            terms: terms.clone(),
                            offset: *offset,
                        }),
                        input.union(vars),
                    )
                }
                ir::Expr::Tuple(lhs, rhs) => {
                    let (replica, source) = (input.clone(), input);
                    let (rhs, vars_rhs) = rhs.apply(source);
                    let (lhs, vars_lhs) = lhs.apply(replica);
                    (
                        Expr::Tuple(
                            Box::new((lhs, vars_lhs.clone())),
                            Box::new((rhs, vars_rhs.clone())),
                        ),
                        OrdSet::unions([vars_lhs, vars_rhs]),
                    )
                }
                ir::Expr::Fst(e) => {
                    let (e, vars_e) = e.apply(input);
                    (Expr::Fst(Box::new((e, vars_e.clone()))), vars_e)
                }
                ir::Expr::Snd(e) => {
                    let (e, vars_e) = e.apply(input);
                    (Expr::Snd(Box::new((e, vars_e.clone()))), vars_e)
                }
                ir::Expr::Lookup(p) => {
                    let (p, vars_p) = p.apply(input);
                    (Expr::Lookup(Box::new(p)), vars_p)
                }
                ir::Expr::Let(v, a, b) => {
                    let (b, vars_b) = b.apply(input);
                    let (a, vars_a) = a.apply(vars_b.clone());
                    (
                        Expr::Let(
                            v.clone(),
                            Box::new((a, vars_a.clone())),
                            Box::new((b, vars_b.clone())),
                        ),
                        vars_a.without(v),
                    )
                }
                ir::Expr::IfThenElse(cond, then_branch, else_branch) => {
                    let (replica, source) = (input.clone(), input);
                    let (t_branch, t_vars) = then_branch.apply(source);
                    let (e_branch, e_vars) = else_branch.apply(replica);
                    let (cond, cond_vars) =
                        cond.apply(OrdSet::unions([t_vars.clone(), e_vars.clone()]));
                    (
                        Expr::IfThenElse(
                            Box::new((cond, cond_vars.clone())),
                            Box::new((t_branch, t_vars)),
                            Box::new((e_branch, e_vars)),
                        ),
                        cond_vars,
                    )
                }
                ir::Expr::LoopSum(bound, body) => {
                    let (bound, bound_vars) = bound.apply(input);
                    let (body, body_vars) = body.apply(ordset![]);
                    (
                        Expr::LoopSum(
                            Box::new((bound, bound_vars.clone())),
                            Box::new((body, body_vars.clone())),
                        ),
                        bound_vars.union(body_vars),
                    )
                }
            }
        }
        if cfg!(debug_assertions) {
            let (expr, output) = apply_imp(self, input.clone());
            println!(
                "[LiveVariableAnalysis] [[ {:?} ]]({:?})\n[LiveVariableAnalysis] {:?}\n",
                self, input, output
            );
            (expr, output)
        } else {
            apply_imp(self, input)
        }
    }
}

use crate::generating_function::{GenFun, GenFunKind};
use crate::lang::Type;
use crate::numbers::{Exp, Natural, Number};
use crate::semantics::{Semantics, ID};
use crate::transformers::{subtract, Event, Projection, Transform, VariableSet};

use num_traits::{One, Pow, Zero};
use std::cmp;
use std::collections::HashMap;
use std::fmt::Debug;

impl<T: Number> Semantics<T> for Annotated<Expr> {
    fn semantics(&self, ret: &str) -> Result<(Type, GenFun<T>), String> {
        semantics(self, ret)
    }
}
pub fn semantics<T: Number>(
    expr: &Annotated<Expr>,
    ret: &str,
) -> Result<(Type, GenFun<T>), String> {
    let mut context: TypingContext = HashMap::new();
    assert!(expr.1.is_empty(), "{:?}", expr.1);
    semantics_expr(&mut context, expr, ret, GenFun::one())
}

pub fn semantics_projection(
    context: &mut TypingContext,
    p: &ProjectionExpr,
) -> Result<(Type, String), String> {
    match p {
        ProjectionExpr::Var(var) => {
            let Some(var_type) = context.get(var) else {
                return Err(format!("[live::semantics_projection ProjectionExpr::Var] Type error: variable {} not found", var));
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
    annotated_bool_expr: &Annotated<BoolExpr>,
    gf: GenFun<T>,
) -> Result<(Type, Event<T>), String> {
    pub fn semantics_bool_expr_impl<T: Number>(
        context: &mut TypingContext,
        annotated_bool_expr: &Annotated<BoolExpr>,
        gf: GenFun<T>,
    ) -> Result<(Type, Event<T>), String> {
        let (bool_expr, _) = annotated_bool_expr;
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
                _ => Err("Type error: `Lookup` requires a Bool or Nat operand".to_string()),
            },
            BoolExpr::Test(expr) => {
                let fresh = ID.next();
                match semantics_expr(context, expr, &fresh.clone(), gf)? {
                    (Type::Bool, expr_gf) | (Type::Nat, expr_gf) => {
                        let Event { pos, neg } = expr_gf.test_fresh(&fresh);
                        Ok((Type::Bool, Event { pos, neg }))
                    }
                    (t, _) => Err(format!(
                        "Type error: `Test` expect a Bool or Nat operand, got {:?}",
                        t
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
    let r = semantics_bool_expr_impl(context, annotated_bool_expr, gf)?;
    // if cfg!(debug_assertions) {
    //     println!("[semantics_bool_expr] {:?}:\n[semantics_bool_expr] pos:{}\n[semantics_bool_expr] neg:{}\n", annotated_bool_expr, r.1.pos, r.1.neg);
    // }
    Ok(r)
}

pub fn type_check_arithmetic(
    context: &mut TypingContext,
    Arithmetic { terms, .. }: &Arithmetic,
) -> Result<Type, String> {
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

pub fn semantics_arithmetic<T: Number>(
    context: &mut TypingContext,
    (arithmetic @ Arithmetic { terms, offset }, alive): &Annotated<Arithmetic>,
    ret: &str,
    gf: GenFun<T>,
) -> Result<(Type, GenFun<T>), String> {
    if let Some(e) = Option::<Expr>::from(arithmetic.clone()) {
        semantics_expr(context, &(e, alive.clone()), ret, gf)
    } else {
        match type_check_arithmetic(context, arithmetic)? {
            Type::Nat => {
                let linear_gf = terms.iter().try_fold(gf.clone(), |acc, (coeff, var)| {
                        match context.get(var) {
                            Some(_t @ (Type::Bool | Type::Nat)) => {
                                let gf_var: GenFun<T> = var.clone().into();
                                let gf_ret: GenFun<T> = ret.to_string().into();
                                Ok(GenFunKind::Let(
                                    var.to_string(),
                                    gf_var * gf_ret.pow(coeff.unwrap_or(1)),
                                    acc,
                                )
                                .into())
                            }
                            _ => Err(format!("[live::semantics_expr Expr::Arithmetic]Type error: variable {} must be Bool or Nat", var)),
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
    expr: &Annotated<Expr>,
    ret: &str,
    gf: GenFun<T>,
) -> Result<(Type, GenFun<T>), String> {
    pub fn semantics_expr_imp<T: Number>(
        context: &mut TypingContext,
        expr: &Annotated<Expr>,
        ret: &str,
        gf: GenFun<T>,
    ) -> Result<(Type, GenFun<T>), String> {
        let gf = if context.get(ret).is_none() {
            Ok(gf)
        } else {
            Err(format!("Type error: variable {} already defined", ret))
        }?;
        let (expr, alive) = expr;
        match expr {
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
                    return Err(format!(
                        "[live::semantics_expr Expr::Var]Type error: variable {} not found",
                        var
                    ));
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
            Expr::Arithmetic(arithmetic) => {
                semantics_arithmetic(context, &(arithmetic.clone(), alive.clone()), ret, gf)
            }
            Expr::Let(var, value, body) => {
                let (_, value_alive) = value.as_ref();
                let (body_expr, body_alive) = body.as_ref();
                let (value_type, value_gf) = semantics_expr(context, value, var, gf)?;
                let None = context.insert(var.clone(), value_type.clone()) else {
                    return Err(format!("Type error: variable {} already defined", var));
                };
                let body_input = value_gf.marginalize_dead(
                    context,
                    &value_alive.update(var.clone()),
                    body_alive,
                );
                if cfg!(debug_assertions) {
                    let dead = subtract(&value_alive.update(var.clone()), body_alive);
                    println!(
                        "[live::semantics_expr] > [[ {:?} |- {:?} ]]\n[live::semantics_expr] dead before: {:?} = {:?} - {:?}\n",
                        context, body_expr, dead, value_alive.update(var.clone()), body_alive
                    );
                }
                let (body_type, body_gf) = semantics_expr(context, body, ret, body_input)?;
                // if cfg!(debug_assertions) {
                //     let dead = subtract(value_alive, alive);
                //     println!(
                //         "[live::semantics_expr] < [[ {:?} |- {:?} : {:?} | {} ]]\n[live::semantics_expr] dead after : {:?} - {:?} = {:?}\n",
                //         context, body_expr, body_type, ret, value_alive, alive, dead
                //     );
                // }
                Ok((body_type, body_gf.marginalize_from(context, var)))
            }
            Expr::IfThenElse(cond, then_branch, else_branch) => {
                let (cond_type, Event { pos, neg }) = semantics_bool_expr(context, cond, gf)?;
                let Type::Bool = cond_type else {
                    return Err("Type error: `IfThenElse` condition must be Bool".to_string());
                };
                let mut then_context = context.clone();
                let mut else_context = context.clone();
                let then_marginlized =
                    pos.marginalize_dead(&mut then_context, alive, &then_branch.1);
                let else_marginlized =
                    neg.marginalize_dead(&mut else_context, alive, &else_branch.1);
                match (
                    semantics_expr(&mut then_context, then_branch, ret, then_marginlized)?,
                    semantics_expr(&mut else_context, else_branch, ret, else_marginlized)?,
                ) {
                    ((then_type, then_gf), (else_type, else_gf)) if then_type == else_type => {
                        Ok((then_type, then_gf + else_gf))
                    }
                    _ => {
                        Err("Type error: `IfThenElse` branches must have the same type".to_string())
                    }
                }
            }
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
        }
    }
    if cfg!(debug_assertions) {
        println!(
            "[live::semantics_expr] >> [[ {:?} |- {:?} : ? | {} ]]({})\n",
            context, expr, ret, gf
        );
        let (expr_type, expr_gf) = semantics_expr_imp(context, expr, ret, gf.clone())?;
        println!(
            "[live::semantics_expr] [[ {:?} |- {:?} : {:?} | {} ]]({})\n[live::semantics_expr] {}\n",
            context, expr, expr_type, ret,
            gf, expr_gf
        );
        Ok((expr_type, expr_gf))
    } else {
        semantics_expr_imp(context, expr, ret, gf)
    }
}
