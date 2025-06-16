use im::OrdSet;
use itertools::iproduct;
use num_traits::{One, Pow, Zero};
use std::fmt::Debug;
use std::iter::zip;

use crate::generating_function::{GenFun, GenFunKind};
use crate::ir::{CompoundDistribution, Distribution, SimpleDistribution};
use crate::lang::{InferenceType, InferenceValue, Parity, Type, TypedVariable, TypingContext};
use crate::numbers::{Exp, Ln, Natural, Number};

enum ProjectionKind {
    Fst,
    Snd,
}

static SEP: &str = "_";
static FST: &str = "fst";
static SND: &str = "snd";

fn projection(sep: &str, name: &str, p: &ProjectionKind) -> String {
    match p {
        ProjectionKind::Fst => format!("{}{}{}", name, sep, FST),
        ProjectionKind::Snd => format!("{}{}{}", name, sep, SND),
    }
}

fn fst(sep: &str, name: &str) -> String {
    projection(sep, name, &ProjectionKind::Fst)
}

fn snd(sep: &str, name: &str) -> String {
    projection(sep, name, &ProjectionKind::Snd)
}

fn expand(_sep: &str, name: &str, ty: &Type) -> Vec<TypedVariable> {
    match ty {
        Type::Real | Type::UnitInterval => vec![TypedVariable {
            id: name.to_string(),
            ty: Type::Real,
        }],
        Type::Bool => vec![TypedVariable {
            id: name.to_string(),
            ty: Type::Bool,
        }],
        Type::Nat => vec![TypedVariable {
            id: name.to_string(),
            ty: Type::Nat,
        }],
        Type::Tuple(t1, t2) => {
            let v1 = expand(_sep, &fst(SEP, name), t1.as_ref()).into_iter();
            let v2 = expand(_sep, &snd(SEP, name), t2.as_ref()).into_iter();
            v1.chain(v2).collect()
        }
    }
}

fn enumerate(ty: &InferenceType) -> Vec<InferenceValue> {
    match ty {
        InferenceType::Bool => vec![InferenceValue::Bool(false), InferenceValue::Bool(true)],
        InferenceType::Tuple(t1, t2) => iproduct!(enumerate(t1), enumerate(t2))
            .map(|(v1, v2)| InferenceValue::Tuple(Box::new(v1), Box::new(v2)))
            .collect(),
    }
}

fn test<T: Number>(name: &str, ty: &Type, gf: GenFun<T>) -> Result<Event<T>, String> {
    let gf0: GenFun<T> = GenFunKind::Let(name.to_owned(), GenFun::zero(), gf.clone()).into();
    match ty {
        Type::Bool | Type::Nat => Ok(Event {
            pos: gf.clone() - gf0.clone(),
            neg: gf0,
        }),
        _ => Err(format!("Invalid type for test: {}", ty)),
    }
}

fn test_fresh<T: Number>(fresh: &str, gf: GenFun<T>) -> Event<T> {
    let gf1: GenFun<T> = GenFunKind::Let(fresh.to_owned(), GenFun::one(), gf.clone()).into();
    let gf0: GenFun<T> = GenFunKind::Let(fresh.to_owned(), GenFun::zero(), gf.clone()).into();
    Event {
        pos: gf1 - gf0.clone(),
        neg: gf0,
    }
}

fn singleton<T: Number>(name: &str, d: &SimpleDistribution) -> GenFun<T> {
    match d {
        SimpleDistribution::Dirac(0) => GenFunKind::Constant(T::one()).into(),
        SimpleDistribution::Dirac(1) => GenFunKind::Var(name.to_owned()).into(),
        SimpleDistribution::Dirac(n) => GenFun::from(GenFunKind::Var(name.to_owned())).pow(*n),
        SimpleDistribution::Bernoulli(p) => {
            let x: GenFun<T> = GenFunKind::<T>::Var(name.to_owned()).into();
            let p: GenFun<T> = (*p).clone().into();
            (GenFun::one() - p.clone()) + p * x
        }
        SimpleDistribution::Binomial(n, p) => {
            let x: GenFun<T> = name.into();
            let p: GenFun<T> = (*p).clone().into();
            ((p.clone() * x) + (GenFun::one() - p)).pow(*n)
        }
        SimpleDistribution::NegBinomial(r, p) => {
            let x: GenFun<T> = name.into();
            let p: GenFun<T> = (*p).clone().into();
            (p.clone() / (GenFun::one() - (GenFun::one() - p) * x)).pow(*r)
        }
        SimpleDistribution::Geometric(p) => {
            let x: GenFun<T> = name.into();
            let p: GenFun<T> = (*p).clone().into();
            p.clone() / (GenFun::one() - (GenFun::one() - p) * x)
        }
        SimpleDistribution::Poisson(r) => {
            let x: GenFun<T> = name.into();
            let r: GenFun<T> = (*r).clone().into();
            (r * (x - GenFun::one())).exp()
        }
        SimpleDistribution::Biased(p) => {
            let x: GenFun<T> = name.into();
            let p: GenFun<T> = (*p).clone().into();
            GenFun::one() + p * x
        }

        // Moment-generating function of the exponential distribution
        //
        SimpleDistribution::Uniform(a, b) => {
            let x: GenFun<T> = name.into();
            let a: GenFun<T> = (*a).clone().into();
            let b: GenFun<T> = (*b).clone().into();
            // MGF of Uniform(a,b) is (e^(bx) - e^(ax))/(x(b-a))
            // For x = 0, MGF = 1
            // ((b.clone() * x.clone()).exp() - (a.clone() * x.clone()).exp())/ (x * (b - a))
            (a.clone() * x.clone()).exp() * ((b - a) * x).uniform_mgf()
        }
        SimpleDistribution::Exponential(lambda) => {
            let x: GenFun<T> = name.into();
            let lambda: GenFun<T> = (*lambda).clone().into();
            lambda.clone() / (lambda - x)
        }
        SimpleDistribution::Gamma(shape, rate) => {
            let x: GenFun<T> = name.into();
            let beta: GenFun<T> = (*rate).clone().into();
            // let alpha: Option<u32> = shape.0.try_into().ok();
            if shape.is_integer() {
                let alpha: Natural = shape.0 as Natural;
                (beta.clone() / (beta - x)).pow(alpha)
            } else {
                let alpha: GenFun<T> = (*shape).clone().into();
                (alpha * (beta.ln() - (beta - x).ln()).ln()).exp()
            }
        }
    }
}

fn extend<T: Number>(
    context: &mut TypingContext,
    ret: &str,
    d: &Distribution,
    gf: GenFun<T>,
) -> Result<GenFun<T>, String> {
    // X ~ D(Y)
    let x: GenFun<T> = ret.into();
    match d {
        Distribution::Simple(s) => Ok(gf * singleton(ret, s)),
        Distribution::Compound(CompoundDistribution::Poisson(r, y)) => {
            match context
                .get(y)
                .ok_or(format!("Type error: variable {y} not found"))?
            {
                Type::Bool | Type::Nat => {
                    // [[ Y : Bool | Nat |- Poisson(r * Y) | X ]] (G) (y, x)
                    // = G( y * exp{ r * (x - 1) } )
                    let r: GenFun<T> = (*r).clone().into();
                    Ok(GenFunKind::Let(
                        y.to_owned(),
                        GenFun::<T>::from(y) * (r * (x - GenFun::one())).exp(),
                        gf,
                    )
                    .into())
                }
                Type::UnitInterval | Type::Real => {
                    // [[ Y : UnitInterval| Real | |- Poisson(r * Y) | X ]] (M) (y, x)
                    // = M( y +  r * (x - 1) )
                    let r: GenFun<T> = (*r).clone().into();
                    Ok(GenFunKind::Let(
                        y.to_owned(),
                        GenFun::<T>::from(y) + (r * (x - GenFun::one())),
                        gf,
                    )
                    .into())
                }
                ty @ Type::Tuple(..) => {
                    Err(format!("Invalid type for Poisson({y}) where {y} : {ty}"))
                }
            }
        }
        Distribution::Compound(CompoundDistribution::Bernoulli(y)) => {
            match context
                .get(y)
                .ok_or(format!("Type error: variable {y} not found"))?
            {
                Type::UnitInterval => {
                    // [[ Y : UnitInterval |- Bernoulli(Y) | X ]] (M) (y, x)
                    // = M(y) + (x - 1) * d M(y) / dy
                    Ok(gf.clone() + (x - GenFun::one()) * gf.derivative(y.clone(), 1))
                }
                Type::Bool => {
                    // [[ Y : Bool |- Bernoulli(Y) | X ]] (G) (y, x)
                    // = G(y) + (x - 1) * y * d G(y) / dy
                    //  = G(y) + (x - 1) * y * (G(1) - G(0))
                    let Event { pos, .. } = gf.clone().test(y, &Type::Bool)?;
                    Ok(gf.clone() + (x - GenFun::one()) * GenFun::<T>::from(y) * pos)
                }
                ty @ (Type::Nat | Type::Real | Type::Tuple(..)) => {
                    Err(format!("Invalid type for Bernoulli({y}) where {y} : {ty}"))
                }
            }
        }
    }
}

fn copy<T: Number>(src: &str, dst: &str, ty: &Type, gf: GenFun<T>) -> Result<GenFun<T>, String> {
    let srcs = expand(SEP, src, ty);
    let dsts = expand(SEP, dst, ty);
    zip(srcs, dsts).try_fold(gf, |acc, (src, dst)| match (src, dst) {
        (
            TypedVariable {
                id: src,
                ty: Type::Bool | Type::Nat,
            },
            TypedVariable {
                id: dst,
                ty: Type::Bool | Type::Nat,
            },
        ) => {
            let gen_var_src: GenFun<T> = src.clone().into();
            let gen_var_dst: GenFun<T> = dst.into();
            Ok(GenFunKind::Let(src, gen_var_src * gen_var_dst, acc).into())
        }
        (
            TypedVariable {
                id: src,
                ty: Type::Real,
            },
            TypedVariable {
                id: dst,
                ty: Type::Real,
            },
        ) => {
            let gen_var_src: GenFun<T> = src.clone().into();
            let gen_var_dst: GenFun<T> = dst.into();
            Ok(GenFunKind::Let(src, gen_var_src + gen_var_dst, acc).into())
        }
        (src, dst) => Err(format!("Incompatible types src: ({}) dst: ({})", src, dst)),
    })
}

fn apply<T: Number>(
    name: &str,
    val: &InferenceValue,
    ty: &InferenceType,
    gf: GenFun<T>,
) -> GenFun<T> {
    match (val, ty) {
        (InferenceValue::Bool(b), InferenceType::Bool) => {
            let c = GenFunKind::Constant(T::from(*b)).into();
            GenFunKind::Let(name.to_owned(), c, gf).into()
        }
        (InferenceValue::Tuple(v1, v2), InferenceType::Tuple(t1, t2)) => {
            apply(&fst(SEP, name), v1, t1, apply(&snd(SEP, name), v2, t2, gf))
        }
        _ => panic!("Invalid value and type combination"),
    }
}

fn query<T: Number>(
    name: &str,
    val: &InferenceValue,
    ty: &InferenceType,
    gf: GenFun<T>,
) -> GenFun<T> {
    let parity = val.parity();
    val.subsets().iter().fold(GenFun::zero(), |acc, sub| {
        let term: GenFun<T> = apply(name, sub, ty, gf.clone());
        acc + (if sub.parity() == parity { term } else { -term })
    })
}

fn populate<T: Number>(
    name: &str,
    ty: &InferenceType,
    gf: GenFun<T>,
) -> Vec<(InferenceValue, GenFun<T>)> {
    ty.enumerate()
        .into_iter()
        .map(|val| {
            let query = query(name, &val, ty, gf.clone());
            (val, query)
        })
        .collect()
}
fn marginalize<T: Number>(name: &str, ty: &Type, gf: GenFun<T>) -> GenFun<T> {
    let vars = expand(SEP, name, ty);
    vars.iter()
        .fold(gf, |acc, TypedVariable { id, ty }| match ty {
            Type::Bool | Type::Nat => GenFunKind::Let(id.clone(), GenFun::one(), acc).into(),
            Type::Real => GenFunKind::Let(id.clone(), GenFun::zero(), acc).into(),
            _ => todo!("not implemented"),
        })
}

fn marginalize_from<T: Number>(
    context: &mut TypingContext,
    name: &str,
    gf: GenFun<T>,
) -> GenFun<T> {
    let Some(t) = context.get(name) else {
        return gf;
    };
    let marginalized = marginalize(name, t, gf);
    context.remove(name);
    marginalized
}

fn marginalize_dead<T: Number>(
    context: &mut TypingContext,
    past: &VariableSet,
    next: &VariableSet,
    gf: GenFun<T>,
) -> GenFun<T> {
    let dead = subtract(past, next);
    dead.iter()
        .fold(gf, |acc, v| marginalize_from(context, v, acc))
}
fn marginalize_many<T: Number>(vars: &[(&String, &Type)], gf: GenFun<T>) -> GenFun<T> {
    let mut gf = gf;
    for (v, t) in vars.iter() {
        gf = marginalize(v, t, gf);
    }
    gf
}

pub type VariableSet = OrdSet<String>;

pub fn subtract(lhs: &VariableSet, rhs: &VariableSet) -> VariableSet {
    assert!(rhs.is_subset(lhs));
    lhs.clone().difference(rhs.clone())
}

pub trait Projection {
    fn fst(&self) -> Self;
    fn snd(&self) -> Self;
}

impl Projection for String {
    fn fst(&self) -> Self {
        projection(SEP, self, &ProjectionKind::Fst)
    }
    fn snd(&self) -> Self {
        projection(SEP, self, &ProjectionKind::Snd)
    }
}

impl Projection for &str {
    fn fst(&self) -> Self {
        Box::leak(projection(SEP, self, &ProjectionKind::Fst).into_boxed_str())
    }

    fn snd(&self) -> Self {
        // Create a new string prefixed with "snd$" using the default separator
        Box::leak(projection(SEP, self, &ProjectionKind::Snd).into_boxed_str())
    }
}

pub trait Expand {
    fn expand(&self, name: &str) -> Vec<TypedVariable>;
}

impl Expand for Type {
    fn expand(&self, name: &str) -> Vec<TypedVariable> {
        expand(SEP, name, self)
    }
}

pub trait Enumerate {
    fn enumerate(&self) -> Vec<InferenceValue>;
}

impl Enumerate for InferenceType {
    fn enumerate(&self) -> Vec<InferenceValue> {
        enumerate(self)
    }
}

impl<T: Number> From<(&String, &SimpleDistribution)> for GenFun<T> {
    fn from((name, d): (&String, &SimpleDistribution)) -> Self {
        singleton(name, d)
    }
}

#[derive(Debug)]
pub struct Event<T> {
    pub pos: GenFun<T>,
    pub neg: GenFun<T>,
}

impl<T: Number> From<(&String, &Type, GenFun<T>)> for Event<T> {
    fn from((name, ty, gf): (&String, &Type, GenFun<T>)) -> Self {
        test(name, ty, gf).unwrap()
    }
}

pub trait Transform<T> {
    fn singleton(name: &str, d: &SimpleDistribution) -> GenFun<T>;
    fn test(self, name: &str, ty: &Type) -> Result<Event<T>, String>;
    fn test_fresh(self, name: &str) -> Event<T>;
    fn copy(self, src: &str, dst: &str, ty: &Type) -> Result<GenFun<T>, String>;
    fn apply(self, name: &str, val: &InferenceValue, ty: &InferenceType) -> GenFun<T>;
    fn query(self, name: &str, val: &InferenceValue, ty: &InferenceType) -> GenFun<T>;
    fn marginalize(self, name: &str, ty: &Type) -> GenFun<T>;
    fn marginalize_many(self, vars: &[(&String, &Type)]) -> GenFun<T>;
    fn marginalize_from(self, context: &mut TypingContext, name: &str) -> GenFun<T>;
    fn marginalize_dead(
        self,
        context: &mut TypingContext,
        past: &VariableSet,
        next: &VariableSet,
    ) -> GenFun<T>;
    fn extend(
        self,
        context: &mut TypingContext,
        name: &str,
        d: &Distribution,
    ) -> Result<GenFun<T>, String>;
    fn populate(self, name: &str, ty: &InferenceType) -> Vec<(InferenceValue, GenFun<T>)>;
}

impl<T: Number> Transform<T> for GenFun<T> {
    fn singleton(name: &str, d: &SimpleDistribution) -> GenFun<T> {
        singleton(name, d)
    }
    fn test(self, name: &str, ty: &Type) -> Result<Event<T>, String> {
        test(name, ty, self)
    }
    fn test_fresh(self, name: &str) -> Event<T> {
        test_fresh(name, self)
    }
    fn copy(self, src: &str, dst: &str, ty: &Type) -> Result<GenFun<T>, String> {
        copy(src, dst, ty, self)
    }
    fn apply(self, name: &str, val: &InferenceValue, ty: &InferenceType) -> GenFun<T> {
        apply(name, val, ty, self)
    }
    fn query(self, name: &str, val: &InferenceValue, ty: &InferenceType) -> GenFun<T> {
        query(name, val, ty, self)
    }
    fn marginalize(self, name: &str, ty: &Type) -> Self {
        marginalize(name, ty, self)
    }
    fn marginalize_many(self, vars: &[(&String, &Type)]) -> GenFun<T> {
        marginalize_many(vars, self)
    }
    fn marginalize_from(self, context: &mut TypingContext, name: &str) -> GenFun<T> {
        marginalize_from(context, name, self)
    }
    fn marginalize_dead(
        self,
        context: &mut TypingContext,
        past: &VariableSet,
        next: &VariableSet,
    ) -> GenFun<T> {
        marginalize_dead(context, past, next, self)
    }
    fn extend(
        self,
        context: &mut TypingContext,
        name: &str,
        d: &Distribution,
    ) -> Result<GenFun<T>, String> {
        extend(context, name, d, self)
    }
    fn populate(self, name: &str, ty: &InferenceType) -> Vec<(InferenceValue, GenFun<T>)> {
        populate(name, ty, self)
    }
}

pub trait ContextedGenFun<T> {}
