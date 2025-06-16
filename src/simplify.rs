use crate::{
    generating_function::{GenFun, GenFunKind},
    numbers::{Exp, Ln, Number, Precision},
};
use num_traits::{One, Pow, Zero};

/// WARNING: This is a debug-only implementation that does not handle DAGs efficiently.
/// For production use, implement a DAG-aware version with memoization to avoid
/// exponential blowup when simplifying expressions with shared subexpressions.
///
/// A DAG-aware substitution function with memoization:
/// Preserves Rc-level sharing.
/// Avoids recomputation by caching already-substituted subtrees.
/// Uses raw pointers as keys in a HashMap.

pub trait Substitution<Input, Output> {
    fn substitute(&self, var: &str, value: &Input) -> Output;
}

impl<T: Number> Substitution<GenFun<T>, GenFun<T>> for GenFun<T> {
    fn substitute(&self, var: &str, value: &GenFun<T>) -> Self {
        let susbstituted = self.0.substitute(var, value);
        if cfg!(debug_assertions) {
            println!(
                "[GenFun::substitute] Input  {} [ {} / {} ]",
                self, var, value
            );
            println!("[GenFun::substitute] Output {}", susbstituted.clone());
            println!("[GenFun::substitute] Input  {:?}", self);
            println!("[GenFun::substitute] Output {:?}", susbstituted.clone());
        }
        susbstituted
    }
}

impl<T: Number> Substitution<GenFun<T>, GenFun<T>> for GenFunKind<T> {
    fn substitute(&self, var: &str, value: &GenFun<T>) -> GenFun<T> {
        match self {
            Self::Var(v) if v == var => value.clone(),
            // FIXME: could use full-DAG memoization here to avoid cloning references
            Self::Var(_) | Self::Constant(_) => (*self).clone().into(),
            Self::Neg(a) => -a.substitute(var, value),
            Self::Add(a, b) => a.substitute(var, value) + b.substitute(var, value),
            Self::Mul(a, b) => a.substitute(var, value) * b.substitute(var, value),
            Self::Div(a, b) => a.substitute(var, value) / b.substitute(var, value),
            Self::Exp(a) => a.substitute(var, value).exp(),
            Self::Ln(a) => a.substitute(var, value).ln(),
            Self::Pow(a, b) => a.substitute(var, value).pow(*b),
            Self::UniformMgf(a) => a.substitute(var, value).uniform_mgf(),
            Self::Let(v, a, b) => {
                if v == var {
                    // If the variable is shadowed, don't substitute in the body
                    Self::Let(v.clone(), a.substitute(var, value), b.clone()).into()
                } else {
                    // Otherwise, substitute in both the binding and body
                    Self::Let(
                        v.clone(),
                        a.substitute(var, value),
                        b.substitute(var, value),
                    )
                    .into()
                }
            }
            Self::Derivative(..) => {
                // TODO
                // (d f(x, y) / d x)[ v / y ]
                // = d (f(x, y) [v / y]) / d v
                GenFunKind::Let(var.to_string(), value.clone(), self.clone().into()).into()
            }
        }
    }
}

pub trait Derive<Output> {
    fn derive(&self, var: &str) -> Output;
}

impl<T: Number> Derive<GenFun<T>> for GenFun<T> {
    fn derive(&self, var: &str) -> GenFun<T> {
        let derived = self.0.derive(var);
        if cfg!(debug_assertions) {
            println!("[GenFun::derive] Input  {}", self);
            println!("[GenFun::derive] Output {}", derived.clone());
            println!("[GenFun::derive] Input  {:?}", self);
            println!("[GenFun::derive] Output {:?}", derived.clone());
        }
        derived
    }
}

impl<T: Number> Derive<GenFun<T>> for GenFunKind<T> {
    fn derive(&self, var: &str) -> GenFun<T> {
        match self {
            Self::Var(v) if v == var => GenFun::one(),
            Self::Var(_) | Self::Constant(_) => GenFun::zero(),
            Self::Neg(a) => -a.derive(var),
            Self::Add(a, b) => a.derive(var) + b.derive(var),
            Self::Mul(a, b) => {
                let da = a.derive(var);
                let db = b.derive(var);
                a.clone() * db + b.clone() * da
            }
            Self::Div(a, b) => {
                let da = a.derive(var);
                let db = b.derive(var);
                (da * b.clone() - a.clone() * db) / b.pow(2)
            }
            Self::Exp(a) => {
                let da = a.derive(var);
                // FIXME: avoid recomputing a.exp() here
                da * a.exp()
            }
            Self::Ln(a) => {
                let da = a.derive(var);
                da / a.clone()
            }
            Self::Pow(a, n) => {
                assert!(
                    *n != 0,
                    "unexpected 0 exponent, should have been simplified away before"
                );
                let da = a.derive(var);
                GenFun::from(*n) * da * a.pow(*n - 1)
            }
            Self::UniformMgf(_) => {
                // h(x) = (exp{ f(x)} - 1) / f(x)
                // h'(x) = f'(x) * (exp { f(x) } (f(x) - 1) + 1) / f(x)^2 when x != 0
                // h'(0) = f'(0) when x = 0
                // let da = a.derive(var);
                // da * (a.exp() * (a.clone() - GenFun::one()) + GenFun::one()) / a.pow(2)
                todo!()
            }
            Self::Derivative(a, x, n) => {
                if x == var {
                    let mut da = a.clone();
                    for _ in 0..*n {
                        da = da.derive(x);
                    }
                    da
                } else {
                    let mut a = a.clone();
                    for _ in 0..*n {
                        a = a.derive(x);
                    }
                    a.derive(var)
                }
            }
            Self::Let(..) => {
                todo!()
            }
        }
    }
}

/// WARNING: This is a debug-only implementation that does not handle DAGs efficiently.
/// For production use, implement a DAG-aware version with memoization to avoid
/// exponential blowup when simplifying expressions with shared subexpressions.

pub trait Simplification<Output> {
    fn simplify(&self) -> Output;
}

impl<T: Number> Simplification<GenFun<T>> for GenFun<T> {
    fn simplify(&self) -> GenFun<T> {
        let simplified = self.0.simplify();
        if cfg!(debug_assertions) {
            println!("[GenFun::simplify] Input  {}", self);
            println!("[GenFun::simplify] Output {}", simplified.clone());
            println!("[GenFun::simplify] Input  {:?}", self);
            println!("[GenFun::simplify] Output {:?}", simplified.clone());
        }
        simplified
    }
}

impl<T: Number> Simplification<GenFun<T>> for GenFunKind<T> {
    fn simplify(&self) -> GenFun<T> {
        match self {
            Self::Let(y, a, b) => {
                let a = a.simplify();
                let b = b.simplify();
                let special_case = match b.as_ref() {
                    Self::Derivative(fx, x, n) if x == y && *n == 1 && a.is_zero() => {
                        match fx.as_ref() {
                            GenFunKind::UniformMgf(mx) => match mx.as_ref() {
                                GenFunKind::Var(z) if z == y => {
                                    Some(GenFun::<T>::from(0.5 as Precision))
                                }
                                _ => None,
                            },
                            _ => None,
                        }
                    }
                    _ => None,
                };
                special_case.unwrap_or_else(|| b.substitute(y, &a).simplify())
            }
            // Recursively simplify all subexpressions
            Self::Neg(a) => -a.simplify(),
            Self::Add(a, b) => a.simplify() + b.simplify(),
            Self::Mul(a, b) => a.simplify() * b.simplify(),
            Self::Div(a, b) => a.simplify() / b.simplify(),
            Self::Exp(a) => a.simplify().exp(),
            Self::Ln(a) => a.simplify().ln(),
            Self::Pow(a, b) => a.simplify().pow(*b),
            Self::UniformMgf(a) => {
                a.simplify().uniform_mgf()
                // // M(x) = (exp { x } - 1) / x
                // let a = a.simplify();
                // if a.is_zero() {
                //     GenFun::one()
                // } else {
                //     (a.exp() - GenFun::one()) / a
                // }
            }
            Self::Derivative(a, x, n) => {
                match a.as_ref() {
                    GenFunKind::UniformMgf(_) => a.derivative(x.clone(), *n),
                    _ => {
                        let mut da = a.clone();
                        for _ in 0..*n {
                            da = da.derive(x);
                        }
                        da.simplify()
                    }
                }
                // a.simplify().derivative(x.clone(), *n)
            }
            // For atomic expressions, just clone them
            Self::Var(_) | Self::Constant(_) => self.clone().into(),
        }
    }
}
