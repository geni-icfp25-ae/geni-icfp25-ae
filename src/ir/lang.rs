use crate::lang;
use crate::numbers::{Fraction, Natural, PositiveProperFraction, Precision};
use std::fmt::Debug;
// use itertools::iproduct;
// use std::{collections::HashMap, fmt::Debug, fmt::Display};

pub type Expr = lang::Expr<Distribution, BoolExpr, lang::Type>;

// pub type BoolExpr = ir::BoolExpr<Distribution>;

#[derive(Debug, Clone, PartialEq)]
pub enum BoolExpr {
    BoolLiteral(bool),
    Test(Box<Expr>),
    Lookup(Box<lang::ProjectionExpr>),
    Not(Box<BoolExpr>),
    And(Box<BoolExpr>, Box<BoolExpr>),
}

#[allow(clippy::should_implement_trait)]
impl BoolExpr {
    pub fn not(expr: BoolExpr) -> BoolExpr {
        match expr {
            BoolExpr::BoolLiteral(b) => BoolExpr::BoolLiteral(!b),
            BoolExpr::Not(e) => *e,
            e => BoolExpr::Not(Box::new(e)),
        }
    }

    pub fn or(lhs: BoolExpr, rhs: BoolExpr) -> BoolExpr {
        BoolExpr::not(BoolExpr::and(BoolExpr::not(lhs), BoolExpr::not(rhs)))
    }
    pub fn and(lhs: BoolExpr, rhs: BoolExpr) -> BoolExpr {
        match (lhs, rhs) {
            (BoolExpr::BoolLiteral(true), b) => b,
            (b, BoolExpr::BoolLiteral(true)) => b,
            (b @ BoolExpr::BoolLiteral(false), _) => b,
            (_, b @ BoolExpr::BoolLiteral(false)) => b,
            (lhs, rhs) => BoolExpr::And(Box::new(lhs), Box::new(rhs)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Distribution {
    Simple(SimpleDistribution),
    Compound(CompoundDistribution),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SimpleDistribution {
    Dirac(Natural), // TODO:: Dirac(u32) Dirac(f64)

    // G_{Biased(p)}(x) = 1 + p x
    Biased(PositiveProperFraction<Precision>),

    // G_{Bernoulli(p)}(x) = (1 - p) + p x
    Bernoulli(PositiveProperFraction<Precision>), // Probability between 0 and 1

    // G_{Binomial(n, p)}(x) = (1 - p + p x)^n
    Binomial(Natural, PositiveProperFraction<Precision>),

    // Infinite support discrete distributions

    // G_{NegBinomial(n, p)}(x) = (p / (1 - (1 - p) x))^n
    NegBinomial(Natural, PositiveProperFraction<Precision>),

    // G_{Geometric(p)}(x) = p / (1 - (1 - p) x)
    Geometric(PositiveProperFraction<Precision>),

    // G_{Poission(λ)}(x) = exp { λ * (x - 1) }
    Poisson(Fraction<Precision>),

    // Continuous distributions

    // Uniform(a, b) where a and b are rational numbers
    // M_{Uniform(a, b)}(x)
    // = (exp { b x } - exp { a x }) / ((b - a) x)
    // = exp { a x } (exp { (b - a) x } - 1) / ((b - a) x)
    Uniform(Fraction<Precision>, Fraction<Precision>),

    // Exponential(λ) where λ > 0 is a rational number
    // M_{Exponential(λ)}(x) = λ / (λ - x)
    Exponential(Fraction<Precision>),

    // Gamma(shape, rate) where shape and rate are rational numbers
    // M_{Gamma(α, β)}(x)
    // = (β / (β - x))^α
    // = exp { a ln { β / (β - x) } }
    // = exp { a [ ln { β } - ln { β - x } ] }
    Gamma(Fraction<Precision>, Fraction<Precision>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompoundDistribution {
    // Poission(λ, Y) where λ is a rational number and Y is a discrete or continuous random variable
    // G_{X ~ Poission(λ Y)}(x) = G_Y(exp{ λ(x - 1) })
    Poisson(Fraction<Precision>, String),

    // X | Y ~ Bernoulli(Y) requires that Y is between 0 and 1
    // G_{X ~ Bernoulli(Y)} is never used in GF compliation, leave is here for demonstration purposes.
    // G_{X ~ Bernoulli(Y)}(x)
    // = E_Y [ 1 - Y + Y x ]
    // = E_Y [ 1 + (x - 1) Y]
    // = 1 + (x - 1) E_Y[Y]
    // = 1 + (x - 1) (d G_Y(y) / dy)(1) for Y : Bool
    // = 1 + (x - 1) (d M_Y(y) / dy)(0) for Y : [0, 1]
    Bernoulli(String),
    // Binomial(String, PositiveProperFraction<Precision>),
    // NegBinomial(String, PositiveProperFraction<Precision>), // NegBinomial(X, p) where X is a natural number and p is a probability between 0 and 1
}
