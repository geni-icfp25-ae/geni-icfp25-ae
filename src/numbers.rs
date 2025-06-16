use num_traits::{Num, Pow};
use std::{
    fmt::{Debug, Display},
    ops::{Deref, Neg},
};

use crate::generating_function::{GenFun, GenFunKind};

pub type Precision = f32;
pub type Natural = u16;
pub trait Ln {
    fn ln(&self) -> Self;
}

impl Ln for Precision {
    fn ln(&self) -> Self {
        Precision::ln(*self)
    }
}

pub trait Exp {
    fn exp(&self) -> Self;
}

impl Exp for Precision {
    fn exp(&self) -> Self {
        Precision::exp(*self)
    }
}
// positive proper fractions
#[derive(Debug, Clone, PartialEq)]
pub struct PositiveProperFraction<T>(T);

impl Deref for PositiveProperFraction<Precision> {
    type Target = Precision;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T, N> From<PositiveProperFraction<N>> for GenFun<T>
where
    T: Num + Debug + Clone + From<N>,
    N: Num + Debug + Clone,
{
    fn from(frac: PositiveProperFraction<N>) -> Self {
        GenFunKind::Constant(frac.0.into()).into()
    }
}

impl<T: Num + Display + PartialOrd> PositiveProperFraction<T> {
    pub fn new(f: T) -> Result<Self, String> {
        if T::zero() <= f && f <= T::one() {
            Ok(PositiveProperFraction(f))
        } else {
            Err(format!(
                "Invalid fraction: {} Must be a positive proper fraction.",
                f
            ))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fraction<T>(pub T);

impl Deref for Fraction<Precision> {
    type Target = Precision;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Fraction<Precision> {
    pub fn is_integer(&self) -> bool {
        self.is_finite() && self.fract() == 0.0
    }
}
impl<T, N> From<Fraction<N>> for GenFun<T>
where
    T: Num + Debug + Clone + From<N>,
    N: Num + Debug + Clone,
{
    fn from(frac: Fraction<N>) -> Self {
        GenFunKind::Constant(frac.0.into()).into()
    }
}

pub trait Number:
    Num
    + Clone
    + Debug
    + Default
    + From<Precision>
    + From<bool>
    + From<Natural>
    + Display
    + Exp
    + Ln
    + Pow<Natural, Output = Self>
    + Neg<Output = Self>
{
}

impl Number for Precision {}
