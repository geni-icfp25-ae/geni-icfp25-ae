use std::{
    fmt::{Debug, Display},
    ops::Deref,
    rc::Rc,
};

use num_traits::{Num, One, Pow, Zero};

use crate::numbers::{Exp, Ln, Natural, Number, Precision};

#[derive(Clone, Debug, PartialEq)]
pub struct GenFun<T>(pub Rc<GenFunKind<T>>);

impl<T> Deref for GenFun<T> {
    type Target = Rc<GenFunKind<T>>;
    fn deref(&self) -> &Rc<GenFunKind<T>> {
        &self.0
    }
}

impl<T: Num + Clone> GenFun<T> {
    fn fmt_prec(&self, parent_prec: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    where
        T: Display + Zero,
    {
        self.0.fmt_prec(parent_prec, f)
    }

    pub fn uniform_mgf(&self) -> GenFun<T> {
        if self.is_zero() {
            GenFun::one()
        } else {
            GenFunKind::UniformMgf(self.clone()).into()
        }
    }

    pub fn derivative(&self, var: String, n: usize) -> GenFun<T> {
        GenFunKind::Derivative(self.clone(), var, n).into()
    }
}

impl<T: Num + Clone + From<Natural> + Pow<Natural, Output = T>> Pow<Natural> for &GenFun<T> {
    type Output = GenFun<T>;
    fn pow(self, n: Natural) -> GenFun<T> {
        if self.is_zero() {
            GenFun::zero()
        } else if self.is_one() {
            GenFun::one()
        } else {
            if let GenFunKind::Exp(a) = self.as_ref() {
                return GenFunKind::Exp(GenFun::<T>::from(n) * a.clone()).into();
            }
            match n {
                0 => GenFun::one(),
                1 => self.clone(),
                _ => match &*self.0 {
                    GenFunKind::Constant(c) => GenFunKind::Constant(c.clone().pow(n)).into(),
                    _ => GenFunKind::Pow(self.clone(), n).into(),
                },
            }
        }
    }
}

impl<T: Num + Clone> Ln for GenFun<T> {
    fn ln(&self) -> GenFun<T> {
        if self.is_one() {
            GenFun::zero()
        } else {
            GenFunKind::Ln(self.clone()).into()
        }
    }
}

impl<T: Num + Clone> Exp for GenFun<T> {
    fn exp(&self) -> GenFun<T> {
        if self.is_zero() {
            GenFun::one()
        } else {
            GenFunKind::Exp(self.clone()).into()
        }
    }
}

impl<T: Display + Zero + Num + Clone> Display for GenFun<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let _ptr = Rc::as_ptr(&self.0);
        // write!(f, "{:?} ", _ptr)?;
        self.fmt_prec(0, f)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum GenFunKind<T> {
    Var(String),
    Constant(T),
    Neg(GenFun<T>),
    Add(GenFun<T>, GenFun<T>),
    Mul(GenFun<T>, GenFun<T>),
    Div(GenFun<T>, GenFun<T>),
    Exp(GenFun<T>),
    Pow(GenFun<T>, Natural),
    Let(String, GenFun<T>, GenFun<T>),
    Ln(GenFun<T>),
    /// The function f(x) = (e^x - 1) / x, continuously extended at x = 0 (i.e. f(0) = 1).
    UniformMgf(GenFun<T>),
    Derivative(GenFun<T>, String, usize),
}

impl<T> From<String> for GenFun<T> {
    fn from(s: String) -> Self {
        GenFunKind::Var(s).into()
    }
}

impl<T: From<Precision>> From<Precision> for GenFun<T> {
    fn from(f: Precision) -> Self {
        GenFunKind::<T>::Constant(T::from(f)).into()
    }
}

impl<T: From<Natural>> From<Natural> for GenFun<T> {
    fn from(u: Natural) -> Self {
        GenFunKind::Constant(u.into()).into()
    }
}

impl<T> From<&String> for GenFun<T> {
    fn from(s: &String) -> Self {
        s.clone().into()
    }
}
impl<T> From<&str> for GenFun<T> {
    fn from(s: &str) -> Self {
        s.to_string().into()
    }
}

impl<T> From<GenFunKind<T>> for GenFun<T> {
    fn from(kind: GenFunKind<T>) -> Self {
        GenFun(Rc::new(kind))
    }
}

impl<T: Num + Clone> GenFunKind<T> {
    fn fmt_prec(&self, parent_prec: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    where
        T: Display + Zero,
    {
        let cur_prec = self.precedence();
        if cur_prec < parent_prec {
            write!(f, "(")?;
        }
        match self {
            Self::Var(v) => write!(f, "{v}")?,
            Self::Constant(c) => write!(f, "{c}")?,
            Self::Add(a, b) => {
                a.fmt_prec(cur_prec, f)?;
                write!(f, " + ")?;
                b.fmt_prec(cur_prec, f)?;
            }
            Self::Neg(a) => {
                write!(f, "-")?;
                a.fmt_prec(cur_prec + 1, f)?;
            }
            Self::Mul(a, b) => {
                a.fmt_prec(cur_prec, f)?;
                write!(f, " * ")?;
                b.fmt_prec(cur_prec, f)?;
            }
            Self::Div(a, b) => {
                a.fmt_prec(cur_prec, f)?;
                write!(f, " / ")?;
                b.fmt_prec(cur_prec + 1, f)?;
            }
            Self::Exp(a) => {
                write!(f, "exp(")?;
                a.fmt_prec(0, f)?;
                write!(f, ")")?;
            }
            Self::Pow(a, b) => {
                a.fmt_prec(cur_prec + 1, f)?;
                write!(f, "^{b}")?;
            }
            Self::Let(y, a, b) => match b.as_ref() {
                Self::Derivative(fx, x, n) if x == y => {
                    write!(
                        f,
                        "(d{} ",
                        if *n == 1 {
                            "".to_string()
                        } else {
                            format!("^{n}")
                        }
                    )?;
                    fx.fmt_prec(0, f)?;
                    write!(f, " / d{x})")?;
                    write!(f, "(")?;
                    a.fmt_prec(0, f)?;
                    write!(f, ")")?;
                }
                _ => {
                    write!(f, "let {y} = ")?;
                    a.fmt_prec(cur_prec, f)?;
                    write!(f, " in ")?;
                    b.fmt_prec(cur_prec, f)?;
                }
            },
            Self::Ln(a) => {
                write!(f, "ln(")?;
                a.fmt_prec(0, f)?;
                write!(f, ")")?;
            }
            Self::UniformMgf(a) => {
                write!(f, "uniform_mgf(")?;
                a.fmt_prec(0, f)?;
                write!(f, ")")?;
            }
            Self::Derivative(a, v, n) => {
                write!(
                    f,
                    "(d{} ",
                    if *n == 1 {
                        "".to_string()
                    } else {
                        format!("^{n}")
                    }
                )?;
                a.fmt_prec(0, f)?;
                write!(f, " / d{v})({v})")?;
            }
        }
        if cur_prec < parent_prec {
            write!(f, ")")?;
        }
        Ok(())
    }
    pub fn precedence(&self) -> usize {
        use GenFunKind::*;
        match self {
            Var(_) | Constant(_) | Exp(_) | Ln(_) | Let(..) | UniformMgf(_) | Derivative(..) => 10,
            Add(..) | Neg(_) => 0,
            Mul(..) | Div(..) => 1,
            Pow(..) => 2,
        }
    }
}

impl<T: Display + Zero + Num + Clone> Display for GenFunKind<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_prec(0, f)
    }
}

impl<T: Num + Clone> One for GenFun<T> {
    fn one() -> Self {
        GenFunKind::Constant(T::one()).into()
    }
}

impl<T: Num + Clone> Zero for GenFun<T> {
    fn zero() -> Self {
        GenFunKind::Constant(T::zero()).into()
    }
    fn is_zero(&self) -> bool {
        match &*self.0 {
            GenFunKind::Constant(c) => c.is_zero(),
            GenFunKind::Neg(a) => a.is_zero(),
            _ => false,
        }
    }
}

impl<T> std::ops::Neg for GenFun<T> {
    type Output = Self;
    fn neg(self) -> Self {
        GenFunKind::Neg(self).into()
    }
}

impl<T: Num + Clone> std::ops::Add for GenFun<T> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        match (self.0.as_ref(), rhs.0.as_ref()) {
            (GenFunKind::Constant(a), GenFunKind::Constant(b)) => {
                GenFunKind::Constant((*a).clone() + (*b).clone()).into()
            }
            _ => {
                if self.is_zero() {
                    rhs
                } else if rhs.is_zero() {
                    self
                } else {
                    GenFunKind::Add(self, rhs).into()
                }
            }
        }
    }
}

impl<T: Num + Clone> std::ops::Sub for GenFun<T> {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        match (self.0.as_ref(), rhs.0.as_ref()) {
            (GenFunKind::Constant(a), GenFunKind::Constant(b)) => {
                GenFunKind::Constant((*a).clone() - (*b).clone()).into()
            }
            _ => self + (-rhs),
        }
    }
}

impl<T: Num + Clone> std::ops::Mul for GenFun<T>
where
    GenFun<T>: Zero,
{
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        if self.is_one() {
            rhs
        } else if rhs.is_one() {
            self
        } else if self.is_zero() || rhs.is_zero() {
            GenFun::zero()
        } else {
            match (self.0.as_ref(), rhs.0.as_ref()) {
                (_, GenFunKind::Div(a, b)) if a.is_one() => {
                    GenFunKind::Div(self.clone(), b.clone()).into()
                }
                (GenFunKind::Div(a, b), _) if a.is_one() => {
                    GenFunKind::Div(rhs.clone(), b.clone()).into()
                }
                (GenFunKind::Constant(x), GenFunKind::Constant(y)) => {
                    GenFunKind::Constant(x.clone() * y.clone()).into()
                }
                (GenFunKind::Constant(x), GenFunKind::Mul(gf_y, z)) => match gf_y.as_ref() {
                    GenFunKind::Constant(y) => {
                        let evaled: GenFun<T> = GenFunKind::Constant(x.clone() * y.clone()).into();
                        evaled * z.clone()
                    }
                    _ => GenFunKind::Mul(self, rhs).into(),
                },
                _ => GenFunKind::Mul(self, rhs).into(),
            }
        }
    }
}

impl<T: Number> std::ops::Div for GenFun<T> {
    type Output = Self;
    fn div(self, rhs: Self) -> Self {
        if rhs.is_one() {
            self
        } else if self.is_zero() {
            GenFun::zero()
        } else if rhs.is_zero() {
            panic!("Division by zero")
        } else {
            GenFunKind::Div(self, rhs).into()
        }
    }
}
