use crate::numbers::Natural;
use itertools::iproduct;
use std::{collections::HashMap, fmt::Debug, fmt::Display};

//
// Common IR for frontend languages
//

// parameterized by
// D = Distribution
// B = BoolExpr
// T = Type
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<D, B, T> {
    Var(String),
    Const(Natural),
    Arithmetic(Arithmetic),
    Tuple(Box<Expr<D, B, T>>, Box<Expr<D, B, T>>),
    Lookup(Box<ProjectionExpr>),
    Fst(Box<Expr<D, B, T>>),
    Snd(Box<Expr<D, B, T>>),
    Sample(D),
    LoopSum(Box<Expr<D, B, T>>, Box<Expr<D, B, T>>),
    Observe(Box<B>),
    IfThenElse(Box<B>, Box<Expr<D, B, T>>, Box<Expr<D, B, T>>),
    Let(String, Box<Expr<D, B, T>>, Box<Expr<D, B, T>>),
    Annotated(Box<Expr<D, B, T>>, T),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arithmetic {
    pub terms: Vec<(Option<Natural>, String)>,
    pub offset: Option<Natural>,
}

impl<D: std::cmp::PartialEq, B: std::cmp::PartialEq, T: std::cmp::PartialEq> Expr<D, B, T> {
    pub fn arithmetic(Arithmetic { terms, offset }: Arithmetic) -> Expr<D, B, T> {
        match (terms.as_slice(), offset) {
            ([], None) => panic!("Empty arithmetic expression"),
            ([], Some(c)) => Expr::Const(c),
            ([(None, x)], None) => Expr::Var(x.clone()),
            _ => Expr::Arithmetic(Arithmetic { terms, offset }),
        }
    }

    // pub fn arithmetic(terms: Vec<(Option<Natural>, String)>, offset: Option<Natural>) -> Expr {
    //     match (terms.as_slice(), offset) {
    //         ([], None) => panic!("Empty arithmetic expression"),
    //         ([], Some(c)) => Expr::Const(c),
    //         ([(None, x)], None) => Expr::Var(x.clone()),
    //         _ => Expr::Arithmetic(Arithmetic { terms, offset }),
    //     }
    // }

    pub fn ifthenelse(
        cond: B,
        then_expr: Expr<D, B, T>,
        else_expr: Expr<D, B, T>,
    ) -> Expr<D, B, T> {
        if then_expr == else_expr {
            then_expr
        } else {
            Expr::IfThenElse(Box::new(cond), Box::new(then_expr), Box::new(else_expr))
        }
    }

    // pub fn as_var(&self) -> Option<Expr> {
    //     match self {
    //         Expr::Var(_) => Some((*self).clone()),
    //         Expr::Arithmetic(Arithmetic { terms, offset }) => {
    //             if let ([(None, x)], None) = (terms.as_slice(), offset) {
    //                 Some(Expr::Var(x.clone()))
    //             } else {
    //                 None
    //             }
    //         }
    //         _ => None,
    //     }
    // }
    // pub fn as_const(&self) -> Option<Expr> {
    //     match self {
    //         Expr::Const(_) => Some((*self).clone()),
    //         Expr::Arithmetic(Arithmetic { terms, offset }) => {
    //             if let ([], Some(c)) = (terms.as_slice(), offset) {
    //                 Some(Expr::Const(*c))
    //             } else {
    //                 None
    //             }
    //         }
    //         _ => None,
    //     }
    // }
}

// impl Arithmetic {
//     pub fn as_var(&self) -> Option<Expr> {
//         let Arithmetic { terms, offset } = self;
//         if let ([(None, x)], None) = (terms.as_slice(), offset) {
//             Some(Expr::Var(x.clone()))
//         } else {
//             None
//         }
//     }
//     pub fn as_const(&self) -> Option<Expr> {
//         let Arithmetic { terms, offset } = self;
//         if let ([], Some(c)) = (terms.as_slice(), offset) {
//             Some(Expr::Const(*c))
//         } else {
//             None
//         }
//     }
// }

impl<D, B, T> From<Arithmetic> for Option<Expr<D, B, T>> {
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

#[derive(Debug, Clone, PartialEq)]
pub enum ProjectionExpr {
    Var(String),
    Fst(Box<ProjectionExpr>),
    Snd(Box<ProjectionExpr>),
}

#[derive(Debug, Clone, PartialEq)]
// Bool : Nat
// UnitInterval : Real
pub enum Type {
    Bool,
    Nat,  // from Poisson, Binomial, Geometric, Dirac
    Real, // from Exponential
    Tuple(Box<Type>, Box<Type>),
    UnitInterval, // from Uniform(0,1)
                  // FNat, // from Categorical, Dirac
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedVariable {
    pub id: String,
    pub ty: Type,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "Bool"),
            Type::Nat => write!(f, "Nat"),
            Type::Real => write!(f, "Real"),
            Type::Tuple(t1, t2) => write!(f, "({} * {})", t1, t2),
            Type::UnitInterval => write!(f, "UnitInterval"),
        }
    }
}

impl Display for TypedVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.id, self.ty)
    }
}

pub type TypingContext = HashMap<String, Type>;

#[derive(Debug, Clone, PartialEq)]
pub enum InferenceType {
    Bool,
    Tuple(Box<InferenceType>, Box<InferenceType>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum InferenceValue {
    Bool(bool),
    Tuple(Box<InferenceValue>, Box<InferenceValue>),
}

impl From<Type> for InferenceType {
    fn from(ty: Type) -> Self {
        match ty {
            Type::Bool => InferenceType::Bool,
            Type::Tuple(t1, t2) => {
                InferenceType::Tuple(Box::new((*t1).into()), Box::new((*t2).into()))
            }
            Type::Real | Type::UnitInterval => todo!("Requires computing dertivatives of the generating function"),
            Type::Nat => todo!("[Poisson, Binomial, Geometric, Dirac] compute higher order derivatives of the generating function"),
        }
    }
}

impl std::fmt::Display for InferenceValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InferenceValue::Bool(b) => write!(f, "{}", *b as u8),
            InferenceValue::Tuple(v1, v2) => write!(f, "({}, {})", v1, v2),
        }
    }
}

pub trait Parity {
    fn parity(&self) -> bool;
}

impl Parity for InferenceValue {
    fn parity(&self) -> bool {
        match self {
            InferenceValue::Bool(b) => *b,
            InferenceValue::Tuple(v1, v2) => v1.parity() ^ v2.parity(),
        }
    }
}

impl InferenceValue {
    pub fn subsets(&self) -> Vec<InferenceValue> {
        match self {
            InferenceValue::Bool(true) => {
                vec![InferenceValue::Bool(false), InferenceValue::Bool(true)]
            }
            InferenceValue::Bool(false) => vec![InferenceValue::Bool(false)],
            InferenceValue::Tuple(v1, v2) => iproduct!(v1.subsets(), v2.subsets())
                .map(|(v1, v2)| InferenceValue::Tuple(Box::new(v1), Box::new(v2)))
                .collect(),
        }
    }
}
