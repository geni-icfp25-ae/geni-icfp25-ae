use std::rc::Rc;
use std::ops::{Add, Mul, Div, Neg, Sub};
use std::fmt::Debug;

use hashbrown::HashMap;
use num_traits::{Num, Pow};

use crate::free_variable_analysis::CachedFreeVariables;
use crate::generating_function::{GenFun, GenFunKind};
use crate::numbers::Precision;

pub struct DerivativeBackend<T> {
    pub cache: HashMap<*const GenFunKind<T>, *const GenFunKind<T>>,
    pub fva: CachedFreeVariables<T>,
}

pub trait Derivative<I,O> {
    fn derivative(&self, i: &I, var: &str) -> O;
}

// fn derivative(&self, f: &GenFunKind<T>, var: &str) -> GenFunKind<T> {
//     let ptr: *const GenFunKind<T> = Rc::as_ptr(&f.clone().into());
//     if Rc::strong_count(&f.clone().into()) > 1 {
//         if let Some(cached_fun) = self.cache.get(&ptr) {
//             if let Some(result) = cached_fun {
//                 return result.clone();
//             }
//         }
//     }
//     let result = f.derivative(var);
//     let ptr = Rc::as_ptr(&result.clone().into());
//     self.cache.insert(ptr, ptr);
//     result
// }
// }
// impl<T: Clone + Debug + Num + From<Precision>> Derivative<GenFunKind<T>, GenFunKind<T>> for DerivativeBackend<T> {
//     /// Computes the first derivative of the function with respect to `var`.
//     fn derivative(&self, i: &GenFunKind<T>, var: &str) -> GenFunKind<T> {
//         match i {
//             // d/dx(C) = 0
//             GenFunKind::Constant(_) => GenFunKind::Constant(T::zero()), 
            
//             // d/dx(x) = 1
//             GenFunKind::Var(v) if v == var => GenFunKind::Constant(T::one()), 
            
//             // d/dx(y) = 0 where x ≠ y
//             GenFunKind::Var(_) => GenFunKind::Constant(T::zero()), 
            
//             // d/dx(-f) = -df/dx
//             GenFunKind::Neg(f) => GenFunKind::Neg(f.derivative(var)), 

//             // d/dx(f + g) = df/dx + dg/dx
//             GenFunKind::Add(f, g) => GenFunKind::Add(f.derivative(var), g.derivative(var)), 

//             // Product Rule: d/dx(fg) = f'g + fg'
//             GenFunKind::Mul(f, g) => { 
//                 let df = f.derivative(var);
//                 let dg = g.derivative(var);
//                 GenFunKind::Add(df * g.clone(), f.clone() * dg)
//             }

//             // Quotient Rule: d/dx(f/g) = (f'g - fg') / g²
//             GenFunKind::Div(f, g) => { 
//                 let df = f.derivative(var);
//                 let dg = g.derivative(var);
//                 let numerator = df * g.clone() - (f.clone() * dg);
//                 let denominator = g.clone().pow(2);
//                 GenFunKind::Div(numerator, denominator)
//             }

//             // d/dx(e^f) = f' * e^f
//             GenFunKind::Exp(f) => { 
//                 let df = f.derivative(var);
//                 GenFunKind::Mul(df, (*self.clone()).into())
//             }

//             // Power Rule: d/dx(f^n) = n * f^(n-1) * f'
//             GenFunKind::Pow(f, n) => { 
//                 let df = f.derivative(var);
//                 GenFunKind::Mul(
//                     GenFunKind::Constant(n.into()).into() * f.pow(n.clone() - 1),
//                     df,
//                 )
//             }
            
//             // d/dx (let x = e1 in e2) = 0 where x does not appear in e2
//             // d/dx (let x = e1 in e2) = (d/dx e1) * (let x = e1 in d/dx e2) where x appears in e2
//             // d/dx (let y = e1 in e2) = (d/dx e1) * (let y = e1 in d/dy e2) + (d/dx e2) where x ≠ y
            
//             GenFunKind::Let(v, value, expr) => {
//                 todo!()
//             }
//         }
//     }
// }
