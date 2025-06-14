use crate::generating_function::{GenFun, GenFunKind};
use hashbrown::HashMap;
use im::ordset;
use num_traits::Num;
use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
    rc::Rc,
};

// use std::collections::BTreeSet;
// type VarSet = BTreeSet<String>;
pub type VarSet = im::OrdSet<String>;

pub trait FreeVariables<T> {
    fn free_variables(&self, cache: &mut CachedFreeVariables<T>) -> VarSet;
}

type HashMapFreeVariables<T> = HashMap<*const GenFunKind<T>, VarSet>;
#[derive(Debug, Clone, Default)]
pub struct CachedFreeVariables<T> {
    pub cache: HashMapFreeVariables<T>,
}

impl<T> Deref for CachedFreeVariables<T> {
    type Target = HashMapFreeVariables<T>;

    fn deref(&self) -> &Self::Target {
        &self.cache
    }
}

impl<T> DerefMut for CachedFreeVariables<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.cache
    }
}

impl<T: Num + Clone + Display> FreeVariables<T> for GenFun<T>
where
    CachedFreeVariables<T>: Deref<Target = HashMapFreeVariables<T>>,
{
    fn free_variables(&self, cache: &mut CachedFreeVariables<T>) -> VarSet {
        let ptr: *const GenFunKind<T> = Rc::as_ptr(&self.0);
        if let Some(cached) = cache.get(&ptr) {
            if cfg!(debug_assertions) {
                println!("+ [GenFun<T>::free_variables] Cache hit  {:?} {}\n  [GenFun<T>::free_variables]             {:?}", ptr, self, cached);
            }
            return cached.clone();
        }
        if cfg!(debug_assertions) {
            println!(
                "> [GenFun<T>::free_variables] Working on {:?} {}",
                ptr, self
            );
        }
        let result = self.0.free_variables(cache);
        if cfg!(debug_assertions) {
            println!(
                "- [GenFun<T>::free_variables] Cache miss {:?} {}\n  [GenFun<T>::free_variables]            {:?}",
                ptr, self, result
            );
        }
        if Rc::strong_count(&self.0) > 1 {
            cache.insert(ptr, result.clone());
        }
        result
    }
}

impl<T: Num + Clone + Display> FreeVariables<T> for GenFunKind<T>
where
    CachedFreeVariables<T>: Deref<Target = HashMapFreeVariables<T>>,
{
    fn free_variables(&self, cache: &mut CachedFreeVariables<T>) -> VarSet {
        use GenFunKind::*;
        match self {
            Constant(_) => ordset![],
            Var(v) => ordset![v.clone()],
            Neg(a) | Exp(a) | Ln(a) | UniformMgf(a) => a.free_variables(cache),
            Add(a, b) | Mul(a, b) | Div(a, b) => {
                let seta = a.free_variables(cache);
                let setb = b.free_variables(cache);
                seta.union(setb)
            }
            Let(var, a, b) => {
                let seta = a.free_variables(cache);
                let setb = b.free_variables(cache);
                seta.union(setb.without(var))
            }
            Pow(a, _) => a.free_variables(cache),
            Derivative(..) => todo!("Derivative"),
        }
    }
}
