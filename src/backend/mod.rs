mod c;
mod interpreter;
mod ocaml;

pub use c::*;
pub use interpreter::*;
pub use ocaml::*;

use crate::{
    free_variable_analysis::{CachedFreeVariables, FreeVariables},
    generating_function::{GenFun, GenFunKind},
    lang::Type,
    numbers::Number,
    transformers::Transform,
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::ops::Deref;
use std::str;
use std::{
    collections::{HashSet, VecDeque},
    fmt::Display,
    rc::Rc,
};
use std::{fmt::Debug, marker::PhantomData};

const TABLE_SIZE: usize = 32;
#[derive(Debug)]
pub struct TopLevelInfo<T> {
    table_size: usize,
    probs: Vec<(crate::lang::InferenceValue, GenFun<T>)>,
    funcs: FxHashMap<*const GenFunKind<T>, FuncSignature>,
    sorted: VecDeque<GenFun<T>>,
}

impl<T: Number> TopLevelInfo<T> {
    fn new(
        source: &GenFun<T>,
        ret: &str,
        ty: &Type,
        validate_id: impl Fn(String) -> String,
        table_size: Option<usize>,
    ) -> Self {
        let mut visited: HashSet<*const GenFunKind<T>, rustc_hash::FxBuildHasher> =
            FxHashSet::default();
        let probs;
        let mut sorted: VecDeque<GenFun<T>> = VecDeque::new();
        let mut funcs: FxHashMap<*const GenFunKind<T>, FuncSignature> = FxHashMap::default();
        let mut free = CachedFreeVariables::default();

        if let Type::Real = ty {
            probs = Vec::new();
            source.0.topological_sort(&mut visited, &mut sorted);
            for gf in &sorted {
                let ptr = Rc::as_ptr(&gf.0);
                let sig = FuncSignature {
                    name: format!("fn{:p}", ptr),
                    args: gf
                        .free_variables(&mut free)
                        .into_iter()
                        .map(&validate_id)
                        .collect(),
                };
                funcs.insert(ptr, sig);
            }

            sorted.push_back(source.clone());
            let ptr = Rc::as_ptr(&source.0);
            let sig = FuncSignature {
                name: format!("mgf{:p}", ptr),
                args: source
                    .free_variables(&mut free)
                    .into_iter()
                    .map(&validate_id)
                    .collect(),
            };
            funcs.insert(ptr, sig);
        } else {
            probs = source.clone().populate(ret, &ty.clone().into());

            for (_, gf) in probs.iter() {
                gf.topological_sort(&mut visited, &mut sorted);
            }

            for gf in &sorted {
                let ptr = Rc::as_ptr(&gf.0);
                let sig = FuncSignature {
                    name: format!("fn{:p}", ptr),
                    args: gf
                        .free_variables(&mut free)
                        .into_iter()
                        .map(&validate_id)
                        .collect(),
                };
                funcs.insert(ptr, sig);
            }

            for (v, gf) in probs.iter() {
                let sig = FuncSignature {
                    name: format!("p_{}", todo_convert_inference_value_string(&v.to_string())),
                    args: gf
                        .free_variables(&mut free)
                        .into_iter()
                        .map(&validate_id)
                        .collect(),
                };
                funcs.insert(Rc::as_ptr(&gf.0), sig);
                sorted.push_back(gf.clone());
            }
        };

        TopLevelInfo {
            table_size: table_size.unwrap_or(TABLE_SIZE),
            probs,
            funcs,
            sorted,
        }
    }
}

struct BackendAndGenFun<'a, 'b, T: Number, B: Backend<T>>(&'a B, &'b GenFun<T>);
struct BackendAndGenFunKind<'a, 'b, T: Number, B: Backend<T>>(&'a B, &'b GenFunKind<T>);

struct BackendAndFuncSignature<'a, 'b, T, B>(&'a B, &'b FuncSignature, PhantomData<T>)
where
    T: Number,
    B: Backend<T>;

impl<'a, 'b, T: Number, B: Backend<T>> From<(&'a B, &'b GenFun<T>)>
    for BackendAndGenFun<'a, 'b, T, B>
{
    fn from((backend, gf): (&'a B, &'b GenFun<T>)) -> Self {
        BackendAndGenFun(backend, gf)
    }
}

pub trait Backend<T: Number>
where
    Self: Display + Deref<Target = TopLevelInfo<T>>,
{
    fn validate_id(id: String) -> String
    where
        Self: Sized,
    {
        id
    }
}

#[derive(Clone, Debug)]
pub struct FuncSignature {
    pub name: String,
    pub args: Vec<String>,
}

pub trait TopologicalSort<T> {
    fn topological_sort(
        &self,
        visited: &mut HashSet<*const GenFunKind<T>, rustc_hash::FxBuildHasher>,
        sorted: &mut VecDeque<GenFun<T>>,
    );
}

impl<T: Clone> TopologicalSort<T> for GenFun<T>
where
    GenFun<T>: Display + Debug,
{
    fn topological_sort(
        &self,
        visited: &mut HashSet<*const GenFunKind<T>, rustc_hash::FxBuildHasher>,
        sorted: &mut VecDeque<GenFun<T>>,
    ) {
        if cfg!(debug_assertions) {
            let ptr = Rc::as_ptr(&self.0);
            println!(
                "[GenFun::<T>::topological_sort] {:?} {} {:?}",
                ptr, self, self
            );
        }
        if Rc::strong_count(&self.0) > 1 {
            let ptr = Rc::as_ptr(&self.0);
            if visited.get(&ptr).is_none() {
                visited.insert(ptr);
                self.0.topological_sort(visited, sorted);
                if cfg!(debug_assertions) {
                    println!(
                        "[GenFun::<T>::topological_sort] push back {:?} {} {:?}",
                        ptr, self, self
                    );
                }
                sorted.push_back(self.clone());
            }
        } else {
            self.0.topological_sort(visited, sorted);
        }
    }
}

impl<T: Clone> TopologicalSort<T> for GenFunKind<T>
where
    GenFun<T>: Display + Debug,
{
    fn topological_sort(
        &self,
        visited: &mut HashSet<*const GenFunKind<T>, rustc_hash::FxBuildHasher>,
        sorted: &mut VecDeque<GenFun<T>>,
    ) {
        match self {
            GenFunKind::Neg(a)
            | GenFunKind::Exp(a)
            | GenFunKind::Ln(a)
            | GenFunKind::UniformMgf(a) => a.topological_sort(visited, sorted),
            GenFunKind::Add(a, b) | GenFunKind::Mul(a, b) | GenFunKind::Div(a, b) => {
                a.topological_sort(visited, sorted);
                b.topological_sort(visited, sorted);
            }
            GenFunKind::Pow(a, _) => a.topological_sort(visited, sorted),
            GenFunKind::Let(_, value, body) => {
                value.topological_sort(visited, sorted);
                body.topological_sort(visited, sorted);
            }
            GenFunKind::Var(_) | GenFunKind::Constant(_) => {}
            GenFunKind::Derivative(..) => todo!("Derivative"),
        }
    }
}

pub fn todo_convert_inference_value_string(input: &str) -> String {
    let cleaned = input
        .chars()
        .filter(|c| *c != '(' && *c != ')' && *c != ' ')
        .collect::<String>();
    cleaned.replace(',', "_")
}
