use std::{fmt::Debug, rc::Rc};

use crate::{
    free_variable_analysis::{CachedFreeVariables, FreeVariables},
    generating_function::{GenFun, GenFunKind},
    lang::{InferenceType, InferenceValue, Type},
    numbers::Number,
    transformers::{Enumerate, Transform},
};

use bitvec::prelude::*;
use hashbrown::HashMap;

type CachedGF<T> = HashMap<BitVec<u8, Msb0>, T>;

#[derive(Debug, Clone, Default)]
pub struct Interpreter<T> {
    pub cache: HashMap<*const GenFunKind<T>, CachedGF<T>>,
    pub free_varaible_analysis: CachedFreeVariables<T>,
}

impl<T: Number> Interpreter<T> {
    pub fn apply(&mut self, gf: &GenFun<T>) -> T {
        let mut ctx: HashMap<String, T> = HashMap::default();
        self.evaluate(&mut ctx, gf)
    }
    pub fn evaluate(&mut self, ctx: &mut HashMap<String, T>, gf: &GenFun<T>) -> T {
        let ptr: *const GenFunKind<T> = Rc::as_ptr(&gf.0);
        if Rc::strong_count(&gf.0) > 1 {
            if let Some(cached_fun) = self.cache.get(&ptr) {
                let args = gf.free_variables(&mut self.free_varaible_analysis);
                let mut bits = BitVec::<u8, Msb0>::new();
                args.iter().for_each(|v| {
                    bits.push(ctx.get(v).unwrap().is_zero());
                });
                if let Some(result) = cached_fun.get(&bits) {
                    if cfg!(debug_assertions) {
                        println!(
                            "+ [Interpreter] Cach hit  {}\n  [Interpreter]           {} |-> {:?}",
                            gf, bits, result
                        );
                    }
                    return result.clone();
                }
            }
        }
        if cfg!(debug_assertions) {
            println!("> [Interpreter] Working on {}", gf);
        }
        let result = self.evaluate_imp(ctx, &gf.0);
        // Only cache the result if it is shared:
        if Rc::strong_count(&gf.0) > 1 {
            let f = self.cache.entry(ptr).or_default();
            let mut bits = BitVec::<u8, Msb0>::new();
            let args = gf.free_variables(&mut self.free_varaible_analysis);
            args.iter().for_each(|v| {
                bits.push(ctx.get(v).unwrap().is_zero());
            });
            if cfg!(debug_assertions) {
                println!(
                    "- [Interpreter] Cach miss {}\n  [Interpreter]           {} |-> {:?}",
                    gf, bits, result
                );
            }
            f.insert(bits, result.clone());
        }
        result
    }
    fn evaluate_imp(&mut self, ctx: &mut HashMap<String, T>, gf: &GenFunKind<T>) -> T {
        use GenFunKind::*;
        match gf {
            Constant(c) => c.clone(),
            Var(v) => ctx
                .get(v)
                .unwrap_or_else(|| panic!("{} not found", v))
                .clone(),
            Neg(a) => -self.evaluate(ctx, a),
            Add(a, b) => self.evaluate(ctx, a) + self.evaluate(ctx, b),
            Div(a, b) => self.evaluate(ctx, a) / self.evaluate(ctx, b),
            Mul(a, b) => {
                let valb = self.evaluate(ctx, b);
                if valb.is_zero() {
                    T::zero()
                } else {
                    let vala = self.evaluate(ctx, a);
                    if valb.is_one() {
                        vala
                    } else {
                        vala * valb
                    }
                }
            }
            Let(x, a, b) => {
                let evaled_a = self.evaluate(ctx, a);
                let oldval = ctx.insert(x.clone(), evaled_a);
                let result = self.evaluate(ctx, b);
                if let Some(val) = oldval {
                    ctx.insert(x.clone(), val)
                } else {
                    ctx.remove(x)
                };
                result
            }
            Pow(a, n) => self.evaluate(ctx, a).pow(*n),
            Exp(a) => self.evaluate(ctx, a).exp(),
            Ln(a) => self.evaluate(ctx, a).ln(),
            UniformMgf(_) => todo!("UniformMgf"),
            Derivative(..) => todo!("Derivative"),
        }
    }

    pub fn populate_table(
        &mut self,
        var: &str,
        ty: &Type,
        gf: &GenFun<T>,
    ) -> Vec<(InferenceValue, T)> {
        let ty = InferenceType::from(ty.clone());
        let support = ty.enumerate();
        support
            .into_iter()
            .map(|v| {
                let prob = self.apply(&gf.clone().query(var, &v, &ty));
                (v, prob)
            })
            .collect()
    }
}
