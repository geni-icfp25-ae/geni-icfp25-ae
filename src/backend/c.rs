use std::{cell::RefCell, fmt::Display, ops::Deref, rc::Rc};

use crate::{
    generating_function::{GenFun, GenFunKind},
    lang::Type,
};

use super::{
    Backend, BackendAndFuncSignature, BackendAndGenFun, BackendAndGenFunKind, Number, TopLevelInfo,
};

use rustc_hash::FxHashMap;

use log::warn;

const MAX_ARGS: usize = 31;

pub trait ToCTypeName {
    fn to_c_type_name() -> String;
}
impl ToCTypeName for f32 {
    fn to_c_type_name() -> String {
        "float".to_string()
    }
}
impl ToCTypeName for f64 {
    fn to_c_type_name() -> String {
        "double".to_string()
    }
}

pub struct CBackend<T> {
    toplevels: TopLevelInfo<T>,
    var_num: RefCell<u32>,
    cur_var_name: RefCell<String>,
    vars_in_scope: RefCell<FxHashMap<String, (u32, u32)>>,
}

impl<T> CBackend<T>
where
    T: Number + ToCTypeName,
{
    pub fn new(source: &GenFun<T>, ret: &str, ty: &Type, table_size: Option<usize>) -> Self {
        CBackend {
            toplevels: TopLevelInfo::new(source, ret, ty, Self::validate_id, table_size),
            var_num: RefCell::new(0),
            cur_var_name: RefCell::new("__v0".to_string()),
            vars_in_scope: FxHashMap::default().into(),
        }
    }

    fn var_increment(&self) {
        let cur_var_num: u32 = *self.var_num.borrow();
        *self.var_num.borrow_mut() = cur_var_num + 1;
        *self.cur_var_name.borrow_mut() = format!("__v{}", *self.var_num.borrow());
    }
}

impl<T> Display for BackendAndGenFun<'_, '_, T, CBackend<T>>
where
    T: Number + ToCTypeName,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let BackendAndGenFun(backend, gf) = *self;
        if Rc::strong_count(&gf.0) > 1 {
            let ptr = Rc::as_ptr(&gf.0);
            let sig = backend.funcs.get(&ptr).unwrap();
            if sig.args.len() <= MAX_ARGS {
                let call = BackendAndFuncSignature(backend, sig, Default::default());
                write!(f, "{}", call)
            } else {
                warn!(
                    "Function {} has {} arguments when we can only memoize \
                    a maximum of {}, so we must inline this.\n\
                    Arguments are {}",
                    sig.name,
                    sig.args.len(),
                    MAX_ARGS,
                    sig.args.join(",")
                );
                let gfk = BackendAndGenFunKind(backend, &gf.0);
                write!(f, "{}", gfk)
            }
        } else {
            let gfk = BackendAndGenFunKind(backend, &gf.0);
            write!(f, "{}", gfk)
        }
    }
}

impl<T> Display for BackendAndGenFunKind<'_, '_, T, CBackend<T>>
where
    T: Number + ToCTypeName,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let BackendAndGenFunKind(backend, gf) = *self;
        use GenFunKind::*;
        match gf {
            Var(v) => {
                backend.var_increment();
                let num_used: u32 = (*backend.vars_in_scope.borrow())
                    .get(v)
                    .unwrap_or(&(0, 0))
                    .0;
                writeln!(
                    f,
                    "\t{} {} = __s{}{};",
                    T::to_c_type_name(),
                    *backend.cur_var_name.borrow(),
                    num_used,
                    v
                )?;
            }
            Constant(c) => {
                backend.var_increment();
                writeln!(
                    f,
                    "\t{} {} = {};",
                    T::to_c_type_name(),
                    *backend.cur_var_name.borrow(),
                    c
                )?;
            }
            Add(a, b) => {
                let a: BackendAndGenFun<'_, '_, _, _> = (backend, a).into();
                let b: BackendAndGenFun<'_, '_, _, _> = (backend, b).into();
                write!(f, "{}", a)?;
                let a_var: String = backend.cur_var_name.borrow().clone();
                write!(f, "{}", b)?;
                let b_var: String = backend.cur_var_name.borrow().clone();
                writeln!(f, "\t{} = {} + {};", b_var, a_var, b_var)?;
            }
            Neg(a) => {
                let a: BackendAndGenFun<'_, '_, _, _> = (backend, a).into();
                write!(f, "{}", a)?;
                let a_var: String = backend.cur_var_name.borrow().clone();
                writeln!(f, "\t{} = -{};", a_var, a_var)?;
            }
            Mul(a, b) => {
                let a: BackendAndGenFun<'_, '_, _, _> = (backend, a).into();
                let b: BackendAndGenFun<'_, '_, _, _> = (backend, b).into();
                write!(f, "{}", a)?;
                let a_var: String = backend.cur_var_name.borrow().clone();
                write!(f, "{}", b)?;
                let b_var: String = backend.cur_var_name.borrow().clone();
                writeln!(f, "\t{} = {} * {};", b_var, a_var, b_var)?;
            }
            Div(a, b) => {
                let a: BackendAndGenFun<'_, '_, _, _> = (backend, a).into();
                let b: BackendAndGenFun<'_, '_, _, _> = (backend, b).into();
                write!(f, "{}", a)?;
                let a_var: String = backend.cur_var_name.borrow().clone();
                write!(f, "{}", b)?;
                let b_var: String = backend.cur_var_name.borrow().clone();
                writeln!(f, "\t{} = {} / {};", b_var, a_var, b_var)?;
            }
            Exp(a) => {
                let a: BackendAndGenFun<'_, '_, _, _> = (backend, a).into();
                write!(f, "{}", a)?;
                let a_var: String = backend.cur_var_name.borrow().clone();
                writeln!(f, "\t{} = exp({});", a_var, a_var)?;
            }
            Pow(a, b) => {
                let a: BackendAndGenFun<'_, '_, _, _> = (backend, a).into();
                write!(f, "{}", a)?;
                let a_var: String = backend.cur_var_name.borrow().clone();
                writeln!(f, "\t{} = pow({},{});", a_var, a_var, b)?;
            }
            Let(v, a, b) => {
                let a: BackendAndGenFun<'_, '_, _, _> = (backend, a).into();
                write!(f, "{}", a)?;
                let a_var: String = backend.cur_var_name.borrow().clone();
                let scope_details: (u32, u32) = match (*backend.vars_in_scope.borrow()).get(v) {
                    Some(scope) => (scope.0 + 1, scope.1),
                    None => (1, 0),
                };
                if scope_details.0 > scope_details.1 {
                    (*backend.vars_in_scope.borrow_mut())
                        .insert(v.clone(), (scope_details.0, scope_details.0));
                    writeln!(
                        f,
                        "\t{} __s{}{} = {};",
                        T::to_c_type_name(),
                        scope_details.0,
                        v,
                        a_var
                    )?;
                } else {
                    (*backend.vars_in_scope.borrow_mut()).insert(v.clone(), scope_details);
                    writeln!(f, "\t__s{}{} = {};", scope_details.0, v, a_var)?;
                }
                let b: BackendAndGenFun<'_, '_, _, _> = (backend, b).into();
                write!(f, "{}", b)?;
                let new_max_scope: u32 = (*backend.vars_in_scope.borrow()).get(v).unwrap().1;
                (*backend.vars_in_scope.borrow_mut())
                    .insert(v.clone(), (scope_details.0 - 1, new_max_scope));
            }
            _ => todo!("not implemented"),
        }
        Ok(())
    }
}

impl<T> Display for BackendAndFuncSignature<'_, '_, T, CBackend<T>>
where
    T: Number + ToCTypeName,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let BackendAndFuncSignature(backend, sig, _) = self;
        backend.var_increment();
        writeln!(
            f,
            "\t{} {} = {}({});",
            T::to_c_type_name(),
            *backend.cur_var_name.borrow(),
            sig.name,
            sig.args
                .clone()
                .into_iter()
                .map(|s| format!(
                    "__s{}{}",
                    (*backend.vars_in_scope.borrow())
                        .get(&s)
                        .unwrap_or(&(0, 0))
                        .0,
                    s
                ))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

impl<T> Backend<T> for CBackend<T>
where
    Self: Display + Deref<Target = TopLevelInfo<T>>,
    T: Number,
{
    fn validate_id(id: String) -> String {
        id
    }
}

impl<T> Deref for CBackend<T> {
    type Target = TopLevelInfo<T>;

    fn deref(&self) -> &Self::Target {
        &self.toplevels
    }
}

const HEADER: &str = r#"#include <math.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <memoization.h>
#define MEMOIZATION_CONTEXT 0
"#;

#[allow(clippy::print_in_format_impl)]
impl<T: Number + ToCTypeName> Display for CBackend<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if cfg!(debug_assertions) {
            println!("[CBackend::<T>::fmt] {:?}", self.toplevels);
        }
        writeln!(f, "{}", HEADER)?;
        for gf in &self.sorted {
            let ptr = Rc::as_ptr(&gf.0);
            let sig = self.toplevels.funcs.get(&ptr).unwrap();
            if sig.args.len() <= MAX_ARGS {
                // TODO: avoid magic string "p",
                // maybe add a `memo: bool` (maybe a enum `Wrapper := Memo | Plain` type ) feild in FuncSignature
                // or maybe check if sig.args.is_empty() to determine if it's a memoized function
                let mut args_map: FxHashMap<String, (u32, u32)> = FxHashMap::default();
                if sig.name.starts_with('p') {
                    write!(f, "{} {}(", T::to_c_type_name(), sig.name,)?;
                } else {
                    write!(
                        f,
                        "MEMOIZED_FUNCT(MEMOIZATION_CONTEXT,{},{}",
                        T::to_c_type_name(),
                        sig.name,
                    )?;
                    for arg in &sig.args {
                        write!(f, ",{},__s1{}", T::to_c_type_name(), &arg)?;
                        args_map.insert(arg.clone(), (1, 0));
                    }
                }
                writeln!(f, ") {{")?;
                (*self.vars_in_scope.borrow_mut()) = args_map;
                let gfk: BackendAndGenFunKind<'_, '_, T, CBackend<T>> =
                    BackendAndGenFunKind(self, &gf.0);
                write!(f, "{}", gfk)?;
                let ret_var: String = self.cur_var_name.borrow().clone();
                writeln!(f, "\treturn {};\n}}", ret_var)?;
            }
        }

        writeln!(
            f,
            r#"int main() {{
initGlobalMemoizationContexts();
enableGlobalMemoizationContext(MEMOIZATION_CONTEXT);"#
        )?;
        for (v, gf) in &self.probs {
            write!(f, "printf(\"Pr({}) \\t= \\t%f\\n\", ", v)?;
            let sig = self.toplevels.funcs.get(&Rc::as_ptr(&gf.0)).unwrap();
            writeln!(f, "{}({}));", sig.name, sig.args.join(","))?;
        }
        write!(
            f,
            r#"disableGlobalMemoizationContext(MEMOIZATION_CONTEXT);
freeGlobalMemoizationContexts();
}}"#
        )
    }
}
