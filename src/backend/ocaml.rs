use std::{fmt::Display, ops::Deref, rc::Rc};

use crate::{
    generating_function::{GenFun, GenFunKind},
    lang::Type,
};

use super::{
    Backend, BackendAndFuncSignature, BackendAndGenFun, BackendAndGenFunKind, Number, TopLevelInfo,
};

trait UnderscorePrefix {
    fn to_underscore_prefix(self) -> String;
}

impl UnderscorePrefix for &String {
    fn to_underscore_prefix(self) -> String {
        if self.starts_with('_') {
            self.to_string()
        } else {
            format!("_{}", self)
        }
    }
}

impl UnderscorePrefix for String {
    fn to_underscore_prefix(self) -> String {
        (&self).to_underscore_prefix()
    }
}

pub struct OCamlBackend<T> {
    toplevels: TopLevelInfo<T>,
}

impl<T> Deref for OCamlBackend<T> {
    type Target = TopLevelInfo<T>;

    fn deref(&self) -> &Self::Target {
        &self.toplevels
    }
}

impl<T: Number> OCamlBackend<T> {
    pub fn new(source: &GenFun<T>, ret: &str, ty: &Type, table_size: Option<usize>) -> Self {
        OCamlBackend {
            toplevels: TopLevelInfo::new(source, ret, ty, Self::validate_id, table_size),
        }
    }
}

impl<T> Display for BackendAndGenFun<'_, '_, T, OCamlBackend<T>>
where
    T: Number,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let BackendAndGenFun(backend, gf) = *self;
        if Rc::strong_count(&gf.0) > 1 {
            let ptr = Rc::as_ptr(&gf.0);
            let sig = backend.funcs.get(&ptr).unwrap();
            let call = BackendAndFuncSignature(backend, sig, Default::default());
            write!(f, "{}", call)
        } else {
            let gfk = BackendAndGenFunKind(backend, &gf.0);
            write!(f, "{}", gfk)
        }
    }
}

impl<T> Display for BackendAndGenFunKind<'_, '_, T, OCamlBackend<T>>
where
    T: Number,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let BackendAndGenFunKind(backend, gf) = *self;
        use GenFunKind::*;
        write!(f, "(")?;
        match gf {
            Var(v) => {
                write!(f, "{}", OCamlBackend::<T>::validate_id(v.clone()))?;
            }
            Constant(c) => {
                write!(f, "{}", c)?;
                if !c.to_string().contains('.') {
                    write!(f, ".")?;
                };
            }
            Add(a, b) => {
                let a: BackendAndGenFun<'_, '_, _, _> = (backend, a).into();
                let b: BackendAndGenFun<'_, '_, _, _> = (backend, b).into();
                write!(f, "{} +. {}", a, b)?;
            }
            Neg(a) => {
                let a: BackendAndGenFun<'_, '_, _, _> = (backend, a).into();
                write!(f, "-. {}", a)?;
            }
            Ln(a) => {
                let a: BackendAndGenFun<'_, '_, _, _> = (backend, a).into();
                write!(f, "Float.log {}", a)?;
            }
            Exp(a) => {
                let a: BackendAndGenFun<'_, '_, _, _> = (backend, a).into();
                write!(f, "Float.exp {}", a)?;
            }
            Mul(a, b) => {
                let a: BackendAndGenFun<'_, '_, _, _> = (backend, a).into();
                let b: BackendAndGenFun<'_, '_, _, _> = (backend, b).into();
                write!(f, "{} *. {}", a, b)?;
            }
            Div(a, b) => {
                let a: BackendAndGenFun<'_, '_, _, _> = (backend, a).into();
                let b: BackendAndGenFun<'_, '_, _, _> = (backend, b).into();
                write!(f, "{} /. {}", a, b)?;
            }
            Pow(a, b) => {
                let a: BackendAndGenFun<'_, '_, _, _> = (backend, a).into();
                write!(f, "{} ** {}.", a, b)?;
            }
            Let(v, a, b) => {
                let a: BackendAndGenFun<'_, '_, _, _> = (backend, a).into();
                let b: BackendAndGenFun<'_, '_, _, _> = (backend, b).into();
                write!(
                    f,
                    "let {} = {} in {}",
                    OCamlBackend::<T>::validate_id(v.clone()),
                    a,
                    b
                )?;
            }
            UniformMgf(_) => todo!("UniformMgf"),
            Derivative(..) => todo!("Derivative"),
        };
        write!(f, ")")
    }
}

impl<T> Display for BackendAndFuncSignature<'_, '_, T, OCamlBackend<T>>
where
    T: Number,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let BackendAndFuncSignature(_, sig, _) = self;
        write!(f, "({} ({}))", sig.name, sig.args.join(","))
    }
}

impl<T: Number> Backend<T> for OCamlBackend<T> {
    fn validate_id(id: String) -> String {
        id.to_underscore_prefix()
    }
}

const MEMO_FN: &str = r#"
let memo f =
  let h = Hashtbl.create tableSize in
  fun x ->
    try Hashtbl.find h x
    with Not_found ->
      let y = f x in
      Hashtbl.add h x y;
      y
;;
"#;

impl<T: Number> Display for OCamlBackend<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let tableSize = {};;\n{}", self.table_size, MEMO_FN,)?;
        for gf in &self.sorted {
            let ptr = Rc::as_ptr(&gf.0);
            let sig = self.funcs.get(&ptr).unwrap();
            let gfk: BackendAndGenFunKind<'_, '_, T, OCamlBackend<T>> =
                BackendAndGenFunKind(self, &gf.0);
            writeln!(
                f,
                "let {} = memo @@ fun ({}) -> ({});;",
                sig.name,
                sig.args.join(","),
                gfk
            )?;
        }

        for (v, gf) in &self.probs {
            let sig = self.funcs.get(&Rc::as_ptr(&gf.0)).unwrap();
            let call: BackendAndFuncSignature<'_, '_, T, OCamlBackend<T>> =
                BackendAndFuncSignature(self, sig, Default::default());
            write!(f, "Printf.printf \"Pr({}) \\t= \\t%f\\n\" {};;", v, call)?;
        }
        Ok(())
    }
}
