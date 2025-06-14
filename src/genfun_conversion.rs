use crate::generating_function::GenFunKind;
use crate::id_manager::IDManager;
use genfer::ppl::Var;
use std::collections::HashMap;

// Helper function to convert string variable names to Var using a HashMap and IDManager
fn to_var(name: &str, var_map: &mut HashMap<String, usize>, id_manager: &IDManager) -> Var {
    let idx = var_map
        .entry(name.to_string())
        .or_insert_with(|| id_manager.next_counter());
    Var(*idx)
}

pub fn to_genfer_genfun<S: crate::numbers::Number, T: genfer::number::Number + From<S>>(
    gf: &crate::generating_function::GenFun<S>,
) -> (
    genfer::generating_function::GenFun<T>,
    HashMap<String, usize>,
) {
    let mut var_map = HashMap::new();
    let id_manager = IDManager::new("");
    (
        to_genfer_genfun_with_vars(gf, &mut var_map, &id_manager),
        var_map,
    )
}

fn to_genfer_genfun_with_vars<S: crate::numbers::Number, T: genfer::number::Number + From<S>>(
    gf: &crate::generating_function::GenFun<S>,
    var_map: &mut HashMap<String, usize>,
    id_manager: &IDManager,
) -> genfer::generating_function::GenFun<T> {
    match gf.as_ref() {
        GenFunKind::Constant(x) => genfer::generating_function::GenFun::constant(x.clone().into()),
        GenFunKind::Var(name) => {
            genfer::generating_function::GenFun::var(to_var(name, var_map, id_manager))
        }
        GenFunKind::Neg(inner) => -to_genfer_genfun_with_vars(inner, var_map, id_manager),
        GenFunKind::Add(lhs, rhs) => {
            let lhs = to_genfer_genfun_with_vars(lhs, var_map, id_manager);
            let rhs = to_genfer_genfun_with_vars(rhs, var_map, id_manager);
            lhs + rhs
        }
        GenFunKind::Mul(lhs, rhs) => {
            let lhs = to_genfer_genfun_with_vars(lhs, var_map, id_manager);
            let rhs = to_genfer_genfun_with_vars(rhs, var_map, id_manager);
            lhs * rhs
        }
        GenFunKind::Div(lhs, rhs) => {
            let lhs = to_genfer_genfun_with_vars(lhs, var_map, id_manager);
            let rhs = to_genfer_genfun_with_vars(rhs, var_map, id_manager);
            lhs / rhs
        }
        GenFunKind::Exp(inner) => to_genfer_genfun_with_vars(inner, var_map, id_manager).exp(),
        GenFunKind::Ln(inner) => to_genfer_genfun_with_vars(inner, var_map, id_manager).log(),
        GenFunKind::Derivative(inner, var, n) => {
            to_genfer_genfun_with_vars(inner, var_map, id_manager)
                .derive(to_var(var, var_map, id_manager), *n)
        }
        GenFunKind::UniformMgf(inner) => genfer::generating_function::GenFun::uniform_mgf(
            to_genfer_genfun_with_vars(inner, var_map, id_manager),
        ),
        GenFunKind::Let(x, e1, e2) => {
            let var = to_var(x, var_map, id_manager);
            let g2 = to_genfer_genfun_with_vars(e2, var_map, id_manager);
            let g1 = to_genfer_genfun_with_vars(e1, var_map, id_manager);
            g2.substitute_var(var, g1)
        }
        gf => todo!("Implement conversion for {:?}", gf),
    }
}
