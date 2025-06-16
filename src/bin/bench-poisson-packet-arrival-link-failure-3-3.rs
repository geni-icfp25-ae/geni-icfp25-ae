use genfer::number::F32;
use gennifer::backend::Interpreter;
use gennifer::ir::parse;
use gennifer::lang::InferenceValue;
use gennifer::live_variable_analysis::LiveVariableAnalysis;
use gennifer::numbers::Precision;
use gennifer::semantics::Semantics;
use im::ordset;
use nom::error::convert_error;
use num_traits::One;

const LINK_FAILURE_3_3: &str = r#"
let X_1_1 = true in
let X_1_2 = ( X_1_1 && sample(Bernoulli(0.5)) ) in
let X_1_3 = ( X_1_2 && sample(Bernoulli(0.5)) ) in

let X_2_1 = ( X_1_1 && sample(Bernoulli(0.5)) ) in
let X_2_2 = (
    X_2_1 && sample(Bernoulli(0.5)) || 
    X_1_2 && sample(Bernoulli(0.5)) || 
    X_1_1 && sample(Bernoulli(0.5))
) in
let X_2_3 = (
    X_2_2 && sample(Bernoulli(0.5)) || 
    X_1_3 && sample(Bernoulli(0.5)) || 
    X_1_2 && sample(Bernoulli(0.5))
) in

let X_3_1 = ( X_2_1 && sample(Bernoulli(0.5))) in
let X_3_2 = (
    X_3_1 && sample(Bernoulli(0.5)) || 
    X_2_2 && sample(Bernoulli(0.5)) || 
    X_2_1 && sample(Bernoulli(0.5))
) in
let X_3_3 = (
    X_3_2 && sample(Bernoulli(0.5)) || 
    X_2_3 && sample(Bernoulli(0.5)) || 
    X_2_2 && sample(Bernoulli(0.5))
) in
X_3_3
"#;

fn solve_link_failure_3_3() -> Result<f32, String> {
    let expr = parse(LINK_FAILURE_3_3)
        .unwrap_or_else(|e| panic!("Parse error:\n{}", convert_error(LINK_FAILURE_3_3, e)));
    let annotated = expr.apply(ordset![]);
    let (ty, gf) = annotated.semantics("_RET")?;
    // .unwrap_or_else(|e| panic!("Semantics error:\n{}", e));
    let mut interpreter: Interpreter<Precision> = Interpreter::default();
    let table = interpreter.populate_table("_RET", &ty, &gf);
    let (InferenceValue::Bool(true), p) = table[1] else {
        return Err("Expected a boolean value for the link failure probability".to_string());
    };
    // println!("Success probability of 3 by 3 link failure  grid network: {}", p);
    Ok(p)
}

fn solve_packet_arrival() -> Result<f32, String> {
    let p = solve_link_failure_3_3()?;
    let body = format!("sample(Bernoulli({}))", p);
    let program = format!("loop sample(Poisson(10.0)) sum {body}");

    let expr = parse(&program)
        .unwrap_or_else(|e| panic!("Parse error:\n{}", convert_error(LINK_FAILURE_3_3, e)));
    let annotated = expr.apply(ordset![]);
    let (_, gf): (_, gennifer::generating_function::GenFun<Precision>) =
        annotated.semantics("_RET")?;
    let (genfer_gf, mapping): (genfer::generating_function::GenFun<F32>, _) =
        gennifer::genfun_conversion::to_genfer_genfun(&gf);
    if cfg!(debug_assertions) {
        println!("Genifer-Genfer Variable Mapping: {:?}", mapping);
    }

    let num_vars = mapping.len();
    let ret_idx = *mapping.get("_RET").unwrap();

    let simplified_genfer_gf = genfer_gf.simplify();
    if cfg!(debug_assertions) {
        println!("Simplified Genfer GF: {}", simplified_genfer_gf);
    }
    let mut substs = vec![F32::one(); num_vars];
    substs[ret_idx] = F32::one();
    if cfg!(debug_assertions) {
        println!("Substitutions: {:?}", substs);
    }
    let expansion = simplified_genfer_gf.eval(&substs, 10);
    if cfg!(debug_assertions) {
        println!("Expansion: {}", expansion);
    }
    // TODO: make this more general
    let expected_value = expansion.array()[[0, 1]].to_f32();
    if cfg!(debug_assertions) {
        println!("Expected value: {}", expected_value);
    }
    Ok(expected_value)
}
fn main() -> Result<(), String> {
    let program = format!(
        r#"
loop sample(Poisson(10.0)) sum (
{}
)
"#,
        LINK_FAILURE_3_3
    );
    print!("{program}");
    let e = solve_packet_arrival()?;
    println!(
        "Expected number of packets arriving at the destination: {}",
        e
    );
    Ok(())
}
