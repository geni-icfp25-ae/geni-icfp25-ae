use genfer::{number::F32, ppl::Var};
use gennifer::{
    ir::{Distribution, SimpleDistribution},
    numbers::{Fraction, PositiveProperFraction, Precision},
    transformers::Transform,
};
use num_traits::{One, Zero};
use std::collections::HashMap;

fn test_exponential() -> Result<(), String> {
    println!("-----------------");
    println!("Exponential Test");
    let ret = "X";
    let gf = gennifer::generating_function::GenFun::<f32>::one().extend(
        &mut HashMap::new(),
        ret,
        &Distribution::Simple(SimpleDistribution::Exponential(Fraction(1.0))),
    )?;
    let (genfer_gf, mapping): (genfer::generating_function::GenFun<F32>, _) =
        gennifer::genfun_conversion::to_genfer_genfun(&gf);
    println!("Genifer {}", gf);
    println!("Genfer {}", genfer_gf);
    println!("mapping: {:?}", mapping);
    assert!(mapping.len() == 1);
    assert!(*mapping.get(ret).unwrap() == 0);

    let simplified_genfer_gf = genfer_gf.simplify();
    println!("Simplified Genfer GF: {}", simplified_genfer_gf);

    let substs = vec![F32::zero()];
    let expansion = simplified_genfer_gf.eval(&substs, 10);
    println!("Expansion: {}", expansion);
    Ok(())
}

fn test_geometric_0() -> Result<(), String> {
    println!("-----------------");
    println!("Geometric Test");
    let ret = "X";
    let gf = gennifer::generating_function::GenFun::<Precision>::one().extend(
        &mut HashMap::new(),
        ret,
        &Distribution::Simple(SimpleDistribution::Geometric(PositiveProperFraction::new(
            0.0,
        )?)),
    )?;
    let (genfer_gf, mapping): (genfer::generating_function::GenFun<F32>, _) =
        gennifer::genfun_conversion::to_genfer_genfun(&gf);
    println!("Genifer {}", gf);
    println!("Genfer {}", genfer_gf);
    println!("mapping: {:?}", mapping);
    assert!(mapping.len() <= 1);
    if mapping.len() == 1 {
        assert!(*mapping.get(ret).unwrap() == 0);
    }

    let simplified_genfer_gf = genfer_gf.simplify();
    println!("Simplified Genfer GF: {}", simplified_genfer_gf);

    let substs = vec![F32::zero()];
    let expansion = simplified_genfer_gf.eval(&substs, 10);
    println!("Expansion: {}", expansion);
    Ok(())
}

fn test_geometric() -> Result<(), String> {
    println!("-----------------");
    println!("Geometric Test");
    let ret = "X";
    let gf = gennifer::generating_function::GenFun::<Precision>::one().extend(
        &mut HashMap::new(),
        ret,
        &Distribution::Simple(SimpleDistribution::Geometric(PositiveProperFraction::new(
            0.5,
        )?)),
    )?;
    let (genfer_gf, mapping): (genfer::generating_function::GenFun<F32>, _) =
        gennifer::genfun_conversion::to_genfer_genfun(&gf);
    println!("Genifer {}", gf);
    println!("Genfer {}", genfer_gf);
    println!("mapping: {:?}", mapping);
    assert!(mapping.len() == 1);
    assert!(*mapping.get(ret).unwrap() == 0);

    let simplified_genfer_gf = genfer_gf.simplify();
    println!("Simplified Genfer GF: {}", simplified_genfer_gf);

    let substs = vec![F32::zero()];
    let expansion = simplified_genfer_gf.eval(&substs, 10);
    println!("Expansion: {}", expansion);
    Ok(())
}

fn test_poisson() -> Result<(), String> {
    println!("-----------------");
    println!("Posssion Test");
    let ret = "X";
    let gf = gennifer::generating_function::GenFun::<Precision>::one().extend(
        &mut HashMap::new(),
        ret,
        // &Distribution::Simple(SimpleDistribution::Poisson(Fraction(3.14))),
        &Distribution::Simple(SimpleDistribution::Poisson(Fraction(6.170807))),
    )?;
    let (genfer_gf, mapping): (genfer::generating_function::GenFun<F32>, _) =
        gennifer::genfun_conversion::to_genfer_genfun(&gf);
    println!("Genifer {}", gf);
    println!("Genfer {}", genfer_gf);
    println!("mapping: {:?}", mapping);
    assert!(mapping.len() == 1);
    assert!(*mapping.get(ret).unwrap() == 0);

    let simplified_genfer_gf = genfer_gf.simplify();
    println!("Simplified Genfer GF: {}", simplified_genfer_gf);

    let substs = vec![F32::one()];
    let expansion = simplified_genfer_gf.eval(&substs, 10);
    println!("Expected {}", expansion.array()[1]);
    Ok(())
}

fn test(gf: &genfer::generating_function::GenFun<F32>, v: f32) {
    println!("-----------------");
    println!("Testing GF: {}, v: {}", gf, v);
    let substs = vec![v.into()];
    let taylor = gf.eval(&substs, 10);
    println!("Taylor expansion: {}", taylor);
    //-----------------
    // Test
    // Taylor expansion: 3.0 + 1.0a
}
fn main() -> Result<(), String> {
    test_exponential()?;
    test_geometric()?;
    test_poisson()?;

    let x = genfer::generating_function::GenFun::<F32>::var(Var(0));
    test(&x, 0.0);
    test(&x, 1.0);
    test(&x, 2.0);
    test(&x, 3.0);

    let x_time_x = x.clone() * x.clone();
    test(&x_time_x, 0.0);
    test(&x_time_x, 1.0);
    test(&x_time_x, 2.0);
    test(&x_time_x, 3.0);

    let one_div_x = genfer::generating_function::GenFun::one() / x.clone();
    test(&one_div_x, 0.0);
    test(&one_div_x, 1.0);
    test(&one_div_x, 2.0);
    test(&one_div_x, 3.0);

    let one = genfer::generating_function::GenFun::one();
    test(&one, 0.0);
    test(&one, 1.0);
    test(&one, 2.0);
    test(&one, 3.0);

    test_geometric_0()?;
    Ok(())
}
