use crate::lang::Arithmetic;
use crate::lang::ProjectionExpr;
use crate::numbers::{Natural, PositiveProperFraction, Precision};
use crate::{ir as gir, surface};
use num_traits::{One, Zero};
use std::collections::HashMap;

//
// Source -> Gennifer IR
//

// Counter for generating unique variable names
static mut COUNTER: usize = 0;

fn fresh() -> String {
    unsafe {
        COUNTER += 1;
        format!("tmp_{}", COUNTER)
    }
}

fn create_xor_expr(a: gir::BoolExpr, b: gir::BoolExpr) -> gir::Expr {
    let condition = gir::BoolExpr::or(
        gir::BoolExpr::and(a.clone(), gir::BoolExpr::not(b.clone())),
        gir::BoolExpr::and(gir::BoolExpr::not(a), b),
    );

    gir::Expr::IfThenElse(
        Box::new(condition),
        Box::new(gir::Expr::Const(1)),
        Box::new(gir::Expr::Const(0)),
    )
}

fn create_and_expr(a: gir::BoolExpr, b: gir::BoolExpr) -> gir::Expr {
    let condition = gir::BoolExpr::and(a, b);

    gir::Expr::IfThenElse(
        Box::new(condition),
        Box::new(gir::Expr::Const(1)),
        Box::new(gir::Expr::Const(0)),
    )
}

fn create_or_expr(a: gir::BoolExpr, b: gir::BoolExpr) -> gir::Expr {
    let condition = gir::BoolExpr::or(a, b);

    gir::Expr::IfThenElse(
        Box::new(condition),
        Box::new(gir::Expr::Const(1)),
        Box::new(gir::Expr::Const(0)),
    )
}

fn mk_nested_tuple(mut exprs: Vec<gir::Expr>) -> gir::Expr {
    if exprs.is_empty() {
        panic!("mk_nested_tuple: empty vec");
    }

    let mut result = exprs.pop().unwrap();
    while let Some(expr) = exprs.pop() {
        result = gir::Expr::Tuple(Box::new(expr), Box::new(result));
    }

    result
}

// https://github.com/SHoltzen/dice/blob/1d8a277636a428eb46164ba4639fab12a310f30c/lib/Passes.ml#L592
fn gen_adder_expr(sz: usize, a: gir::Expr, b: gir::Expr) -> gir::Expr {
    let a_name = fresh();
    let b_name = fresh();
    // let tmp_a = gir::Expr::Var(a_name.clone());
    // let tmp_b = gir::Expr::Var(b_name.clone());
    let carry_name = format!("carry{}", fresh());
    let output_name = format!("output{}", fresh());

    let lsb_idx = sz - 1;
    let halfout_name = format!("{}_0", output_name);
    let halfcarry_name = format!("{}_0", carry_name);

    // Extract MSB from both inputs
    let lsb_a = extract_bit(&a_name, lsb_idx, sz);
    let lsb_b = extract_bit(&b_name, lsb_idx, sz);

    // Half adder logic: sum = a XOR b, carry = a AND b
    let halfout_expr = create_xor_expr(
        gir::BoolExpr::Test(Box::new(lsb_a.clone())),
        gir::BoolExpr::Test(Box::new(lsb_b.clone())),
    );

    let halfcarry_expr = create_and_expr(
        gir::BoolExpr::Test(Box::new(lsb_a)),
        gir::BoolExpr::Test(Box::new(lsb_b)),
    );

    // Generate full adders for remaining bits
    let mut full_assignments = Vec::new();

    for bit in 0..(sz - 1) {
        let curidx = bit + 1;
        let curbit = sz - bit - 2;
        let curout_name = format!("{}_{}", output_name, curidx);
        let curinput_a = extract_bit(&a_name, curbit, sz);
        let curinput_b = extract_bit(&b_name, curbit, sz);
        let prevcarry_name = format!("{}_{}", carry_name, curidx - 1);
        let curcarry_name = format!("{}_{}", carry_name, curidx);

        let prevcarry = gir::Expr::Var(prevcarry_name);

        let curout_expr = create_xor_expr(
            gir::BoolExpr::Test(Box::new(prevcarry.clone())),
            gir::BoolExpr::Test(Box::new(create_xor_expr(
                gir::BoolExpr::Test(Box::new(curinput_a.clone())),
                gir::BoolExpr::Test(Box::new(curinput_b.clone())),
            ))),
        );

        let curcarry_expr = create_or_expr(
            gir::BoolExpr::and(
                gir::BoolExpr::Test(Box::new(prevcarry.clone())),
                gir::BoolExpr::Test(Box::new(curinput_a.clone())),
            ),
            gir::BoolExpr::or(
                gir::BoolExpr::and(
                    gir::BoolExpr::Test(Box::new(prevcarry)),
                    gir::BoolExpr::Test(Box::new(curinput_b.clone())),
                ),
                gir::BoolExpr::and(
                    gir::BoolExpr::Test(Box::new(curinput_a)),
                    gir::BoolExpr::Test(Box::new(curinput_b)),
                ),
            ),
        );

        full_assignments.push((curout_name, curout_expr));
        full_assignments.push((curcarry_name, curcarry_expr));
    }

    let mut output_vars = Vec::new();
    for idx in 0..sz {
        output_vars.push(gir::Expr::Var(format!("{}_{}", output_name, sz - 1 - idx)));
    }

    let inner = mk_nested_tuple(output_vars);

    // Build the let-bindings from innermost to outermost
    let mut result = inner;

    // Add full adder assignments (in reverse order for proper nesting)
    for (name, expr) in full_assignments.into_iter().rev() {
        result = gir::Expr::Let(name, Box::new(expr), Box::new(result));
    }
    result = gir::Expr::Let(halfcarry_name, Box::new(halfcarry_expr), Box::new(result));
    result = gir::Expr::Let(halfout_name, Box::new(halfout_expr), Box::new(result));
    result = gir::Expr::Let(b_name, Box::new(b), Box::new(result));
    result = gir::Expr::Let(a_name, Box::new(a), Box::new(result));

    result
}

fn gen_adder_expr_constant(sz: usize, a: gir::Expr, b: Natural) -> gir::Expr {
    // Convert constant to binary representation
    let b_bits = int_to_binary_be(b as usize, sz)
        .into_iter()
        .collect::<Vec<_>>();

    let a_name = fresh();
    let carry_name = format!("carry{}", fresh());
    let output_name = format!("output{}", fresh());

    let msb_idx = sz - 1;
    let halfout_name = format!("{}_0", output_name);
    let halfcarry_name = format!("{}_0", carry_name);

    let msb_a = extract_bit(&a_name, msb_idx, sz);
    let b_msb = b_bits[msb_idx]; // Constant bit value

    // Half adder logic with constant:
    let (halfout_expr, halfcarry_expr) = if b_msb {
        (
            create_xor_expr(
                gir::BoolExpr::Test(Box::new(msb_a.clone())),
                gir::BoolExpr::BoolLiteral(true),
            ),
            create_and_expr(
                gir::BoolExpr::Test(Box::new(msb_a)),
                gir::BoolExpr::BoolLiteral(true),
            ),
        )
    } else {
        (
            create_xor_expr(
                gir::BoolExpr::Test(Box::new(msb_a)),
                gir::BoolExpr::BoolLiteral(false),
            ),
            create_and_expr(
                gir::BoolExpr::BoolLiteral(false),
                gir::BoolExpr::BoolLiteral(false),
            ),
        )
    };

    // Generate full adders for remaining bits
    let mut full_assignments = Vec::new();

    for bit in 0..(sz - 1) {
        let curidx = bit + 1;
        let curbit = sz - bit - 2;
        let curout_name = format!("{}_{}", output_name, curidx);
        let curinput_a = extract_bit(&a_name, curbit, sz);
        let b_curbit = b_bits[curbit]; // Constant bit value
        let prevcarry_name = format!("{}_{}", carry_name, curidx - 1);
        let curcarry_name = format!("{}_{}", carry_name, curidx);

        let prevcarry = gir::Expr::Var(prevcarry_name);

        // Full adder logic with constant bit:
        let curout_expr = if b_curbit {
            create_xor_expr(
                gir::BoolExpr::Test(Box::new(prevcarry.clone())),
                gir::BoolExpr::Test(Box::new(create_xor_expr(
                    gir::BoolExpr::Test(Box::new(curinput_a.clone())),
                    gir::BoolExpr::BoolLiteral(true),
                ))),
            )
        } else {
            create_xor_expr(
                gir::BoolExpr::Test(Box::new(prevcarry.clone())),
                gir::BoolExpr::Test(Box::new(create_xor_expr(
                    gir::BoolExpr::Test(Box::new(curinput_a.clone())),
                    gir::BoolExpr::BoolLiteral(false),
                ))),
            )
        };

        // Carry logic with constant bit:
        let curcarry_expr = if b_curbit {
            create_or_expr(
                gir::BoolExpr::Test(Box::new(prevcarry)),
                gir::BoolExpr::Test(Box::new(curinput_a)),
            )
        } else {
            create_and_expr(
                gir::BoolExpr::Test(Box::new(prevcarry)),
                gir::BoolExpr::Test(Box::new(curinput_a)),
            )
        };

        full_assignments.push((curout_name, curout_expr));
        full_assignments.push((curcarry_name, curcarry_expr));
    }

    let mut output_vars = Vec::new();
    for idx in 0..sz {
        output_vars.push(gir::Expr::Var(format!("{}_{}", output_name, sz - 1 - idx)));
    }

    let inner = mk_nested_tuple(output_vars);
    let mut result = inner;

    for (name, expr) in full_assignments.into_iter().rev() {
        result = gir::Expr::Let(name, Box::new(expr), Box::new(result));
    }
    result = gir::Expr::Let(halfcarry_name, Box::new(halfcarry_expr), Box::new(result));
    result = gir::Expr::Let(halfout_name, Box::new(halfout_expr), Box::new(result));
    result = gir::Expr::Let(a_name, Box::new(a), Box::new(result));

    result
}

// x + y + 42
// Arithmetic {
//     terms: vec![
//         (None, "x"),
//         (None, "y"),
//     ],
//     offset: Some(2)
// };

fn desugar_arithmetic(arith: &Arithmetic, bit_width: usize) -> gir::Expr {
    let Arithmetic { terms, offset } = arith;

    let mut result = if terms.is_empty() {
        // I think the parser guarantees this
        panic!("Empty arithmetic terms")
    } else {
        // Start with the first term
        let (first_coeff, first_var) = &terms[0];
        let first_var_expr = gir::Expr::Var(first_var.clone());

        match first_coeff {
            None | Some(1) => {
                // No coefficient or coefficient of 1: just use the variable
                first_var_expr
            }
            Some(_c) => {
                // Coefficient > 1: repeated addition
                // let mut term_result = first_var_expr.clone();
                // for _ in 1..*c {
                //     term_result = gen_adder_expr(bit_width, term_result, first_var_expr.clone());
                // }
                // term_result
                panic!("Multiplier not yet implemented")
            }
        }
    };

    for (coeff, var_name) in terms.iter().skip(1) {
        let var_expr = gir::Expr::Var(var_name.clone());

        match coeff {
            None | Some(1) => {
                result = gen_adder_expr(bit_width, result, var_expr);
            }
            Some(_c) => {
                // Coefficient > 1: result = result + (c * var)
                // let mut term_result = var_expr.clone();
                // for _ in 1..*c {
                //     term_result = gen_adder_expr(bit_width, term_result, var_expr.clone());
                // }
                // result = gen_adder_expr(bit_width, result, term_result);
                panic!("Multiplier not yet implemented")
            }
        }
    }

    // Add the offset constant if present
    if let Some(offset_value) = offset {
        result = gen_adder_expr_constant(bit_width, result, *offset_value);
    }

    result
}

fn num_binary_digits(n: usize) -> usize {
    if n == 0 {
        1
    } else {
        (n as f64).log2().floor() as usize + 1
    }
}

// // convert integer -> binary representation (little endian)
// fn int_to_binary_le(mut n: usize, num_bits: usize) -> Vec<bool> {
//     let mut bits = Vec::with_capacity(num_bits);
//     for _ in 0..num_bits {
//         bits.push(n & 1 == 1);
//         n >>= 1;
//     }
//     bits
// }

fn int_to_binary_be(n: usize, num_bits: usize) -> Vec<bool> {
    let mut bits = Vec::with_capacity(num_bits);

    // Build bits from MSB to LSB
    for i in (0..num_bits).rev() {
        bits.push((n >> i) & 1 == 1);
    }

    bits
}

fn bit_var_name(i: usize) -> String {
    format!("d_{}", i)
}

fn desugar_categorical(probs: &[PositiveProperFraction<Precision>]) -> gir::Expr {
    let n_outcomes = probs.len();
    if n_outcomes == 0 {
        panic!("Empty categorical distribution");
    }

    // Calculate the number of bits needed
    let num_bits = num_binary_digits(n_outcomes - 1);

    // Generate bit assignments from least significant to most significant
    let mut result_expr = create_tuple_result(num_bits);

    for bit_idx in (0..num_bits).rev() {
        let bit_var = bit_var_name(bit_idx);
        let bit_expr = generate_bit_expression(bit_idx, num_bits, probs);
        result_expr = gir::Expr::Let(bit_var, Box::new(bit_expr), Box::new(result_expr));
    }

    result_expr
}

// For bit blasting
// This returns a tuple of (d_0, (d_1, (d_2, ...))) - right-nested like Racket cons list
fn create_tuple_result(num_bits: usize) -> gir::Expr {
    if num_bits == 0 {
        panic!("Cannot create tuple with 0 bits");
    }

    if num_bits == 1 {
        return gir::Expr::Var(bit_var_name(0));
    }

    // Build from right to left (last bit first)
    let mut result = gir::Expr::Var(bit_var_name(num_bits - 1));

    for i in (0..(num_bits - 1)).rev() {
        result = gir::Expr::Tuple(Box::new(gir::Expr::Var(bit_var_name(i))), Box::new(result));
    }

    result
}

// Extract bit from right-nested tuple structure: (d_0, (d_1, (d_2, d_3)))
// Like ca*d+r in Racket
fn extract_bit(var_name: &str, bit_idx: usize, total_bits: usize) -> gir::Expr {
    assert!(
        bit_idx < total_bits,
        "Bit index {} out of bounds for {} bits",
        bit_idx,
        total_bits
    );

    if total_bits == 1 {
        return gir::Expr::Var(var_name.to_string());
    }

    let mut current = gir::Expr::Var(var_name.to_string());

    if bit_idx == 0 {
        // car
        gir::Expr::Fst(Box::new(current))
    } else {
        // cd+r
        for _ in 0..bit_idx {
            current = gir::Expr::Snd(Box::new(current));
        }

        // Then car, if needed
        if bit_idx == total_bits - 1 {
            current
        } else {
            gir::Expr::Fst(Box::new(current))
        }
    }
}

// This generates the prob and the nested if for each let binding of d_x
fn generate_bit_expression(
    bit_idx: usize,
    num_bits: usize,
    probs: &[PositiveProperFraction<Precision>],
) -> gir::Expr {
    // For each possible assignment of previous bits, calculate conditional probability
    let num_prev_assignments = 1 << bit_idx; // 2^bit_idx

    if bit_idx == 0 {
        // First bit - unconditional probability
        let prob = calculate_bit_probability(bit_idx, num_bits, probs, &[]);
        return gir::Expr::Sample(gir::Distribution::Simple(
            gir::SimpleDistribution::Bernoulli(prob),
        ));
    }

    let mut conditions = Vec::new();

    for prev_assignment in 0..num_prev_assignments {
        let prev_bits = int_to_binary_be(prev_assignment, bit_idx);
        let prob = calculate_bit_probability(bit_idx, num_bits, probs, &prev_bits);
        let condition = build_condition_for_prev_bits(&prev_bits);
        conditions.push((condition, prob));
    }

    // Build nested if-then-else
    build_nested_ite(conditions)
}

fn calculate_bit_probability(
    bit_idx: usize,
    num_bits: usize,
    probs: &[PositiveProperFraction<Precision>],
    prev_bits: &[bool],
) -> PositiveProperFraction<Precision> {
    // we will unwrap the `new` constructor for `PositiveProperFraction` here
    // which means that we are always assuming that the number, N, is
    // 0 <= N <= 1

    // Start with zero probability
    // btw, we know these unwraps are safe - they always meet the condition
    // stated above because N = 0
    let mut prob_bit_true = PositiveProperFraction::new(0.0).unwrap();
    let mut prob_total = PositiveProperFraction::new(0.0).unwrap();

    for (outcome_idx, prob) in probs.iter().enumerate() {
        let outcome_bits = int_to_binary_be(outcome_idx, num_bits);

        // Check if this outcome is consistent with previous bits
        let mut consistent = true;
        for (i, &prev_bit) in prev_bits.iter().enumerate() {
            if outcome_bits[i] != prev_bit {
                consistent = false;
                break;
            }
        }

        // Basically consistent means we're on the same path
        // i.e we never "fail"
        // so the probability is just the sum
        if consistent {
            // TODO: implement binary operations directly on PositiveProperFraction using traits
            prob_total = PositiveProperFraction::new(*prob_total + **prob).unwrap();
            if outcome_bits[bit_idx] {
                prob_bit_true = PositiveProperFraction::new(*prob_bit_true + **prob).unwrap();
            }
        }
    }

    if prob_total == PositiveProperFraction::new(0.0).unwrap() {
        // No valid outcomes - return 0 probability
        PositiveProperFraction::new(0.0).unwrap()
    } else {
        // normalize for the cases where we don't follow the consistent path
        let conditional_prob = *prob_bit_true / *prob_total;
        PositiveProperFraction::new(conditional_prob).unwrap()
    }
}

fn build_condition_for_prev_bits(prev_bits: &[bool]) -> gir::BoolExpr {
    if prev_bits.is_empty() {
        return gir::BoolExpr::BoolLiteral(true);
    }

    let mut condition = if prev_bits[0] {
        gir::BoolExpr::Test(Box::new(gir::Expr::Var(bit_var_name(0))))
    } else {
        gir::BoolExpr::Not(Box::new(gir::BoolExpr::Test(Box::new(gir::Expr::Var(
            bit_var_name(0),
        )))))
    };

    for (i, &bit_value) in prev_bits.iter().enumerate().skip(1) {
        let bit_test = if bit_value {
            gir::BoolExpr::Test(Box::new(gir::Expr::Var(bit_var_name(i))))
        } else {
            gir::BoolExpr::Not(Box::new(gir::BoolExpr::Test(Box::new(gir::Expr::Var(
                bit_var_name(i),
            )))))
        };
        condition = gir::BoolExpr::And(Box::new(condition), Box::new(bit_test));
    }

    condition
}

// We want to build something like this from the list of (condition, prob) tuple:
// if condition_0 then
//     sample(Bernoulli(prob_0))
// else if condition_1 then
//     sample(Bernoulli(prob_1))
// else if condition_2 then
//     sample(Bernoulli(prob_2))
// else
//     sample(Bernoulli(prob_3))
fn build_nested_ite(
    mut conditions: Vec<(gir::BoolExpr, PositiveProperFraction<Precision>)>,
) -> gir::Expr {
    if conditions.is_empty() {
        panic!("No conditions provided");
    }

    if conditions.len() == 1 {
        let (_, prob) = conditions.pop().unwrap();
        return gir::Expr::Sample(gir::Distribution::Simple(
            gir::SimpleDistribution::Bernoulli(prob),
        ));
    }

    // Build nested if-then-else from the conditions
    let (_last_condition, last_prob) = conditions.pop().unwrap();
    let mut result = gir::Expr::Sample(gir::Distribution::Simple(
        gir::SimpleDistribution::Bernoulli(last_prob),
    ));

    while let Some((condition, prob)) = conditions.pop() {
        let then_expr = gir::Expr::Sample(gir::Distribution::Simple(
            gir::SimpleDistribution::Bernoulli(prob),
        ));
        result = gir::Expr::IfThenElse(Box::new(condition), Box::new(then_expr), Box::new(result));
    }

    result
}

pub fn desugar_surface_expr(source: &surface::Expr) -> gir::Expr {
    let (result, _) = desugar_surface_expr_with_type(&mut HashMap::new(), source);
    result
}

fn type_check_projection(
    context: &HashMap<String, surface::Type>,
    proj: &ProjectionExpr,
) -> Result<surface::Type, String> {
    match proj {
        ProjectionExpr::Var(var) => context
            .get(var)
            .ok_or(format!("Variable {} not found", var))
            .cloned(),
        // .map(|t| t.clone()),
        ProjectionExpr::Fst(inner) => match type_check_projection(context, inner)? {
            surface::Type::Tuple(first, _) => Ok(*first),
            _ => Err("fst requires tuple".to_string()),
        },
        ProjectionExpr::Snd(inner) => match type_check_projection(context, inner)? {
            surface::Type::Tuple(_, second) => Ok(*second),
            _ => Err("snd requires tuple".to_string()),
        },
    }
}

pub fn desugar_surface_expr_with_type(
    context: &mut HashMap<String, surface::Type>,
    source: &surface::Expr,
) -> (gir::Expr, surface::Type) {
    match source {
        surface::Expr::Var(v) => {
            let ty = context
                .get(v)
                // .expect(&format!("Variable {} not found", v))
                .unwrap_or_else(|| panic!("Variable {} not found", v))
                .clone();
            (gir::Expr::Var(v.to_string()), ty)
        }

        surface::Expr::Const(c) => {
            let ty = if *c == 0 || *c == 1 {
                surface::Type::Bool
            } else {
                surface::Type::Nat
            };
            (gir::Expr::Const(*c), ty)
        }

        surface::Expr::Tuple(first, second) => {
            let (first_expr, first_ty) = desugar_surface_expr_with_type(context, first);
            let (second_expr, second_ty) = desugar_surface_expr_with_type(context, second);
            let tuple_ty = surface::Type::Tuple(Box::new(first_ty), Box::new(second_ty));
            (
                gir::Expr::Tuple(Box::new(first_expr), Box::new(second_expr)),
                tuple_ty,
            )
        }

        surface::Expr::Lookup(p) => {
            let ty = type_check_projection(context, p).expect("Projection type check failed");
            (gir::Expr::Lookup(p.clone()), ty)
        }

        surface::Expr::Fst(e) => {
            let (expr, expr_ty) = desugar_surface_expr_with_type(context, e);
            match expr_ty {
                surface::Type::Tuple(first_ty, _) => (gir::Expr::Fst(Box::new(expr)), *first_ty),
                _ => panic!("Type error: fst requires a tuple"),
            }
        }

        surface::Expr::Snd(e) => {
            let (expr, expr_ty) = desugar_surface_expr_with_type(context, e);
            match expr_ty {
                surface::Type::Tuple(_, second_ty) => (gir::Expr::Snd(Box::new(expr)), *second_ty),
                _ => panic!("Type error: snd requires a tuple"),
            }
        }

        surface::Expr::LoopSum(bound, body) => {
            let (bound_expr, _) = desugar_surface_expr_with_type(context, bound);
            let (body_expr, _) = desugar_surface_expr_with_type(context, body);
            (
                gir::Expr::LoopSum(Box::new(bound_expr), Box::new(body_expr)),
                surface::Type::Nat,
            )
        }

        surface::Expr::Let(name, value, body) => {
            let (value_expr, value_ty) = desugar_surface_expr_with_type(context, value);

            // Add variable to context
            let old_binding = context.insert(name.clone(), value_ty);

            let (body_expr, body_ty) = desugar_surface_expr_with_type(context, body);

            // Restore old binding or remove
            match old_binding {
                Some(old_ty) => {
                    context.insert(name.clone(), old_ty);
                }
                None => {
                    context.remove(name);
                }
            }

            (
                gir::Expr::Let(name.to_string(), Box::new(value_expr), Box::new(body_expr)),
                body_ty,
            )
        }

        surface::Expr::Sample(surface::Distribution::Simple(
            surface::SimpleDistribution::Dirac(n),
        )) => {
            let ty = if *n == 0 || *n == 1 {
                surface::Type::Bool
            } else {
                surface::Type::Nat
            };
            (
                gir::Expr::Sample(gir::Distribution::Simple(gir::SimpleDistribution::Dirac(
                    *n,
                ))),
                ty,
            )
        }

        surface::Expr::Sample(surface::Distribution::Simple(
            surface::SimpleDistribution::Bernoulli(p),
        )) => (
            gir::Expr::Sample(gir::Distribution::Simple(
                gir::SimpleDistribution::Bernoulli(p.clone()),
            )),
            surface::Type::Bool,
        ),

        surface::Expr::Sample(surface::Distribution::Simple(
            surface::SimpleDistribution::Binomial(n, p),
        )) => (
            gir::Expr::Sample(gir::Distribution::Simple(
                gir::SimpleDistribution::Binomial(*n, p.clone()),
            )),
            surface::Type::Nat,
        ),

        surface::Expr::Sample(surface::Distribution::Simple(
            surface::SimpleDistribution::NegBinomial(n, p),
        )) => (
            gir::Expr::Sample(gir::Distribution::Simple(
                gir::SimpleDistribution::NegBinomial(*n, p.clone()),
            )),
            surface::Type::Nat,
        ),

        surface::Expr::Sample(surface::Distribution::Simple(
            surface::SimpleDistribution::Geometric(p),
        )) => (
            gir::Expr::Sample(gir::Distribution::Simple(
                gir::SimpleDistribution::Geometric(p.clone()),
            )),
            surface::Type::Nat,
        ),

        surface::Expr::Sample(surface::Distribution::Simple(
            surface::SimpleDistribution::Poisson(lambda),
        )) => (
            gir::Expr::Sample(gir::Distribution::Simple(gir::SimpleDistribution::Poisson(
                lambda.clone(),
            ))),
            surface::Type::Nat,
        ),

        surface::Expr::Sample(surface::Distribution::Simple(
            surface::SimpleDistribution::Uniform(a, b),
        )) => {
            let ty = if a.is_zero() && b.is_one() {
                surface::Type::UnitInterval
            } else {
                surface::Type::Real
            };
            (
                gir::Expr::Sample(gir::Distribution::Simple(gir::SimpleDistribution::Uniform(
                    a.clone(),
                    b.clone(),
                ))),
                ty,
            )
        }

        surface::Expr::Sample(surface::Distribution::Simple(
            surface::SimpleDistribution::Exponential(lambda),
        )) => (
            gir::Expr::Sample(gir::Distribution::Simple(
                gir::SimpleDistribution::Exponential(lambda.clone()),
            )),
            surface::Type::Real,
        ),

        surface::Expr::Sample(surface::Distribution::Simple(
            surface::SimpleDistribution::Gamma(alpha, beta),
        )) => (
            gir::Expr::Sample(gir::Distribution::Simple(gir::SimpleDistribution::Gamma(
                alpha.clone(),
                beta.clone(),
            ))),
            surface::Type::Real,
        ),

        surface::Expr::Sample(surface::Distribution::Compound(
            surface::CompoundDistribution::Poisson(lambda, y),
        )) => (
            gir::Expr::Sample(gir::Distribution::Compound(
                gir::CompoundDistribution::Poisson(lambda.clone(), y.clone()),
            )),
            surface::Type::Nat,
        ),

        surface::Expr::Sample(surface::Distribution::Compound(
            surface::CompoundDistribution::Bernoulli(y),
        )) => (
            gir::Expr::Sample(gir::Distribution::Compound(
                gir::CompoundDistribution::Bernoulli(y.clone()),
            )),
            surface::Type::Bool,
        ),

        surface::Expr::Observe(e) => {
            let bool_expr = desugar_surface_bool_expr_with_type(context, e.as_ref());
            (gir::Expr::Observe(Box::new(bool_expr)), surface::Type::Bool)
        }

        surface::Expr::IfThenElse(condition, then_expr, else_expr) => {
            let bool_cond = desugar_surface_bool_expr_with_type(context, condition.as_ref());
            let (then_desugared, then_ty) = desugar_surface_expr_with_type(context, then_expr);
            let (else_desugared, else_ty) = desugar_surface_expr_with_type(context, else_expr);

            // Check that both branches have the same type
            if then_ty != else_ty {
                panic!("Type error: IfThenElse branches must have the same type");
            }

            (
                gir::Expr::IfThenElse(
                    Box::new(bool_cond),
                    Box::new(then_desugared),
                    Box::new(else_desugared),
                ),
                then_ty,
            )
        }

        surface::Expr::Annotated(e, ty) => {
            let (expr, inferred_ty) = desugar_surface_expr_with_type(context, e);

            // Check that annotation matches inferred type
            if inferred_ty.is_subtype(ty) {
                panic!(
                    "Type error: annotation {:?} doesn't match inferred type {:?}",
                    ty, inferred_ty
                );
            }

            (expr, ty.clone())
        }

        // Bit blasting cases
        surface::Expr::Sample(surface::Distribution::Simple(
            surface::SimpleDistribution::Categorial(probs),
        )) => {
            let num_bits = num_binary_digits(probs.len() - 1) as u16;
            (desugar_categorical(probs), surface::Type::FNat(num_bits))
        }

        surface::Expr::Arithmetic(arith) => {
            let bit_width = infer_arithmetic_bit_width(context, arith);
            let bit_tuple_type = create_bit_tuple_type(bit_width);
            (desugar_arithmetic(arith, bit_width), bit_tuple_type)
        }
    }
}

fn get_bit_width_from_context(context: &HashMap<String, surface::Type>, var_name: &str) -> usize {
    match context.get(var_name) {
        Some(ty) => get_bit_width_from_type(ty),
        None => panic!("Variable '{}' not found in context", var_name),
    }
}

fn infer_arithmetic_bit_width(
    context: &HashMap<String, surface::Type>,
    arith: &Arithmetic,
) -> usize {
    let Arithmetic { terms, offset: _ } = arith;

    if terms.is_empty() {
        panic!("Arithmetic expression has no variables");
    }

    // Get bit width from the first variable
    let first_var_name = &terms[0].1;
    let expected_bit_width = get_bit_width_from_context(context, first_var_name);

    // Ensure all variables have the same bit width
    for (_, var_name) in terms.iter().skip(1) {
        let var_bit_width = get_bit_width_from_context(context, var_name);
        if var_bit_width != expected_bit_width {
            panic!(
                "All variables in arithmetic expression must have the same bit width. \
                 Expected {} bits (from '{}'), but '{}' has {} bits",
                expected_bit_width, first_var_name, var_name, var_bit_width
            );
        }
    }
    // TODO: we also need to check the bit of the offset

    expected_bit_width
}

fn create_bit_tuple_type(num_bits: usize) -> surface::Type {
    if num_bits == 1 {
        surface::Type::Bool
    } else {
        // Create right-nested tuple: (Bool, (Bool, (Bool, Bool)))
        let mut result = surface::Type::Bool;
        for _ in 1..num_bits {
            result = surface::Type::Tuple(Box::new(surface::Type::Bool), Box::new(result));
        }
        result
    }
}

fn get_bit_width_from_type(ty: &surface::Type) -> usize {
    match ty {
        surface::Type::Bool => 1,
        surface::Type::FNat(n) => *n as usize,
        surface::Type::Nat => 16, // represented as u16
        // not too sure about this
        surface::Type::Tuple(l, r) => get_bit_width_from_type(l) + get_bit_width_from_type(r),
        _ => panic!("Cannot determine bit width for type {:?}", ty),
    }
}

fn desugar_surface_bool_expr_with_type(
    context: &HashMap<String, surface::Type>,
    source: &surface::BoolExpr,
) -> gir::BoolExpr {
    match source {
        surface::BoolExpr::BoolLiteral(b) => gir::BoolExpr::BoolLiteral(*b),
        surface::BoolExpr::Test(e) => {
            let (expr, _) = desugar_surface_expr_with_type(&mut context.clone(), e.as_ref());
            gir::BoolExpr::Test(Box::new(expr))
        }
        surface::BoolExpr::Lookup(p) => gir::BoolExpr::Lookup(p.clone()),
        surface::BoolExpr::Not(b) => gir::BoolExpr::Not(Box::new(
            desugar_surface_bool_expr_with_type(context, b.as_ref()),
        )),
        surface::BoolExpr::And(p, q) => gir::BoolExpr::And(
            Box::new(desugar_surface_bool_expr_with_type(context, p.as_ref())),
            Box::new(desugar_surface_bool_expr_with_type(context, q.as_ref())),
        ),
        surface::BoolExpr::TestEquality(var, val, ty) => {
            let var_type = context
                .get(var)
                .unwrap_or_else(|| panic!("Variable {} not found", var));
            // .expect(&format!("Variable {} not found", var));
            let bit_width = get_bit_width_from_type(var_type);
            // Ensure that the size var_type and _ty are the same
            match ty {
                // TODO: We only want to desugar if ty is FNat[x],
                // gir should actually have TestEquality for infinite support variables
                None => desugar_equality_test(var, *val, bit_width),
                Some(ty) => {
                    let val_size = get_bit_width_from_type(ty);
                    assert!(
                        val_size == bit_width,
                        "Ensure sure bit widths align for test equality"
                    );
                    desugar_equality_test(var, *val, bit_width)
                }
            }
        }
    }
}

fn combine_with_and(mut tests: Vec<gir::BoolExpr>) -> gir::BoolExpr {
    match tests.len() {
        0 => gir::BoolExpr::BoolLiteral(true),
        1 => tests.pop().unwrap(),
        _ => {
            let first = tests.remove(0);
            let rest = combine_with_and(tests);
            gir::BoolExpr::And(Box::new(first), Box::new(rest))
        }
    }
}

// Generate boolean expression to test if variable equals the given number
fn desugar_equality_test(var_name: &str, target_value: Natural, num_bits: usize) -> gir::BoolExpr {
    let target_bits = int_to_binary_be(target_value as usize, num_bits);

    let mut bit_tests = Vec::new();

    for (bit_idx, &expected_bit) in target_bits.iter().enumerate() {
        let bit_expr = extract_bit(var_name, bit_idx, num_bits);
        let bit_test = gir::BoolExpr::Test(Box::new(bit_expr));

        let final_test = if expected_bit {
            bit_test
        } else {
            gir::BoolExpr::Not(Box::new(bit_test))
        };

        bit_tests.push(final_test);
    }

    combine_with_and(bit_tests)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_smoker() {
        // variable Smoker {
        //   type discrete [ 2 ] { True, False };
        // }

        // probability ( Smoker ) {
        //    table 0.3, 0.7;
        // }
        let probs = vec![
            PositiveProperFraction::new(0.3).unwrap(),
            PositiveProperFraction::new(0.7).unwrap(),
        ];

        // let d_0 = sample(Bern(0.700000)) in
        // d_0
        let expr = desugar_categorical(&probs);
        println!("{:?}", expr);
        // Let("d_0", Sample(Simple(Bernoulli(PositiveProperFraction(0.7)))), Var("d_0"))
    }

    #[test]
    fn test_pollution() {
        // variable Pollution {
        //   type discrete [ 3 ] { low, medium, high };
        // }

        // probability ( Pollution ) {
        //    table 0.5, 0.4, 0.1;
        // }

        let probs = vec![
            PositiveProperFraction::new(0.5).unwrap(),
            PositiveProperFraction::new(0.4).unwrap(),
            PositiveProperFraction::new(0.1).unwrap(),
        ];

        let expr = desugar_categorical(&probs);
        println!("{:?}", expr);

        // Let("d_0", Sample(Simple(Bernoulli(PositiveProperFraction(0.4)))),
        // Let("d_1", IfThenElse(Not(Test(Var("d_0"))), Sample(Simple(Bernoulli(PositiveProperFraction(0.16666666666666669)))), Sample(Simple(Bernoulli(PositiveProperFraction(0.0))))),
        // Tuple(Var("d_0"), Var("d_1"))))
    }

    #[test]
    fn test_weather() {
        //  variable Weather {
        //    type discrete [4] { sunny, cloudy, rainy, stormy };
        //  }
        //
        //  probability (Weather) {
        //    table 0.4, 0.3, 0.2, 0.1;
        //  }

        let probs = vec![
            PositiveProperFraction::new(0.4).unwrap(),
            PositiveProperFraction::new(0.3).unwrap(),
            PositiveProperFraction::new(0.2).unwrap(),
            PositiveProperFraction::new(0.1).unwrap(),
        ];

        let expr = desugar_categorical(&probs);
        println!("{:?}", expr);
        // Let("d_0", Sample(Simple(Bernoulli(PositiveProperFraction(0.4000000000000001)))),
        // Let("d_1", IfThenElse(Not(Test(Var("d_0"))), Sample(Simple(Bernoulli(PositiveProperFraction(0.3333333333333333)))), Sample(Simple(Bernoulli(PositiveProperFraction(0.25))))),
        // Tuple(Var("d_0"), Var("d_1"))))
    }

    #[test]
    fn test_day_of_week() {
        // variable Day {
        //   type discrete [7] { mon, tue, wed, thu, fri, sat, sun };
        // }

        // probability (Day) {
        //   table 0.15, 0.15, 0.15, 0.15, 0.15, 0.125, 0.125;
        // }

        let probs = vec![
            PositiveProperFraction::new(0.15).unwrap(),
            PositiveProperFraction::new(0.15).unwrap(),
            PositiveProperFraction::new(0.15).unwrap(),
            PositiveProperFraction::new(0.15).unwrap(),
            PositiveProperFraction::new(0.15).unwrap(),
            PositiveProperFraction::new(0.125).unwrap(),
            PositiveProperFraction::new(0.125).unwrap(),
        ];

        let expr = desugar_categorical(&probs);
        println!("{:?}", expr);
        // Let("d_0", Sample(Simple(Bernoulli(PositiveProperFraction(0.425)))),
        // Let("d_1", IfThenElse(Not(Test(Var("d_0"))), Sample(Simple(Bernoulli(PositiveProperFraction(0.47826086956521746)))), Sample(Simple(Bernoulli(PositiveProperFraction(0.35294117647058826))))),
        // Let("d_2", IfThenElse(And(Not(Test(Var("d_0"))), Not(Test(Var("d_1")))), Sample(Simple(Bernoulli(PositiveProperFraction(0.5)))),
        //            IfThenElse(And(Test(Var("d_0")), Not(Test(Var("d_1")))), Sample(Simple(Bernoulli(PositiveProperFraction(0.45454545454545453)))),
        //            IfThenElse(And(Not(Test(Var("d_0"))), Test(Var("d_1"))), Sample(Simple(Bernoulli(PositiveProperFraction(0.45454545454545453)))), Sample(Simple(Bernoulli(PositiveProperFraction(0.0))))))),
        // Tuple(Tuple(Var("d_0"), Var("d_1")), Var("d_2")))))
    }

    #[test]
    fn test_equality_one() {
        // Test X = 1 should generate: Test(X) for single bit
        let expr = desugar_equality_test("X", 6, 3);
        println!("{:?}", expr);
    }

    #[test]
    fn test_desugar() {
        let probs = vec![
            PositiveProperFraction::new(0.25).unwrap(),
            PositiveProperFraction::new(0.25).unwrap(),
            PositiveProperFraction::new(0.25).unwrap(),
            PositiveProperFraction::new(0.25).unwrap(),
        ];
        let surface = surface::Expr::Sample(surface::Distribution::Simple(
            surface::SimpleDistribution::Categorial(probs),
        ));
        let expr = desugar_surface_expr(&surface);
        println!("{:?}", expr);
        // Let("d_0", Sample(Simple(Bernoulli(PositiveProperFraction(0.5)))),
        // Let("d_1", IfThenElse(Not(Test(Var("d_0"))), Sample(Simple(Bernoulli(PositiveProperFraction(0.5)))), Sample(Simple(Bernoulli(PositiveProperFraction(0.5))))),
        // Tuple(Var("d_0"), Var("d_1"))))
    }

    #[test]
    fn test_1_bit_adder() {
        // Test 1-bit addition: a + b where a, b are single bits
        let a = gir::Expr::Var("a".to_string());
        let b = gir::Expr::Var("b".to_string());

        let expr = gen_adder_expr(1, a, b);
        println!("{:?}", expr);
        // Let("tmp_1", Var("a"),
        // Let("tmp_2", Var("b"),
        // Let("outputtmp_4_0", IfThenElse(Not(And(Not(And(Test(Var("tmp_1")), Not(Test(Var("tmp_2"))))), Not(And(Not(Test(Var("tmp_1"))), Test(Var("tmp_2")))))), Const(1), Const(0)),
        // Let("carrytmp_3_0", IfThenElse(And(Test(Var("tmp_1")), Test(Var("tmp_2"))), Const(1), Const(0)),
        // Var("outputtmp_4_0")))))
    }

    #[test]
    fn test_2_bit_adder() {
        // Test 2-bit addition: (a1,a0) + (b1,b0)
        let a = gir::Expr::Tuple(
            Box::new(gir::Expr::Var("a0".to_string())),
            Box::new(gir::Expr::Var("a1".to_string())),
        );
        let b = gir::Expr::Tuple(
            Box::new(gir::Expr::Var("b0".to_string())),
            Box::new(gir::Expr::Var("b1".to_string())),
        );

        let result = gen_adder_expr(2, a, b);

        println!("{:?}", result);
        // Let("tmp_1", Tuple(Var("a0"), Var("a1")),
        // Let("tmp_2", Tuple(Var("b0"), Var("b1")),
        // Let("outputtmp_4_0", IfThenElse(Not(And(Not(And(Test(Snd(Var("tmp_1"))), Not(Test(Snd(Var("tmp_2")))))), Not(And(Not(Test(Snd(Var("tmp_1")))), Test(Snd(Var("tmp_2"))))))), Const(1), Const(0)),
        // Let("carrytmp_3_0", IfThenElse(And(Test(Snd(Var("tmp_1"))), Test(Snd(Var("tmp_2")))), Const(1), Const(0)),
        // Let("outputtmp_4_1", IfThenElse(Not(And(Not(And(Test(Var("carrytmp_3_0")), Not(Test(IfThenElse(Not(And(Not(And(Test(Fst(Var("tmp_1"))), Not(Test(Fst(Var("tmp_2")))))), Not(And(Not(Test(Fst(Var("tmp_1")))), Test(Fst(Var("tmp_2"))))))), Const(1), Const(0)))))), Not(And(Not(Test(Var("carrytmp_3_0"))), Test(IfThenElse(Not(And(Not(And(Test(Fst(Var("tmp_1"))), Not(Test(Fst(Var("tmp_2")))))), Not(And(Not(Test(Fst(Var("tmp_1")))), Test(Fst(Var("tmp_2"))))))), Const(1), Const(0))))))), Const(1), Const(0)),
        // Let("carrytmp_3_1", IfThenElse(Not(And(Not(And(Test(Var("carrytmp_3_0")), Test(Fst(Var("tmp_1"))))), And(Not(And(Test(Var("carrytmp_3_0")), Test(Fst(Var("tmp_2"))))), Not(And(Test(Fst(Var("tmp_1"))), Test(Fst(Var("tmp_2")))))))), Const(1), Const(0)),
        // Tuple(Var("outputtmp_4_1"), Var("outputtmp_4_0"))))))))
    }
}
