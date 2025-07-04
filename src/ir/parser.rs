use crate::{
    ir::lang::{BoolExpr, CompoundDistribution, Distribution, Expr, SimpleDistribution},
    lang::{Arithmetic, ProjectionExpr},
    numbers::{Fraction, PositiveProperFraction, Precision},
};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0},
    combinator::{map, opt, recognize, verify},
    error::{context, ParseError},
    multi::{many0_count, separated_list0},
    number::complete::{double, float},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    AsChar, Finish, InputIter, InputTakeAtPosition, Parser, Slice,
};
use std::{fmt::Debug, ops::RangeFrom, str::FromStr};

type IResult<I, O> = nom::IResult<I, O, nom::error::VerboseError<I>>;

const RESERVED_KEYWORDS: &[&str] = &[
    "if",
    "then",
    "else",
    "true",
    "false",
    "let",
    "in",
    "loop",
    "sum",
    "fst",
    "snd",
    "sample",
    "observe",
    "Bernoulli",
    "Poisson",
    "Geometric",
    "Binomial",
    "Exponential",
    "Gamma",
];

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn ws<I, O, E: ParseError<I>, F: Parser<I, O, E>>(f: F) -> impl FnMut(I) -> nom::IResult<I, O, E>
where
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    preceded(multispace0, f)
}

fn parenthesized<I, O, E: ParseError<I>, F: Parser<I, O, E>>(
    f: F,
) -> impl FnMut(I) -> nom::IResult<I, O, E>
where
    I: Slice<RangeFrom<usize>> + InputIter + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    <I as InputIter>::Item: AsChar,
{
    delimited(ws(char('(')), f, ws(char(')')))
}

/// Parse the entire source input
pub fn parse(input: &str) -> Result<Expr, nom::error::VerboseError<&str>> {
    let (input, expr) = terminated(parse_expr, multispace0)(input).finish()?;
    if input.is_empty() {
        Ok(expr)
    } else {
        Err(nom::error::VerboseError::from_error_kind(
            input,
            nom::error::ErrorKind::Eof,
        ))
    }
}

/// Parse an expression
fn parse_expr(input: &str) -> IResult<&str, Expr> {
    fn parse_expr_lookup(input: &str) -> IResult<&str, Expr> {
        let (input, _) = multispace0(input)?;
        let (input, _) = multispace0(input)?;
        // println!("[parse_expr_lookup] input: {:?}", input[0..20].to_string());
        if input.starts_with("let") {
            return parse_let_expr(input);
        } else if input.starts_with("if") {
            return parse_if_then_else(input);
        } else if input.starts_with("loop") {
            return parse_loop_sum(input);
        }
        alt((
            parenthesized(parse_expr),
            parse_sample,
            parse_observed,
            parse_tuple,
            map(parse_bool_native, |v| Expr::Const(v.into())),
            parse_affine,
            // parse_fst,
            // parse_snd,
            map(parse_look_up, |p: ProjectionExpr| Expr::Lookup(Box::new(p))),
            map(parenthesized(parse_bool_expr), |b| {
                Expr::ifthenelse(b, Expr::Const(1), Expr::Const(0))
            }),
        ))(input)
    }
    context(
        "\
        <expression> ::= \
            '(' <expression> ')' | \
            <identifier> | <arithmetic> | \
            '(' <expression> ',' <expression> ')' | \"(fst\" <expression> \")\" | \"(snd\" <expression> \")\"| \
            'sample' '(' <SimpleDistribution> ')' | \"if\" <expression> \"then\" <expression> \"else\" <expression> | \"let\" <identifier> \"=\" <expression> \"in\" <expression> \
        ", 
        parse_expr_lookup
    )(input)
}

fn parse_term<N>(input: &str) -> IResult<&str, (Option<N>, String)>
where
    N: FromStr,
    N::Err: Debug,
{
    context(
        "natural * identifier",
        pair(
            map(opt(pair(parse_natural, ws(char('*')))), |opt| {
                opt.map(|(n, _)| n)
            }),
            parse_identifier,
        ),
    )(input)
}

fn parse_linear<N>(input: &str) -> IResult<&str, Vec<(Option<N>, String)>>
where
    N: FromStr,
    N::Err: Debug,
{
    context(
        "linear expression: f_1 * x_1 + ... + f_n * x_n",
        separated_list0(ws(char('+')), parse_term),
    )(input)
}
/// Parse an affine( expression
fn parse_affine(input: &str) -> IResult<&str, Expr> {
    context("a ::= f1*x1 + f2*x2 + ... + q", |input| {
        let (input, terms) = parse_linear(input)?;
        let (input, constant) = if terms.is_empty() {
            let (input, constant) = preceded(opt(ws(char('+'))), parse_natural)(input)?;
            (input, Some(constant))
        } else {
            opt(preceded(ws(char('+')), parse_natural))(input)?
        };
        Ok((
            input,
            Expr::arithmetic(Arithmetic {
                terms: terms.into_iter().collect(),
                offset: constant,
            }),
        ))
    })(input)
}

/// Parse a natural number
fn parse_natural<N>(input: &str) -> IResult<&str, N>
where
    N: FromStr,
    N::Err: Debug,
{
    context(
        "natural number",
        map(ws(digit1), |n: &str| n.parse().unwrap()),
    )(input)
}

/// Parse an identifier
fn parse_identifier(input: &str) -> IResult<&str, String> {
    let identifier = recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ));
    context(
        "identifier",
        context(
            "check reserved keywords",
            verify(map(ws(identifier), String::from), |s: &str| {
                !RESERVED_KEYWORDS.contains(&s)
            }),
        ),
    )(input)
}

/// Parse a tuple
fn parse_tuple(input: &str) -> IResult<&str, Expr> {
    context(
        "e -> (e, e)",
        map(
            delimited(
                ws(char('(')),
                separated_pair(parse_expr, ws(char(',')), parse_expr),
                ws(char(')')),
            ),
            |(e1, e2)| Expr::Tuple(Box::new(e1), Box::new(e2)),
        ),
    )(input)
}

// /// Parse fst
// fn parse_fst(input: &str) -> IResult<&str, Expr> {
//     context(
//         "e -> fst e",
//         map(preceded(tag("fst"), parse_expr), |e| Expr::Fst(Box::new(e))),
//     )(input)
// }

// /// Parse snd
// fn parse_snd(input: &str) -> IResult<&str, Expr> {
//     context(
//         "e -> snd e",
//         map(preceded(tag("snd"), parse_expr), |e| Expr::Snd(Box::new(e))),
//     )(input)
// }

fn parse_distribution(input: &str) -> IResult<&str, Distribution> {
    context(
        "SimpleDistribution | CompoundDistribution",
        alt((
            map(parse_simple_distribution, Distribution::Simple),
            map(parse_compound_distribution, Distribution::Compound),
        )),
    )(input)
}
/// Parse sample
fn parse_sample(input: &str) -> IResult<&str, Expr> {
    context(
        "e -> sample (d) ",
        map(
            preceded(
                ws(tag("sample")),
                delimited(ws(char('(')), parse_distribution, ws(char(')'))),
            ),
            Expr::Sample,
        ),
    )(input)
}

fn parse_observed(input: &str) -> IResult<&str, Expr> {
    context(
        "e -> observe b",
        map(preceded(ws(tag("observe")), parse_bool_expr), |b| {
            Expr::Observe(Box::new(b))
        }),
    )(input)
}
trait ParseFloat {
    fn parse(input: &str) -> IResult<&str, Self>
    where
        Self: Sized;
}

impl ParseFloat for f64 {
    fn parse(input: &str) -> IResult<&str, Self> {
        double(input)
    }
}

impl ParseFloat for f32 {
    fn parse(input: &str) -> IResult<&str, Self> {
        float(input)
    }
}

fn parse_bernoulli(input: &str) -> IResult<&str, SimpleDistribution> {
    context(
        "d -> Bern(p)",
        ws(map(
            preceded(
                ws(tag("Bernoulli")),
                delimited(ws(char('(')), ws(Precision::parse), ws(char(')'))),
            ),
            |p| match PositiveProperFraction::new(p) {
                Ok(p) => SimpleDistribution::Bernoulli(p),
                Err(e) => panic!("Parse error: {}", e),
            },
        )),
    )(input)
}

fn parse_geometric(input: &str) -> IResult<&str, SimpleDistribution> {
    context(
        "d -> Geometric(p)",
        ws(map(
            preceded(
                ws(tag("Geometric")),
                delimited(ws(char('(')), ws(Precision::parse), ws(char(')'))),
            ),
            |p| match PositiveProperFraction::new(p) {
                Ok(p) => SimpleDistribution::Geometric(p),
                Err(e) => panic!("Parse error: {}", e),
            },
        )),
    )(input)
}

fn parse_poisson(input: &str) -> IResult<&str, SimpleDistribution> {
    context(
        "d -> Poisson(r)",
        ws(map(
            preceded(
                ws(tag("Poisson")),
                delimited(ws(char('(')), ws(Precision::parse), ws(char(')'))),
            ),
            |r| SimpleDistribution::Poisson(Fraction(r)),
        )),
    )(input)
}

fn parse_binomial(input: &str) -> IResult<&str, SimpleDistribution> {
    context(
        "d -> Binomial(n, p)",
        ws(map(
            preceded(
                ws(tag("Binomial")),
                delimited(
                    ws(char('(')),
                    separated_pair(ws(parse_natural), ws(char(',')), ws(Precision::parse)),
                    ws(char(')')),
                ),
            ),
            |(n, p)| {
                let p = match PositiveProperFraction::new(p) {
                    Ok(p) => p,
                    Err(e) => panic!("Parse error: {}", e),
                };
                SimpleDistribution::Binomial(n, p)
            },
        )),
    )(input)
}

fn parse_negbinomial(input: &str) -> IResult<&str, SimpleDistribution> {
    context(
        "d -> NegBinomial(n, p)",
        ws(map(
            preceded(
                ws(tag("NegBinomial")),
                delimited(
                    ws(char('(')),
                    separated_pair(ws(parse_natural), ws(char(',')), ws(Precision::parse)),
                    ws(char(')')),
                ),
            ),
            |(n, p)| {
                let p = match PositiveProperFraction::new(p) {
                    Ok(p) => p,
                    Err(e) => panic!("Parse error: {}", e),
                };
                SimpleDistribution::NegBinomial(n, p)
            },
        )),
    )(input)
}

fn parse_biased(input: &str) -> IResult<&str, SimpleDistribution> {
    context(
        "d -> Biased(p)",
        ws(map(
            preceded(
                ws(tag("Biased")),
                delimited(ws(char('(')), ws(Precision::parse), ws(char(')'))),
            ),
            |p| match PositiveProperFraction::new(p) {
                Ok(p) => SimpleDistribution::Biased(p),
                Err(e) => panic!("Parse error: {}", e),
            },
        )),
    )(input)
}

/// Parse a SimpleDistribution
fn parse_simple_distribution(input: &str) -> IResult<&str, SimpleDistribution> {
    context(
        "d ::= Bern(p) | Poisson(r) | Geometric(p) | Binomial(n, p) | NegBinomial(n, p) | Biased(p)",
        alt((
            parse_bernoulli,
            parse_poisson,
            parse_geometric,
            parse_binomial,
            parse_negbinomial,
            parse_exponential,
            parse_gamma,
            parse_uniform,
            parse_biased,
        )),
    )(input)
}

fn parse_exponential(input: &str) -> IResult<&str, SimpleDistribution> {
    context(
        "d -> Exponential(lambda)",
        ws(map(
            preceded(
                ws(tag("Exponential")),
                delimited(ws(char('(')), ws(Precision::parse), ws(char(')'))),
            ),
            |lambda| SimpleDistribution::Exponential(Fraction(lambda)),
        )),
    )(input)
}
fn parse_uniform(input: &str) -> IResult<&str, SimpleDistribution> {
    context(
        "d -> Uniform(a, b)",
        ws(map(
            preceded(
                ws(tag("Uniform")),
                delimited(
                    ws(char('(')),
                    separated_pair(ws(Precision::parse), ws(char(',')), ws(Precision::parse)),
                    ws(char(')')),
                ),
            ),
            |(a, b)| SimpleDistribution::Uniform(Fraction(a), Fraction(b)),
        )),
    )(input)
}

fn parse_gamma(input: &str) -> IResult<&str, SimpleDistribution> {
    context(
        "d -> Gamma(shape, rate)",
        ws(map(
            preceded(
                ws(tag("Gamma")),
                delimited(
                    ws(char('(')),
                    separated_pair(ws(Precision::parse), ws(char(',')), ws(Precision::parse)),
                    ws(char(')')),
                ),
            ),
            |(shape, rate)| SimpleDistribution::Gamma(Fraction(shape), Fraction(rate)),
        )),
    )(input)
}
fn parse_bernoulli_var(input: &str) -> IResult<&str, CompoundDistribution> {
    context(
        "d -> Bernoulli(x)",
        ws(map(
            preceded(
                ws(tag("Bernoulli")),
                delimited(ws(char('(')), ws(parse_identifier), ws(char(')'))),
            ),
            CompoundDistribution::Bernoulli,
        )),
    )(input)
}
fn parse_poisson_var(input: &str) -> IResult<&str, CompoundDistribution> {
    context(
        "d -> Poisson(r * x)",
        ws(map(
            preceded(
                ws(tag("Poisson")),
                delimited(
                    ws(char('(')),
                    separated_pair(ws(Precision::parse), ws(char('*')), ws(parse_identifier)),
                    ws(char(')')),
                ),
            ),
            |(r, x)| CompoundDistribution::Poisson(Fraction(r), x),
        )),
    )(input)
}
fn parse_compound_distribution(input: &str) -> IResult<&str, CompoundDistribution> {
    context(
        "d ::= Poisson(r * x)",
        alt((parse_poisson_var, parse_bernoulli_var)),
    )(input)
}

/// Parse if-then-else
fn parse_if_then_else(input: &str) -> IResult<&str, Expr> {
    context(
        "<expression> -> \"if\" <boolean-expr> \"then\" <expression> \"else\" <expression>",
        map(
            tuple((
                preceded(
                    ws(context("expected \"if\"", tag("if"))),
                    context(
                        "\"if\" HERE \"then\" <expression> \"else\" <expression>",
                        parse_bool_expr,
                    ),
                ),
                preceded(
                    ws(context("expected \"then\"", tag("then"))),
                    context(
                        "\"if\" <expression> \"then\" HERE \"else\" <expression>",
                        parse_expr,
                    ),
                ),
                preceded(
                    ws(context("expected \"else\"", tag("else"))),
                    context(
                        "\"if\" <expression> \"then\" <expression> \"else\" HERE",
                        parse_expr,
                    ),
                ),
            )),
            |(cond, then_branch, else_branch)| Expr::ifthenelse(cond, then_branch, else_branch),
        ),
    )(input)
}

fn parse_loop_sum(input: &str) -> IResult<&str, Expr> {
    context(
        "e -> loop e sum e",
        map(
            tuple((
                preceded(
                    ws(context("expected \"loop\"", tag("loop"))),
                    context("loop HERE sum e", parse_expr),
                ),
                preceded(
                    ws(context("expected \"sum\"", tag("sum"))),
                    context("loop e sum HERE", parse_expr),
                ),
            )),
            |(e1, e2)| Expr::LoopSum(Box::new(e1), Box::new(e2)),
        ),
    )(input)
}
/// Parse let expression
fn parse_let_expr(input: &str) -> IResult<&str, Expr> {
    context(
        "e -> let x = e in e",
        map(
            tuple((
                preceded(
                    ws(context("expected \"let\"", tag("let"))),
                    context("let HERE = e in e", parse_identifier),
                ),
                preceded(
                    ws(context("expected \"=\"", char('='))),
                    context("let x = HERE in e", parse_expr),
                ),
                preceded(
                    ws(context("expected \"in\"", tag("in"))),
                    context("let x = e in HERE", parse_expr),
                ),
            )),
            |(var, val, body)| Expr::Let(var, Box::new(val), Box::new(body)),
        ),
    )(input)
}

// Parse a boolean expression
// <boolean-expr>     ::= <boolean-term> <boolean-or>
// <boolean-or>       ::= "||" <boolean-expr> | ε
// <boolean-term>     ::= <boolean-factor> <boolean-and>
// <boolean-and>      ::= "&&" <boolean-term> | ε
// <boolean-factor>   ::= '!' <boolean-factor> | '(' <boolean-expr> ')' | <expresion> | true | false
fn parse_bool_expr(input: &str) -> IResult<&str, BoolExpr> {
    // println!("[parse_bool_expr] input: {:?}", input.to_string());
    context(
        "<boolean-expr> ::= <boolean-term> <boolean-or>",
        map(
            pair(parse_bool_term, parse_bool_or),
            |(b1, rhs)| match rhs {
                Some(b2) => BoolExpr::or(b1, b2),
                None => b1,
            },
        ),
    )(input)
}

fn parse_bool_or(input: &str) -> IResult<&str, Option<BoolExpr>> {
    context(
        "<boolean-or> ::= \"||\" <boolean-expr> | ε",
        opt(preceded(ws(tag("||")), parse_bool_expr)),
    )(input)
}

fn parse_bool_term(input: &str) -> IResult<&str, BoolExpr> {
    context(
        "<boolean-term> ::= <boolean-factor> <boolean-and>",
        map(
            pair(parse_bool_factor, parse_bool_and),
            |(b1, rhs)| match rhs {
                Some(b2) => BoolExpr::and(b1, b2),
                None => b1,
            },
        ),
    )(input)
}

fn parse_bool_and(input: &str) -> IResult<&str, Option<BoolExpr>> {
    context(
        "<boolean-and> ::= \"&&\" <boolean-term> | ε",
        opt(preceded(ws(tag("&&")), parse_bool_term)),
    )(input)
}

fn parse_bool_factor(input: &str) -> IResult<&str, BoolExpr> {
    context("<boolean-factor> ::= '!' <boolean-factor> | '(' <boolean-expr> ')' | <lookup>  | true | false | <exprssion>",
        alt((
            context(
                "<boolean-not> ::= '!' <boolean-factor>",
                map(
                    preceded(ws(tag("!")), parse_bool_factor),
                    BoolExpr::not,
                )
            ),
            parenthesized(parse_bool_expr),
            map(parse_look_up, |p| BoolExpr::Lookup(Box::new(p))),
            parse_bool_literal,
            map(parse_expr, |e| BoolExpr::Test(Box::new(e))),
        ))
    )(input)
}

fn parse_look_up(input: &str) -> IResult<&str, ProjectionExpr> {
    // println!("[parse_look_up] input: {:?}", input[0..20].to_string());
    context(
        "<lookup> ::= <identifier> | \"(fst\" <lookup> \")\" | \"(snd\" <lookup> \")\"",
        alt((
            map(parse_identifier, ProjectionExpr::Var),
            context(
                "\"(fst\" <lookup>\")\"",
                parenthesized(map(preceded(ws(tag("fst")), parse_look_up), |p| {
                    ProjectionExpr::Fst(Box::new(p))
                })),
            ),
            context(
                "\"(snd\" <lookup>\")\"",
                parenthesized(map(preceded(ws(tag("snd")), parse_look_up), |p| {
                    ProjectionExpr::Snd(Box::new(p))
                })),
            ),
        )),
    )(input)
}

fn parse_bool_native(input: &str) -> IResult<&str, bool> {
    alt((
        map(context("expected true", ws(tag("true"))), |_| true),
        map(context("expected false", ws(tag("false"))), |_| false),
    ))(input)
}

fn parse_bool_literal(input: &str) -> IResult<&str, BoolExpr> {
    map(parse_bool_native, BoolExpr::BoolLiteral)(input)
}
