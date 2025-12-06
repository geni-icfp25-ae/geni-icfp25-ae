use std::fs::File;
use std::io::{self, Write};
use std::path::PathBuf;
use std::time::Duration;

use gennifer::backend::{Backend, CBackend, Interpreter, OCamlBackend};
use gennifer::ir::parse;
use gennifer::ir::type_check;
use gennifer::live_variable_analysis::Info;
use gennifer::live_variable_analysis::LiveVariableAnalysis;
use gennifer::numbers::Precision;
use gennifer::semantics::Semantics;
use gennifer::simplify::Simplification;
use gennifer::surface;

use clap::Parser;
use im::ordset;
use itertools::Either;
use nom::error::convert_error;

use genfer::number::F64;
use num_traits::identities::One;

/// Command-line argument parser
#[derive(Parser)]
struct Args {
    /// Input file containing the source code
    file_name: PathBuf,

    /// Print the parsed program (--pp)
    #[arg(long, alias = "pp")]
    print_program: bool,

    /// Print the compiled generating function (--pg)
    #[arg(long, alias = "pg")]
    print_gf: bool,

    /// Print the time consumption (--pt)
    #[arg(long, alias = "pt")]
    print_time: bool,

    #[arg(long)]
    simplify: bool,

    /// Type check the program
    #[arg(long)]
    type_check: bool,

    /// Interpret the program
    #[arg(long, short)]
    interpret: bool,

    /// Use the taylor expansion engine from Genfer to interpret the program
    /// WARRING: This is experimental and may not work as expected.
    /// WARNING: Only tested for the Possion Packet Arrival Benchmark
    #[arg(long)]
    taylor: bool,

    /// Skip the table printing
    #[arg(long, alias = "skip")]
    skip_table: bool,

    /// Apply free variable analysis before compiling to generating functions
    #[arg(long, short)]
    live: bool,
    
    /// Turn the free variable analysis off
    /// --live --live_off: error
    /// --live_off only: live variable analysis off
    /// --live only: live variable analysis on
    /// neither: live variable analysis on
    #[arg(long, help="Turn the free variable analysis off\n--live --live_off \t: \terror\n--live_off only     \t: \tlive variable analysis off\n--live only     \t: \tlive variable analysis on\nneither         \t: \tlive variable analysis on")]
    live_off: bool,
    
    /// Compile the generating function to a C program
    #[arg(long)]
    c: bool,
    /// Compile the generating function to a OCaml program
    #[arg(long)]
    ocaml: bool,

    /// Output file name
    #[arg(long, short)]
    output: Option<PathBuf>,

    /// Hash table size for the memoization
    #[arg(long)]
    table_size: Option<usize>,

    /// Quiet level for warning messages
    #[arg(long)]
    quiet: bool,

    /// Gennifer surface language - gl?
    #[arg(long)]
    surface: bool,
}

fn run() -> io::Result<()> {
    let mut args = Args::parse();
    if args.live_off && args.live {
        panic!("Cannot set both --live and --live-off");
    } else if !args.live_off && !args.live {
        args.live = true;
    }
    let contents = std::fs::read_to_string(&args.file_name).unwrap();

    let parse_time;
    let desugar_time;
    let expr = {
        if args.surface {
            let parse_start = std::time::Instant::now();
            let expr = surface::parse(&contents).unwrap_or_else(|e| {
                panic!("Parse error:\n{}", convert_error(contents.as_ref(), e))
            });
            parse_time = parse_start.elapsed();
            let desugar_start = std::time::Instant::now();
            let gir_expr = surface::desugar_surface_expr(&expr);
            desugar_time = desugar_start.elapsed();

            gir_expr
        } else {
            let parse_start = std::time::Instant::now();
            let expr = parse(&contents).unwrap_or_else(|e| {
                panic!("Parse error:\n{}", convert_error(contents.as_ref(), e))
            });
            parse_time = parse_start.elapsed();
            desugar_time = Duration::from_secs(0);
            expr
        }
    };

    let type_check_time;
    if args.type_check {
        let type_checke_start = std::time::Instant::now();
        let ty = type_check(&expr).unwrap_or_else(|e| panic!("Type check error:\n{}", e));
        type_check_time = type_checke_start.elapsed();
        println!("{:#?}", ty);
    } else {
        type_check_time = Duration::from_secs(0);
    }

    let live_variable_analysis_time: Duration;
    let compilation_time: Duration;
    let interpret_time: Duration;
    let transpilation_preparation_time: Duration;
    let transpilation_io_time: Duration;

    let live_variable_set_info;
    let expr = if args.live {
        let live_variable_analysis_start = std::time::Instant::now();
        let annotated = expr.apply(ordset![]);
        live_variable_analysis_time = live_variable_analysis_start.elapsed();
        live_variable_set_info = Some(annotated.info());
        Either::Left(annotated)
    } else {
        live_variable_analysis_time = Duration::from_secs(0);
        live_variable_set_info = None;
        Either::Right(expr)
    };

    if args.print_time {
        println!(
            "\nLive variable set information {:?}",
            live_variable_set_info
        );
    }

    if args.print_program {
        println!("{:#?}", expr);
    }

    if args.interpret || args.ocaml || args.c || args.print_gf {
        let compilation_start = std::time::Instant::now();
        let compiled = expr.either(
            |annotated| annotated.semantics("_RET"),
            |expr| expr.semantics("_RET"),
        );
        compilation_time = compilation_start.elapsed();
        let (ty, mut gf) = compiled.unwrap_or_else(|e| panic!("Semantics error:\n{}", e));
        if args.print_gf {
            println!("{}", gf)
        }
        if args.simplify {
            gf = gf.simplify();
            if args.print_gf {
                println!("[main] simplified: {}", gf);
                if cfg!(debug_assertions) {
                    println!("[main] simplified: {:#?}", gf);
                }
            }
        }
        if args.interpret {
            let mut interpreter: Interpreter<Precision> = Interpreter::default();
            let interpret_start = std::time::Instant::now();
            if args.taylor {
                let (genfer_gf, mapping): (genfer::generating_function::GenFun<F64>, _) =
                    gennifer::genfun_conversion::to_genfer_genfun(&gf);
                println!("Genifer-Genfer Variable Mapping: {:?}", mapping);

                let num_vars = mapping.len();
                let ret_idx = *mapping.get("_RET").unwrap();

                let simplified_genfer_gf = genfer_gf.simplify();
                println!("Simplified Genfer GF: {}", simplified_genfer_gf);

                let mut substs = vec![F64::one(); num_vars];
                substs[ret_idx] = F64::one();
                println!("Substitutions: {:?}", substs);
                let expansion = simplified_genfer_gf.eval(&substs, 10);
                println!("Expansion: {}", expansion);
                // TODO: make this more general
                println!("Expected {}", expansion.array()[[0, 1]]);
                interpret_time = interpret_start.elapsed();
            } else {
                let table = interpreter.populate_table("_RET", &ty, &gf);
                interpret_time = interpret_start.elapsed();
                if !args.skip_table {
                    for (v, p) in table {
                        println!("Pr({}) = {:.7}", v, p);
                    }
                }
            }
            transpilation_preparation_time = Duration::from_secs(0);
            transpilation_io_time = Duration::from_secs(0);
        } else {
            interpret_time = Duration::from_secs(0);
            if args.ocaml || args.c {
                let mut output: Box<dyn Write> = if let Some(p) = args.output {
                    Ok::<Box<dyn Write>, std::io::Error>(Box::new(File::create(p)?))
                } else {
                    Ok::<Box<dyn Write>, std::io::Error>(Box::new(io::stdout()))
                }?;
                let transpilation_preparation_start = std::time::Instant::now();
                stderrlog::new()
                    .module(module_path!())
                    .quiet(args.quiet)
                    // Max Verbosity level, we only print warning messages so far
                    .verbosity(4)
                    .init()
                    .unwrap();
                let output_program: Box<dyn Backend<Precision>> = if args.ocaml {
                    Box::new(OCamlBackend::new(&gf, "_RET", &ty, args.table_size))
                } else if args.c {
                    Box::new(CBackend::new(&gf, "_RET", &ty, args.table_size))
                } else {
                    unreachable!()
                };
                transpilation_preparation_time = transpilation_preparation_start.elapsed();
                let transpilation_io_start = std::time::Instant::now();
                if !args.skip_table {
                    writeln!(output, "{}", output_program)?;
                }
                transpilation_io_time = transpilation_io_start.elapsed();
            } else {
                transpilation_preparation_time = Duration::from_secs(0);
                transpilation_io_time = Duration::from_secs(0);
            }
        }
    } else {
        compilation_time = Duration::from_secs(0);
        interpret_time = Duration::from_secs(0);
        transpilation_preparation_time = Duration::from_secs(0);
        transpilation_io_time = Duration::from_secs(0);
    }
    if args.print_time {
        let total = parse_time
            + type_check_time
            + live_variable_analysis_time
            + compilation_time
            + interpret_time
            + transpilation_preparation_time
            + desugar_time
            + transpilation_io_time;
        println!(
            "Time consumption, {}, {}, {}, {}, {}, {}, {}, {}, {}",
            parse_time.as_secs_f64(),
            desugar_time.as_secs_f64(),
            type_check_time.as_secs_f64(),
            live_variable_analysis_time.as_secs_f64(),
            compilation_time.as_secs_f64(),
            interpret_time.as_secs_f64(),
            transpilation_preparation_time.as_secs_f64(),
            transpilation_io_time.as_secs_f64(),
            total.as_secs_f64()
        );
        println!("    Parse         \t: {:?}", parse_time);
        println!("    Desugar       \t: {:?}", desugar_time);
        println!("    Type check    \t: {:?}", type_check_time);
        println!("    LVA           \t: {:?}", live_variable_analysis_time);
        println!("    Compilation   \t: {:?}", compilation_time);
        println!("    Interpret     \t: {:?}", interpret_time);
        println!(
            "    Transpilation \t: {:?} = {:?} + {:?}",
            transpilation_preparation_time + transpilation_io_time,
            transpilation_preparation_time,
            transpilation_io_time
        );
        println!("    Total         \t: {:?}", total);
    };
    Ok(())
}

pub fn main() {
    // Use a constant stack size of 64 GB. On some OSes, for example
    // MacOS, the default stack size is too small to run some examples such as
    // digitRecognition.
    const STACK_SIZE: usize = 64 * 1024 * 1024 * 1024;
    let child = std::thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(run)
        .unwrap();
    match child.join() {
        Ok(Ok(_)) => {}
        Ok(Err(e)) => eprintln!("Error: {}", e),
        Err(e) => eprintln!("Error: {:?}", e),
    }
}
