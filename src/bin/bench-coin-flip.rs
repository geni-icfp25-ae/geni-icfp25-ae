use clap::Parser;
use gennifer::backend::TopologicalSort;
use gennifer::free_variable_analysis::{CachedFreeVariables, FreeVariables};
use gennifer::generating_function::{GenFun, GenFunKind};
use gennifer::transformers::{Event, Transform};
use num_traits::{One, Zero};
use rustc_hash::FxHashSet;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::time::{Duration, Instant};

/// Command-line arguments

#[derive(Parser, Debug)]
struct Args {
    /// Number of insertions
    n: usize,
    #[arg(long)]
    fva: bool,

    #[arg(long)]
    dfs: bool,
}

static NAME: &str = "_RET";

fn next_coin_flip(gf: GenFun<f32>, name: String) -> GenFun<f32> {
    gf * (GenFun::from(GenFunKind::Constant(0.5))
        + GenFun::from(GenFunKind::Constant(0.5)) * GenFun::from(GenFunKind::Var(name)))
}

fn coin_flip(n: usize) -> GenFun<f32> {
    let start = Instant::now();
    let mut gf = GenFun::<f32>::one();
    let mut root = GenFun::<f32>::zero();

    for i in 0..n {
        let _i = format!("_{}", i);
        gf = next_coin_flip(gf, _i.clone());
        let Event { pos, neg } = gf.test_fresh(&_i);
        root = root + pos * NAME.into();
        gf = neg;
    }
    root = root + gf;
    if cfg!(debug_assertions) {
        println!("Root: {}", root);
    }
    println!("[coin_flip] {:?}", start.elapsed());
    root
}

fn benchmark_fva(gf: &GenFun<f32>) -> Duration {
    let start = Instant::now();
    let mut fva = CachedFreeVariables::default();
    let set = gf.free_variables(&mut fva);
    println!("Free variables: {:?}", set);
    let duration = start.elapsed();
    println!("[benchmark_fva] {:?}", duration);
    duration
}

fn benchmark_dfs(gf: &GenFun<f32>) -> Duration {
    let start = Instant::now();
    let mut visited: HashSet<*const GenFunKind<f32>, rustc_hash::FxBuildHasher> =
        FxHashSet::default();
    let mut sorted: VecDeque<GenFun<f32>> = VecDeque::new();
    gf.topological_sort(&mut visited, &mut sorted);
    println!(
        "visited.len() = {}, sorted.len() = {}",
        visited.len(),
        sorted.len()
    );
    let duration = start.elapsed();
    println!("[benchmark_dfs] {:?}", duration);
    duration
}

fn run() {
    let start = Instant::now();
    let args = Args::parse(); // Parse command-line arguments
    let root = coin_flip(args.n);
    print!("n = {} ", args.n);
    let fva_time = if args.fva {
        benchmark_fva(&root)
    } else {
        Duration::from_secs(0)
    };
    let dfs_time = if args.dfs {
        benchmark_dfs(&root)
    } else {
        Duration::from_secs(0)
    };
    let duration: Duration = start.elapsed();
    println!("[run] {:?}", duration);
    println!(
        "{}, {}, {}, {}",
        args.n,
        fva_time.as_secs_f32(),
        dfs_time.as_secs_f32(),
        duration.as_secs_f32()
    )
}

pub fn main() {
    // Use a constant stack size of 64 GB. On some OSes, for example
    // MacOS, the default stack size is too small to run some examples such as
    // digitRecognition.
    const STACK_SIZE: usize = 64 * 1024 * 1024 * 1024;
    let start = Instant::now();
    let _ = std::thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(run)
        .unwrap()
        .join();
    println!("[main] {:?}", start.elapsed());
}
