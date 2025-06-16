use clap::{Parser, ValueEnum};
use gennifer::generating_function::{GenFun, GenFunKind};
use num_traits::One;
use rustc_hash::FxHashSet;
use std::collections::VecDeque;
use std::hash::Hash;
use std::rc::Rc;
use std::time::Instant;

#[derive(Debug, Clone, ValueEnum)]
enum HashLib {
    Hashbrown,
    RustcHash,
}

/// Command-line arguments
#[derive(Parser, Debug)]
#[command(name = "HashSet Benchmark")]
#[command(about = "Benchmark insertions into HashSet of raw pointers", long_about = None)]
struct Args {
    /// Number of insertions
    n: usize,

    #[arg(long, value_enum)]
    lib: HashLib,
}

#[allow(dead_code)]
trait HashSet<T: Eq + Hash> {
    fn insert(&mut self, value: T);
    fn contains(&self, value: &T) -> bool;
    fn len(&self) -> usize;
}

impl<T: Hash + Eq + 'static> From<&HashLib> for Box<dyn HashSet<T>> {
    fn from(lib: &HashLib) -> Self {
        match lib {
            HashLib::Hashbrown => {
                println!("Using hashbrown::HashSet");
                Box::<hashbrown::HashSet<T>>::default()
            }
            HashLib::RustcHash => {
                println!("Using rustc_hash::FxHashSet");
                Box::<FxHashSet<T>>::default()
            }
        }
    }
}

impl<T: Eq + Hash> HashSet<T> for hashbrown::HashSet<T> {
    fn insert(&mut self, value: T) {
        self.insert(value);
    }
    fn contains(&self, value: &T) -> bool {
        self.contains(value)
    }
    fn len(&self) -> usize {
        self.len()
    }
}

impl<T: Eq + Hash> HashSet<T> for FxHashSet<T> {
    fn insert(&mut self, value: T) {
        self.insert(value);
    }
    fn contains(&self, value: &T) -> bool {
        self.contains(value)
    }
    fn len(&self) -> usize {
        self.len()
    }
}

fn type_of_val<T>(_: &T) -> &'static str {
    std::any::type_name::<T>()
}

fn benchmark_hashset_insertions(args: &Args) {
    let start = Instant::now();
    let mut visited: Box<dyn HashSet<*const GenFunKind<f64>>> = (&args.lib).into();
    println!("type(visited) = {}", type_of_val(&visited));
    let mut sorted: VecDeque<GenFun<f64>> = VecDeque::new();

    for _ in 0..args.n {
        let gf = GenFun::<f64>::one();
        let ptr = Rc::as_ptr(&gf.0);
        visited.insert(ptr);
        sorted.push_back(gf);
    }
    assert!(args.n == visited.len());
    assert!(args.n == sorted.len());
    println!(
        "visited.len() = {}, sorted.len() = {}, args.n = {}",
        visited.len(),
        sorted.len(),
        args.n
    );
    println!("[benchmark_hashset_insertions] {:?}", start.elapsed());
}

fn main() {
    let args = Args::parse(); // Parse command-line arguments
    let start = Instant::now();
    benchmark_hashset_insertions(&args);
    println!("[main] {:?}", start.elapsed());
}
