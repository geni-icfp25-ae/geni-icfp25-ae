use gennifer::{
    backend::Interpreter,
    ir::{parse, type_check},
    live_variable_analysis::LiveVariableAnalysis,
    numbers::Precision,
    semantics::Semantics,
};
use glob::glob;
use im::ordset;
use nom::error::convert_error;
use std::fs;

#[test]
fn test_live_variable_analysis() {
    println!("Running integration tests for live variable analysis");
    // Find all `.gir` files in the `./test` directory
    for entry in glob("./tests/test_*.gir").expect("Failed to read glob pattern") {
        match entry {
            Ok(path) => {
                println!("Processing file: {:?}", path);

                // Read the file content
                let content = fs::read_to_string(&path).expect("Failed to read file");

                let expr = parse(&content).unwrap_or_else(|e| {
                    panic!("Parse error:\n{}", convert_error(content.as_ref(), e))
                });
                let _ = type_check(&expr).unwrap_or_else(|e| panic!("Type check error:\n{}", e));
                let (t_plain, gf_plain) = expr
                    .semantics(&"_RET".to_string())
                    .unwrap_or_else(|e| panic!("Semantics error:\n{}", e));
                let (t_smart, gf_smart) = expr
                    .apply(ordset![])
                    .semantics(&"_RET".to_string())
                    .unwrap_or_else(|e| panic!("Semantics error:\n{}", e));
                assert!(t_plain == t_smart);

                let mut interpreter_plain: Interpreter<Precision> = Interpreter::default();
                let mut interpreter_smart: Interpreter<Precision> = Interpreter::default();
                let table_plain =
                    interpreter_plain.populate_table(&"_RET".to_string(), &t_plain, &gf_plain);
                let table_smart =
                    interpreter_smart.populate_table(&"_RET".to_string(), &t_smart, &gf_smart);

                assert!(table_plain.len() == table_smart.len());
                for i in 0..table_plain.len() {
                    println!(
                        "Pr({}) = {} | {}",
                        table_plain[i].0, table_plain[i].1, table_smart[i].1
                    );
                    assert!(table_plain[i].0 == table_smart[i].0);
                    assert!((table_plain[i].1 - table_smart[i].1).abs() < 1e-6);
                }
                assert!(table_plain == table_smart);
                println!("")
            }
            Err(e) => eprintln!("Error reading file: {}", e),
        }
    }
}
