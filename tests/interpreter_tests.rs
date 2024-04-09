// This file contains tests for the interpreter module.

use egglog::{EGraph, SerializeConfig};

use churchroad::{import_churchroad, interpret, InterpreterResult};

macro_rules! interpreter_test {
    ($test_name:ident, $expected:expr, $filename:literal, $time:literal, $env:expr) => {
        #[test]
        fn $test_name() {
            let program = std::fs::read_to_string($filename).unwrap();
            let mut egraph: EGraph = EGraph::default();
            import_churchroad(&mut egraph);
            egraph.parse_and_run_program(&program).unwrap();
            egraph
                .parse_and_run_program("(relation IsRoot (Expr)) (IsRoot out)")
                .unwrap();
            let serialized = egraph.serialize(SerializeConfig::default());
            let (_, is_root_node) = serialized
                .nodes
                .iter()
                .find(|(_, n)| n.op == "IsRoot")
                .unwrap();
            if is_root_node.children.len() != 1 {
                panic!("IsRoot relation must have exactly one child");
            }
            let root_id = is_root_node.children.first().unwrap();
            let (_, root_node) = serialized
                .nodes
                .iter()
                .find(|(node_id, _)| **node_id == *root_id)
                .unwrap();

            assert_eq!(
                $expected,
                interpret(&serialized, &root_node.eclass, $time, $env).unwrap()
            );
        }
    };
}

interpreter_test!(
    test_alu_0,
    InterpreterResult::Bitvector(0b01010101, 8),
    "tests/interpreter_tests/ALU.egg",
    0,
    &[
        ("a", vec![0b01010101]),
        ("b", vec![0b11111111]),
        ("op", vec![1])
    ]
    .into()
);

interpreter_test!(
    test_alu_1,
    InterpreterResult::Bitvector(0b11111111, 8),
    "tests/interpreter_tests/ALU.egg",
    0,
    &[
        ("a", vec![0b01010101]),
        ("b", vec![0b11111111]),
        ("op", vec![0])
    ]
    .into()
);

// #[test]
// fn interpreter_returns_bv() {
//     let filename = "tests/interpreter_tests/ALU.egg";
//     let program = std::fs::read_to_string(filename).unwrap();
//     let result = interpret(program, "0".to_string());

//     let classid: ClassId = ClassId::from("2");

//     assert_eq!(result.unwrap(), InterpreterResult::Bitvector(0, 0));
// }

// #[test]
// fn interpreter_fails_on_multi_node_eclass() {
//     let filename = "tests/interpreter_tests/ALU.egg";
//     let program = std::fs::read_to_string(filename).unwrap();
//     let result = interpret(program, "Unit-0".to_string());
//     assert!(result.is_err_and(|e| e.to_string().contains("one node")));
// }
