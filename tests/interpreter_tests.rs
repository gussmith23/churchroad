// This file contains tests for the interpreter module.

use churchroad::{interpret, InterpreterResult};

#[test]
fn interpreter_returns_bv() {
    let filename = "tests/interpreter_tests/ALU.egg";
    let program = std::fs::read_to_string(filename).unwrap();
    let result = interpret(program, "1".to_string());
    assert_eq!(result.unwrap(), InterpreterResult::Bitvector(0, 0));
}

#[test]
fn interpreter_fails_on_multi_node_eclass() {
    let filename = "tests/interpreter_tests/ALU.egg";
    let program = std::fs::read_to_string(filename).unwrap();
    let result = interpret(program, "Unit-0".to_string());
    assert!(result.is_err_and(|e| e.to_string().contains("one node")));
}
