// This file contains tests for the interpreter module.

use churchroad::{interpret, InterpreterResult};

#[test]
fn interpreter_returns_bv() {
    // TODO(@ninehusky): This will fail right now, because the PR for $shiftx hasn't been merged yet.
    let filename = "tests/interpreter_tests/lut6.egg";
    let program = std::fs::read_to_string(filename).unwrap();
    let result = interpret(program, 2);
    assert_eq!(result, InterpreterResult::Bitvector(0, 0));
}
