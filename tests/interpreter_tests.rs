// This file contains tests for the interpreter module.

use std::io::Write;

use egglog::{EGraph, SerializeConfig};

use churchroad::{import_churchroad, interpret, InterpreterResult};

macro_rules! interpreter_test {
    ($test_name:ident, $expected:expr, $filename:literal, $time:literal, $env:expr, $out: literal) => {
        #[test]
        fn $test_name() {
            let program = std::fs::read_to_string($filename).unwrap();
            let mut egraph: EGraph = EGraph::default();
            import_churchroad(&mut egraph);
            egraph.parse_and_run_program(&program).unwrap();
            egraph
                .parse_and_run_program(
                    format!("(relation IsRoot (Expr)) (IsRoot {})", $out).as_str(),
                )
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

#[test]
fn verilator() {
    let program = std::fs::read_to_string("tests/interpreter_tests/verilog/testbench.sv.template").unwrap()
        .replace("{input_output_declarations}", "logic O;")
        .replace("{test_module_name}", "LUT6")
        .replace("{test_module_port_list}",
         ".I0(inputs[0]), .I1(inputs[1]), .I2(inputs[2]), .I3(inputs[3]), .I4(inputs[4]), .I5(inputs[5]), .O(O)")
        .replace("{max_input_bitwidth}", "1");

    std::fs::write("tests/interpreter_tests/verilog/testbench.sv", &program).unwrap();

    println!("program: {}", program);

    let verilator_output = std::process::Command::new("verilator")
        .arg("--binary")
        .arg("-j")
        .arg("1")
        .arg("-Itests/interpreter_tests/verilog")
        .arg("tests/interpreter_tests/verilog/testbench.sv")
        .output()
        .expect("failed to execute verilator");

    if !verilator_output.status.success() {
        println!("stderr: {}", std::str::from_utf8(&verilator_output.stderr).unwrap());
    }
    assert!(verilator_output.status.success());

    let mut child = std::process::Command::new("./obj_dir/Vtestbench")
        .stdin(std::process::Stdio::piped())
        .spawn()
        .unwrap();

    let child_stdin = child.stdin.as_mut().unwrap();

    // i'll clean this up later
    let num_inputs = 6;
    let num_test_cases = 1;
    let inputs = vec![0b1, 0b0, 0b1, 0b0, 0b1, 0b0];

    child_stdin
        .write_all(format!("{} {}\n", num_inputs, num_test_cases).as_bytes())
        .unwrap();
    for input in inputs {
        child_stdin
            .write_all(format!("{:X}\n", input).as_bytes())
            .unwrap();
    }

    let output = child.wait_with_output().unwrap();

    println!("output: {:?}", output);
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
    .into(),
    "out"
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
    .into(),
    "out"
);

// kind of a dummy test for now to just see if we can import a LUT6,
// eventually this will mean something
interpreter_test!(
    test_lut6_0,
    InterpreterResult::Bitvector(0b101010, 6),
    "tests/interpreter_tests/LUT6.egg",
    0,
    &[
        ("I0", vec![0b1]),
        ("I1", vec![0b1]),
        ("I2", vec![0b1]),
        ("I3", vec![0b1]),
        ("I4", vec![0b1]),
        ("I5", vec![0b1]),
    ]
    .into(),
    "O"
);
