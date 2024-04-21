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
    if std::env::var("CHURCHROAD_DIR").is_err() {
        panic!("Please set the CHURCHROAD_DIR environment variable!");
    }
    let churchroad_dir_str = std::env::var("CHURCHROAD_DIR").unwrap();
    let churchroad_dir = std::path::Path::new(&churchroad_dir_str);

    let temp_dir = std::env::temp_dir();

    let testbench_template_path =
        churchroad_dir.join("tests/interpreter_tests/verilog/testbench.sv.template");
    let makefile_template_path = churchroad_dir.join("tests/interpreter_tests/Makefile.template");

    let testbench_path = temp_dir.join("testbench.sv");
    let makefile_path = temp_dir.join("Makefile");

    let testbench_prog = std::fs::read_to_string(testbench_template_path).unwrap()
        .replace("{input_output_declarations}", "logic O;")
        .replace("{test_module_name}", "LUT6")
        .replace("{test_module_port_list}",
         ".I0(inputs[0]), .I1(inputs[1]), .I2(inputs[2]), .I3(inputs[3]), .I4(inputs[4]), .I5(inputs[5]), .O(O)")
        .replace("{max_input_bitwidth}", "1");

    let executable_name = "executable";
    let verilator_output_dir = temp_dir.join("obj_dir");
    let executable_path = verilator_output_dir.join(executable_name);
    let default_extra_args = format!(
        "-I{}",
        churchroad_dir
            .join("tests/interpreter_tests/verilog")
            .to_str()
            .unwrap()
    );
    let makefile_prog = std::fs::read_to_string(makefile_template_path)
        .unwrap()
        .replace("{testbench_file_path}", testbench_path.to_str().unwrap())
        .replace(
            "{verilator_output_dir}",
            verilator_output_dir.to_str().unwrap(),
        )
        .replace("{simulation_executable_name}", executable_name)
        .replace("{extra_verilator_args}", &default_extra_args);

    std::fs::write(&testbench_path, &testbench_prog).unwrap();
    std::fs::write(&makefile_path, &makefile_prog).unwrap();

    // TODO(@ninehusky): We can get rid of the necessity for a Makefile after this PR is merged
    // into Verilator: https://github.com/verilator/verilator/pull/5031
    let verilator_compile_output = std::process::Command::new("make")
        .arg("--environment-overrides")
        .arg("--always-make")
        .arg("-f")
        .arg(makefile_path)
        .output()
        .expect("make died");

    assert!(verilator_compile_output.status.success());

    // simulation process
    let mut sim_proc = std::process::Command::new(executable_path)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .unwrap();

    let sim_proc_stdin = sim_proc.stdin.as_mut().unwrap();

    // i'll clean this up later
    let num_inputs = 6;
    let num_test_cases = 1;
    let inputs = vec![0b1, 0b0, 0b1, 0b0, 0b1, 0b0];

    sim_proc_stdin
        .write_all(format!("{} {}\n", num_inputs, num_test_cases).as_bytes())
        .unwrap();
    for input in inputs {
        sim_proc_stdin
            .write_all(format!("{:X}\n", input).as_bytes())
            .unwrap();
    }

    let output = sim_proc.wait_with_output().unwrap();

    let test_output_path = temp_dir.join("test_output.txt");
    let test_error_path = temp_dir.join("test_error.txt");

    std::fs::write(&test_output_path, output.stdout).unwrap();
    std::fs::write(&test_error_path, output.stderr).unwrap();

    println!("logged output to: {}", test_output_path.to_str().unwrap());
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

// TODO(@ninehusky): Fix this test, it's currently broken until we fix the interpreter
// and use the non-broken LUT6.egg file.
interpreter_test!(
    test_lut6_0,
    // grab the only 1-bit from INIT
    InterpreterResult::Bitvector(0b1, 1),
    "tests/interpreter_tests/LUT6-modified.egg",
    0,
    &[
        ("INIT", vec![0x0000000000000001]),
        ("I0", vec![0b0]),
        ("I1", vec![0b0]),
        ("I2", vec![0b0]),
        ("I3", vec![0b0]),
        ("I4", vec![0b0]),
        ("I5", vec![0b1]),
    ]
    .into(),
    "O"
);
