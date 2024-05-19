// This file contains tests for the interpreter module.

use std::{collections::HashMap, io::Write, path::PathBuf};

use egraph_serialize::NodeId;
use rand::{rngs::StdRng, RngCore, SeedableRng};

use egglog::{EGraph, SerializeConfig};

use churchroad::{get_bitwidth_for_node, import_churchroad, interpret, InterpreterResult};

// Creates an EGraph from a Verilog file using Churchroad, and returns the serialized EGraph and the root node.
fn prep_interpreter(
    module_verilog_path: PathBuf,
    test_output_dir: PathBuf,
    top_module_name: &str,
    out: &str,
) -> (egraph_serialize::EGraph, egraph_serialize::Node) {
    if std::env::var("CHURCHROAD_DIR").is_err() {
        panic!("Please set the CHURCHROAD_DIR environment variable!");
    }

    let churchroad_dir_str: String = std::env::var("CHURCHROAD_DIR").unwrap();

    let churchroad_yosys_dir =
        std::path::Path::new(&churchroad_dir_str).join("yosys-plugin/churchroad.so");

    let churchroad_src_path = test_output_dir.join(format!("{}.egg", top_module_name));

    let yosys_commands = format!(
        "read_verilog -sv {}; prep -top {}; pmuxtree; write_lakeroad",
        module_verilog_path.to_str().unwrap(),
        top_module_name,
    );

    let yosys_proc = std::process::Command::new("yosys")
        .arg("-m")
        .arg(churchroad_yosys_dir)
        .arg("-q")
        .arg("-p")
        .arg(yosys_commands)
        .output();

    let yosys_output = yosys_proc.unwrap();

    if !yosys_output.status.success() {
        panic!(
            "Yosys failed, stderr: {:?}",
            String::from_utf8(yosys_output.stderr)
        );
    }

    std::fs::write(&churchroad_src_path, yosys_output.stdout).unwrap();
    println!(
        "logged Churchroad source to: {}",
        churchroad_src_path.to_str().unwrap()
    );

    // prep egraph for interpretation
    let mut egraph: EGraph = EGraph::default();
    import_churchroad(&mut egraph);
    egraph
        .parse_and_run_program(&std::fs::read_to_string(churchroad_src_path).unwrap())
        .unwrap();

    egraph
        .parse_and_run_program("(run-schedule (saturate typing))")
        .unwrap();

    let serialized = egraph.serialize(SerializeConfig::default());

    let (_, is_output_node) = serialized
        .nodes
        .iter()
        .find(|(_, n)| {
            n.op == "IsPort"
                && n.children[2] == NodeId::from("Output-0")
                && serialized.nodes.get(&n.children[1]).unwrap().op.as_str()
                    == format!("\"{}\"", out)
        })
        .unwrap();

    // output the serialized egraph to "DSP48E2.json"
    serialized
        .to_json_file(test_output_dir.join("serialized.json"))
        .unwrap();

    println!(
        "Logged JSON to: {}",
        test_output_dir.join("serialized.json").to_str().unwrap()
    );

    // Each node should have a HasType node associated with it.
    for (node_id, node) in serialized.nodes.iter() {
        // if the node has op in the list, don't care
        let list = vec!["Var", "Op1"];
        if !list.contains(&node.op.as_str()) {
            continue;
        }
        let _ = get_bitwidth_for_node(&serialized, node_id);
        println!("good");
    }

    let output_id = is_output_node.children.last().unwrap();
    let (_, output_node) = serialized
        .nodes
        .iter()
        .find(|(node_id, _)| **node_id == *output_id)
        .unwrap();

    (serialized.clone(), output_node.clone())
}

#[test]
fn test_lut6_verilator() {
    if std::env::var("CHURCHROAD_DIR").is_err() {
        panic!("Please set the CHURCHROAD_DIR environment variable!");
    }
    let churchroad_dir_str: String = std::env::var("CHURCHROAD_DIR").unwrap();
    let churchroad_dir = std::path::Path::new(&churchroad_dir_str);
    let testbench_template_path =
        churchroad_dir.join("tests/interpreter_tests/verilog/testbench.sv.template");
    let makefile_template_path = churchroad_dir.join("tests/interpreter_tests/Makefile.template");

    let inputs = vec![
        ("INIT", 64),
        ("I0", 1),
        ("I1", 1),
        ("I2", 1),
        ("I3", 1),
        ("I4", 1),
        ("I5", 1),
    ];
    let outputs: Vec<(&str, i32)> = vec![("O", 1)];

    let include_dirs = vec![churchroad_dir.join("tests/interpreter_tests/verilog/")];

    verilator_intepreter_fuzz_test(
        testbench_template_path,
        makefile_template_path,
        "LUT6",
        inputs,
        outputs,
        include_dirs,
        std::env::temp_dir(),
        100,
        5,
        churchroad_dir.join("tests/interpreter_tests/verilog/LUT6-modified.v"),
    );
}

// This test runs verilator against our interpreter, failing if the outputs of the two differ.
//
// testbench_template_path: path to the testbench template file
// makefile_template_path: path to the Makefile template file
// top_module_name: name of the top module in the Verilog file
// inputs: list of tuples of input names and their bitwidths
// outputs: list of tuples of output names and their bitwidths
// include_dirs: list of directories to include in the Verilator compilation
// test_output_dir: directory to output test files to
// churchroad_src_path: path to the Churchroad source file that the interpreter will use
// num_test_cases: number of test cases to run
// num_clock_cycles: number of clock cycles to run each test case for
// filename: name of the file to run Verilator on. For example, if the file is `adder.v`, this should be `"adder.v"`
fn verilator_intepreter_fuzz_test(
    testbench_template_path: PathBuf,
    makefile_template_path: PathBuf,
    top_module_name: &str,
    inputs: Vec<(&str, i32)>,
    outputs: Vec<(&str, i32)>,
    include_dirs: Vec<PathBuf>,
    test_output_dir: PathBuf,
    num_test_cases: usize,
    num_clock_cycles: usize,
    verilog_module_path: PathBuf,
) {
    let testbench_path = test_output_dir.join("testbench.sv");
    let makefile_path = test_output_dir.join("Makefile");

    // just grab the filename without any leading directories
    let filename = verilog_module_path.file_name().unwrap().to_str().unwrap();

    let testbench_prog = std::fs::read_to_string(testbench_template_path)
        .unwrap()
        .replace("{filename}", filename)
        // TODO(@ninehusky): this'll eventually need to include parameters as well, right?
        .replace(
            "{input_output_declarations}",
            format!(
                "{}",
                outputs
                    .iter()
                    .map(|(name, bw)| format!("logic [{}:0] {};\n", bw - 1, name))
                    .collect::<Vec<String>>()
                    .join("\n")
            )
            .as_str(),
        )
        .replace("{test_module_name}", top_module_name)
        .replace(
            "{test_module_port_list}",
            format!(
                "{}, {}",
                format!(
                    "{}",
                    inputs
                        .iter()
                        .enumerate()
                        .map(|(i, (name, _))| format!(".{}(inputs[{}])", name, i))
                        .collect::<Vec<String>>()
                        .join(",\n")
                ),
                format!(
                    "{}",
                    outputs
                        .iter()
                        .map(|(name, _)| format!(".{}({})", name, name))
                        .collect::<Vec<String>>()
                        .join(", ")
                        .as_str()
                )
            )
            .as_str(),
        )
        .replace(
            "{max_input_bitwidth}",
            inputs
                .iter()
                .map(|(_, bitwidth)| bitwidth)
                .max()
                .unwrap()
                .to_string()
                .as_str(),
        )
        .replace(
            "{display_outputs}",
            outputs
                .iter()
                .map(|(name, _)| {
                    format!(
                        "$display(\"output: %d\\n\", simulate_with_verilator_test_module.{});",
                        name
                    )
                })
                .collect::<Vec<String>>()
                .join("\n")
                .as_str(),
        );

    let executable_name = "executable";
    let verilator_output_dir = test_output_dir.join("obj_dir");
    let executable_path = verilator_output_dir.join(executable_name);

    let default_extra_args = format!(
        "-I{}",
        include_dirs
            .iter()
            .map(|path| path.to_str().unwrap())
            .collect::<Vec<&str>>()
            .join(" -I")
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
        .unwrap();

    if !verilator_compile_output.status.success() {
        panic!(
            "Verilator failed, stderr: {:?}",
            String::from_utf8(verilator_compile_output.stderr)
        );
    }

    let mut sim_proc = std::process::Command::new(executable_path)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .unwrap();

    let sim_proc_stdin = sim_proc.stdin.as_mut().unwrap();

    let num_inputs = inputs.len();

    // TODO(@ninehusky): this is going to assume we only want to interpret on one output found in the EGraph.
    let (serialized, root_node) = prep_interpreter(
        verilog_module_path.clone(),
        test_output_dir.clone(),
        top_module_name,
        outputs[0].0,
    );

    sim_proc_stdin
        .write_all(format!("{} {} {}\n", num_inputs, num_test_cases, num_clock_cycles).as_bytes())
        .unwrap();

    // create seeded rng
    let mut rng = StdRng::seed_from_u64(0xb0bacafe);
    let mut interpreter_results: Vec<InterpreterResult> = Vec::new();
    for _ in 0..num_test_cases {
        let mut env: HashMap<&str, Vec<u64>> = HashMap::new();
        let input_values: Vec<Vec<u64>> = inputs
            .iter()
            .map(|(name, bw)| {
                // generate num_clock_cycles random values for each input
                let vals: Vec<u64> = (0..num_clock_cycles)
                    .map(|_| {
                        let mut val = rng.next_u64();
                        if *bw != 64 {
                            val = val & ((1 << bw) - 1);
                        }
                        val
                    })
                    .collect();
                env.insert(name, vals.clone());
                vals
            })
            .collect();

        for t in 0..num_clock_cycles {
            for input in input_values.iter() {
                sim_proc_stdin
                    .write_all(format!("{:X}\n", input[t]).as_bytes())
                    .unwrap();
            }
            let result = interpret(&serialized, &root_node.eclass, t, &env).unwrap();
            interpreter_results.push(result);
        }
    }

    let output = sim_proc.wait_with_output().unwrap();

    let output_str = String::from_utf8(output.stdout).unwrap();
    let verilator_output_values: Vec<u64> = output_str
        .lines()
        // filter all lines that don't start with "output: "
        .filter(|line| line.len() > 0 && line.starts_with("output: "))
        .map(|line| line.trim_start_matches("output: ").parse().unwrap())
        .collect();

    for (InterpreterResult::Bitvector(val, _), verilator_result) in interpreter_results
        .iter()
        .zip(verilator_output_values.iter())
    {
        assert_eq!(val, verilator_result);
    }

    let test_output_path = test_output_dir.join("test_output.txt");
    let test_error_path = test_output_dir.join("test_error.txt");

    std::fs::write(&test_output_path, output_str).unwrap();
    std::fs::write(&test_error_path, output.stderr).unwrap();

    println!("logged output to: {}", test_output_path.to_str().unwrap());
}

macro_rules! interpreter_test {
    ($test_name:ident, $expected:expr, $verilog_path:literal, $module_name:literal, $time:literal, $env:expr, $out: literal) => {
        #[test]
        fn $test_name() {
            let (serialized, root_node) = prep_interpreter(
                PathBuf::from($verilog_path),
                std::env::temp_dir(),
                $module_name,
                $out,
            );

            assert_eq!(
                $expected,
                interpret(&serialized, &root_node.eclass, $time, $env).unwrap()
            );
        }
    };
}

interpreter_test!(
    test_alu_and_single_cycle,
    InterpreterResult::Bitvector(0b01010101, 8),
    "tests/interpreter_tests/verilog/ALU.sv",
    "ALU",
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
    test_alu_and_single_cycle_2,
    InterpreterResult::Bitvector(0b01010101, 8),
    "tests/interpreter_tests/verilog/ALU.sv",
    "ALU",
    0,
    &[
        ("a", vec![0b01010101, 0b10101010]),
        ("b", vec![0b11111111, 0b00000000]),
        ("op", vec![1, 0])
    ]
    .into(),
    "out"
);

interpreter_test!(
    test_alu_or_second_cycle,
    InterpreterResult::Bitvector(0b10101010, 8),
    "tests/interpreter_tests/verilog/ALU.sv",
    "ALU",
    1,
    &[
        ("a", vec![0b01010101, 0b10101010]),
        ("b", vec![0b11111111, 0b00000000]),
        ("op", vec![1, 0])
    ]
    .into(),
    "out"
);

// Test just to see if the interpreter even loads in the DSP

interpreter_test!(
    dummy_dsp_test,
    InterpreterResult::Bitvector(0, 1),
    "tests/interpreter_tests/verilog/DSP48E2.v",
    "DSP48E2",
    0,
    &[
        ("ACASCREG", vec![1]),
        ("ADREG", vec![0]),
        ("ALUMODEREG", vec![0]),
        ("AMULTSEL", vec![0]),
        ("AREG", vec![1]),
        ("AUTORESET_PATDET", vec![3]),
        ("AUTORESET_PRIORITY", vec![6]),
        ("A_INPUT", vec![7]),
        ("BCASCREG", vec![1]),
        ("BMULTSEL", vec![1]),
        ("BREG", vec![1]),
        ("B_INPUT", vec![7]),
        ("CARRYINREG", vec![0]),
        ("CARRYINSELREG", vec![0]),
        ("CREG", vec![0]),
        ("DREG", vec![0]),
        ("INMODEREG", vec![1]),
        ("IS_ALUMODE_INVERTED", vec![0]),
        ("IS_CARRYIN_INVERTED", vec![0]),
        ("IS_CLK_INVERTED", vec![0]),
        ("IS_INMODE_INVERTED", vec![0]),
        ("IS_OPMODE_INVERTED", vec![0]),
        ("IS_RSTALLCARRYIN_INVERTED", vec![0]),
        ("IS_RSTALUMODE_INVERTED", vec![0]),
        ("IS_RSTA_INVERTED", vec![0]),
        ("IS_RSTB_INVERTED", vec![0]),
        ("IS_RSTCTRL_INVERTED", vec![0]),
        ("IS_RSTC_INVERTED", vec![0]),
        ("IS_RSTD_INVERTED", vec![0]),
        ("IS_RSTINMODE_INVERTED", vec![0]),
        ("IS_RSTM_INVERTED", vec![0]),
        ("IS_RSTP_INVERTED", vec![0]),
        ("MASK", vec![0]),
        ("MREG", vec![0]),
        ("OPMODEREG", vec![1]),
        ("PATTERN", vec![0]),
        ("PREADDINSEL", vec![0]),
        ("PREG", vec![0]),
        ("RND", vec![0]),
        ("SEL_MASK", vec![8]),
        ("SEL_PATTERN", vec![9]),
        ("USE_MULT", vec![10]),
        ("USE_PATTERN_DETECT", vec![11]),
        ("USE_SIMD", vec![12]),
        ("USE_WIDEXOR", vec![13]),
        ("XORSIMD", vec![26]),
        ("A", vec![0b000000000000]),
        ("ACIN", vec![0]),
        ("ALUMODE", vec![7]),
        ("B", vec![0b000000000000]),
        // .B({ b[11], b[11], b[11], b[11], b[11], b[11], b } ),
        ("BCIN", vec![0]),
        ("C", vec![0]),
        ("CARRYCASCIN", vec![0]),
        ("CARRYIN", vec![0]),
        ("CARRYINSEL", vec![7]),
        ("CEA1", vec![1]),
        ("CEA2", vec![1]),
        ("CEAD", vec![1]),
        ("CEALUMODE", vec![1]),
        ("CEB1", vec![1]),
        ("CEB2", vec![1]),
        ("CEC", vec![1]),
        ("CECARRYIN", vec![1]),
        ("CECTRL", vec![1]),
        ("CED", vec![1]),
        ("CEINMODE", vec![1]),
        ("CEM", vec![1]),
        ("CEP", vec![1]),
        ("CLK", vec![1]),
        ("D", vec![0]),
        ("INMODE", vec![8]),
        ("MULTSIGNIN", vec![0]),
        ("OPMODE", vec![181]),
        ("PCIN", vec![0]),
        ("RSTA", vec![0]),
        ("RSTALLCARRYIN", vec![0]),
        ("RSTALUMODE", vec![0]),
        ("RSTB", vec![0]),
        ("RSTC", vec![0]),
        ("RSTCTRL", vec![0]),
        ("RSTD", vec![0]),
        ("RSTINMODE", vec![0]),
        ("RSTM", vec![0]),
        ("RSTP", vec![0])
    ]
    .into(),
    "P"
);
