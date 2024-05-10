// Runs the Churchroad interpreter on a given .egg file.

use std::collections::HashMap;

use egglog::{EGraph, SerializeConfig};

use egraph_serialize::NodeId;

use churchroad::{import_churchroad, interpret};

fn main() {
    // Usage: cargo run <filename>
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: cargo run <filename>");
        std::process::exit(1);
    }

    let churchroad_src_path = &args[1];

    let mut egraph: EGraph = EGraph::default();
    import_churchroad(&mut egraph);
    egraph
        .parse_and_run_program(&std::fs::read_to_string(churchroad_src_path).unwrap())
        .unwrap();
    let serialized = egraph.serialize(SerializeConfig::default());
    let (_, is_output_node) = serialized
        .nodes
        .iter()
        .find(|(_, n)| n.op == "IsPort" && n.children[2] == NodeId::from("Output-0"))
        .unwrap();
    let output_id = is_output_node.children.last().unwrap();
    let (_, output_node) = serialized
        .nodes
        .iter()
        .find(|(node_id, _)| **node_id == *output_id)
        .unwrap();

    // get user input: # of test cases, # of time steps
    let mut test_cases = String::new();
    println!("num test cases:");
    std::io::stdin().read_line(&mut test_cases).unwrap();
    let test_cases: usize = test_cases.trim().parse().unwrap();

    let mut time_steps = String::new();
    println!("num time steps:");
    std::io::stdin().read_line(&mut time_steps).unwrap();
    let time_steps: usize = time_steps.trim().parse().unwrap();

    let mut num_inputs = String::new();
    println!("num inputs:");
    std::io::stdin().read_line(&mut num_inputs).unwrap();
    let num_inputs: usize = num_inputs.trim().parse().unwrap();

    for _ in 0..test_cases {
        let mut input_map: HashMap<String, Vec<u64>> = HashMap::new();
        for time in 0..time_steps {
            for _ in 0..num_inputs {
                // input will be of form "variable_name: value"
                let mut input = String::new();
                std::io::stdin().read_line(&mut input).unwrap();

                let inputs = input.trim().split(": ").collect::<Vec<&str>>();
                assert!(
                    inputs.len() == 2,
                    "input must be of form 'variable_name: value'"
                );

                let var_name = inputs[0].to_string();
                let value: u64 = inputs[1].trim().parse().unwrap();

                println!("input map is: {:?}", input_map);
                let mut old_inputs = input_map.get(&var_name).unwrap_or(&vec![]).clone();

                if old_inputs.len() > time {
                    panic!(
                        "You've added an input for {} twice in clock cycle {}.",
                        var_name, time
                    );
                }

                old_inputs.push(value);

                input_map.insert(var_name, old_inputs);
            }
        }

        for (_, values) in input_map.iter() {
            assert!(values.len() == time_steps);
        }

        // now, run the interpreter
        for time in 0..time_steps {
            match interpret(&serialized, &output_node.eclass, time, &input_map) {
                Ok(churchroad::InterpreterResult::Bitvector(val, bw)) => {
                    println!("({}, {})", val, bw);
                }
                _ => panic!(),
            }
        }
    }
}
