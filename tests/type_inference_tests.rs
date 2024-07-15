use egraph_serialize::NodeId;

use egglog::{EGraph, SerializeConfig};

use churchroad::{get_bitwidth_for_node, import_churchroad};

macro_rules! type_inference_test {
    ($test_name:ident, $churchroad_src:literal) => {
        #[test]
        fn $test_name() {
            let mut egraph = EGraph::default();
            import_churchroad(&mut egraph);
            egraph.parse_and_run_program($churchroad_src).unwrap();
            egraph
                .parse_and_run_program("(run-schedule (saturate typing))")
                .unwrap();

            // assert that everything has a type
            let serialized = egraph.serialize(SerializeConfig::default());

            for (id, node) in serialized.nodes.iter() {
                let typed_ops = vec![
                    String::from("Op1"),
                    String::from("Op2"),
                    String::from("Op3"),
                ];
                let mut untyped_nodes: Vec<&NodeId> = vec![];
                if typed_ops.contains(&node.op) {
                    match get_bitwidth_for_node(&serialized, id) {
                        Ok(bw) => {
                            println!("Node {:?} has bw {}", id, bw);
                        }
                        Err(_) => {
                            untyped_nodes.push(id);
                        }
                    }
                }
                assert!(
                    untyped_nodes.is_empty(),
                    "Nodes {:?} are untyped",
                    untyped_nodes
                )
            }
        }
    };
}

// TODO(@ninehusky): include some tests that fail

type_inference_test!(
    bitwise_reduce_xor_type_test,
    r#"
    (let v0 (Wire "a" 8))

    (let v1 (Op1 (ReduceXor) v0))
    "#
);

type_inference_test!(
    bitwise_reduce_and_type_test,
    r#"
    (let v0 (Wire "a" 8))

    (let v1 (Op1 (ReduceAnd) v0))
    "#
);

type_inference_test!(
    bitwise_reduce_or_type_test,
    r#"
    (let v0 (Wire "a" 8))

    (let v1 (Op1 (ReduceOr) v0))
    "#
);

type_inference_test!(
    bitwise_ne_type_test,
    r#"
    (let v0 (Wire "a" 8))
    (let v1 (Wire "b" 8))

    (let v2 (Op2 (Ne) v0 v1))
    "#
);

type_inference_test!(
    bitwise_eq_type_test,
    r#"
    (let v0 (Wire "a" 8))
    (let v1 (Wire "b" 8))

    (let v2 (Op2 (Eq) v0 v1))
    "#
);

type_inference_test!(
    bitwise_not_type_test,
    r#"
    (let v0 (Wire "a" 8))

    (let v1 (Op1 (Not) v0))
    "#
);

type_inference_test!(
    sub_type_test,
    r#"
    (let v0 (Wire "a" 8))
    (let v1 (Wire "b" 8))

    (let v2 (Op2 (Sub) v0 v1))
    "#
);

type_inference_test!(
    mul_type_test,
    r#"
    (let v0 (Wire "a" 8))
    (let v1 (Wire "b" 8))

    (let v2 (Op2 (Mul) v0 v1))
    "#
);

type_inference_test!(
    xor_type_test,
    r#"
    (let v0 (Wire "a" 8))
    (let v1 (Wire "b" 8))

    (let v2 (Op2 (Xor) v0 v1))
    "#
);

type_inference_test!(
    logic_and_type_test,
    r#"
    (let v0 (Wire "a" 8))
    (let v1 (Wire "b" 8))

    (let v2 (Op2 (LogicAnd) v0 v1))
    "#
);

type_inference_test!(
    logic_or_type_test,
    r#"
    (let v0 (Wire "a" 8))
    (let v1 (Wire "b" 8))

    (let v2 (Op2 (LogicOr) v0 v1))
    "#
);

type_inference_test!(
    logic_not_type_test,
    r#"
    (let v0 (Wire "a" 8))

    (let v1 (Op1 (LogicNot) v0))
    "#
);

type_inference_test!(
    extract_type_test,
    r#"
    (let v0 (Wire "a" 8))
    (let v1 (Op1 (Extract 0 0) v0))
    "#
);

type_inference_test!(
    extract_more_type_test,
    r#"
    (let v0 (Wire "a" 8))
    (let v1 (Op1 (Extract 1 1) v0))
    "#
);
