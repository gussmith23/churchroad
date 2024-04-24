use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use egraph_serialize::ClassId;

use egglog::{
    ast::{Literal, Symbol},
    constraint::{SimpleTypeConstraint, TypeConstraint},
    sort::{FromSort, I64Sort, IntoSort, Sort, VecSort},
    ArcSort, EGraph, PrimitiveLike, Term, TermDag, Value,
};

// The result of interpreting a Churchroad program.
#[derive(Debug, PartialEq, Clone)]
pub enum InterpreterResult {
    // Bitvector(value, bitwidth)
    Bitvector(u64, u64),
}
// Interprets a Churchroad program.
pub fn interpret(
    egraph: &egraph_serialize::EGraph,
    class_id: &ClassId,
    time: usize,
    env: &HashMap<&str, Vec<u64>>,
) -> Result<InterpreterResult, String> {
    let result = match egraph
        .classes()
        .iter()
        .filter(|(id, _)| id == &class_id)
        .next()
    {
        Some((id, _)) => interpret_helper(&egraph, id, time, env),
        None => return Err("No class with the given ID.".to_string()),
    };

    result
}

fn interpret_helper(
    egraph: &egraph_serialize::EGraph,
    id: &ClassId,
    time: usize,
    env: &HashMap<&str, Vec<u64>>,
) -> Result<InterpreterResult, String> {
    let node_ids = &egraph.classes().get(id).unwrap().nodes;
    if node_ids.len() != 1 {
        return Err(format!(
            "There should be exactly one node in the class, but there are {}.",
            node_ids.len()
        ));
    }

    let node_id = node_ids.first().unwrap();
    let node = egraph.nodes.get(node_id).unwrap();

    match node.op.as_str() {
        "Var" => {
            let bw: u64 = egraph
                .nodes
                .get(&node.children[1])
                .unwrap()
                .op
                .parse()
                .unwrap();
            let name = egraph.nodes.get(&node.children[0]).unwrap().op.as_str();
            // cut off the quotes on the beginning and end
            let name = &name[1..name.len() - 1];

            Ok(InterpreterResult::Bitvector(
                env.get(name)
                    .unwrap_or_else(|| panic!("didn't find var {:?}", name))
                    .get(time)
                    .unwrap()
                    .clone(),
                bw,
            ))
        }
        "Op0" | "Op1" | "Op2" | "Op3" => {
            assert!(node.children.len() >= 1);
            let op = egraph.nodes.get(&node.children[0]).unwrap();

            let children: Vec<_> = node
                .children
                .iter()
                .skip(1)
                .map(|id| {
                    let child = egraph.nodes.get(id).unwrap();
                    interpret_helper(egraph, &child.eclass, time, env)
                })
                .collect();

            match op.op.as_str() {
                // Binary operations that preserve bitwidth.
                "And" | "Or" | "Shr" => {
                    assert_eq!(children.len(), 2);
                    match (&children[0], &children[1]) {
                        (
                            Ok(InterpreterResult::Bitvector(a, a_bw)),
                            Ok(InterpreterResult::Bitvector(b, b_bw)),
                        ) => {
                            assert_eq!(a_bw, b_bw);
                            let result = match op.op.as_str() {
                                "And" => a & b,
                                "Or" => a | b,
                                "Shr" => a >> b,
                                _ => unreachable!(),
                            };
                            Ok(InterpreterResult::Bitvector(result, *a_bw))
                        }
                        _ => todo!(),
                    }
                }
                "Mux" => {
                    assert_eq!(children.len(), 3);

                    match children[0] {
                        Ok(InterpreterResult::Bitvector(cond, _)) => {
                            if cond == 0 {
                                children[1].clone()
                            } else {
                                children[2].clone()
                            }
                        }
                        _ => todo!(),
                    }
                }
                "BV" => {
                    assert_eq!(op.children.len(), 2);
                    let args = &op
                        .children
                        .iter()
                        .map(|id| {
                            let (_, node) = egraph
                                .nodes
                                .iter()
                                .find(|(node_id, _)| **node_id == *id)
                                .unwrap();
                            assert_eq!(node.children.len(), 0);
                            let val: u64 = node.op.parse().unwrap();
                            val
                        })
                        .collect::<Vec<_>>()[..];

                    Ok(InterpreterResult::Bitvector(args[0], args[1]))
                }
                "Extract" => {
                    assert_eq!(op.children.len(), 2);
                    let args = &op
                        .children
                        .iter()
                        .map(|id| {
                            let (_, node) = egraph
                                .nodes
                                .iter()
                                .find(|(node_id, _)| **node_id == *id)
                                .unwrap();
                            assert_eq!(node.children.len(), 0);
                            let val: u64 = node.op.parse().unwrap();
                            val
                        })
                        .collect::<Vec<_>>()[..];

                    let i = args[0];
                    let j = args[1];

                    let val = match children[0].as_ref().unwrap() {
                        InterpreterResult::Bitvector(val, bw) => {
                            // from Rosette docs:
                            // https://docs.racket-lang.org/rosette-guide/sec_bitvectors.html#%28def._%28%28lib._rosette%2Fbase%2Fbase..rkt%29._extract%29%29
                            // TODO(@ninehusky): here, we should also assert that j >= 0 if churchroad handles signed numbers
                            assert!(*bw > i && i >= j);

                            // TODO(@ninehusky): check this, because copilot wrote this
                            let mask = (1 << (i - j + 1)) - 1;
                            (val >> j) & mask
                        }
                    };

                    Ok(InterpreterResult::Bitvector(
                        val,
                        (i - j + 1).try_into().unwrap(),
                    ))
                }
                "Concat" => match (&children[0], &children[1]) {
                    (
                        Ok(InterpreterResult::Bitvector(a, a_bw)),
                        Ok(InterpreterResult::Bitvector(b, b_bw)),
                    ) => {
                        let result = (a << b_bw) | b;
                        Ok(InterpreterResult::Bitvector(result, a_bw + b_bw))
                    }
                    _ => todo!(),
                },
                "ZeroExtend" => {
                    let extension_bw: u64 = egraph
                        .nodes
                        .iter()
                        .find(|(id, _)| *id == &op.children[0])
                        .unwrap()
                        .1
                        .op
                        .parse()
                        .unwrap();
                    match children[0] {
                        Ok(InterpreterResult::Bitvector(val, _)) => {
                            Ok(InterpreterResult::Bitvector(val, extension_bw))
                        }
                        _ => todo!(),
                    }
                }
                _ => todo!("unimplemented op: {:?}", op.op),
            }
        }
        _ => todo!("unimplemented node type: {:?}", node.op),
    }
}

pub fn to_verilog(term_dag: &TermDag, id: usize) -> String {
    // let mut wires = HashMap::default();

    fn id_to_wire_name(id: usize) -> String {
        format!("wire_{}", id)
    }

    let mut inputs = String::new();
    let mut logic_declarations = String::new();
    let mut registers = String::new();
    let mut module_declarations = String::new();

    let mut queue = vec![id];
    let mut done = HashSet::new();

    while let Some(id) = queue.pop() {
        done.insert(id);
        let term = term_dag.get(id);

        match term {
            Term::Lit(Literal::String(_)) => (),
            Term::Lit(Literal::Int(v)) => {
                logic_declarations.push_str(&format!(
                    "logic [31:0] {this_wire} = {val};\n",
                    this_wire = id_to_wire_name(id),
                    val = v
                ));
            }
            Term::Var(_) => todo!(),
            Term::App(s, v) => match (s.as_str(), v.as_slice()) {
                ("Reg", &[default_id, clk_id, d_id]) => {
                    let default_val = match term_dag.get(default_id) {
                        Term::Lit(Literal::Int(default_val)) => default_val,
                        _ => panic!(),
                    };

                    logic_declarations.push_str(
                        format!(
                            "logic {this_wire} = {default};\n",
                            this_wire = id_to_wire_name(id),
                            default = default_val
                        )
                        .as_str(),
                    );

                    registers.push_str(&format!(
                        "always @(posedge {clk}) begin
                            {this_wire} <= {d};
                        end\n",
                        clk = id_to_wire_name(clk_id),
                        this_wire = id_to_wire_name(id),
                        d = id_to_wire_name(d_id)
                    ));

                    if !done.contains(&d_id) {
                        queue.push(d_id);
                    }
                    if !done.contains(&clk_id) {
                        queue.push(clk_id);
                    }
                }
                ("Var", [name_id, bw_id]) => {
                    let name = match term_dag.get(*name_id) {
                        Term::Lit(Literal::String(name)) => name,
                        _ => panic!(),
                    };
                    let bw = match term_dag.get(*bw_id) {
                        Term::Lit(Literal::Int(bw)) => bw,
                        _ => panic!(),
                    };

                    inputs.push_str(
                        format!("input [{bw}-1:0] {name};\n", bw = bw, name = name).as_str(),
                    );

                    logic_declarations.push_str(
                        format!(
                            "logic [{bw}-1:0] {this_wire} = {name};\n",
                            bw = bw,
                            this_wire = id_to_wire_name(id),
                            name = name
                        )
                        .as_str(),
                    );
                }
                ("Mux", []) => (),
                ("LUT4", []) => (),
                ("Or", []) => (),
                ("Bitvector", [_]) => (),
                ("Eq", []) => (),
                ("BV", [val_id, bw_id]) => {
                    let val = match term_dag.get(*val_id) {
                        Term::Lit(Literal::Int(val)) => val,
                        _ => panic!(),
                    };
                    let bw = match term_dag.get(*bw_id) {
                        Term::Lit(Literal::Int(bw)) => bw,
                        _ => panic!(),
                    };
                    logic_declarations.push_str(
                        format!(
                            "logic [{bw}-1:0] {this_wire} = {bw}'d{val};\n",
                            bw = bw,
                            this_wire = id_to_wire_name(id),
                            val = val
                        )
                        .as_str(),
                    );
                }
                ("Extract", [hi_id, lo_id, expr_id]) => {
                    let hi = match term_dag.get(*hi_id) {
                        Term::Lit(Literal::Int(hi)) => hi,
                        _ => panic!(),
                    };
                    let lo = match term_dag.get(*lo_id) {
                        Term::Lit(Literal::Int(lo)) => lo,
                        _ => panic!(),
                    };
                    logic_declarations.push_str(&format!(
                        "logic {this_wire} = {expr}[{hi}:{lo}];\n",
                        hi = hi,
                        lo = lo,
                        this_wire = id_to_wire_name(id),
                        expr = id_to_wire_name(*expr_id),
                    ));

                    if !done.contains(&expr_id) {
                        queue.push(*expr_id);
                    }
                }
                ("Concat", [expr0_id, expr1_id]) => {
                    logic_declarations.push_str(&format!(
                        "logic {this_wire} = {{ {expr0}, {expr1} }};\n",
                        this_wire = id_to_wire_name(id),
                        expr0 = id_to_wire_name(*expr0_id),
                        expr1 = id_to_wire_name(*expr1_id),
                    ));

                    if !done.contains(&expr0_id) {
                        queue.push(*expr0_id);
                    }
                    if !done.contains(&expr1_id) {
                        queue.push(*expr1_id);
                    }
                }
                ("ZeroExtend", [expr_id, bw_id]) => {
                    let bw = match term_dag.get(*bw_id) {
                        Term::Lit(Literal::Int(bw)) => bw,
                        _ => panic!(),
                    };
                    logic_declarations.push_str(&format!(
                        "logic {this_wire} = {{ {bw}'d0, {expr} }};\n",
                        this_wire = id_to_wire_name(id),
                        bw = bw,
                        expr = id_to_wire_name(*expr_id),
                    ));

                    if !done.contains(&expr_id) {
                        queue.push(*expr_id);
                    }
                }
                ("Sketch1", [op_id, expr_id])
                    if match term_dag.get(*op_id) {
                        Term::App(s, v) => s.as_str() == "LUT4" && v.is_empty(),
                        _ => false,
                    } =>
                {
                    logic_declarations.push_str(&format!(
                        "logic {this_wire};\n",
                        this_wire = id_to_wire_name(id),
                    ));

                    module_declarations.push_str(&format!(
                        "lut4 lut4_{id} (.in({expr}), .out({y}));\n",
                        id = id,
                        expr = id_to_wire_name(*expr_id),
                        y = id_to_wire_name(id),
                    ));

                    if !done.contains(&expr_id) {
                        queue.push(*expr_id);
                    }
                }
                _ => todo!("{:?}", (s, v)),
            },
            _ => todo!("{:?}", term),
        }
    }

    format!(
        "module top({inputs});
            {inputs}
            {logic_declarations}
            {registers}
            {module_declarations}
        endmodule",
        inputs = inputs,
        logic_declarations = logic_declarations,
        registers = registers,
        module_declarations = module_declarations,
    )
}

/// Import Churchroad language into an EGraph.
///
/// TODO(@gussmith23): Ideally, this would be done via an `import` statement.
/// That's not currently possible because of the Rust-defined primitive
/// `debruijnify` in Churchroad.
pub fn import_churchroad(egraph: &mut EGraph) {
    // STEP 1: import primary language definitions.
    egraph
        .parse_and_run_program(r#"(include "egglog_src/churchroad.egg")"#)
        .unwrap();

    // STEP 2: add the `debruijnify` primitive to the egraph. This depends on
    // the above language definitions, but it's not possible to do it in egglog,
    // hence it's a Rust function.
    add_debruijnify(egraph);

    // STEP 3: import module enumeration rewrites. These depend on the
    // `debruijnify` primitive.
    egraph
        .parse_and_run_program(r#"(include "egglog_src/module_enumeration_rewrites.egg")"#)
        .unwrap();
}

/// Add the `debruijnify` primitive to an [`EGraph`].
fn add_debruijnify(egraph: &mut EGraph) {
    struct DeBruijnify {
        in_sort: Arc<VecSort>,
        out_sort: Arc<VecSort>,
        i64_sort: Arc<I64Sort>,
    }

    impl PrimitiveLike for DeBruijnify {
        fn name(&self) -> Symbol {
            "debruijnify".into()
        }

        fn get_type_constraints(&self) -> Box<dyn TypeConstraint> {
            Box::new(SimpleTypeConstraint::new(
                self.name(),
                vec![self.in_sort.clone(), self.out_sort.clone()],
            ))
        }

        fn apply(&self, values: &[crate::Value], egraph: &EGraph) -> Option<crate::Value> {
            let in_vec = Vec::<Value>::load(&self.in_sort, &values[0]);

            let mut seen_values: HashMap<Value, i64> = HashMap::new();
            let mut next_id = 0;
            let mut out = vec![];

            for value in in_vec {
                // Get representative value.
                let value = egraph.find(value);

                // If we haven't assinged it a number yet, give it the next one.
                if !seen_values.contains_key(&value) {
                    seen_values.insert(value, next_id);
                    next_id += 1;
                }

                // Add the number to the output vector.
                out.push(seen_values[&value].store(&self.i64_sort).unwrap());
            }

            out.store(&self.out_sort)
        }
    }

    egraph.add_primitive(DeBruijnify {
        i64_sort: egraph.get_sort().unwrap(),
        in_sort: egraph
            .get_sort_by(|s: &Arc<VecSort>| s.name() == "ExprVec".into())
            .unwrap(),
        out_sort: egraph
            .get_sort_by(|s: &Arc<VecSort>| s.name() == "IVec".into())
            .unwrap(),
    });
}

/// Generate all module enumeration rewrites used by Churchroad.
///
/// This function is used to generate the contents of the the
/// `egglog_src/module_enumeration_rewrites.egg` file. A test in this file
/// ensures that the generated file matches what this function produces.
pub fn generate_module_enumeration_rewrites(enumeration_ruleset_name: &str) -> String {
    format!(
            "
(ruleset {enumeration_ruleset_name})
{rewrites}",
            enumeration_ruleset_name = enumeration_ruleset_name,
            rewrites = vec![
                // Var
                // Note that this puts a loop in the graph, because a Var
                // becomes a hole applied to itself. We just need to be careful
                // about that during extraction.
                format!("(rewrite (Var name bw) (apply (MakeModule (Hole) (vec-of 0)) (vec-of (Var_ name bw))) :ruleset {})", enumeration_ruleset_name),

                // 0-ary
                generate_module_enumeration_rewrite(&[], Some(enumeration_ruleset_name)),
                // 1-ary
                generate_module_enumeration_rewrite(&[true], Some(enumeration_ruleset_name)),
                generate_module_enumeration_rewrite(&[false], Some(enumeration_ruleset_name)),
                // 2-ary
                generate_module_enumeration_rewrite(&[true, true], Some(enumeration_ruleset_name)),
                generate_module_enumeration_rewrite(&[true, false], Some(enumeration_ruleset_name)),
                generate_module_enumeration_rewrite(&[false, true], Some(enumeration_ruleset_name)),
                generate_module_enumeration_rewrite(
                    &[false, false],
                    Some(enumeration_ruleset_name)
                ),
                // 3-ary
                generate_module_enumeration_rewrite(
                    &[true, true, true],
                    Some(enumeration_ruleset_name)
                ),
                generate_module_enumeration_rewrite(
                    &[true, true, false],
                    Some(enumeration_ruleset_name)
                ),
                generate_module_enumeration_rewrite(
                    &[true, false, true],
                    Some(enumeration_ruleset_name)
                ),
                generate_module_enumeration_rewrite(
                    &[true, false, false],
                    Some(enumeration_ruleset_name)
                ),
                generate_module_enumeration_rewrite(
                    &[false, true, true],
                    Some(enumeration_ruleset_name)
                ),
                generate_module_enumeration_rewrite(
                    &[false, true, false],
                    Some(enumeration_ruleset_name)
                ),
                generate_module_enumeration_rewrite(
                    &[false, false, true],
                    Some(enumeration_ruleset_name)
                ),
                generate_module_enumeration_rewrite(
                    &[false, false, false],
                    Some(enumeration_ruleset_name)
                ),
                // clang-format on
            ]
            .join("\n"),
        )
}

/// Generate module enumeration rewrite.
///
/// - hole_indicator: a list of booleans indicating whether the Op's
///   argument at the given index is a hole. If true, the argument will
///   become a `(Hole)`. If not, it will expect a module application:
///   `(apply (MakeModule graph indices) args)`.
///
/// ```
/// use churchroad::generate_module_enumeration_rewrite;
/// assert_eq!(generate_module_enumeration_rewrite(&[true, false, true], None),
///           "(rewrite
///   (Op3 op expr0 (apply (MakeModule graph1 _) args1) expr2)
///   (apply (MakeModule (Op3_ op (Hole) graph1 (Hole)) (debruijnify (vec-append (vec-pop (vec-of (Var \"unused\" 0))) (vec-of expr0) args1 (vec-of expr2)))) (vec-append (vec-pop (vec-of (Var \"unused\" 0))) (vec-of expr0) args1 (vec-of expr2)))
/// )");
/// ```
pub fn generate_module_enumeration_rewrite(
    hole_indicator: &[bool],
    ruleset: Option<&str>,
) -> String {
    let arity: usize = hole_indicator.len();

    fn make_apply_pattern(idx: usize) -> String {
        format!("(apply (MakeModule graph{idx} _) args{idx})", idx = idx)
    }

    fn make_opaque_expr_pattern(idx: usize) -> String {
        format!("expr{idx}", idx = idx)
    }

    let arg_patterns = hole_indicator
        .iter()
        .enumerate()
        .map(|(idx, is_hole)| {
            if *is_hole {
                make_opaque_expr_pattern(idx)
            } else {
                make_apply_pattern(idx)
            }
        })
        .collect::<Vec<_>>();

    let lhs = format!(
        "(Op{arity} op {args})",
        arity = arity,
        args = arg_patterns.join(" ")
    );

    let args_rhs_patterns = hole_indicator
        .iter()
        .enumerate()
        .map(|(idx, is_hole)| {
            if *is_hole {
                "(Hole)".to_string()
            } else {
                format!("graph{idx}", idx = idx).to_string()
            }
        })
        .collect::<Vec<_>>();

    // Creates the list of arguments for the module application.
    // the (vec-pop (vec-of ..)) thing is a hack for type inference not working
    let args_list_expr = format!(
        "(vec-append (vec-pop (vec-of (Var \"unused\" 0))) {args})",
        args = hole_indicator
            .iter()
            .enumerate()
            .map(|(idx, is_hole)| {
                if *is_hole {
                    format!("(vec-of expr{idx})", idx = idx)
                } else {
                    format!("args{idx}", idx = idx)
                }
            })
            .collect::<Vec<_>>()
            .join(" ")
    );

    let rhs = format!(
        "(apply (MakeModule (Op{arity}_ op {graphs}) (debruijnify {args})) {args})",
        arity = arity,
        graphs = args_rhs_patterns.join(" "),
        args = args_list_expr,
    );

    format!(
        "(rewrite
  {lhs}
  {rhs}
{ruleset_flag})",
        lhs = lhs,
        rhs = rhs,
        ruleset_flag = match ruleset {
            Some(ruleset) => format!(":ruleset {}\n", ruleset),
            None => "".to_string(),
        },
    )
}

/// List all modules present in the egraph.
pub fn list_modules(egraph: &mut EGraph, num_variants: usize) {
    for s in egraph
        .parse_and_run_program(
            format!("(query-extract :variants {num_variants} (MakeModule mod args))").as_str(),
        )
        .unwrap()
    {
        println!("{}", s);
    }
}

/// ```
/// use churchroad::*;
/// use egglog::{ArcSort, EGraph, Term, TermDag, Value};
///
/// // Get an egraph, load in a simple design.
/// let mut egraph = EGraph::default();
///
/// import_churchroad(&mut egraph);
/// egraph
///     .parse_and_run_program(
///         r#"
/// ; wire declarations
/// ; $and$<<EOF:2$1_Y
/// (let v0 (Wire "v0" 2))
/// ; a
/// (let v1 (Wire "v1" 2))
/// ; b
/// (let v2 (Wire "v2" 1))
/// ; o
/// (let v3 (Wire "v3" 1))
///
/// ; cells
/// ; TODO not handling signedness
/// (let v4 (Op1 (ZeroExtend 2) v2))
/// (union v0 (Op2 (And) v1 v4))
/// (let v5 (Op1 (Extract 0 0) v0))
/// (union v3 (Op1 (Extract 0 0) v5))
///
/// ; inputs
/// (IsPort "" "a" (Input) (Var "a" 2))
/// (union v1 (Var "a" 2))
/// (IsPort "" "b" (Input) (Var "b" 1))
/// (union v2 (Var "b" 1))
///
/// ; outputs
/// (IsPort "" "o" (Output) v3)
///
/// ; delete wire expressions
/// (delete (Wire "v0" 2))
/// (delete (Wire "v1" 2))
/// (delete (Wire "v2" 1))
/// (delete (Wire "v3" 1))
/// "#,
///     )
///     .unwrap();
///
/// let (inputs, outputs) = get_inputs_and_outputs(&mut egraph);
///
/// // We should have found two inputs, a and b.
/// assert_eq!(inputs.len(), 2);
///
/// fn value_to_string(value: &Value, sort: ArcSort, egraph: &EGraph) -> String {
///     let mut termdag = TermDag::default();
///     let (_, term) = egraph.extract(value.clone(), &mut termdag, &sort);
///     termdag.to_string(&term)
/// }
///
/// // Get expressions for each input.
/// let input_exprs: Vec<String> = inputs
///     .iter()
///     .map(|(sort, value)| value_to_string(value, sort.clone(), &egraph))
///     .collect();
///
/// assert_eq!(input_exprs, vec!["(Var \"a\" 2)", "(Var \"b\" 1)"]);
///
/// let output_expr = value_to_string(&outputs[0].1, outputs[0].0.clone(), &egraph);
/// assert_eq!(output_expr, "(Op1 (Extract 0 0) (Op1 (Extract 0 0) (Op2 (And) (Var \"a\" 2) (Op1 (ZeroExtend 2) (Var \"b\" 1)))))");
/// ```
// TODO(@gussmith23): This really shouldn't require mutability.
pub fn get_inputs_and_outputs(
    egraph: &mut EGraph,
) -> (Vec<(ArcSort, Value)>, Vec<(ArcSort, Value)>) {
    // Get the inputs and outputs.
    let mut inputs = vec![];
    let mut outputs = vec![];
    const NUM_TO_GET: usize = 100;
    let (results, termdag) = egraph.function_to_dag("IsPort".into(), NUM_TO_GET).unwrap();
    assert!(results.len() < NUM_TO_GET);
    for (term, output) in &results {
        assert!(
            matches!(output, Term::Lit(Literal::Unit)),
            "IsPort relation shouldn't have any outputs."
        );

        let children = match term {
            Term::App(_, children) => children,
            _ => panic!(),
        };

        let inout_term = children[2];

        enum InOut {
            Input,
            Output,
        }
        let in_or_out = match termdag.get(inout_term) {
            Term::App(in_or_out, v) => {
                assert_eq!(v.len(), 0);
                if in_or_out == "Input".into() {
                    InOut::Input
                } else if in_or_out == "Output".into() {
                    InOut::Output
                } else {
                    panic!()
                }
            }
            _ => panic!(),
        };

        let churchroad_term = children[3];

        let (sort, value) = egraph
            .eval_expr(
                &egglog::ast::parse::ExprParser::new()
                    .parse(&termdag.to_string(&termdag.get(churchroad_term)))
                    .unwrap(),
            )
            .unwrap();

        match in_or_out {
            InOut::Input => {
                inputs.push((sort, value));
            }
            InOut::Output => {
                outputs.push((sort, value));
            }
        }
    }

    return (inputs, outputs);
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::path::Path;

    use egglog::EGraph;

    /// Doing some exploration of where cyclic extraction breaks in egglog with
    /// Andrew and Vishal.
    #[test]
    fn generate_loop() {
        let mut egraph = EGraph::default();
        import_churchroad(&mut egraph);

        egraph
            .parse_and_run_program(
                r#"
                (let placeholder (Wire "placeholder" 8))
                (let reg (Op1 (Reg 0) placeholder))
                (union placeholder reg)
                (delete (Wire "placeholder" 8))
            "#,
            )
            .unwrap();

        // Uncomment to write out the SVG.
        // let serialized = egraph.serialize_for_graphviz(true);
        // let svg_path = Path::new("tmp").with_extension("svg");
        // serialized.to_svg_file(svg_path).unwrap();

        // Extract reg from Egraph.
        let mut _termdag = TermDag::default();
        let (_sort, _value) = egraph
            .eval_expr(&egglog::ast::Expr::Var((), "reg".into()))
            .unwrap();
        // This will panic, which is what we were trying to get to.
        // It panics with `No cost for Value { tag: "Expr", bits: 6 }`
        // which is basically egglog saying that it can't get a cost because
        // of the cycle. I expected it to loop infinitely, but it's smarter than
        // that.
        //let (_, extracted) = egraph.extract(value, &mut termdag, &sort);

        // Next: can we serialize the egraph? That's the first step to building
        // a new extraction algorithm.
    }

    #[test]
    fn test_module_enumeration_rewrites_up_to_date() {
        // Read in egglog_src/module_enumeration_rewrites.egg and check that it
        // matches the output of generate_module_enumeration_rewrites.
        let actual = std::fs::read_to_string(
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("egglog_src")
                .join("module_enumeration_rewrites.egg"),
        )
        .unwrap();
        let expected = super::generate_module_enumeration_rewrites("enumerate-modules");
        assert_eq!(
            expected, actual,
            "Copy and paste this up-to-date source into module_enumeartion_rewrites.egg:\n{}",
            expected
        );
    }

    #[test]
    fn demo_2024_02_06() {
        // Set the environment variable DEMO_2024_02_06_WRITE_SVGS to anything
        // to produce SVGs.
        fn write_svg(egraph: &EGraph, path: &str) {
            if std::env::var("DEMO_2024_02_06_WRITE_SVGS").is_err() {
                return;
            }
            let serialized = egraph.serialize_for_graphviz(true);
            let svg_path = Path::new(path).with_extension("svg");
            serialized.to_svg_file(svg_path).unwrap();
        }

        ///////////////////////////// BEGIN DEMO ///////////////////////////////

        // We currently need to import Churchroad via Rust (rather than using an
        // egglog `include`) because it depends on a custom primitive.
        let mut egraph = EGraph::default();
        import_churchroad(&mut egraph);

        // Churchroad programs can be very simple circuits, e.g. this one-bit and:
        egraph
            .parse_and_run_program(
                r#"

                (let one-bit-and (Op2 (And) (Var "a" 1) (Var "b" 1)))

            "#,
            )
            .unwrap();
        write_svg(&egraph, "1.svg");

        // Clean up the last example...
        let mut egraph = EGraph::default();
        import_churchroad(&mut egraph);

        // The first interesting feature of Churchroad is that it can represent
        // cyclic circuits using the native features of the egraph. For example,
        // a simple counter circuit looks like this:
        //
        //        ┌────┐
        //      ┌─▼─┐ ┌┴─┐
        //      │reg│ │+1│
        //      └─┬─┘ └▲─┘
        //        └────┘
        //
        // In Churchroad, we can capture this easily using the following
        // commands:
        egraph
            .parse_and_run_program(
                r#"

                ; Instantiate a placeholder wire, which will be connected later.
                (let placeholder (Wire "placeholder" 8))

                ; Generate the +1 box, but feed it with a temporary placeholder.
                (let plusone  (Op2 (Add) placeholder (Op0 (BV 1 8))))

                ; Generate the register, whose input is the output of +1.
                (let reg (Op1 (Reg 0) plusone))

                ; Finally, connect the placeholder to the output of the register
                ; and delete the placeholder.
                (union placeholder reg)
                (delete (Wire "placeholder" 8))

            "#,
            )
            .unwrap();
        write_svg(&egraph, "2.svg");

        // Clean up the last example...
        let mut egraph = EGraph::default();
        import_churchroad(&mut egraph);

        // The next interesting feature of Churchroad is that the representation
        // and its rewrites allow it to find repeated patterns across the
        // egraph.
        //
        // First, let's discuss the underlying representation that allows this.
        // As we saw in the first example, Churchroad can represent circuits
        // directly. However, Churchroad can also represent circuits as
        // applications of abstract modules to concrete inputs:
        egraph
            .parse_and_run_program(
                r#"

                ; An abstract `and` module.
                (let and-module (MakeModule (Op2_ (And) (Hole) (Hole)) (vec-of 0 1)))

                ; We can represent a concrete `and` by applying the abstract
                ; module to concrete inputs.
                (let and (apply and-module (vec-of (Var "a" 1) (Var "b" 1))))

            "#,
            )
            .unwrap();
        write_svg(&egraph, "3.svg");

        // Clean up the last example...
        let mut egraph = EGraph::default();
        import_churchroad(&mut egraph);

        // Translating from the first form to the second (`apply`-based) form is
        // achieved simply with rewrites!
        egraph
            .parse_and_run_program(
                r#"

                ; First, "direct" form.
                (let and (Op2 (And) (Var "a" 1) (Var "b" 1)))

                ; Run module enumeration rewrites to convert to "apply" form.
                (run-schedule (repeat 1 enumerate-modules))
    
            "#,
            )
            .unwrap();
        write_svg(&egraph, "4.svg");

        // Clean up the last example...
        let mut egraph = EGraph::default();
        import_churchroad(&mut egraph);

        // So why do this? Well the `apply`-based form allows us to find
        // repeated patterns in the egraph. As a simple example, imagine we have
        // a series of two `and` gates in a row. This form will allow us to
        // discover that the two `and` gates are the same:
        egraph
            .parse_and_run_program(
                r#"

                ; First, "direct" form.
                (let and (Op2 (And) (Var "a" 1) (Op2 (And) (Var "b" 1) (Var "c" 1))))

                ; Run module enumeration rewrites to convert to "apply" form.
                (run-schedule (saturate enumerate-modules))
    
            "#,
            )
            .unwrap();
        write_svg(&egraph, "5.svg");
    }

    #[test]
    fn test_module_instance() {
        let mut egraph = EGraph::default();
        import_churchroad(&mut egraph);
        egraph.parse_and_run_program(r#"
            ; wire declarations
            ; a
            (let v0 (Wire "v0" 1))
            ; b
            (let v1 (Wire "v1" 1))
            ; out
            (let v2 (Wire "v2" 1))

            ; cells
            (let some_module_instance (ModuleInstance "some_module" (vec-of "a" "b") (vec-of v0 v1)))
            (union (GetOutput some_module_instance "out") v2)

            ; inputs
            (IsPort "" "a" (Input) (Var "a" 1))
            (union v0 (Var "a" 1))
            (IsPort "" "b" (Input) (Var "b" 1))
            (union v1 (Var "b" 1))

            ; outputs
            (IsPort "" "out" (Output) v2)

            ; delete wire expressions
            (delete (Wire "v0" 1))
            (delete (Wire "v1" 1))
            (delete (Wire "v2" 1))
            "#).unwrap();
    }
}
