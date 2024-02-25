use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use egglog::{
    ast::{Literal, Symbol},
    constraint::{SimpleTypeConstraint, TypeConstraint},
    sort::{FromSort, I64Sort, IntoSort, Sort, VecSort},
    EGraph, PrimitiveLike, Term, TermDag, Value,
};

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
    egraph
        .parse_and_run_program(
            r#"
(include "egglog_src/churchroad.egg")
    "#,
        )
        .unwrap();

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

    let enumeration_ruleset_name = "enumerate-modules";
    egraph
        .parse_and_run_program(&format!(
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
        ))
        .unwrap();
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

#[cfg(test)]
mod tests {
    // use super::*;

    use std::path::Path;

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
}
