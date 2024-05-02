use egglog::{
    ast::{parse::ExprParser, Expr},
    ArcSort, EGraph, TermDag, Value,
};
use log::warn;
use std::{collections::HashMap, path::Path};

macro_rules! egglog_test {
    ($name:ident, $path:literal) => {
        egglog_test!($name, $path, |_unused| {});
    };
    ($name:ident, $path:literal, $after_lambda:expr) => {
        #[test]
        fn $name() {
            let mut egraph = egglog::EGraph::default();
            churchroad::import_churchroad(&mut egraph);
            egraph
                .parse_and_run_program(
                    &std::fs::read_to_string(Path::new(env!("CARGO_MANIFEST_DIR")).join($path))
                        .unwrap(),
                )
                .unwrap();
            // Uncomment to see SVGs, or add the following lines to your $after.
            // let serialized = egraph.serialize_for_graphviz();
            // let svg_path = Path::new($path).with_extension("svg");
            // serialized.to_svg_file(svg_path).unwrap();

            let after_lambda = $after_lambda;
            after_lambda(&mut egraph);
        }
    };
}

egglog_test!(test_1, "tests/egglog_tests/construct_sequential_cycle.egg");
egglog_test!(permuter, "tests/egglog_tests/permuter.egg");
egglog_test!(typing, "tests/egglog_tests/typing.egg");

fn create_rewrites(
    egraph: &egglog::EGraph,
    value: &Value,
    sort: &ArcSort,
    num_rewrites: i64,
    replacement_map: &HashMap<Expr, Expr>,
) {
    // I want to find the "expr" in the egraph, where "expr" isn't actually an
    // expression for now, but instead just a let-bound name e.g. "lut6out".
    // Seems like there should be a way to get these definitions. I guess part
    // of the point of the egraph is that the name will just point to an eclass,
    // not a specific expression. We'll still need to extract. This is where
    // we can do interesting things. Whatever we extract will act as the LHS of
    // the rewrite. So if we can extract multiple things, we'll get multiple
    // LHSs. For now we just need to get a single one extracting.

    /// Extract a random term from the egraph at the given value.
    fn extract_random(egraph: &egglog::EGraph, value: &Value, sort: &ArcSort, _seed: i64) -> Expr {
        warn!("This function currently always returns the same expr.");
        let mut termdag = TermDag::default();
        let (_size, extracted) = egraph.extract(*value, &mut termdag, sort);
        termdag.term_to_expr(&extracted)
    }

    // Get a bunch of random exprs that will serve as the left hand sides of the
    // rewrite. Sort and dedup to remove duplciates.
    let mut exprs: Vec<_> = (0..num_rewrites)
        .map(|seed| extract_random(egraph, value, sort, seed))
        .collect();
    exprs.sort();
    exprs.dedup();

    if exprs.len() < num_rewrites.try_into().unwrap() {
        warn!(
            "Expected {} exprs to turn into rewrites, but only got {}; there \
               must have been duplicates. In the future we can add a retry \
               mechanism.",
            num_rewrites,
            exprs.len()
        );
    }

    for expr in &exprs {
        println!("{}", expr);
    }

    // Replace requested expressions with other expressions. This allows us to,
    // for example, replace the LUT memory input(s) with symbolic solvable
    // expressions, in the case of a LUT expression. This replacement isn't
    // checked for correctness.
    fn replace_in_expr(expr: Expr, replacement_map: &HashMap<Expr, Expr>) -> Expr {
        expr.visit_exprs(&mut |expr| {
            if replacement_map.contains_key(&expr) {
                replacement_map[&expr].clone()
            } else {
                expr.clone()
            }
        })
    }

    let exprs: Vec<_> = exprs
        .iter()
        .map(|expr| replace_in_expr(expr.clone(), replacement_map))
        .collect();

    for expr in &exprs {
        println!("{}", expr);
    }
    // let rewrites = [make_rewrite(/*LHS*/ lhs_expr, /* RHS */ expr) for lhs_expr in exprs];

    // Or remove duplicate expressions before the previous step.
    // let rewrites = remove_duplicates(rewrites);
}

egglog_test!(
    agilex_alm,
    "tests/egglog_tests/agilex_alm.egg",
    |egraph: &mut EGraph| {
        let (sort, value) = egraph
            .eval_expr(&egglog::ast::Expr::Var((), "lut6out".into()))
            .unwrap();
        create_rewrites(
            egraph,
            &value,
            &sort,
            1,
            &vec![
                (
                    ExprParser::new().parse("(Var \"a\" 1)").unwrap(),
                    Expr::Var((), "a".into()),
                ),
                (
                    ExprParser::new().parse("(Var \"b\" 1)").unwrap(),
                    Expr::Var((), "b".into()),
                ),
                (
                    ExprParser::new().parse("(Var \"c0\" 1)").unwrap(),
                    Expr::Var((), "c0".into()),
                ),
                (
                    ExprParser::new().parse("(Var \"c1\" 1)").unwrap(),
                    Expr::Var((), "c1".into()),
                ),
                (
                    ExprParser::new().parse("(Var \"d0\" 1)").unwrap(),
                    Expr::Var((), "d0".into()),
                ),
                (
                    ExprParser::new().parse("(Var \"d1\" 1)").unwrap(),
                    Expr::Var((), "d1".into()),
                ),
                (
                    ExprParser::new().parse("(Var \"e\" 1)").unwrap(),
                    Expr::Var((), "e".into()),
                ),
                (
                    ExprParser::new().parse("(Var \"f\" 1)").unwrap(),
                    Expr::Var((), "f".into()),
                ),
                (
                    ExprParser::new().parse("(Var \"lut4_g0_mem\" 16)").unwrap(),
                    ExprParser::new()
                        .parse("(Symbolic \"lut4_g0_mem\" 16)")
                        .unwrap(),
                ),
                (
                    ExprParser::new().parse("(Var \"lut4_p0_mem\" 16)").unwrap(),
                    ExprParser::new()
                        .parse("(Symbolic \"lut4_p0_mem\" 16)")
                        .unwrap(),
                ),
                (
                    ExprParser::new().parse("(Var \"lut4_g1_mem\" 16)").unwrap(),
                    ExprParser::new()
                        .parse("(Symbolic \"lut4_g1_mem\" 16)")
                        .unwrap(),
                ),
                (
                    ExprParser::new().parse("(Var \"lut4_p1_mem\" 16)").unwrap(),
                    ExprParser::new()
                        .parse("(Symbolic \"lut4_p1_mem\" 16)")
                        .unwrap(),
                ),
            ]
            .into_iter()
            .collect(),
        );
    }
);

egglog_test!(half_adder, "tests/egglog_tests/half_adder.egg");

#[test]
fn antiunify() {
    let mut egraph = egglog::EGraph::default();
    churchroad::import_churchroad(&mut egraph);

    egraph
        .parse_and_run_program(
            r#"
(let out (debruijnify (vec-of (Var "x" 8) (Var "y" 16))))
(check (= out (vec-of 0 1)))
(let out2 (debruijnify (vec-of (Var "x" 8) (Var "y" 16) (Var "x" 8) (Var "z" 1) (Var "y" 16))))
(check (= out2 (vec-of 0 1 0 2 1)))

(let const (Op0 (BV 23 8)))
(run enumerate-modules 1)
(check 
 (= 
  const
  ; The (vec-pop (vec-of ...)) thing is a hack, should be removable in the future.
  (apply (MakeModule (Op0_ (BV 23 8)) (vec-pop (vec-of 0))) (vec-pop (vec-of (Var "unused" 0))))))


(let not (Op1 (Not) (Var "x" 8)))
(run enumerate-modules 1)
(check
 (=
  not
  (apply (MakeModule (Op1_ (Not) (Hole)) (vec-of 0)) (vec-of (Var "x" 8)))))

(let notnot (Op1 (Not) (Op1 (Not) (Var "x" 8))))
(run enumerate-modules 1)  
(check
 (=
  notnot
  (apply (MakeModule (Op1_ (Not) (Op1_ (Not) (Hole))) (vec-of 0)) (vec-of (Var "x" 8)))))

(let and (Op2 (And) (Var "x" 8) (Var "y" 8)))
(run enumerate-modules 1)
(check
 (=
  and
  (apply (MakeModule (Op2_ (And) (Hole) (Hole)) (vec-of 0 1)) (vec-of (Var "x" 8) (Var "y" 8)))))

(let orand (Op2 (Or) and and))
(run enumerate-modules 1)
(check
 (= 
  orand
  (apply (MakeModule (Op2_ (Or) (Op2_ (And) (Hole) (Hole)) (Hole)) (vec-of 0 1 2)) (vec-of (Var "x" 8) (Var "y" 8) and ))))
(check
 (= 
  orand
  (apply (MakeModule (Op2_ (Or) (Hole) (Op2_ (And) (Hole) (Hole))) (vec-of 0 1 2)) (vec-of and (Var "x" 8) (Var "y" 8)))))
(check
 (= 
  orand
  (apply (MakeModule (Op2_ (Or) (Op2_ (And) (Hole) (Hole)) (Op2_ (And) (Hole) (Hole))) (vec-of 0 1 0 1)) (vec-of (Var "x" 8) (Var "y" 8) (Var "x" 8) (Var "y" 8)))))
    "#,
        )
        .unwrap();

    // let serialized = egraph.serialize_for_graphviz(true);
    // let svg_path = Path::new("tmp").with_extension("svg");
    // serialized.to_svg_file(svg_path).unwrap();
}

#[test]
fn antiunify_permuter() {
    let mut egraph = egglog::EGraph::default();
    churchroad::import_churchroad(&mut egraph);

    egraph
        .parse_and_run_program(
            r#"
(include "tests/egglog_tests/permuter.egg")
"#,
        )
        .unwrap();

    // Run enumeration rewrites
    egraph
        .parse_and_run_program("(run-schedule (saturate enumerate-modules))")
        .unwrap();

    egraph
        .parse_and_run_program("(run-schedule (saturate typing))")
        .unwrap();

    egraph
        .parse_and_run_program("(run-schedule (repeat 100 (seq (run expansion) (saturate typing) (saturate enumerate-modules))))")
        .unwrap();

    // let serialized = egraph.serialize_for_graphviz(true);
    // let svg_path = Path::new("tmp").with_extension("svg");
    // serialized.to_svg_file(svg_path).unwrap();
}

#[test]
fn find_loop() {
    let mut egraph = egglog::EGraph::default();
    churchroad::import_churchroad(&mut egraph);

    egraph
        .parse_and_run_program(
            r#"

; This file comes from an example permuter design given to us by Intel.

; wire declarations
; $0\dout[15:0]
(let v0 (Wire "v0" 16))
; $auto$rtlil.cc:2492:Or$13
(let v1 (Wire "v1" 1))
; $auto$rtlil.cc:2558:Mux$11
(let v2 (Wire "v2" 16))
; $auto$rtlil.cc:2558:Mux$9
(let v3 (Wire "v3" 16))
; $procmux$3_CMP
(let v4 (Wire "v4" 1))
; $procmux$4_CMP
(let v5 (Wire "v5" 1))
; $procmux$5_CMP
(let v6 (Wire "v6" 1))
; clk
(let v7 (Wire "v7" 1))
; control
(let v8 (Wire "v8" 2))
; din
(let v9 (Wire "v9" 16))
; dout
(let v10 (Wire "v10" 16))

; cells
; Loop involves this node; if we uncomment it the loop comes back.
;(union v1 (Op2 (Or) v5 v4))
; { \din [11:8] \din [15:12] \din [3:0] \din [7:4] }
(let v11 (Op1 (Extract 7 4) v9))
(let v12 (Op1 (Extract 3 0) v9))
(let v13 (Op1 (Extract 15 12) v9))
(let v14 (Op1 (Extract 11 8) v9))
(let v15 (Op2 (Concat) v11 v12))
(let v16 (Op2 (Concat) v15 v13))
(let v17 (Op2 (Concat) v16 v14))
(union v2 (Op3 (Mux) v6 v9 v17))
(union v0 (Op3 (Mux) v1 v2 v3))
; { \din [7:0] \din [15:8] }
(let v18 (Op1 (Extract 15 8) v9))
(let v19 (Op1 (Extract 7 0) v9))
(let v20 (Op2 (Concat) v18 v19))
; { \din [3:0] \din [7:4] \din [11:8] \din [15:12] }
(let v21 (Op2 (Concat) v13 v14))
(let v22 (Op2 (Concat) v21 v11))
(let v23 (Op2 (Concat) v22 v12))
(union v3 (Op3 (Mux) v4 v20 v23))

(run enumerate-modules 100)
"#,
        )
        .unwrap();

    churchroad::list_modules(&mut egraph, 1000);
}
