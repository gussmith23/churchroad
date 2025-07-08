use core::panic;
use std::collections::HashMap;
use std::fmt::Display;
use std::fs::create_dir_all;
use std::io::{stdin, stdout, Write};
use std::path::PathBuf;
use std::sync::{Arc, LazyLock};

use churchroad::global_greedy_dag::GlobalGreedyDagExtractor;
use churchroad::{
    call_lakeroad_on_primitive_interface_and_spec, find_primitive_interfaces_serialized,
    find_spec_for_primitive_interface_including_nodes, from_verilog_file, get_bitwidth_for_node,
    get_inputs_and_outputs_serialized, node_to_string, to_verilog_egraph_serialize, util,
    RandomExtractor,
};
use clap::ValueHint::FilePath;
use clap::{ArgAction, Parser, ValueEnum};
use egglog::sort::EqSort;
use egglog::{ArcSort, EGraph, SerializeConfig};
use egraph_serialize::{ClassId, NodeId};
use log::{debug, info, warn};
use tempfile::NamedTempFile;

static EXPR_SORT: LazyLock<ArcSort> = std::sync::LazyLock::new(|| {
    Arc::new(EqSort {
        name: "Expr".into(),
    })
});

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(long, value_hint=FilePath)]
    filepath: PathBuf,

    #[arg(long)]
    top_module_name: String,

    #[arg(long)]
    svg_dirpath: Option<PathBuf>,

    #[arg(long)]
    architecture: Architecture,

    #[arg(long)]
    simulate: bool,

    #[arg(long)]
    out_filepath: Option<PathBuf>,

    #[arg(long)]
    cvc5: bool,
    #[arg(long)]
    bitwuzla: bool,
    #[arg(long)]
    yices: bool,
    #[arg(long)]
    stp: bool,

    #[arg(long, action=ArgAction::Append)]
    simulate_with_verilator_arg: Vec<String>,

    /// Interact with the egraph on the command line after running rewrites
    #[arg(long)]
    interact: bool,
}

#[derive(ValueEnum, Clone, Debug)]
enum Architecture {
    XilinxUltrascalePlus,
}

/// Run commands to interact with the egraph.
fn _egraph_interact(egraph: &mut EGraph) {
    println!("Now interacting with egraph. Type any egglog command. Use Ctrl+D to exit.");
    'interact: loop {
        print!("> ");
        stdout().flush().unwrap();
        let mut buf = String::new();
        stdin().read_line(&mut buf).unwrap();
        if buf.is_empty() {
            log::info!("EOF while interacting; continuing on with Churchroad.");
            break 'interact;
        }
        let out = egraph.parse_and_run_program(None, &buf);
        if let Ok(out) = out {
            println!("{}", out.join("\n"));
        } else {
            println!("Error: {:?}", out);
        }
    }
}

// TODO(@gussmith23): Seems redundant to do this; I think clap already does something like this under the hood.
impl Display for Architecture {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Architecture::XilinxUltrascalePlus => f.write_str("xilinx-ultrascale-plus"),
        }
    }
}

fn main() {
    env_logger::init();
    let args = Args::parse();

    // STEP 1: Read in design, put it in an egraph.
    // simcheck=true just runs some basic checks.
    let mut egraph = from_verilog_file(
        &args.filepath,
        &args.top_module_name,
        true,
        true,
        HashMap::default(),
    );

    info!("Loaded design into egraph.");

    // Get initial input and output ports.
    let outputs: Vec<_> = {
        let serialized = egraph.serialize(SerializeConfig::default());
        get_inputs_and_outputs_serialized(&serialized)
            .1
            .drain(..)
            .map(|(output_name, class_id)| (egraph.class_id_to_value(&class_id), output_name))
            .collect()
    };

    let output_names_and_bws: Vec<_> = {
        let serialized = egraph.serialize(SerializeConfig::default());
        get_inputs_and_outputs_serialized(&serialized)
            .1
            .drain(..)
            .map(|(output_name, class_id)| {
                (
                    output_name,
                    get_bitwidth_for_node(&serialized, &serialized[&class_id].nodes[0]).unwrap(),
                )
            })
            .collect()
    };
    let input_names_and_bws: Vec<_> = {
        let serialized = egraph.serialize(SerializeConfig::default());
        get_inputs_and_outputs_serialized(&serialized)
            .0
            .drain(..)
            .map(|(input_name, class_id)| {
                (
                    input_name,
                    get_bitwidth_for_node(&serialized, &serialized[&class_id].nodes[0]).unwrap(),
                )
            })
            .collect()
    };

    if let Some(svg_dirpath) = &args.svg_dirpath {
        create_dir_all(svg_dirpath).unwrap();
        let serialized = egraph.serialize(SerializeConfig::default());
        serialized
            .to_svg_file(svg_dirpath.join("initial_egraph.svg"))
            .unwrap();
        info!(
            "Initial egraph svg: {}",
            svg_dirpath.join("initial_egraph.svg").to_string_lossy()
        );
    }

    // STEP 2: Run mapping rewrites, proposing potential mappings which Lakeroad
    // will later confirm or prove not possible via program synthesis.
    //
    // Currently, we only have a single rewrite, which looks for "narrower"
    // multiplies which should fit on a single DSP.
    //
    // In the future, there's much more we can do here, including:
    // - Parameterizing rewrites based on architecture (i.e. instead of
    //   hardcoding "18" below, we can get the appropriate width from the
    //   architecture description.)
    // - Mixing mapping rewrites with "expansion" rewrites. For example, adding
    //   a rewrite which breaks a large multiply into smaller multiplies. Again,
    //   these rewrites can also be parameterized by arch. descr.
    // - Automated generation of rewrites. This is a more interesting research
    //   question! Could be a place we use ChatGPT; i.e. give it the PDF of
    //   the DSP manual, give it a description of the Churchroad IR, and ask it
    //   to propose patterns.
    info!("Running rewrites.");
    egraph
        .parse_and_run_program(
            None,
            r#"
        ; Discover the "interesting" parts of bitvectors---basically, the parts
        ; that are not just zero-extension or sign-extension bits.
        (relation RealBitwidth (Expr i64))
        (rule 
         ((Var name bw))
         ((RealBitwidth (Var name bw) bw))
         :ruleset typing)
        (rule
            ((= ?extended (Op1 (ZeroExtend ?n) ?expr))
             ; This is already known based on the ops above, but good for sanity
             ; checking.
             (HasType ?extended (Bitvector ?n))
             (HasType ?expr (Bitvector ?m))
             (>= ?n ?m))
            ((RealBitwidth ?extended ?m))
            :ruleset typing)
        (rule
            ((= ?extended (Op1 (SignExtend ?n) ?expr))
             ; This is already known based on the ops above, but good for sanity
             ; checking.
             (HasType ?extended (Bitvector ?n))
             (HasType ?expr (Bitvector ?m))
             (>= ?n ?m))
            ((RealBitwidth ?extended ?m))
            :ruleset typing)
        ; Real width of an extract.
        ; worked examples:
        ; 9876543210
        ;    [xx   ] <-- real width of orig = 7
        ; [  xx]     <-- extract 9:4 inclusive
        ; new real width = 3 (x locations)
        ; r - lo 
        ; (min of hi, r-1) down to lo
        ; (min hi r-1) - lo + 1
        ; = (min 9 7-1) - 4 + 1 = 3
        ; example 2
        ; 9876543210
        ; [  xxxxxx]<-- real width of orig = 10
        ;    [xxxxx]<-- extract 6:0 inclusive
        ; new real width = 7
        ; (min 6 10-1) - 0 + 1 = 7
        (rule
            ((= ?extracted (Op1 (Extract ?hi ?lo) ?expr))
             (RealBitwidth ?expr ?m))
            ((RealBitwidth ?extracted (+ 1 (- (min ?hi (- ?m 1)) ?lo))))
            :ruleset typing)
        ; The max bitwidth of a multiply is the sum of the bitwidths of the
        ; operands, which could be less than the bitwidth of the mul expr. 
        (rule
         ((= ?mul-expr (Op2 (Mul) ?a ?b))
          (RealBitwidth ?a ?a-bw)
          (RealBitwidth ?b ?b-bw)
          (HasType ?mul-expr (Bitvector ?n)))
         ((RealBitwidth ?mul-expr (min ?n (+ ?a-bw ?b-bw))))
         :ruleset typing)
        ; Real bitwidth of shifting by a constant
        (rule
         ((= ?shifted (Op2 (Shl) ?a (Op0 (BV ?shift-amount ?_))))
          (RealBitwidth ?a ?a-bw)
          (HasType ?shifted (Bitvector ?n)))
         ((RealBitwidth ?shifted (min ?n (+ ?a-bw ?shift-amount))))
         :ruleset typing)
        ; Real bitwidth of an add
        (rule
         ((= ?add-expr (Op2 (Add) ?a ?b))
          (RealBitwidth ?a ?a-bw)
          (RealBitwidth ?b ?b-bw)
          (HasType ?add-expr (Bitvector ?n)))
         ((RealBitwidth ?add-expr (min ?n (max ?a-bw ?b-bw))))
         :ruleset typing)
        
        (ruleset mapping)
        ;; TODO need to write a rewrite that deals with multiplying zero extended bvs
        (rule 
            ((= ?expr (Op2 (Mul) ?a ?b))
             (HasType ?expr (Bitvector ?n))
             (< ?n 18))
            (
             (let ?id (random-string 64))
             (union ?a (InputOutputMarker "a" ?id))
             (union ?b (InputOutputMarker "b" ?id))
             (union ?expr (InputOutputMarker "out" ?id))
             (union ?expr (PrimitiveInterfaceDSP 0 ?id ?a ?b)))
            :ruleset mapping)
        ;; TODO bitwidths are hardcoded here
        (rule 
            ((= expr (Op2 (Mul) (Op1 (ZeroExtend ?n) ?a) (Op1 (ZeroExtend ?n) ?b)))
             (HasType expr (Bitvector ?n))
             (HasType ?a (Bitvector ?a-bw))
             (HasType ?b (Bitvector ?b-bw))
             (<= ?a-bw 16)
             (<= ?b-bw 16)
             (< ?n 36)
             )
            (
             (let ?id (random-string 64))
             (union ?a (InputOutputMarker "a" ?id))
             (union ?b (InputOutputMarker "b" ?id))
             (union expr (InputOutputMarker "out" ?id))
             (union expr (PrimitiveInterfaceDSP 0 ?id ?a ?b)))
            :ruleset mapping)
        (rule 
            ((= expr (Op2 (Mul) ?a ?b))
             (HasType expr (Bitvector ?n))
             (RealBitwidth ?a ?a-bw)
             (RealBitwidth ?b ?b-bw)
             (<= ?a-bw 17)
             (<= ?b-bw 17)
             (<= ?n 48)
             )
            ((let ?id (random-string 64))
             (union ?a (InputOutputMarker "a" ?id))
             (union ?b (InputOutputMarker "b" ?id))
             (union expr (InputOutputMarker "out" ?id))
             (union expr (PrimitiveInterfaceDSP 0 ?id ?a ?b)))
            :ruleset mapping)
        ; One stage DSP.
        (rule 
            ((= ?expr (Op2 (Reg ?init) ?clk (Op2 (Mul) ?a ?b)))
             (RealBitwidth ?a ?a-bw)
             (RealBitwidth ?b ?b-bw)
             (RealBitwidth (Op2 (Mul) ?a ?b) ?mul-bw)
             (HasType ?a (Bitvector ?a-bw-full))
             (HasType ?b (Bitvector ?b-bw-full))
             (<= ?a-bw 17)
             (<= ?b-bw 17)
             (<= ?mul-bw 48)
             )
            ((let ?id (random-string 64))
             (union ?a (InputOutputMarker "a" ?id))
             (union ?b (InputOutputMarker "b" ?id))
             (union ?expr (InputOutputMarker "out" ?id))
             (union ?expr 
              (PrimitiveInterfaceDSP 1 ?id ?a ?b)))
            :ruleset mapping)
        (rule 
            ((= ?expr (Op2 (Add) (Op1 ?extract-or-zero-extend-TODO-kind-of-a-hack (Op2 (Mul) ?a ?b)) ?c))
             (RealBitwidth ?a ?a-bw)
             (RealBitwidth ?b ?b-bw)
             (RealBitwidth ?c ?c-bw)
             (RealBitwidth (Op2 (Mul) ?a ?b) ?mul-bw)
             (HasType ?expr (Bitvector ?add-bw))
             (HasType ?a (Bitvector ?a-bw-full))
             (HasType ?b (Bitvector ?b-bw-full))
             (HasType ?c (Bitvector ?c-bw-full))
             (<= ?a-bw 17)
             (<= ?b-bw 17)
             (<= ?c-bw 48)
             (<= ?mul-bw 48)
             ; TODO we need some kind of constraint here
             (<= ?add-bw 48)
             )
            ((let ?id (random-string 64))
             (union ?a (InputOutputMarker "a" ?id))
             (union ?b (InputOutputMarker "b" ?id))
             (union ?c (InputOutputMarker "c" ?id))
             (union ?expr (InputOutputMarker "out" ?id))
             (union ?expr 
              (PrimitiveInterfaceDSP3 0 ?id ?a ?b ?c)))
            :ruleset mapping)
        (rule 
            ((= ?expr (Op2 (Add) (Op2 (Ashr) ?c (Op0 (BV 17 ?unused-bv-bw))) (Op1 (SignExtend ?unused-sign-extend-bw) (Op1 (Extract ?unused-extract-idx-hi ?unused-extract-idx-lo) (Op2 (Mul) ?a ?b)))))
             (RealBitwidth ?a ?a-bw)
             (RealBitwidth ?b ?b-bw)
             (RealBitwidth ?c ?c-bw)
             (RealBitwidth (Op2 (Mul) ?a ?b) ?mul-bw)
             (HasType ?expr (Bitvector ?add-bw))
             (HasType ?a (Bitvector ?a-bw-full))
             (HasType ?b (Bitvector ?b-bw-full))
             (HasType ?c (Bitvector ?c-bw-full))
             (<= ?a-bw 17)
             (<= ?b-bw 17)
             (<= ?c-bw 48)
             (<= ?mul-bw 48)
             ; TODO we need some kind of constraint here
             (<= ?add-bw 48)
             )
            ((let ?id (random-string 64))
             (union ?a (InputOutputMarker "a" ?id))
             (union ?b (InputOutputMarker "b" ?id))
             (union ?c (InputOutputMarker "c" ?id))
             (union ?expr (InputOutputMarker "out" ?id))
             (union ?expr 
              (PrimitiveInterfaceDSP3 0 ?id ?a ?b ?c)))
            :ruleset mapping)
        (rule 
            ((= ?expr (Op2 (Add) (Op2 (Mul) (Op1 (ZeroExtend ?n) ?a) (Op1 (ZeroExtend ?n) ?b)) ?c))
             (HasType ?expr (Bitvector ?n))
             (HasType ?a (Bitvector ?a-bw))
             (HasType ?b (Bitvector ?b-bw))
             (HasType ?c (Bitvector ?c-bw))
             (<= ?a-bw 16)
             (<= ?b-bw 16)
             (<= ?c-bw 48)
             (< ?n 36)
             )
            (
             (let ?id (random-string 64))
             (union ?a (InputOutputMarker "a" ?id))
             (union ?b (InputOutputMarker "b" ?id))
             (union ?c (InputOutputMarker "c" ?id))
             (union ?expr (InputOutputMarker "out" ?id))
             (union ?expr (PrimitiveInterfaceDSP3 0 ?id ?a ?b ?c)))
            :ruleset mapping)
        ; Adder with DSP.
        (rule 
            ((= ?expr (Op2 (Add) ?a ?b))
             (HasType ?expr (Bitvector ?n))
             (RealBitwidth ?a ?a-bw)
             (RealBitwidth ?b ?b-bw)
             (<= ?a-bw 48)
             (<= ?b-bw 48)
             (<= ?n 48)
             )
            (
             (let ?id (random-string 64))
             (union ?a (InputOutputMarker "a" ?id))
             (union ?b (InputOutputMarker "b" ?id))
             (union ?expr (InputOutputMarker "out" ?id))
             (union ?expr (PrimitiveInterfaceWideAddDSP 0 ?id ?a ?b)))
            :ruleset mapping)
        
        (ruleset transform)
        ;(rule
        ;    ((= ?expr (Op2 (Mul) (Op1 (ZeroExtend b-bw) a) b))
        ;     (HasType ?expr (Bitvector ?expr-bw))
        ;     (HasType a (Bitvector a-bw))
        ;     (HasType b (Bitvector b-bw))
        ;     (<= ?expr-bw 48)
        ;     (<= a-bw 16)
        ;     (<= b-bw 32)
        ;     (= 0 (% ?expr-bw 2)))
        ;    ((union 
        ;       ?expr 
        ;       (Op2 (Add)
        ;        (Op2 (Mul) (Op1 (ZeroExtend ?expr-bw) a) (Op1 (ZeroExtend ?expr-bw) (Op1 (Extract (- (/ ?expr-bw 2) 1) 0) b)))
        ;        (Op2 (Shl) (Op2 (Mul) (Op1 (ZeroExtend ?expr-bw) a) (Op1 (ZeroExtend ?expr-bw) (Op1 (Extract (- ?expr-bw 1) (/ ?expr-bw 2)) b))) (Op0 (BV (/ ?expr-bw 2) ?expr-bw))))))
        ;    :ruleset transform)

        ; General mul splitting rewrite
        ; TODO there's gotta be things wrong here w/ sign vs zero extend
        ; TODO This is buggy, keeps running forever
        ;(rule
        ;    ((= ?expr (Op2 (Mul) ?a ?b))
        ;     (RealBitwidth ?b ?b-real-bw)
        ;     (HasType ?expr (Bitvector ?expr-bw))
        ;     (> ?b-real-bw 16))
        ;    ((union 
        ;       ?expr 
        ;       (Op2 (Add)
        ;        (Op2 (Mul) 
        ;         ?a
        ;         ; TODO hardcoded extraction width
        ;         (Op1 (ZeroExtend ?expr-bw) (Op1 (Extract 15 0) ?b)))
        ;        (Op2 (Shl) 
        ;         (Op2 (Mul) 
        ;          ?a
        ;          ; TODO hardcoded extraction width
        ;          (Op1 (ZeroExtend ?expr-bw) (Op1 (Extract (- ?b-real-bw 1) 16) ?b)))
        ;         ; TODO hardcoded shift amount
        ;         (Op0 (BV 16 ?expr-bw))))))
        ;    :ruleset transform)
        ;; And the other direction
        ;(rule
        ;    ((= ?expr (Op2 (Mul) ?a ?b))
        ;     (RealBitwidth ?a ?a-real-bw)
        ;     (HasType ?expr (Bitvector ?expr-bw))
        ;     (> ?a-real-bw 16))
        ;    ((union 
        ;       ?expr 
        ;       (Op2 (Add)
        ;        (Op2 (Mul) 
        ;         ; TODO hardcoded extraction width
        ;         (Op1 (ZeroExtend ?expr-bw) (Op1 (Extract 15 0) ?a))
        ;         ?b)
        ;        (Op2 (Shl) 
        ;         (Op2 (Mul) 
        ;          ; TODO hardcoded extraction width
        ;          (Op1 (ZeroExtend ?expr-bw) (Op1 (Extract (- ?a-real-bw 1) 16) ?a))
        ;          ?b
        ;          )
        ;         ; TODO hardcoded shift amount
        ;         (Op0 (BV 16 ?expr-bw))))))
        ;    :ruleset transform)

        ; TODO working on this
        ; Mul splitting rewrite that is actually used to map to DSPs on Xilinx.
        ; in math:
        ; a * b = (a * b1 + (a * b0) >> 17)[15:0] ++ (a * b0)[15:0]
        ; where b1 and b0 are the upper and lower halves of b, and here we're
        ; assuming b=32 bits and a=16 bits. But we will write the rewrite
        ; to be general.
        ;
        ; Here's a generalized version of the rule, verified in Rosette:
        ; (define a-bw 9)
        ; (define b-bw 8)
        ; (define a0-bw 4)
        ; (define out-bw (+ a-bw b-bw))
        ; (define ap-bw (- a-bw a0-bw))
        ; (define-symbolic a (bitvector a-bw))
        ; (define-symbolic b (bitvector b-bw))
        ; (define a0 (extract (- a0-bw 1) 0 a))
        ; (define ap (extract (- a-bw 1) a0-bw a))
        ; ;;; Initial implementation
        ; (define spec (bvmul (sign-extend a (bitvector out-bw)) (sign-extend b (bitvector out-bw))))
        ; ;;; Rewritten implementation, broken up for DSP mapping
        ; (define lower-mul
        ;   (bvmul (zero-extend a0 (bitvector (+ a0-bw b-bw))) (sign-extend b (bitvector (+ a0-bw b-bw)))))
        ; (define upper-mul
        ;   (bvmul (sign-extend ap (bitvector (+ ap-bw b-bw))) (sign-extend b (bitvector (+ ap-bw b-bw)))))
        ; (define rewritten
        ;   ; how wide should we extend the inputs of the add to? to however wide
        ;   ; that portion of the multiplication is. so i think it's full mul width - a0-bw
        ;   (concat (bvadd (sign-extend upper-mul (bitvector (- out-bw a0-bw)))
        ;                  (sign-extend (bvashr lower-mul (bv a0-bw (+ a0-bw b-bw))) (bitvector (- out-bw a0-bw))))
        ;           (extract (- a0-bw 1) 0 lower-mul)))
        ; (define maybe-model (verify (assert (bveq spec rewritten))))
        (rule
         (
          (= ?expr (Op2 (Mul) ?a ?b))
          (RealBitwidth ?a ?a-real-bw)
          (RealBitwidth ?b ?b-real-bw)
          (HasType ?expr (Bitvector ?expr-bw))
          ; Make sure we can actually split this multiply
          (> ?a-real-bw 17)
         )
         (
          (let a0-bw 17)
          (let a0 (Op1 (Extract (- a0-bw 1) 0) ?a))
          (let a1 (Op1 (Extract (- ?a-real-bw 1) a0-bw) ?a))
          (let a1-bw (- ?a-real-bw a0-bw))
          (let b-extracted (Op1 (Extract (- ?b-real-bw 1) 0) ?b))
          (let b-bw ?b-real-bw)
          (let out-bw (+ ?a-real-bw ?b-real-bw))
          (let lower-mul-bw (+ a0-bw ?b-real-bw))
          (let lower-mul
            (Op2 (Mul) (Op1 (ZeroExtend lower-mul-bw) a0) (Op1 (SignExtend lower-mul-bw) b-extracted)))
          (let upper-mul-bw (+ a1-bw ?b-real-bw))
          (let upper-mul
            (Op2 (Mul) (Op1 (SignExtend upper-mul-bw) a1) (Op1 (SignExtend upper-mul-bw) b-extracted)))
          (let rewritten
           ; how wide should we extend the inputs of the add to? to however wide
           ; that portion of the multiplication is. so i think it's full mul width - a0-bw
           ; TODO still working on this
           ;
           ; Note that we need to extend the expression back up to the original
           ; expr's bitwidth at the very end.
           (Op1 (SignExtend ?expr-bw)
            (Op2 (Concat) 
             (Op2 (Add) 
              upper-mul
              (Op1 (SignExtend upper-mul-bw) 
               (Op1 (Extract (- (- lower-mul-bw a0-bw) 1) 0)
                (Op2 (Ashr) lower-mul (Op0 (BV a0-bw lower-mul-bw))))))
             (Op1 (Extract (- a0-bw 1) 0) lower-mul))))
          (union ?expr rewritten)
         )
         :ruleset transform)
        ; Commutated version of the above rule.
        ; TODO(@gussmith23): these manually need to be kept in sync.
        ; TODO(@gussmith23): Could definitely be copy-paste errors in here.
        (rule
         (
          (= ?expr (Op2 (Mul) ?a ?b))
          (RealBitwidth ?a ?a-real-bw)
          (RealBitwidth ?b ?b-real-bw)
          (HasType ?expr (Bitvector ?expr-bw))
          ; Make sure we can actually split this multiply
          (> ?b-real-bw 17)
         )
         (
          (let b0-bw 17)
          (let b0 (Op1 (Extract (- b0-bw 1) 0) ?b))
          (let b1 (Op1 (Extract (- ?b-real-bw 1) b0-bw) ?b))
          (let b1-bw (- ?b-real-bw b0-bw))
          ; TODO(@gussmith23): this can't just be named a, collides with the top-level binding. we should remove those bindings...
          (let a-extracted (Op1 (Extract (- ?a-real-bw 1) 0) ?a))
          (let a-bw ?a-real-bw)
          (let out-bw (+ ?a-real-bw ?b-real-bw))
          (let lower-mul-bw (+ b0-bw a-bw))
          (let upper-mul-bw (+ b1-bw a-bw))
          (let lower-mul
            (Op2 (Mul) (Op1 (SignExtend lower-mul-bw) a-extracted) (Op1 (ZeroExtend lower-mul-bw) b0)))
          (let upper-mul
            (Op2 (Mul) (Op1 (SignExtend upper-mul-bw) a-extracted) (Op1 (SignExtend upper-mul-bw) b1)))
          (let rewritten
           ; how wide should we extend the inputs of the add to? to however wide
           ; that portion of the multiplication is. so i think it's full mul width - a0-bw
           ; TODO still working on this
           ;
           ; Note that we need to extend the expression back up to the original
           ; expr's bitwidth at the very end.
           (Op1 (SignExtend ?expr-bw)
            (Op2 (Concat) 
             ; I think this add should just be upper-mul-bw in size.
             ; The second concat arg is b0-bw bits wide, and the whole concat
             ; should be b0+b1+a bits = upper-mul-bw + b0-bw.
             (Op2 (Add) 
              ; is (- out-bw b0-bw) >= upper-mul-bw?
              ; out-bw - b0-bw >= b1-bw + a-bw
              ; a-bw + b1-bw + b0-bw - b0-bw >= b1-bw + a-bw
              ; b0-bw - b0-bw >= 0
              ; Ah, they're actually equal.
              ; So this expr can just be upper-mul.
              upper-mul
              ; Do we know upper-mul-bw >= lower-mul-bw - b0-bw?
              ; b1-bw + a-bw >= b0-bw + a-bw - b0-bw
              ; b1-bw + a-bw >= a-bw
              ; b1-bw >= 0
              (Op1 (SignExtend upper-mul-bw) 
               (Op1 (Extract (- (- lower-mul-bw b0-bw) 1) 0)
                (Op2 (Ashr) lower-mul (Op0 (BV b0-bw (+ b0-bw a-bw)))))))
             (Op1 (Extract (- b0-bw 1) 0) lower-mul))))
          (union ?expr rewritten)
         )
         :ruleset transform)


        ; mul shrinking
        ; When a mul doesn't need all of its bits, we can shrink it and then 
        ; extend the result.
        ;(rule
        ;    ((= ?expr (Op2 (Mul) ?a ?b))
        ;     (RealBitwidth ?a ?a-real-bw)
        ;     (RealBitwidth ?b ?b-real-bw)
        ;     (HasType ?expr (Bitvector ?expr-bw))
        ;     (< (* 2 (max ?a-real-bw ?b-real-bw)) ?expr-bw))
        ;    ((union 
        ;       ?expr 
        ;       (Op1 (ZeroExtend ?expr-bw) 
        ;        (Op2 (Mul) 
        ;         (Op1 (Extract (- (* 2 (max ?a-real-bw ?b-real-bw)) 1) 0) ?a)
        ;         (Op1 (Extract (- (* 2 (max ?a-real-bw ?b-real-bw)) 1) 0) ?b)))))
        ;    :ruleset transform)
        ; Add shrinking
        ;(rule
        ; ((= ?expr (Op2 (Add) ?a ?b))
        ;  (RealBitwidth ?a ?a-real-bw)
        ;  (RealBitwidth ?b ?b-real-bw)
        ;  (HasType ?expr (Bitvector ?expr-bw))
        ;  (< (max ?a-real-bw ?b-real-bw) ?expr-bw))
        ; ((union 
        ;   ?expr 
        ;   (Op1 (ZeroExtend ?expr-bw) 
        ;    (Op2 (Add) 
        ;     (Op1 (Extract (- (max ?a-real-bw ?b-real-bw) 1) 0) ?a)
        ;     (Op1 (Extract (- (max ?a-real-bw ?b-real-bw) 1) 0) ?b)))))
        ; :ruleset transform)
        

        (ruleset simplification)
        ;(rule
        ; ((= ?expr (Op1 (ZeroExtend ?m) (Op1 (ZeroExtend ?n) ?e)))
        ;  (>= ?m ?n)
        ;  ; prevents subsumption from deleting the only thing in the eclass
        ;  (!= ?expr (Op1 (ZeroExtend ?m) ?e))
        ;  )
        ; ((union ?expr (Op1 (ZeroExtend ?m) ?e))
        ;  (subsume (Op1 (ZeroExtend ?m) (Op1 (ZeroExtend ?n) ?e)))))
        ; If we're extracting through a zero-extend, we can sometimes delete the
        ; zero-extend.
        ;(rule
        ; ((= ?expr (Op1 (Extract ?hi ?lo) (Op1 (ZeroExtend ?n) ?e)))
        ;  (HasType ?e (Bitvector ?orig-bw))
        ;  (< ?hi ?orig-bw)
        ;  (< ?lo ?orig-bw))
        ; ((union ?expr (Op1 (Extract ?hi ?lo) ?e))
        ;  ; TODO(@gussmith23): For now, best to not subsume.
        ;  ; I don't think I fully understand the consequences of subsumption.
        ;  ; I ran into an issue where we accidentally subsumed the only thing
        ;  ; in the eclass, which is bad. Better to know that something will be
        ;  ; left in the eclass before subsuming.
        ;  (subsume (Op1 (Extract ?hi ?lo) (Op1 (ZeroExtend ?n) ?e)))
        ;  )
        ; :ruleset simplification)
        ; This rule is inserting loops, even if we uncomment the (!= ?expr ?e) line.
        ;(rule
        ; ((= ?expr (Op1 (Extract ?hi ?lo) ?e))
        ;  (HasType ?e (Bitvector ?bw-inner))
        ;  (HasType ?expr (Bitvector ?bw-outer))
        ;  (= ?bw-outer ?bw-inner)
        ;  ; It could be the case that ?expr==?e, in which case this rewrite
        ;  ; would only subsume the expression, which isn't useful. Imagine the
        ;  ; case where the expression is the only thing in the eclass; you don't
        ;  ; want to delete it in this case.
        ;  ;
        ;  ; On second thought, better to just not subsume til I understand it
        ;  ; better.
        ;  ;(!= ?expr ?e)
        ;  )
        ; ((union ?expr ?e)
        ;  ; TODO(@gussmith23): For now, best to not subsume.
        ;  ;(subsume (Op1 (Extract ?hi ?lo) ?e))
        ;  )
        ; :ruleset simplification)
   "#,
        )
        .unwrap();
    egraph
        .parse_and_run_program(
            None,
            //"(run-schedule (saturate (seq (saturate typing) transform (saturate typing) (saturate simplification) (saturate typing) (saturate mapping) )))",
            "(run-schedule (saturate (seq (saturate typing) (saturate transform) (saturate retiming) (run mapping) (saturate typing))))",
        )
        .unwrap();

    warn!("I don't think the add shrinking rewrite is correct---it's not considering that nbit+nbit =n+1bit.");

    // May need this rebuild. See
    // https://github.com/egraphs-good/egglog/pull/391
    // egraph.rebuild();

    if let Some(svg_dirpath) = &args.svg_dirpath {
        create_dir_all(svg_dirpath).unwrap();
        let serialized = egraph.serialize(SerializeConfig::default());
        serialized
            .to_svg_file(svg_dirpath.join("after_rewrites.svg"))
            .unwrap();
        info!(
            "Egraph after rewrites: {}",
            svg_dirpath.join("after_rewrites.svg").to_string_lossy()
        );

        // Extracting random programs for debugging.
        // let mut set_of_exprs = HashSet::new();
        // for _ in 0..100 {
        //     let choices = &RandomExtractor.extract(&serialized, &[]);
        //     set_of_exprs.insert(node_to_string(
        //         &serialized,
        //         &choices[&egraph.value_to_class_id(&outputs[0].0)],
        //         &choices,
        //     ));
        // }
        // debug!(
        //     "Random programs:\n{}",
        //     set_of_exprs.drain().collect::<Vec<_>>().join("\n")
        // );
    }
    {
        // Generate a bunch of random expressions for the classes that need to
        // be mapped. This algorithm does a BFS from the root classes, and at
        // each class, does the following:
        // - if the class is extractable, ignore it and don't go deeper through
        //   this class.
        // - if the class is unextractable and has _only_ non-whitelisted nodes,
        //   it represents a class that is currently blocking extraction b/c it
        //   needs a whitelisted node inserted. So we should extract random
        //   expressions for this class and print them.
        // - if the class is unextractable and has at least one whitelisted node
        //   that is unextracable because its children are unextractable, go
        //   deeper through only those children.

        let serialized_egraph = egraph.serialize(SerializeConfig::default());

        let roots = &outputs
            .iter()
            .map(|(value, _)| {
                egraph.value_to_class_id(&EXPR_SORT, &egraph.find(&EXPR_SORT, *value))
            })
            .collect::<Vec<_>>();

        let (class_blame, _node_blame) =
            determine_extractable(&serialized_egraph, roots, extractable_predicate);

        let mut queue = vec![];
        let mut seen = HashSet::new();
        let mut classes_of_interest = HashSet::new();

        // Start from root classes.
        for root_class in roots {
            queue.push(root_class.clone());
        }

        while let Some(class_id) = queue.pop() {
            if !seen.insert(class_id.clone()) {
                continue;
            }

            let blame = &class_blame[&class_id];

            match blame {
                ClassBlame::Extractable => {
                    // Ignore, don't go deeper.
                }
                ClassBlame::UnextractableClass => {
                    // This is a class we should generate expressions for.
                    classes_of_interest.insert(class_id.clone());
                }
                ClassBlame::UnextractableNodes(node_ids) => {
                    // This class has whitelisted nodes that can't be extracted
                    // due to problems deeper in the tree. Go deeper.

                    // for each node in this class
                    for node_id in node_ids {
                        // For each child of the node
                        for child_node_id in &serialized_egraph[node_id].children {
                            let child_class = &serialized_egraph[child_node_id].eclass;
                            queue.push(child_class.clone());
                        }
                    }
                }
            }
        }

        // For all the classes of interest, extract random expressions.
        const NUM_EXPRS_TO_EXTRACT: usize = 10;
        for class_id in &classes_of_interest {
            debug!(
                "Class {} needs to be mapped. Example expressions:\n{}",
                class_id,
                (0..NUM_EXPRS_TO_EXTRACT)
                    .map(|_| {
                        let choices = RandomExtractor {
                            filter_fn: Some(Box::new(|egraph, node_id| {
                                egraph[node_id].op != "InputOutputMarker"
                            })),
                            // If there's a PrimitiveInterfaceDSP or
                            // PrimitiveInterfaceDSP3 node, choose that above
                            // all other things.
                            extract_fn: Some(Box::new(|egraph, class_id| {
                                egraph[class_id]
                                    .nodes
                                    .iter()
                                    .find(|node_id| {
                                        let node = &egraph.nodes[*node_id];
                                        node.op.starts_with("PrimitiveInterfaceDSP")
                                            || node.op.starts_with("PrimitiveInterfaceDSP3")
                                    })
                                    .cloned()
                            })),
                        }
                        .extract(&serialized_egraph, roots);
                        node_to_string(&serialized_egraph, &choices[class_id], &choices)
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
            );
        }
    }

    {
        // Write out a pruned version of the SVG.
        let mut serialized_egraph = egraph.serialize(SerializeConfig::default());

        let roots = &outputs
            .iter()
            .map(|(value, _)| {
                egraph.value_to_class_id(&EXPR_SORT, &egraph.find(&EXPR_SORT, *value))
            })
            .collect::<Vec<_>>();

        let (_class_blame, node_blame) =
            determine_extractable(&serialized_egraph, roots, extractable_predicate);

        let mut keep_nodes = HashSet::new();

        let mut queue = vec![];
        let mut seen = HashSet::new();

        // Add the extractable
        for root_class in roots {
            for node in serialized_egraph[root_class]
                .nodes
                .iter()
                .filter(|node| matches!(node_blame[node], NodeBlame::UnextractableClasses(_)))
            {
                keep_nodes.insert(node.clone());
                queue.push(node.clone());
            }
        }

        // Keep all Op nodes.
        for (node_id, node) in &serialized_egraph.nodes {
            if node.eclass.to_string().starts_with("Op-") {
                keep_nodes.insert(node_id.clone());
            }
        }

        // Generate labels before we do any pruning, so that all information is
        // available for use in generating labels.
        let _labels: HashMap<NodeId, String> = serialized_egraph
            .nodes
            .iter()
            .map(|(node_id, _node)| {
                // For Op0, Op1, Op2, and Op3, we want to display the actual operation
                // in the node name.
                let node = &serialized_egraph.nodes[node_id];
                if node.op == "Op0" || node.op == "Op1" || node.op == "Op2" || node.op == "Op3" {
                    let op = serialized_egraph.nodes[&node.children[0]].op.clone();
                    (
                        node_id.clone(),
                        format!(
                            "{}{:?} {} {} {}",
                            if roots.contains(&node.eclass) {
                                "root "
                            } else {
                                ""
                            },
                            node_blame[node_id],
                            node.op,
                            op,
                            // The children of the op itself. This makes it so that we get e.g. Op0 BV 16 32.
                            serialized_egraph.nodes[&node.children[0]]
                                .children
                                .iter()
                                .map(|child| serialized_egraph.nodes[child].op.clone())
                                .collect::<Vec<_>>()
                                .join(" ")
                        ),
                    )
                } else {
                    (node_id.clone(), node.op.clone())
                }
            })
            .collect();

        // In the first part of the pruning algorithm, BFS from the roots until
        // we hit extractable nodes.
        //
        // This is the queue that the next BFS will start from. It's filled with
        // the frontier nodes of this BFS, ie the places we stopped.
        let mut next_queue = vec![];
        while let Some(node_id) = queue.pop() {
            if !seen.insert(node_id.clone()) {
                continue;
            }

            match &node_blame[&node_id] {
                NodeBlame::Extractable => {
                    // Don't prune the node, but stop at this node. Don't
                    // continue on to its children.
                    keep_nodes.insert(node_id.clone());
                    next_queue.push(node_id.clone());

                    // Keep the children, just to see what they are.
                    for child in &serialized_egraph[&node_id].children {
                        let eclass_id = &serialized_egraph[child].eclass;
                        // Push all the nodes in the eclass onto the queue, and keep all of them.
                        for node in &serialized_egraph[eclass_id].nodes {
                            keep_nodes.insert(node.clone());
                        }
                    }
                }
                _ => {
                    // Don't prune the node, and keep going on to its children.
                    // We don't stop until we hit the extractable case above.
                    keep_nodes.insert(node_id.clone());

                    for child in &serialized_egraph[&node_id].children {
                        let eclass_id = &serialized_egraph[child].eclass;
                        // Push all the nodes in the eclass onto the queue, and keep all of them.
                        for node in &serialized_egraph[eclass_id].nodes {
                            keep_nodes.insert(node.clone());
                            queue.push(node.clone());
                        }
                    }
                }
            }
        }

        // Move on to the next part of the algorithm, starting from the frontier
        // nodes generated in the fisrt BFS.
        queue = next_queue;

        while let Some(node_id) = queue.pop() {
            if !seen.insert(node_id.clone()) {
                continue;
            }

            // Should we keep this node? If it's unextractable and reachable
            // from the roots, then it's interesting. if it's extractable and
            // reachable from the roots, it's not actually that interesting.
            // Prune it. We should keep some of the extractable nodes just for
            // context, though.

            match &node_blame[&node_id] {
                NodeBlame::Extractable => {
                    // Keep the node, but don't add the children.
                    keep_nodes.insert(node_id);
                }
                NodeBlame::NotWhitelisted => {
                    // Node isn't whitelisted; it's good to see it's in the
                    // graph for context, but it's obvious why it's not
                    // extractable so we can just prune the whole subtree.
                    // Later, we might want to keep more of the subtree.
                    keep_nodes.insert(node_id);
                    // Don't add children to queue.
                }
                NodeBlame::UnextractableClasses(_unextractable_classes) => {
                    // Node is whitelisted but its children aren't extractable.
                    // This case is interesting; we should continue recursion
                    keep_nodes.insert(node_id.clone());
                    for child in &serialized_egraph[&node_id].children {
                        let eclass_id = &serialized_egraph[child].eclass;
                        // Push all the nodes in the eclass onto the queue, and keep all of them.
                        for node in &serialized_egraph[eclass_id].nodes {
                            keep_nodes.insert(node.clone());
                            queue.push(node.clone());
                        }
                    }
                }
            }
        }

        for (node_id, _node) in &serialized_egraph.nodes.clone() {
            if !keep_nodes.contains(node_id) {
                serialized_egraph.nodes.shift_remove(node_id).unwrap();
            }
        }

        if let Some(svg_dirpath) = &args.svg_dirpath {
            serialized_egraph
                .to_svg_file(svg_dirpath.join("after_rewrites_pruned.svg")) //, Some(labels))
                .unwrap();
            info!(
                "Egraph after rewrites pruned: {}",
                svg_dirpath
                    .join("after_rewrites_pruned.svg")
                    .to_string_lossy()
            );
        }

        // for (node_id, node) in &serialized_egraph.nodes.clone() {
        //     // If it's an Op0, Op1, Op2, or Op3, move the first child (the
        //     // actual operation) into the op name. Has to happen before removing
        //     // unextractable nodes.
        //     // It seems like we can't actually modify op without breaking
        //     // everything. Hence the label_fn lambda I added to to_svg_file.
        //     // if node.op == "Op0" || node.op == "Op1" || node.op == "Op2" || node.op == "Op3" {
        //     //     let op = serialized_egraph.nodes[&node.children[0]].op.clone();
        //     //     let new_op = format!("{} {}", node.op, op);
        //     //     debug!("Changing op from {} to {}", node.op, new_op);
        //     //     serialized_egraph.nodes[node_id].op = new_op;
        //     //     // serialized_egraph.nodes.shift_remove(&node.children[0]).unwrap();
        //     // }

        //     if !extractable_predicate(&serialized_egraph, node_id) {
        //         serialized_egraph.nodes.shift_remove(node_id).unwrap();
        //     }
        //     // If it's a PrimitiveInterfaceDSP or PrimitiveInterfaceDSP3, remove the first child (the unique string).
        //     if node.op == "PrimitiveInterfaceDSP" || node.op == "PrimitiveInterfaceDSP3" {
        //         serialized_egraph
        //             .nodes
        //             .shift_remove(&node.children[0])
        //             .unwrap();
        //     }
        // }
        // if let Some(svg_dirpath) = &args.svg_dirpath {
        //     serialized_egraph
        //         .to_svg_file(
        //             svg_dirpath.join("after_rewrites_pruned.svg"),
        //             Some(|node_id, egraph| {
        //                 // For Op0, Op1, Op2, and Op3, we want to display the actual operation
        //                 // in the node name.
        //                 let node = &egraph.nodes[node_id];
        //                 if node.op == "Op0"
        //                     || node.op == "Op1"
        //                     || node.op == "Op2"
        //                     || node.op == "Op3"
        //                 {
        //                     let op = egraph.nodes[&node.children[0]].op.clone();
        //                     format!(
        //                         "{} {} {}",
        //                         node.op,
        //                         op,
        //                         // The children of the op itself. This makes it so that we get e.g. Op0 BV 16 32.
        //                         egraph.nodes[&node.children[0]]
        //                             .children
        //                             .iter()
        //                             .map(|child| egraph.nodes[child].op.clone())
        //                             .collect::<Vec<_>>()
        //                             .join(" ")
        //                     )
        //                 } else {
        //                     node.op.clone()
        //                 }
        //             }),
        //         )
        //         .unwrap();
        //     info!(
        //         "Egraph after rewrites pruned: {}",
        //         svg_dirpath
        //             .join("after_rewrites_pruned.svg")
        //             .to_string_lossy()
        //     );
        // }
    }

    if args.interact {
        _egraph_interact(&mut egraph);
    }

    let serialized_egraph = egraph.serialize(SerializeConfig::default());

    {
        let serialized_egraph = egraph.serialize(SerializeConfig::default());
        // NEW STEP 3: attempt to extract an implementation mapped to DSPs. At this
        // point, we'll be extracting *potential* DSPs; that is, we won't actually
        // have attempted mapping with Lakeroad yet. The goal at this stage is to
        // evaluate whether there is *any* potential, fully-mapped implementation of
        // the design. If there's not, there's very little reason to start running
        // Lakeroad. Furthermore, if there *is* a potentially fully-mapped
        // implementation, there's no reason to run Lakeroad on potential DSPs that
        // aren't included in that implementation. (Previously, we ran Lakeroad on
        // all potential DSPs in the egraph, which was unnecessary.)
        determine_extractable(
            &serialized_egraph,
            &outputs
                .iter()
                .map(|(value, _)| {
                    egraph.value_to_class_id(&EXPR_SORT, &egraph.find(&EXPR_SORT, *value))
                })
                .collect::<Vec<_>>(),
            extractable_predicate,
        );
        let choices = GlobalGreedyDagExtractor {
            fail_on_partial: false,
            extractable_predicate,
        }
        .extract(
            &serialized_egraph,
            &outputs
                .iter()
                .map(|(value, _)| {
                    egraph.value_to_class_id(&EXPR_SORT, &egraph.find(&EXPR_SORT, *value))
                })
                .collect::<Vec<_>>(),
        )
        .unwrap_or_else(|e| {
            panic!("Failed to extract design: {}", e);
        });

        for (value, output_name) in outputs.iter().cloned() {
            let class_id = egraph.value_to_class_id(&EXPR_SORT, &egraph.find(&EXPR_SORT, value));
            log::debug!(
                "For output {}, extracted\n{}",
                output_name,
                node_to_string(
                    &serialized_egraph,
                    choices.get(&class_id).unwrap(),
                    &choices
                )
            );
        }
    }

    // STEP 3: Collect all proposed mappings.
    // In this step, we simply find all mapping proposals, i.e. all places where
    // the above rewrites *think* we might be able to use a DSP. In the next
    // step, we'll actually confirm or deny whether these mappings can work.
    //
    // In the future, this step might also involve ranking potential mapping
    // proposals, because in a large design, there will likely be many of them!
    // There are many potential ways to rank: heuristics, cost models, etc.
    //
    //
    // TODO(@gussmith23): Make this return Vec<(choices, nodeid)>.
    // Basically it can have the same API as the spec finding function. They're
    // both doing very similar things: basically, an extraction. They're just
    // extracting different things for the same classes.
    let node_ids = find_primitive_interfaces_serialized(&serialized_egraph);

    info!(
        "Found {} potential mappings; running Lakeroad on each.",
        node_ids.len()
    );

    // find_bad(&serialized_egraph);

    // Check that everything has a type
    {
        let classes_without_hastype = util::missing_hastype(&serialized_egraph);
        if !classes_without_hastype.is_empty() {
            let extracted = RandomExtractor::default().extract(&serialized_egraph, &[]);
            // TODO(@gussmith23): Clean this up. We're currently printing out
            // the expressions twice.
            for class in classes_without_hastype.iter() {
                for node_id in &serialized_egraph[class].nodes {
                    println!(
                        "{}",
                        node_to_string(&serialized_egraph, node_id, &extracted)
                    );
                }
            }
            warn!(
                "Not all classes have type information.\n{}",
                classes_without_hastype
                    .iter()
                    .map(|class_id| format!(
                        "Class ID: {}\n{}",
                        class_id,
                        serialized_egraph[class_id]
                            .nodes
                            .iter()
                            .map(|node_id| util::display_enode_serialized(
                                &serialized_egraph,
                                node_id,
                                10
                            ))
                            .collect::<Vec<_>>()
                            .join("\n")
                    ))
                    .collect::<Vec<_>>()
                    .join("\n")
            );
        }
    }

    if args.interact {
        _egraph_interact(&mut egraph);
    }

    // STEP 5: For each proposed mapping, attempt synthesis with Lakeroad.
    for sketch_template_node_id in &node_ids {
        // TODO(@gussmith23): This is a hack, see https://github.com/egraphs-good/egglog/issues/392
        // Doing everything over the serialized egraph, so I don't actually need this anymore.
        // let canonical: usize = egraph.find(*value).bits.try_into().unwrap();
        // let canonical_id: egraph_serialize::ClassId = canonical.to_string().into();
        // let (choices, spec_node_id) =
        //     find_spec_for_primitive_interface(&canonical_id, &serialized_egraph);

        // STEP 5.1: For each proposed mapping, extract a "spec".
        // In the above step, we extracted all of the proposed mapping nodes.
        // These nodes are just markers that say "this eclass could potentially
        // be implemented with a DSP." To actually do synthesis with Lakeroad,
        // we need to extract *yet another* representative from the eclass:
        // one that can serve as a specification which Lakeroad can synthesize
        // against. Currently, this mostly just means extracting *any*
        // expression from the eclass which can be converted to valid Verilog.
        //
        // In the future, we could also consider extracting *multiple*
        // representatives per eclass, which gives us more specs to attempt
        // synthesis against. Given that solvers are strange and often benefit
        // from running in a portfolio, having many equivalent specs might
        // increase chances at synthesis termination.
        let (spec_choices, spec_node_id) = find_spec_for_primitive_interface_including_nodes(
            &serialized_egraph[sketch_template_node_id].eclass,
            &serialized_egraph,
            {
                // Use the children of the sketch template node as the
                // required-to-be-extracted nodes.
                //
                // For each of the children, ensure we're not choosing an
                // InputOutputMarker but instead a node that can be converted to
                // Verilog.
                serialized_egraph[sketch_template_node_id]
                    .children
                    .iter()
                    // First argument is the string ID, we can skip this.
                    .skip(1)
                    .map(|child_id| {
                        let eclass = &serialized_egraph[child_id].eclass;
                        serialized_egraph[eclass]
                            .nodes
                            .iter()
                            .find(|node_id| {
                                let node = &serialized_egraph[*node_id];
                                // TODO(@gussmith23): There are definitely other
                                // things to filter here. Also, we're doing this
                                // filtering so frequently in a few different
                                // ways -- the logic should probably be
                                // centralized somewhere.
                                node.op != "InputOutputMarker"
                            })
                            .expect("there should be non-filtered nodes in here")
                    })
                    .cloned()
                    .collect()
            },
        );

        // log::info!(
        //     "Calling Lakeroad with spec:\n{}\nand sketch:\n{}",
        //     node_to_string(&serialized_egraph, &spec_node_id, &spec_choices),
        //     serialized_egraph[sketch_template_node_id].op
        // );

        // STEP 5.2: Call Lakeroad.
        let commands = call_lakeroad_on_primitive_interface_and_spec(
            &serialized_egraph,
            &spec_choices,
            &spec_node_id,
            sketch_template_node_id,
            &args.architecture.to_string(),
            (args.cvc5, args.yices, args.stp, args.bitwuzla),
        );

        log::debug!(
            "First few lines of commands generated from Lakeroad output:\n{}",
            commands.lines().take(10).collect::<Vec<_>>().join("\n")
        );

        // STEP 5.3: Insert Lakeroad's results back into the egraph.
        // If Lakeroad finds a mapping, insert the mapping into the egraph.
        // If Lakeroad proves UNSAT, put some kind of marker into the egraph
        // to indicate that this mapping shouldn't be attempted again.
        egraph.parse_and_run_program(None, &commands).unwrap();

        info!("Inserted Lakeroad's results back into egraph.");

        // Write out image if the user requested it.
        if let Some(svg_dirpath) = &args.svg_dirpath {
            let serialized = egraph.serialize(SerializeConfig::default());
            serialized
                .to_svg_file(svg_dirpath.join("during_lakeroad.svg"))
                .unwrap();
            info!(
                "Egraph after nth call to Lakeroad: {}",
                svg_dirpath.join("during_lakeroad.svg").to_string_lossy()
            );
        }
    }

    // Write out image if the user requested it.
    if let Some(svg_dirpath) = args.svg_dirpath {
        let serialized = egraph.serialize(SerializeConfig::default());
        serialized
            .to_svg_file(svg_dirpath.join("after_lakeroad.svg"))
            .unwrap();
        info!(
            "Egraph after all calls to Lakeroad: {}",
            svg_dirpath.join("after_lakeroad.svg").to_string_lossy()
        );
    }

    // STEP 6: Extract a lowered design.
    //
    // Once we have attempted all mappings, we should ideally be able to extract
    // a design in structural Verilog.
    //
    // Future work at this stage will involve building an extractor which
    // which actually attempts to find an *optimal* design, not just *any*
    // design.

    let serialized = egraph.serialize(SerializeConfig::default());
    let choices = GlobalGreedyDagExtractor {
        // This can be false as long as we set roots to a value in extract().
        fail_on_partial: false,
        extractable_predicate: structural_predicate,
    }
    .extract(
        &serialized,
        &outputs
            .iter()
            .cloned()
            .map(|(value, _output_name)| {
                egraph.value_to_class_id(&EXPR_SORT, &egraph.find(&EXPR_SORT, value))
            })
            .collect::<Vec<_>>()[..],
    )
    .unwrap_or_else(|e| {
        panic!("Failed to extract design: {}", e);
    });
    let verilog = to_verilog_egraph_serialize(
        &serialized,
        &choices,
        "clk",
        [].into(),
        // Use the original outputs as the outputs of the final design.
        Some(
            outputs
                .iter()
                .cloned()
                .map(|(value, output_name)| {
                    (
                        egraph.value_to_class_id(&EXPR_SORT, &egraph.find(&EXPR_SORT, value)),
                        output_name,
                    )
                })
                .collect(),
        ),
    );

    debug!("Final extracted Verilog:\n{}", &verilog);

    if let Some(out_filepath) = &args.out_filepath {
        std::fs::write(out_filepath, &verilog).unwrap();
    } else {
        println!("{}", verilog);
    }

    // STEP 7: Simulate.
    if args.simulate {
        // If we didn't write to file, we need to write to a temp file.
        let old_verilog_filepath = if let Some(out_filepath) = &args.out_filepath {
            out_filepath.to_owned()
        } else {
            let (_, path) = NamedTempFile::new().unwrap().keep().unwrap();
            std::fs::write(&path, &verilog).unwrap();
            path
        };

        // First, we have to rename the output module, because our
        // simulate_with_verilator.py script can't simulate two modules with the
        // same name against each other.
        let verilog_file = NamedTempFile::new().unwrap();
        // Use Yosys to rename the module.
        let new_module_name = format!("{}_simulate_with_verilator", args.top_module_name);
        let yosys_output = std::process::Command::new("yosys")
            .arg("-p")
            .arg(format!(
                "read_verilog -sv {}; rename {} {}; write_verilog {}",
                old_verilog_filepath.to_string_lossy(),
                //args.top_module_name,
                {
                    warn!("TODO(@gussmith23): hardcoded.");
                    "top"
                },
                new_module_name,
                verilog_file.path().to_string_lossy()
            ))
            .output()
            .unwrap();
        if !yosys_output.status.success() {
            panic!(
                "Yosys failed to rename the module. stdout:\n{}\nstderr:\n{}",
                String::from_utf8_lossy(&yosys_output.stdout),
                String::from_utf8_lossy(&yosys_output.stderr)
            );
        }

        let lakeroad_dir = PathBuf::from(
            std::env::var("LAKEROAD_DIR")
                .expect("LAKEROAD_DIR environment variable should be set."),
        );

        let mut cmd = std::process::Command::new("python3");
        cmd.arg(lakeroad_dir.join("bin").join("simulate_with_verilator.py"))
            .arg("--verilog_filepath")
            .arg(verilog_file.path())
            .arg("--test_module_name")
            .arg(new_module_name)
            .arg("--ground_truth_module_name")
            .arg(args.top_module_name)
            .arg("--verilator_extra_arg")
            .arg(args.filepath)
            .args(args.simulate_with_verilator_arg);

        for input in &input_names_and_bws {
            cmd.arg("--input_signal")
                .arg(format!("{}:{}", input.0, input.1));
        }
        for output in &output_names_and_bws {
            cmd.arg("--output_signal")
                .arg(format!("{}:{}", output.0, output.1));
        }

        let output = cmd.output().unwrap();

        if !output.status.success() {
            panic!(
                "Simulation with Verilator failed. stdout:\n{}\nstderr:\n{}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            );
        }

        info!("Simulation with Verilator succeeded.");
    }
}

use std::collections::HashSet;
#[derive(Debug)]
enum ClassBlame {
    /// Class is extractable.
    Extractable,
    /// Class is unextractable because none of its whitelisted nodes are extractable.
    UnextractableNodes(Vec<NodeId>),
    /// Class is unextractable because none of its nodes are whitelisted.
    UnextractableClass,
}
#[derive(Debug)]
enum NodeBlame {
    /// Node is extractable.
    Extractable,
    NotWhitelisted,
    /// Unextractable because at least one of its children is marked
    /// unextractable.
    UnextractableClasses(Vec<ClassId>),
}
fn determine_extractable(
    egraph: &egraph_serialize::EGraph,
    _roots: &[ClassId],
    predicate: fn(&egraph_serialize::EGraph, &NodeId) -> bool,
) -> (HashMap<ClassId, ClassBlame>, HashMap<NodeId, NodeBlame>) {
    let mut class_blame = HashMap::new();
    let mut node_blame = HashMap::new();

    // Mark the basic nodes as extractable. I think the easiest way to do this
    // is by the type of the node, which is currently embedded in the string id.
    // The easiest first pass is to just mark all non-exprs as extractable, I
    // think?
    for (node_id, node) in &egraph.nodes {
        let class_name = node.eclass.to_string();
        let split: Vec<_> = class_name.split("-").collect();
        assert_eq!(split.len(), 2);
        let type_name = split[0];
        // Match on the type.
        match type_name {
            "Op" | "Unit" | "i64" | "String" | "Type" | "PortDirection" => {
                class_blame.insert(node.eclass.clone(), ClassBlame::Extractable);
                node_blame.insert(node_id.clone(), NodeBlame::Extractable);
            }
            "Expr" => {
                // Do nothing; we will analyze whether the Exprs are extractable below.
            }
            other => panic!("Unhandled type {other}"),
        }
    }

    let mut keep_going = true;
    while keep_going {
        keep_going = false;

        for (node_id, node) in &egraph.nodes {
            if node_blame.contains_key(node_id) {
                continue;
            }

            // If it's not whitelisted, then it's not extractable.
            if !predicate(egraph, node_id) {
                node_blame.insert(node_id.clone(), NodeBlame::NotWhitelisted);
                keep_going = true;
                continue;
            }

            // Get the extractability of all the classes.
            let class_extractability = node
                .children
                .iter()
                .filter_map(|child_id| {
                    let child_id = &egraph[child_id].eclass;
                    class_blame.get(child_id)
                })
                .collect::<Vec<_>>();

            // If we don't have extractability information for all the classes,
            // we can't determine the extractability of this node yet.
            if class_extractability.len() != node.children.len() {
                // keep_going = true; // TODO this might lead to infinite loops.
                // debug!(
                //     "Can't determine extractability of node {:?} as not all child classes have extractability information.",
                //     node_id
                // );
                continue;
            }

            // Otherwise, we can determine the extractability of this node.
            // Get the list of unextractable classes.
            let unextractable_children = node
                .children
                .iter()
                .zip(class_extractability)
                .filter_map(|(child_id, blame)| {
                    if !matches!(blame, ClassBlame::Extractable) {
                        Some(egraph[child_id].eclass.clone())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            if unextractable_children.is_empty() {
                // debug!(
                //     "Node {:?} is extractable as all of its child nodes are extractable.",
                //     node_id
                // );
                node_blame.insert(node_id.clone(), NodeBlame::Extractable);
                keep_going = true;
            } else {
                // debug!(
                //     "Node {:?} is unextracable as some of its children are unextractable.",
                //     node_id
                // );
                node_blame.insert(
                    node_id.clone(),
                    NodeBlame::UnextractableClasses(unextractable_children),
                );
                keep_going = true;
            }
        }

        // Iterate over all the classes and determine their extractability.
        for (class_id, class) in egraph.classes().iter() {
            if class_blame.contains_key(class_id) {
                continue;
            }

            // Get the extractability of all the nodes.
            let node_extractability = class
                .nodes
                .iter()
                .filter_map(|node_id| node_blame.get(node_id))
                .collect::<Vec<_>>();

            // If any of the nodes are extractable, then the class is extractable.
            if node_extractability
                .iter()
                .any(|blame| matches!(blame, NodeBlame::Extractable))
            {
                // debug!(
                //     "Class {:?} is extractable as one of its child nodes is extractable.",
                //     class_id
                // );
                class_blame.insert(class_id.clone(), ClassBlame::Extractable);
                keep_going = true;
                continue;
            }

            // If none of the nodes are currently extractable BUT we don't have
            // extractability information for all the nodes, we can't determine
            // the extractability of this class yet.
            if node_extractability.len() != class.nodes.len() {
                // debug!(
                //     "Can't determine extractability of class {:?} as not all nodes have extractability information.",
                //     class_id
                // );
                continue;
            }

            // Three cases:
            // None of the nodes are whitelisted.
            // Some of the nodes are whitelisted but none are extractable.
            // Some of the nodes are extractable.
            if node_extractability
                .iter()
                .all(|blame| matches!(blame, NodeBlame::NotWhitelisted))
            {
                // debug!(
                //     "Class {:?} is unextractable as none of its child nodes are whitelisted.",
                //     class_id
                // );
                class_blame.insert(class_id.clone(), ClassBlame::UnextractableClass);
                keep_going = true;
            } else if node_extractability
                .iter()
                .any(|blame| matches!(blame, NodeBlame::Extractable))
            {
                // debug!(
                //     "Class {:?} is extractable as at least one of its child nodes is extractable.",
                //     class_id
                // );
                class_blame.insert(class_id.clone(), ClassBlame::Extractable);
                keep_going = true;
            } else {
                // Find the unextractable nodes.
                // TODO: do we include nodes that aren't whitelisted here?
                let unextractable_nodes = class
                    .nodes
                    .iter()
                    .filter(|node_id| !matches!(node_blame[*node_id], NodeBlame::Extractable))
                    .cloned()
                    .collect::<Vec<_>>();
                assert!(!unextractable_nodes.is_empty());
                class_blame.insert(
                    class_id.clone(),
                    ClassBlame::UnextractableNodes(unextractable_nodes),
                );
                keep_going = true;
                // debug!(
                //     "Class {:?} is unextractable as none of its child nodes are extractable, but some are whitelisted.",
                //     class_id
                // );
            }
        }
    }

    // If there are cycles of unextractable classes/nodes, the above algorithm
    // will terminate without assigning all blames. Anything that is still
    // unassigned is unextractable.

    // TODO(@gussmith23): we should sanity check the results at this point
    // to make sure we can actually find the cycle.

    // Assert all classes and nodes have extractability information.
    for (class_id, _class) in egraph.classes().iter() {
        if !class_blame.contains_key(class_id) {
            class_blame.insert(class_id.clone(), ClassBlame::UnextractableClass);
            warn!(
                "Class {:?} is missing extractability information, marking as unextractable",
                class_id
            );
        }
    }
    for (node_id, _node) in egraph.nodes.iter() {
        if !node_blame.contains_key(node_id) {
            node_blame.insert(node_id.clone(), NodeBlame::UnextractableClasses(vec![]));
            warn!(
                "Node {:?} is missing extractability information, marking as unextractable",
                node_id
            );
        }
    }

    (class_blame, node_blame)
}

fn extractable_predicate(egraph: &egraph_serialize::EGraph, node_id: &NodeId) -> bool {
    // Ignore Unit, as we never want to extract anything from this class.
    if egraph[&egraph[node_id].eclass]
        .id
        .to_string()
        .starts_with("Unit-")
    {
        return false;
    }

    let op_whitelist = vec![
        "Op0".into(),
        "Op1".into(),
        "Op2".into(),
        "Op3".into(),
        "Var".into(),
        "StringConsList".into(),
        "ExprConsList".into(),
        "GetOutput".into(),
        "PrimitiveInterfaceDSP".into(),
        "PrimitiveInterfaceDSP3".into(),
        "PrimitiveInterfaceWideAddDSP".into(),
    ];
    let sub_op_whitelist = [
        "Extract".into(),
        "Concat".into(),
        "BV".into(),
        "CRString".into(),
        "ZeroExtend".into(),
        "SignExtend".into(),
        "Shr".into(),
        "Shl".into(),
        "Ashr".into(),
    ];
    warn!("Shift ops really should not be considered extractable, but they are for now.");
    if !egraph[&egraph[node_id].eclass]
        .id
        .to_string()
        .starts_with("Expr-")
    {
        return true;
    }
    let node = &egraph[node_id];
    if !op_whitelist.contains(&node.op) {
        return false;
    }
    if ["Op0", "Op1", "Op2", "Op3"].contains(&node.op.as_str()) {
        let sub_op = &egraph[&node.children[0]].op;
        if !sub_op_whitelist.contains(sub_op) {
            return false;
        }
    }
    true
}
/// Whether a node is valid for structural Verilog.
fn structural_predicate(egraph: &egraph_serialize::EGraph, node_id: &NodeId) -> bool {
    // Ignore Unit, as we never want to extract anything from this class.
    if egraph[&egraph[node_id].eclass]
        .id
        .to_string()
        .starts_with("Unit-")
    {
        return false;
    }

    let op_whitelist = [
        "Op0".into(),
        "Op1".into(),
        "Op2".into(),
        "Op3".into(),
        "Var".into(),
        "StringConsList".into(),
        "ExprConsList".into(),
        "GetOutput".into(),
    ];
    let sub_op_whitelist = [
        "Extract".into(),
        "Concat".into(),
        "BV".into(),
        "CRString".into(),
        "ZeroExtend".into(),
        "SignExtend".into(),
        "Shr".into(),
        "Shl".into(),
        "Ashr".into(),
    ];
    warn!("Shift ops really should not be considered extractable, but they are for now.");
    if !egraph[&egraph[node_id].eclass]
        .id
        .to_string()
        .starts_with("Expr-")
    {
        return true;
    }
    let node = &egraph[node_id];
    if !op_whitelist.contains(&node.op) {
        return false;
    }
    if ["Op0", "Op1", "Op2", "Op3"].contains(&node.op.as_str()) {
        let sub_op = &egraph[&node.children[0]].op;
        if !sub_op_whitelist.contains(sub_op) {
            return false;
        }
    }
    true
}
