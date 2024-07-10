use std::path::PathBuf;

use churchroad::from_verilog_file;
use clap::{Parser, ValueEnum};
use clap::ValueHint::FilePath;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(long, value_hint=FilePath)]
    filepath: PathBuf,

    #[arg(long)]
    top_module_name: String,

    #[arg(long)]
    svg_filepath: Option<PathBuf>,

    #[arg(long)]
    architecture: Architecture,
}

#[derive(ValueEnum, Clone, Debug)]
enum Architecture {
    XilinxUltrascalePlus
}

fn main() {
    let args = Args::parse();

    let mut egraph = from_verilog_file(&args.filepath, &args.top_module_name);

    egraph
        .parse_and_run_program(
            r#"
        (ruleset mapping)
        (rule 
            ((= expr (Op2 (Mul) a b))
             (HasType expr (Bitvector n))
             (< n 18))
            ((union expr (DSP48E2 a b)))
            :ruleset mapping)
    "#,
        )
        .unwrap();

    egraph
        .parse_and_run_program("(run-schedule (saturate typing))")
        .unwrap();
    egraph
        .parse_and_run_program("(run-schedule (saturate mapping))")
        .unwrap();

    if let Some(svg_filepath) = args.svg_filepath {
        let serialized = egraph.serialize_for_graphviz(true);
        serialized.to_svg_file(svg_filepath).unwrap();
    }
}
