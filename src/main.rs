use std::path::PathBuf;

use churchroad::{
    call_lakeroad_on_primitive_interface_and_spec, find_primitive_interface_values,
    find_primitive_interfaces_serialized, find_spec_for_primitive_interface, from_verilog_file,
};
use clap::ValueHint::FilePath;
use clap::{Parser, ValueEnum};
use egglog::SerializeConfig;
use egraph_serialize::NodeId;

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
    XilinxUltrascalePlus,
}

// TODO(@gussmith23): Seems redundant to do this; I think clap already does something like this under the hood.
impl ToString for Architecture {
    fn to_string(&self) -> String {
        match self {
            Architecture::XilinxUltrascalePlus => "xilinx-ultrascale-plus".to_owned(),
        }
    }
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
            ((union expr (PrimitiveInterfaceDSP a b)))
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

    // May need this rebuild. See
    // https://github.com/egraphs-good/egglog/pull/391
    // egraph.rebuild();

    if let Some(svg_filepath) = args.svg_filepath {
        let serialized = egraph.serialize_for_graphviz(true, usize::MAX, usize::MAX);
        serialized.to_svg_file(svg_filepath).unwrap();
    }

    let serialized_egraph = egraph.serialize(SerializeConfig::default());
    // TODO(@gussmith23): Make this return Vec<(choices, nodeid)>.
    // Basically it can have the same API as the spec finding function. They're
    // both doing very similar things: basically, an extraction. They're just
    // extracting different things for the same classes.
    let node_ids = find_primitive_interfaces_serialized(&serialized_egraph);
    for sketch_template_node_id in &node_ids {
        // TODO(@gussmith23): This is a hack, see https://github.com/egraphs-good/egglog/issues/392
        // Doing everything over the serialized egraph, so I don't actually need this anymore.
        // let canonical: usize = egraph.find(*value).bits.try_into().unwrap();
        // let canonical_id: egraph_serialize::ClassId = canonical.to_string().into();
        // let (choices, spec_node_id) =
        //     find_spec_for_primitive_interface(&canonical_id, &serialized_egraph);

        let (spec_choices, spec_node_id) = find_spec_for_primitive_interface(
            &serialized_egraph[sketch_template_node_id].eclass,
            &serialized_egraph,
        );

        call_lakeroad_on_primitive_interface_and_spec(
            &serialized_egraph,
            &spec_choices,
            &spec_node_id,
            &sketch_template_node_id,
            &args.architecture.to_string(),
        );
    }
}
