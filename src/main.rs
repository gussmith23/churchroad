use clap::Parser;
use clap::ValueHint::FilePath;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Name of the person to greet
    #[arg(long, value_hint=FilePath)]
    filepath: String,
}

fn main() {
    let args = Args::parse();
    

}
