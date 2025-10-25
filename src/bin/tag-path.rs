use clap::{Parser, Subcommand};
use itertools::Itertools;
use tag::TaggedPath;

#[derive(Parser)]
#[clap(author, version, about = "A utility to manage tagged paths", long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Print tags in the given path
    Tags { path: TaggedPath },
    /// Print the extension of the given path
    Ext { path: TaggedPath },
}

fn main() {
    let args = Args::parse();

    match args.command {
        Some(Commands::Tags { path }) => print!("{}", path.tags().format("\n")),
        Some(Commands::Ext { path }) => println!("{}", path.ext()),
        None => {}
    }
}
