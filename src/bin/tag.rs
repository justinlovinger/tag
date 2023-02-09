use std::path::PathBuf;

use clap::{Parser, Subcommand};
use tag::{Tag, SEPARATORS, TAG_END};

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Add tags to files
    Add {
        /// Tag to add
        #[clap(required = true, value_name = "TAG", value_parser = tag_parser)]
        tag: Tag,

        /// Files to tag
        #[clap(required = true, value_name = "FILE")]
        files: Vec<PathBuf>,
    },
}

fn tag_parser(s: &str) -> Result<Tag, String> {
    Tag::new(s.to_owned()).ok_or(format!(
        "tags cannot start with `{}` or contain `{}` or `{}`",
        TAG_END, SEPARATORS[0], SEPARATORS[1],
    ))
}

fn main() {
    let args = Args::parse();

    if let Some(Commands::Add { tag, files }) = args.command {
        println!("Add {} to {:?}", tag, files);
    }
}
