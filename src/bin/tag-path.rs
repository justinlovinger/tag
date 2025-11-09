use clap::{Parser, Subcommand};
use itertools::Itertools;
use tag::{combine, sort_tags_by_subfrequency, TaggedPath};

#[derive(Parser)]
#[command(author, version, about = "A utility to manage tagged paths", long_about = None)]
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
    /// Sort tags by descending frequency
    ///
    /// Frequency is calculated within each subset
    /// formed by selecting the next tag.
    /// For example,
    /// `foo.x foo.x foo-bar.x foo-baz-bar.x baz.x baz.x`
    /// is sorted into
    /// `foo.x foo.x foo-bar.x foo-bar-baz.x baz.x baz.x`
    /// because `bar` is more common than `baz`
    /// within the subset of paths containing `foo`.
    ///
    /// Paths are returned without directories.
    SortTagsBySubfrequency {
        #[arg(value_name = "PATH")]
        paths: Vec<TaggedPath>,
    },
    /// Combine paths into optimal directories
    Combine {
        #[arg(value_name = "PATH")]
        paths: Vec<TaggedPath>,
    },
}

fn main() {
    let args = Args::parse();

    match args.command {
        Some(Commands::Tags { path }) => print!("{}", path.tags().format("\n")),
        Some(Commands::Ext { path }) => println!("{}", path.ext()),
        Some(Commands::SortTagsBySubfrequency { paths }) => {
            println!("{}", sort_tags_by_subfrequency(&paths).format("\n"))
        }
        Some(Commands::Combine { paths }) => {
            println!(
                "{}",
                combine(&paths).format_with("\n", |path, f| f(&path.display()))
            )
        }
        None => {}
    }
}
