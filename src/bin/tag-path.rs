use clap::{Parser, Subcommand, ValueEnum};
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
    /// Print extension of the given path
    Ext { path: TaggedPath },
    /// Sort tags within each path
    ///
    /// Paths are returned in the order given.
    ///
    /// Paths may be returned without directory separators.
    ///
    /// Subfrequency is frequency calculated within each subset
    /// formed by selecting the next tag.
    /// For example,
    /// in `foo-baz-bar.x foo.x foo.x foo-bar.x baz.x baz.x`,
    /// `foo-baz-bar.x` is sorted as `foo-bar-baz.x`
    /// because `bar` is more common than `baz`
    /// within the subset of paths containing `foo`,
    /// even though `baz` is more common globally.
    Sort {
        #[arg(long, short, value_enum, default_value_t=SortMethod::default())]
        method: SortMethod,

        #[arg(value_name = "PATH")]
        paths: Vec<TaggedPath>,
    },
    /// Combine paths into optimal directories
    ///
    /// Paths are returned in the order given.
    Combine {
        #[arg(value_name = "PATH")]
        paths: Vec<TaggedPath>,
    },
}

#[derive(Copy, Clone, Default, ValueEnum)]
enum SortMethod {
    /// Sort tags by descending subfrequency
    #[default]
    Subfrequency,
}

fn main() {
    let args = Args::parse();

    match args.command {
        Some(Commands::Tags { path }) => print!("{}", path.tags().format("\n")),
        Some(Commands::Ext { path }) => println!("{}", path.ext()),
        Some(Commands::Sort { method, paths }) => {
            println!(
                "{}",
                match method {
                    SortMethod::Subfrequency => sort_tags_by_subfrequency(&paths).format("\n"),
                }
            )
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
