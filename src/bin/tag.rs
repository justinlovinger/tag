use std::{
    env::current_dir,
    fmt,
    io::{self, Write},
    path::PathBuf,
};

use clap::{Parser, Subcommand, ValueEnum};
use tag::{combine, find, sort_tags_by_subfrequency, Tag, TaggedPath};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Use given path as working directory
    ///
    /// All other paths will be resolved
    /// relative to this path.
    /// This is equivalent to calling `cd PATH`
    /// before running this program.
    #[arg(long, value_name = "PATH")]
    working_directory: Option<PathBuf>,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Print tags in the given path
    Tags { path: TaggedPath },
    /// Print extension of the given path
    Ext { path: TaggedPath },
    /// Print tagged paths with given tags
    Find {
        /// Find paths including _all_ these tags
        #[arg(value_name = "TAG")]
        include: Vec<Tag>,

        /// Ignore paths including _any_ of these tags
        #[arg(long, short, value_name = "TAG")]
        exclude: Vec<Tag>,
    },
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
    /// Return paths with optimal directory separators
    ///
    /// Paths are returned in the order given.
    ///
    /// At most one tag of the form `_[0-9]*` may be added
    /// to each path
    /// to avoid files starting with `.` and being hidden in Linux
    /// or to differentiate paths.
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

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    if let Some(path) = args.working_directory {
        std::env::set_current_dir(path)?;
    }

    match args.command {
        Some(Commands::Tags { path }) => print(path.tags())?,
        Some(Commands::Ext { path }) => println!("{}", path.ext()),
        Some(Commands::Find { include, exclude }) => {
            print(find(current_dir()?, include, exclude)?)?
        }
        Some(Commands::Sort { method, paths }) => print(match method {
            SortMethod::Subfrequency => sort_tags_by_subfrequency(&paths),
        })?,
        Some(Commands::Combine { paths }) => {
            let mut out = stdout();
            for path in combine(&paths) {
                writeln!(out, "{}", path.display())?
            }
        }
        None => {}
    }

    Ok(())
}

fn print<T>(it: impl IntoIterator<Item = T>) -> io::Result<()>
where
    T: fmt::Display,
{
    let mut out = stdout();
    for item in it {
        writeln!(out, "{item}")?
    }
    Ok(())
}

fn stdout() -> impl Write {
    io::BufWriter::new(io::stdout().lock())
}
