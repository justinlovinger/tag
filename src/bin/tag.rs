use std::{
    env::current_dir,
    fmt,
    io::{self, stdin, Write},
    path::PathBuf,
};

use clap::{
    error::{ContextKind, ContextValue},
    CommandFactory, Parser, Subcommand, ValueEnum,
};
use tag::{combine, find, sort_tags_by_subfrequency, uncombine, Tag, TaggedPath};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
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
    /// Print tagged paths with given tags
    Find {
        /// Find tagged paths relative to this path
        ///
        /// Only tags relative to this path are considered.
        #[arg(value_name = "PATH")]
        dir: Option<PathBuf>,

        /// Find paths including _all_ these tags
        #[arg(long, short, value_name = "TAG")]
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
    /// Return paths without directory separators or `_[0-9]*` tags
    ///
    /// Paths are returned in the order given.
    Uncombine {
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

    match args.command {
        Some(Commands::Tags { path }) => print(path.tags())?,
        Some(Commands::Ext { path }) => println!("{}", path.ext()),
        Some(Commands::Find {
            dir,
            include,
            exclude,
        }) => print(find(
            dir.unwrap_or_else(|| current_dir().unwrap()),
            include,
            exclude,
        )?)?,
        Some(Commands::Sort { method, mut paths }) => {
            if paths.is_empty() {
                paths = stdin_paths().collect();
            }

            print(match method {
                SortMethod::Subfrequency => sort_tags_by_subfrequency(&paths),
            })?
        }
        Some(Commands::Combine { mut paths }) => {
            if paths.is_empty() {
                paths = stdin_paths().collect();
            }

            let mut out = stdout();
            for path in combine(&paths) {
                writeln!(out, "{}", path.display())?
            }
        }
        Some(Commands::Uncombine { mut paths }) => {
            if paths.is_empty() {
                paths = stdin_paths().collect();
            }

            print(uncombine(paths))?
        }
        None => {}
    }

    Ok(())
}

fn stdin_paths() -> impl Iterator<Item = TaggedPath> {
    stdin()
        .lines()
        .map(|line| match TaggedPath::new(line.unwrap()) {
            Ok(x) => x,
            Err(e) => {
                let mut err = clap::Error::new(clap::error::ErrorKind::ValueValidation);
                err.insert(
                    ContextKind::Usage,
                    ContextValue::StyledStr(e.to_string().into()),
                );
                err.insert(
                    ContextKind::InvalidArg,
                    ContextValue::String("PATH".to_owned()),
                );
                err.insert(
                    ContextKind::InvalidValue,
                    ContextValue::String(e.into_string()),
                );
                err.format(&mut Args::command()).exit()
            }
        })
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
