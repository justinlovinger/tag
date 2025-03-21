use std::{
    env::current_dir,
    fmt,
    io::{self, Write},
    path::PathBuf,
    str::FromStr,
};

use clap::{Parser, Subcommand};
use itertools::Itertools;
use tag::{Name, Tag, TaggedFilesystem, TaggedFilesystemBuilder, TaggedPath};

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
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
    /// Initialize metadata for tagged files
    ///
    /// The working directory will become the root of a tagged filesystem.
    Init,
    /// Rebuild tagged paths and clean tags
    ///
    /// - Remove tags for missing files
    /// - Add an empty tag-directory for files with a missing tag-directory
    /// - Remove tagged paths for missing files
    /// - Add tagged paths for files missing them
    /// - Remove extra tags from tagged paths
    /// - Add missing tags to tagged paths
    /// - Organize tagged paths
    ///
    /// Tags are split into directories,
    /// most frequent first.
    /// Ties are broken in favor of longer tags
    /// and then lexicographical order.
    /// Unique tags are inlined.
    ///
    /// Untagged files are not changed.
    Build {
        /// Names of files to clean up and build
        ///
        /// If empty,
        /// build all files.
        #[clap(required = false, value_name = "NAME")]
        names: Option<Vec<Name>>,
    },
    /// Print tagged paths of files with given tags
    Find {
        /// Find files including *all* these tags
        #[clap(value_name = "TAG")]
        include: Vec<Tag>,

        /// Ignore files including *any* of these tags
        #[clap(long, short, value_name = "TAG")]
        exclude: Vec<Tag>,

        /// Sort output by name
        #[clap(long)]
        sort_by_name: bool,
    },
}

#[derive(Clone, Debug)]
enum PathOrName {
    Path(TaggedPath),
    Name(Name),
}

impl FromStr for PathOrName {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.to_owned();
        match TaggedPath::new(s) {
            Ok(path) => Ok(Self::Path(path)),
            Err(e) => {
                let path_msg = e.to_string();
                Name::new(e.into_string())
                    .map_err(|e| anyhow::Error::msg(format!("{path_msg}. {e}.")))
                    .map(Self::Name)
            }
        }
    }
}

impl From<PathOrName> for Name {
    fn from(value: PathOrName) -> Self {
        match value {
            PathOrName::Path(path) => path.name().to_owned(),
            PathOrName::Name(name) => name,
        }
    }
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    if let Some(path) = args.working_directory {
        std::env::set_current_dir(path)?;
    }

    if let Some(Commands::Init) = args.command {
        TaggedFilesystem::init(current_dir()?)?;
    } else {
        let filesystem = TaggedFilesystemBuilder::new(current_dir()?)
            .build()?
            .expect("The working directory is not tagged. Please run `tag init` to initialize.");
        match args.command {
            Some(Commands::Init) => unreachable!(),
            Some(Commands::Build { names }) => match names {
                Some(names) => filesystem.build_some(names)?,
                None => filesystem.build()?,
            },
            Some(Commands::Find {
                include,
                exclude,
                sort_by_name,
            }) => {
                let files = filesystem.find(include, exclude)?;
                if sort_by_name {
                    print(files.sorted_unstable_by(|file, other| file.name().cmp(other.name())))?
                } else {
                    print(files)?
                }
            }
            None => {}
        }
    }

    Ok(())
}

fn print<T>(it: impl IntoIterator<Item = T>) -> io::Result<()>
where
    T: fmt::Display,
{
    let mut out = writer();
    for item in it {
        writeln!(out, "{item}")?
    }
    Ok(())
}

fn writer() -> impl Write {
    io::BufWriter::new(io::stdout().lock())
}
