use std::{
    env::current_dir,
    fmt,
    io::{self, Write},
    path::PathBuf,
    str::FromStr,
};

use clap::{Parser, Subcommand};
use itertools::Itertools;
use tag::{
    Name, Tag, TaggedFilesystem, TaggedFilesystemBuilder, TaggedPath, DIR_SEPARATOR,
    INLINE_SEPARATOR, TAG_END,
};

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Make no changes to the filesystem
    #[arg(long)]
    dry_run: bool,

    /// Print operations taken by the program
    #[arg(short, long)]
    verbose: bool,

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
    Build,
    /// Create an empty file with the given tags and name
    Touch {
        /// Tags to add to the file
        #[clap(value_name = "TAG", value_parser = tag_parser)]
        tags: Vec<Tag>,

        /// Name of the file
        #[clap(required = true, value_name = "NAME", value_parser = name_parser)]
        name: Name,
    },
    /// Create an empty directory with the given tags and name
    Mkdir {
        /// Tags to add to the directory
        #[clap(value_name = "TAG", value_parser = tag_parser)]
        tags: Vec<Tag>,

        /// Name of the directory
        #[clap(required = true, value_name = "NAME", value_parser = name_parser)]
        name: Name,
    },
    /// Remove a tagged file or directory
    ///
    /// Tags for the file will also be removed.
    Rm {
        /// File to remove
        #[clap(required = true, value_name = "PATH|NAME")]
        file: PathOrName,
    },
    /// Change the name of a tagged file or directory
    ///
    /// Tags remain unchanged.
    Rename {
        /// File or directory to rename
        #[clap(required = true, value_name = "PATH|NAME")]
        file: PathOrName,

        /// New name
        #[clap(required = true, value_name = "NEW_NAME", value_parser = name_parser)]
        new_name: Name,
    },
    /// Add tags to files and print new tagged paths
    Add {
        /// Tag to add
        #[clap(required = true, value_name = "TAG", value_parser = tag_parser)]
        tag: Tag,

        /// Files to tag
        #[clap(required = true, value_name = "FILE", value_parser = tagged_file_parser)]
        files: Vec<TaggedPath>,
    },
    /// Delete tags from files and print new tagged paths
    Del {
        /// Tag to delete
        #[clap(required = true, value_name = "TAG", value_parser = tag_parser)]
        tag: Tag,

        /// Files to delete tag from
        #[clap(required = true, value_name = "FILE", value_parser = tagged_file_parser)]
        files: Vec<TaggedPath>,
    },
    /// Add and delete tags from files and print new tagged paths
    Mod {
        /// Tags to add
        #[clap(short, long, value_name = "TAG", value_parser = tag_parser)]
        add: Vec<Tag>,

        /// Tags to delete
        #[clap(short, long, value_name = "TAG", value_parser = tag_parser)]
        del: Vec<Tag>,

        /// Files to modify
        #[clap(required = true, value_name = "FILE", value_parser = tagged_file_parser)]
        files: Vec<TaggedPath>,
    },
    /// Print tagged paths of files with given tags
    Find {
        /// Find files including *all* these tags
        #[clap(value_name = "TAG", value_parser = tag_parser)]
        include: Vec<Tag>,

        /// Ignore files including *any* of these tags
        #[clap(long, short, value_name = "TAG", value_parser = tag_parser)]
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
                let msg = e.to_string();
                Name::new(e.into_string())
                    .ok_or_else(|| anyhow::Error::msg(format!("{msg}. {}.", name_error())))
                    .map(Self::Name)
            }
        }
    }
}

fn tag_parser(s: &str) -> Result<Tag, String> {
    Tag::new(s.to_owned()).ok_or_else(|| format!(
        "tags cannot start with `.` or `{TAG_END}` or contain `{INLINE_SEPARATOR}` or `{DIR_SEPARATOR}`"
    ))
}

fn name_parser(s: &str) -> Result<Name, String> {
    Name::new(s.to_owned()).ok_or_else(name_error)
}

fn name_error() -> String {
    format!("name cannot contain `{DIR_SEPARATOR}`")
}

fn tagged_file_parser(s: &str) -> Result<TaggedPath, String> {
    TaggedPath::new(s.to_owned()).map_err(|e| e.to_string())
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    if let Some(path) = args.working_directory {
        std::env::set_current_dir(path)?;
    }

    if let Some(Commands::Init) = args.command {
        TaggedFilesystem::init()?;
    } else {
        let filesystem = TaggedFilesystemBuilder::new(current_dir()?)
            .dry_run(args.dry_run)
            .verbose(args.verbose)
            .build()?
            .expect("The working directory is not tagged. Please run `tag init` to initialize.");
        match args.command {
            Some(Commands::Init) => unreachable!(),
            Some(Commands::Build) => filesystem.build()?,
            Some(Commands::Touch { tags, name }) => print_paths([filesystem.touch(tags, name)?])?,
            Some(Commands::Mkdir { tags, name }) => print_paths([filesystem.mkdir(tags, name)?])?,
            Some(Commands::Rm { file }) => match file {
                PathOrName::Path(path) => filesystem.rm(path.name().to_owned()),
                PathOrName::Name(name) => filesystem.rm(name),
            }?,
            Some(Commands::Rename { file, new_name }) => print_paths([match file {
                PathOrName::Path(path) => filesystem.rename(path.name().to_owned(), new_name),
                PathOrName::Name(name) => filesystem.rename(name, new_name),
            }?])?,
            // Ideally,
            // Clap would collect `add`, `del`, and `files` as the correct collections,
            // so we would not need to allocate each twice,
            // see <https://github.com/clap-rs/clap/issues/3114>.
            Some(Commands::Add { tag, files }) => {
                print_paths(filesystem.add(tag, files.into_iter().collect())?)?
            }
            Some(Commands::Del { tag, files }) => {
                print_paths(filesystem.del(tag, files.into_iter().collect())?)?
            }
            Some(Commands::Mod { add, del, files }) => print_paths(filesystem.r#mod(
                add.into_iter().collect(),
                del.into_iter().collect(),
                files.into_iter().collect(),
            )?)?,
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

fn print_paths(paths: impl IntoIterator<Item = PathBuf>) -> io::Result<()> {
    let mut out = writer();
    for path in paths {
        // `display()` should not affect results
        // because invalid unicode should error before here.
        writeln!(out, "{}", path.display())?
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
