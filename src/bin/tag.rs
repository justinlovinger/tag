use std::{
    fmt,
    io::{self, Write},
    path::PathBuf,
};

use clap::{Parser, Subcommand};
use filesystem::OsFileSystem;
use itertools::Itertools;
use tag::{Tag, TaggedFile, TaggedFilesystemBuilder, DIR_SEPARATOR, INLINE_SEPARATOR, TAG_END};

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
    /// Add tags to files and print new paths
    ///
    /// Relevant tagged files under the working directory are organized
    /// in the process.
    Add {
        /// Tag to add
        #[clap(required = true, value_name = "TAG", value_parser = tag_parser)]
        tag: Tag,

        /// Files to tag
        #[clap(required = true, value_name = "FILE", value_parser = tagged_file_parser)]
        files: Vec<TaggedFile>,
    },
    /// Delete tags from files and print new paths
    ///
    /// Relevant tagged files under the working directory are organized
    /// in the process.
    Del {
        /// Tag to delete
        #[clap(required = true, value_name = "TAG", value_parser = tag_parser)]
        tag: Tag,

        /// Files to delete tag from
        #[clap(required = true, value_name = "FILE", value_parser = tagged_file_parser)]
        files: Vec<TaggedFile>,
    },
    /// Add and delete tags from files and print new paths
    ///
    /// Relevant tagged files under the working directory are organized
    /// in the process.
    Mod {
        /// Tags to add
        #[clap(short, long, value_name = "TAG", value_parser = tag_parser)]
        add: Vec<Tag>,

        /// Tags to delete
        #[clap(short, long, value_name = "TAG", value_parser = tag_parser)]
        del: Vec<Tag>,

        /// Files to modify
        #[clap(required = true, value_name = "FILE", value_parser = tagged_file_parser)]
        files: Vec<TaggedFile>,
    },
    /// Print path of file with given tags and name
    ///
    /// Relevant tagged files under the working directory are organized
    /// in the process.
    /// If no file exists with the given tags and name,
    /// files are organized
    /// as if such a file existed,
    /// thereby ensuring the path is valid.
    #[command(allow_missing_positional = true)]
    Path {
        #[clap(value_name = "TAG", value_parser = tag_parser)]
        tags: Vec<Tag>,

        #[clap(required = true, value_name = "NAME")]
        name: String,
    },
    /// Organize all tagged files under the working directory
    ///
    /// Tags are split into directories,
    /// most frequent first.
    /// Ties are broken in favor of longer tags
    /// and then lexicographical order.
    /// Unique tags are inlined.
    ///
    /// Untagged files are not changed.
    Organize,
    /// Print paths of files with given tags
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

fn tag_parser(s: &str) -> Result<Tag, String> {
    Tag::new(s.to_owned()).ok_or(format!(
        "tags cannot start with `.` or `{TAG_END}` or contain `{INLINE_SEPARATOR}` or `{DIR_SEPARATOR}`"
    ))
}

fn tagged_file_parser(s: &str) -> Result<TaggedFile, String> {
    TaggedFile::new(s.to_owned()).map_err(|e| e.to_string())
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    if let Some(path) = args.working_directory {
        std::env::set_current_dir(path)?;
    }

    let filesystem = TaggedFilesystemBuilder::new(OsFileSystem::new())
        .dry_run(args.dry_run)
        .verbose(args.verbose)
        .build();
    match args.command {
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
        Some(Commands::Mod { add, del, files }) => print_paths(filesystem.modify(
            add.into_iter().collect(),
            del.into_iter().collect(),
            files.into_iter().collect(),
        )?)?,
        Some(Commands::Path { tags, name }) => print_paths([filesystem.path(tags, name)?])?,
        Some(Commands::Organize) => filesystem.organize()?,
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
