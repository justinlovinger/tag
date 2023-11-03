use std::path::PathBuf;

use clap::{Parser, Subcommand};
use filesystem::OsFileSystem;
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
    /// All tagged files under the working directory are organized
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
    /// All tagged files under the working directory are organized
    /// in the process.
    Del {
        /// Tag to delete
        #[clap(required = true, value_name = "TAG", value_parser = tag_parser)]
        tag: Tag,

        /// Files to delete tag from
        #[clap(required = true, value_name = "FILE", value_parser = tagged_file_parser)]
        files: Vec<TaggedFile>,
    },
    /// Print path of file with given tags and name
    ///
    /// All tagged files under the working directory are organized
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
        Some(Commands::Add { tag, files }) => print_paths(filesystem.add(tag, files)?),
        Some(Commands::Del { tag, files }) => print_paths(filesystem.del(tag, files)?),
        Some(Commands::Path { tags, name }) => print_paths([filesystem.path(tags, name)?]),
        Some(Commands::Organize) => filesystem.organize()?,
        Some(Commands::Find { include, exclude }) => filesystem
            .find(include, exclude)?
            .for_each(|file| println!("{file}")),
        None => {}
    }
    Ok(())
}

fn print_paths(paths: impl IntoIterator<Item = PathBuf>) {
    for path in paths {
        // `display()` should not affect results
        // because invalid unicode should error before here.
        println!("{}", path.display())
    }
}
