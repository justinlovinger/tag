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
    /// Organize all tagged files under the given directory
    ///
    /// Tags are split into directories,
    /// most frequent first.
    /// Ties are broken in favor of longer tags
    /// and then lexicographical order.
    /// Unique tags are inlined.
    ///
    /// Untagged files are not changed.
    Organize {
        /// Directory to organize
        #[arg(default_value = ".")]
        path: PathBuf,
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

    let filesystem = TaggedFilesystemBuilder::new(OsFileSystem::new())
        .dry_run(args.dry_run)
        .verbose(args.verbose)
        .build();
    for path in match args.command {
        Some(Commands::Add { tag, files }) => filesystem.add(tag, files)?,
        Some(Commands::Del { tag, files }) => filesystem.del(tag, files)?,
        Some(Commands::Path { tags, name }) => vec![filesystem.path(tags, name)?],
        Some(Commands::Organize { path }) => {
            std::env::set_current_dir(path)?;
            filesystem.organize()?;
            Vec::new()
        }
        None => Vec::new(),
    } {
        // `display()` should not affect results
        // because invalid unicode should error before here.
        println!("{}", path.display())
    }

    Ok(())
}
