use clap::{Parser, Subcommand};
use filesystem::OsFileSystem;
use tag::{
    AddError, DelError, Tag, TaggedFile, TaggedFilesystemBuilder, DIR_SEPARATOR, INLINE_SEPARATOR,
    TAG_END,
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
        #[clap(required = true, value_name = "FILE", value_parser = tagged_file_parser)]
        files: Vec<TaggedFile>,
    },
    /// Delete tags from files
    Del {
        /// Tag to delete
        #[clap(required = true, value_name = "TAG", value_parser = tag_parser)]
        tag: Tag,

        /// Files to delete tag from
        #[clap(required = true, value_name = "FILE", value_parser = tagged_file_parser)]
        files: Vec<TaggedFile>,
    },
    /// Organize all tagged files under the current working directory
    ///
    /// Tags are split into directories,
    /// most frequent first.
    /// Ties are broken in favor of longer tags
    /// and then lexicographical order.
    /// Unique tags are inlined.
    ///
    /// Untagged files are not changed.
    Organize,
}

fn tag_parser(s: &str) -> Result<Tag, String> {
    Tag::new(s.to_owned()).ok_or(format!(
        "tags cannot start with `{TAG_END}`, contain `{INLINE_SEPARATOR}` or `{DIR_SEPARATOR}`, or consist of only '.'"
    ))
}

fn tagged_file_parser(s: &str) -> Result<TaggedFile, String> {
    TaggedFile::new(s.to_owned()).map_err(|e| format!(
        "{e}: tagged files must contain zero or more unique tags ended by `{INLINE_SEPARATOR}` or `{DIR_SEPARATOR}` with the tagging portion ended by `{TAG_END}`"
    ))
}

fn main() -> std::io::Result<()> {
    let args = Args::parse();

    let filesystem = TaggedFilesystemBuilder::new(OsFileSystem::new())
        .dry_run(args.dry_run)
        .verbose(args.verbose)
        .build();
    match args.command {
        Some(Commands::Add { tag, files }) => {
            for file in files {
                match filesystem.add_tag(&tag, file) {
                    Ok(_) => {}
                    Err(AddError::FilesystemError(e)) => return Err(e),
                    Err(AddError::HasTagError(e)) => println!("{e}"),
                }
            }
        }
        Some(Commands::Del { tag, files }) => {
            for file in files {
                match filesystem.del_tag(&tag, file) {
                    Ok(_) => {}
                    Err(DelError::FilesystemError(e)) => return Err(e),
                    Err(DelError::LacksTagError(e)) => println!("{e}"),
                }
            }
        }
        Some(Commands::Organize) => filesystem.organize()?,
        None => (),
    }

    Ok(())
}
