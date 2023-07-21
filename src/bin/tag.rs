use clap::{Parser, Subcommand};
use filesystem::OsFileSystem;
use tag::{Tag, TaggedFile, TaggedFilesystem, SEPARATORS, TAG_END};

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Args {
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
}

fn tag_parser(s: &str) -> Result<Tag, String> {
    Tag::new(s.to_owned()).ok_or(format!(
        "tags cannot start with `{}` or contain `{}` or `{}`",
        TAG_END, SEPARATORS[0], SEPARATORS[1],
    ))
}

fn tagged_file_parser(s: &str) -> Result<TaggedFile, String> {
    TaggedFile::new(s.to_owned()).ok_or(format!(
        "tagged files must contain zero or more tags ended by `{}` or `{}` with the tagging portion ended by `{}`",
        SEPARATORS[0], SEPARATORS[1], TAG_END
    ))
}

fn main() -> std::io::Result<()> {
    let args = Args::parse();

    let filesystem = TaggedFilesystem::new(OsFileSystem::new());
    if let Some(Commands::Add { tag, files }) = args.command {
        for file in files {
            match filesystem.add_tag(&tag, file) {
                Ok(_) => {}
                Err(tag::AddError::FilesystemError(e)) => return Err(e),
                Err(tag::AddError::FileAlreadyHasTag(e)) => println!("{e}"),
            }
        }
    }

    Ok(())
}
