use std::{
    env::current_dir,
    fmt,
    io::{self, Write},
    path::PathBuf,
};

use clap::{Parser, Subcommand};
use tag::{find, Tag};

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
    /// Print tagged paths of files with given tags
    Find {
        /// Find files including *all* these tags
        #[arg(value_name = "TAG")]
        include: Vec<Tag>,

        /// Ignore files including *any* of these tags
        #[arg(long, short, value_name = "TAG")]
        exclude: Vec<Tag>,
    },
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    if let Some(path) = args.working_directory {
        std::env::set_current_dir(path)?;
    }

    match args.command {
        Some(Commands::Find { include, exclude }) => {
            print(find(current_dir()?, include, exclude)?)?
        }
        None => {}
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
