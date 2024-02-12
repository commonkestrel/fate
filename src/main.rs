use clap::{Parser, Subcommand};

mod build;
pub mod cfg;
pub mod diagnostic;
mod lsp;
mod new;
pub mod span;

use build::{BuildArgs, BuildError};
use lsp::{LspArgs, LspError};
use new::{NewArgs, NewError};
use thiserror::Error;

#[derive(Debug, Parser)]
#[clap(name = "fate", version)]
struct Args {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Build(BuildArgs),
    New(NewArgs),
    Lsp(LspArgs),
}

#[derive(Debug, Error)]
enum Error {
    #[error(transparent)]
    New(#[from] NewError),
    #[error(transparent)]
    Build(#[from] BuildError),
    #[error(transparent)]
    Lsp(#[from] LspError),
}

#[async_std::main]
async fn main() -> Result<(), Error> {
    let args = Args::parse();

    Ok(match args.command {
        Command::New(args) => new::new(args).await?,
        Command::Build(args) => build::build(args).await?,
        // Oh how I yearn for `!` to be stable
        Command::Lsp(args) => lsp::start(args).await.map(|_| unreachable!())?,
    })
}
