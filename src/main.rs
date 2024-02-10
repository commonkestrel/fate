use clap::{Parser, Subcommand};

mod build;
mod diagnostic;
mod lsp;
mod new;

use build::BuildArgs;
use lsp::LspArgs;
use new::NewArgs;

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

fn main() {}
