use clap::{Parser, Subcommand};

mod build;
mod lsp;
mod new;
mod diagnostic;

use build::BuildArgs;
use new::NewArgs;
use lsp::LspArgs;

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
    Lsp(LspArgs)
}

fn main() {
    
}
