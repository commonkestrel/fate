use clap::Args;
use thiserror::Error;

mod ascii;
mod expr;
mod generator;
mod lexer;
mod parse;

pub use parse::Span;

#[derive(Debug, Args)]
pub struct BuildArgs {}

#[derive(Debug, Error)]
pub enum BuildError {}

pub fn build(args: BuildArgs) -> Result<(), BuildError> {
    Ok(())
}
