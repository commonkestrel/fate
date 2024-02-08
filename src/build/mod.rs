use clap::Args;

mod ascii;
mod lexer;
mod parse;
mod expr;
mod generator;

pub use parse::Span;

#[derive(Debug, Args)]
pub struct BuildArgs {

}
