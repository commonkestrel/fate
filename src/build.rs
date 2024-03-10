use crate::{
    build::lexer::TokenInner,
    cfg::{Config, ConfigError, ProjectType},
};
use async_std::fs::File;
use clap::Args;
use std::path::PathBuf;
use thiserror::Error;

mod ascii;
mod deps;
mod ast;
mod generator;
pub mod lexer;
mod symbol_table;
mod syntax;
pub mod token;
mod type_resolution;

const SOURCE: &str = "src";

#[derive(Debug, Args)]
pub struct BuildArgs {
    /// A singular file to compile
    file: Option<PathBuf>,
    /// The home directory of the project (default to the current working directory)
    #[clap(short = 'd', long, alias = "dir")]
    home: Option<PathBuf>,
}

#[derive(Debug, Error)]
pub enum BuildError {
    #[error(transparent)]
    Config(#[from] ConfigError),
    #[error("unable to open root file")]
    Root(std::io::Error),
    #[error("error lexing file `{0}`")]
    Lex(PathBuf),
}

pub async fn build(args: BuildArgs) -> Result<(), BuildError> {
    let cfg = Config::parse(args.home.as_ref()).await?;

    let root_file = match cfg.project.ty {
        ProjectType::Exe => "main.fate",
        ProjectType::Lib => "lib.fate",
    };

    let home_path = args
        .home
        .map(|dir| dir.join(SOURCE))
        .unwrap_or(PathBuf::from(SOURCE));

    let root_path = home_path.join(root_file);
    let root = File::open(&root_path)
        .await
        .map_err(|err| BuildError::Root(err))?;

    let root_stream = match lexer::lex(root_path.to_string_lossy().to_string(), root).await {
        Ok(stream) => stream,
        Err(errors) => {
            for err in errors {
                err.emit().await
            }

            return Err(BuildError::Lex(root_path));
        }
    };

    if cfg!(debug_assertions) {
        println!(
            "{:#?}",
            root_stream
                .into_iter()
                .map(|tok| tok.into_inner())
                .collect::<Vec<TokenInner>>()
        );
    }

    Ok(())
}
