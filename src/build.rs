use crate::{
    build::syntax::lex::Token,
    cfg::{Config, ConfigError, ProjectType},
};
use async_std::fs::File;
use clap::Args;
use colored::Colorize;
use std::path::PathBuf;
use std::time::{Duration, Instant};
use thiserror::Error;

use self::syntax::parse;

mod ascii;
mod deps;
mod generator;
mod symbol_table;
mod syntax {
    pub mod ast;
    pub mod lex;
    pub mod parse;
    pub mod token;
}
mod frontend {
    pub mod type_resolution;
    pub mod ast {
        pub mod typed;
    }
}

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

    let start = Instant::now();

    let root_stream = match syntax::lex::lex(root_path.to_string_lossy().to_string(), root).await {
        Ok(stream) => stream,
        Err(errors) => {
            for err in errors {
                err.emit().await
            }

            return Err(BuildError::Lex(root_path));
        }
    };

    // if cfg!(debug_assertions) {
    //     println!(
    //         "{:#?}",
    //         root_stream
    //             .stream
    //             .iter()
    //             .map(|tok| tok.inner())
    //             .collect::<Vec<&Token>>()
    //     );
    // }

    let functions =
        match parse::parse_functions(root_stream.stream, root_stream.source, root_stream.lookup) {
            Ok(stream) => stream,
            Err(errors) => {
                errors.emit().await;

                return Err(BuildError::Lex(root_path));
            }
        };

    let mut errors = Vec::new();
    for function in &functions {
        function
            .parameters
            .inner()
            .values()
            .for_each(|param| param.ty.bubble_errors(&mut errors));

        function.return_type.bubble_errors(&mut errors);
        function.body.bubble_errors(&mut errors);
    }

    if !errors.is_empty() {
        for error in errors {
            error.emit().await
        }

        return Err(BuildError::Lex(root_path));
    }

    // println!("{functions:#?}");
    println!(
        "\t{} building {} in {}",
        "Finished".bold().green(),
        root_path.display(),
        elapsed(start.elapsed()),
    );

    Ok(())
}

fn elapsed(duration: Duration) -> String {
    let secs = duration.as_secs();

    if secs >= 60 {
        format!("{}m {:02}s", secs / 60, secs % 60)
    } else {
        format!("{}.{:02}s", secs, duration.subsec_nanos() / 10_000_000)
    }
}
