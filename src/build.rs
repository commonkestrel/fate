use crate::{
    build::syntax::lex::Token,
    cfg::{Config, ConfigError, ProjectType}, error,
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
mod symbol_table;
mod depgraph;
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

mod backend {
    pub mod linker;
    pub mod codegen;
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

    let namespace =
        match parse::parse(root_stream.stream, home_path, root_stream.source, root_stream.lookup) {
            Ok(stream) => stream,
            Err(errors) => {
                for err in errors {
                    err.emit().await;
                }

                error!("build failed due to previous errors").emit().await;

                return Err(BuildError::Lex(root_path));
            }
        };

    // println!("{functions:#?}");
    println!(
        "    {} building `{}` in {}",
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
