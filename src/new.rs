use async_std::{fs, path::PathBuf, stream::StreamExt};
use clap::Args;
use thiserror::Error;

const SRC_DIR: &str = "src";
const EXE_PATH: &str = "src/main.fate";
const LIB_PATH: &str = "src/lib.fate";
const CONFIG_PATH: &str = "Fate.toml";
const EXE_FILE: &str = include_str!("../template/src/main.fate");
const LIB_FILE: &str = include_str!("../template/src/lib.fate");
const CONFIG_FILE: &str = include_str!("../template/Fate.toml");

#[derive(Debug, Args)]
pub struct NewArgs {
    name: String,
    #[clap(long)]
    /// Path to
    dir: Option<PathBuf>,
    /// Creates a new project with a library template.
    #[clap(long, action)]
    lib: bool,
}

#[derive(Debug, Error)]
pub enum NewError {
    #[error("directory already exists and is not empty")]
    NonEmptyDir,
    #[error(transparent)]
    Dir(std::io::Error),
    #[error(transparent)]
    Write(std::io::Error),
}

pub async fn new(args: NewArgs) -> Result<(), NewError> {
    let mut placeholder = args.dir.unwrap_or(PathBuf::from(&args.name));
    placeholder.push("placeholder");
    let home = placeholder.with_file_name("");

    let exists = match home.read_dir().await {
        Ok(mut i) => {
            if i.next().await.is_some() {
                return Err(NewError::NonEmptyDir);
            } else {
                true
            }
        }
        Err(_) => false,
    };

    if !exists {
        fs::create_dir(home)
            .await
            .map_err(|err| NewError::Dir(err))?;
    }

    let src = placeholder.with_file_name(SRC_DIR);
    fs::create_dir(src)
        .await
        .map_err(|err| NewError::Dir(err))?;

    fs::write(
        placeholder.with_file_name(CONFIG_PATH),
        CONFIG_FILE
            .replace("%NAME%", &args.name)
            .replace("%TYPE%", if args.lib { "lib" } else { "exe" }),
    )
    .await
    .map_err(|err| NewError::Write(err))?;
    if args.lib {
        fs::write(placeholder.with_file_name(LIB_PATH), LIB_FILE)
            .await
            .map_err(|err| NewError::Write(err))?;
    } else {
        fs::write(placeholder.with_file_name(EXE_PATH), EXE_FILE)
            .await
            .map_err(|err| NewError::Write(err))?;
    }

    Ok(())
}
