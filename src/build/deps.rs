use std::{env, path::PathBuf};

pub fn download_dependency(url: &str) -> Result<PathBuf, git2::Error> {
    todo!()
}

fn fate_home() -> Option<PathBuf> {
    env::var("FATE_HOME")
        .ok()
        .map(|h| PathBuf::from(h))
        .or(home::home_dir().map(|h| h.join("fate")))
}
