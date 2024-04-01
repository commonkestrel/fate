use std::{collections::HashMap, path::PathBuf};

use async_std::fs;
use semver::Version;
use serde::{Deserialize, Serialize};
use thiserror::Error;

const CONFIG: &str = "Fate.toml";

#[derive(Debug, Error)]
pub enum ConfigError {
    #[error("unable to open config file")]
    Read(#[from] std::io::Error),
    #[error("error deserializing config file")]
    De(#[from] toml::de::Error),
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Config {
    pub project: Project,
    pub dependencies: HashMap<String, Dependency>,
}

impl Config {
    pub async fn parse(dir: Option<&PathBuf>) -> Result<Self, ConfigError> {
        let file = dir
            .map(|dir| dir.join(CONFIG))
            .unwrap_or(PathBuf::from(CONFIG));

        let content = fs::read_to_string(file).await?;

        Ok(toml::from_str(&content)?)
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Project {
    pub name: String,
    pub description: Option<String>,
    pub version: Version,
    pub authors: Option<Vec<String>>,
    #[serde(rename = "type")]
    pub ty: ProjectType,
}

#[derive(Debug, Deserialize, Serialize)]
pub enum Dependency {
    Simple(String),
    Detailed {
        git: Option<String>,
        branch: Option<String>,
        tag: Option<String>,
        package: Option<String>,
        path: Option<PathBuf>,
    },
}

#[derive(Debug, Deserialize, Serialize)]
pub enum ProjectType {
    #[serde(rename = "exe")]
    Exe,
    #[serde(rename = "lib")]
    Lib,
}
