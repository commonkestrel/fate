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

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Dependency {
    Simple(String),
    Detailed {
        /// Dependency from git repo URL
        #[serde(skip_serializing_if = "Option::is_none")]
        git: Option<String>,
        /// Dependency from specific git branch
        #[serde(skip_serializing_if = "Option::is_none")]
        branch: Option<String>,
        /// Dependency from specific git tag
        #[serde(skip_serializing_if = "Option::is_none")]
        tag: Option<String>,
        ///Dependency from specific git commit
        #[serde(skip_serializing_if = "Option::is_none")]
        rev: Option<String>,
        /// Use this as the package name instead of the table key.
        /// 
        /// By using this, a package can have multiple versions of the same dependency.
        #[serde(skip_serializing_if = "Option::is_none")]
        package: Option<String>,
        /// Dependency from path
        #[serde(skip_serializing_if = "Option::is_none")]
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
