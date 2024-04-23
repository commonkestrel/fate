use std::collections::{HashMap, HashSet};

use async_std::sync::RwLock;

pub struct DepGraph<T: Eq> {
    nodes: Vec<T>,
    deps: HashMap<usize, HashSet<usize>>,
    resolved: RwLock<HashSet<usize>>,
}

impl<T: Eq> DepGraph<T> {
    pub fn register_node(&mut self, node: T) {
        self.__register_node(node);
    }

    pub fn register_dependency(&mut self, node: T, dependency: T) {
        let node_pos = self.__register_node(node);
        let dependency_pos = self.__register_node(dependency);

        self.deps
            .entry(node_pos)
            .and_modify(|node| { node.insert(dependency_pos); })
            .or_insert_with(|| {
                let mut deps = HashSet::with_capacity(1);
                deps.insert(dependency_pos);
                deps
            });
    }

    pub async fn mark_resolved(&self, node: &T) -> Result<(), ()> {
        let pos = self.__pos(node).ok_or(())?;
        self.resolved.write().await.insert(pos);

        Ok(())
    }

    fn __register_node(&mut self, node: T) -> usize {
        match self.__pos(&node) {
            Some(pos) => pos,
            None => {
                self.nodes.push(node);
                self.nodes.len() - 1
            }
        }
    }

    fn __pos(&self, node: &T) -> Option<usize> {
        self.nodes.iter().position(|n| n == node)
    }
}

pub struct DepGraphIterator<'a, T: Eq + 'a> {
    depgraph: &'a DepGraph<T>,
    target: usize,
    satisfied: Vec<usize>,
    curpath: Vec<usize>,
}
