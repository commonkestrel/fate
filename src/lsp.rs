use clap::Args;
use thiserror::Error;
use tower_lsp::{
    jsonrpc,
    lsp_types::{InitializeParams, InitializeResult, InitializedParams, MessageType},
    Client, LanguageServer, LspService, Server,
};

#[derive(Debug, Args)]
pub struct LspArgs {}

#[derive(Debug, Error)]
pub enum LspError {}

struct Backend {
    client: Client,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult::default())
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }
}

pub async fn start(args: LspArgs) -> Result<(), LspError> {
    let (service, socket) = LspService::new(|client| Backend { client });
    Server::new(async_std::io::stdin(), async_std::io::stdout(), socket)
        .serve(service)
        .await;

    Ok(())
}
