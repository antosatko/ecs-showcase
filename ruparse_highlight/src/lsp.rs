use std::env;
use std::fs::{File, write};
use std::io::Write;
use std::path::PathBuf;

use dashmap::DashMap;
use ruparse::Parser;
use ruparse_playground::grammar::gen_parser;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::html::Style;
use crate::{html, index_file};

// This list must match the order of your 'ty' u8 mapping
pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::COMMENT,      // 0
    SemanticTokenType::VARIABLE,     // 1
    SemanticTokenType::KEYWORD,      // 2
    SemanticTokenType::STRING,       // 3
    SemanticTokenType::NUMBER,       // 4
    SemanticTokenType::OPERATOR,     // 5
    SemanticTokenType::TYPE,         // 6
    SemanticTokenType::DECORATOR,    // 7 (Used for SpecialOperator)
    SemanticTokenType::new("label"), // 8
];

pub struct Backend {
    pub(crate) client: Client,
    pub(crate) document_map: DashMap<String, String>,
    pub(crate) parser: Parser<'static>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                // 1. Tell the editor we want the FULL text every time the user types
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                // 2. Register Semantic Tokens
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: TextDocumentRegistrationOptions {
                                document_selector: Some(vec![DocumentFilter {
                                    language: Some("ecslang".to_string()),
                                    scheme: Some("file".to_string()),
                                    pattern: Some("*lang".to_string()),
                                }]),
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: TOKEN_TYPES.to_vec(),
                                    token_modifiers: vec![],
                                },
                                range: Some(false),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Server initialized")
            .await;
    }

    // --- Document Synchronization ---

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.document_map.insert(
            params.text_document.uri.to_string(),
            params.text_document.text,
        );
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        // Because we asked for FULL sync, changes[0] contains the entire new file text
        if let Some(change) = params.content_changes.into_iter().next() {
            self.document_map
                .insert(params.text_document.uri.to_string(), change.text);
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.document_map
            .remove(&params.text_document.uri.to_string());
    }

    // --- Semantic Tokens ---

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri_str = params.text_document.uri.to_string();

        let src = match self.document_map.get(&uri_str) {
            Some(text) => text.clone(),
            None => return Ok(None),
        };

        // 1. Run your indexer
        let mut spans = match index_file(&self.parser, &src) {
            Ok(s) => s,
            Err(_) => return Err(tower_lsp::jsonrpc::Error::parse_error()),
        };

        let html_str = html::MyStyle.render(&spans, &src);

        let out_path: PathBuf = env::current_exe()
            .ok()
            .and_then(|p| p.parent().map(|dir| dir.join("out.html")))
            .unwrap_or_else(|| PathBuf::from("out.html"));

        match File::create(out_path) {
            Ok(mut file) => {
                let _ = file.write_all(html_str.as_bytes());
            }
            _ => (),
        };
        // 2. CRITICAL: Sort spans by line, then column.
        // Delta encoding will fail catastrophically if this isn't done.
        spans.sort_by(|a, b| a.line.cmp(&b.line).then(a.column.cmp(&b.column)));

        // 3. Delta Encoding
        let mut pre_line = 0;
        let mut pre_start = 0;
        let mut data = Vec::with_capacity(spans.len());

        for span in spans {
            let line = span.line as u32;

            let start = span.column as u32;

            let delta_line = line - pre_line;
            let delta_start = if delta_line == 0 {
                start - pre_start
            } else {
                start
            };

            data.push(SemanticToken {
                delta_line: delta_line,
                delta_start: delta_start,
                length: span.len as u32,
                token_type: span.ty as u32,
                token_modifiers_bitset: 0,
            });

            pre_line = line;
            pre_start = start;
        }
        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        })))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}
