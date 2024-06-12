use fir::{DiagnosticFilter, DiagnosticKind, DynamicComponent, TargetContext};
use hashbrown::HashMap;
use log::info;
use lsp_server::{Message, Notification, Request};
use lsp_types::{
    CodeActionKind, CodeActionOptions, CodeActionProviderCapability, CompletionOptions,
    DeclarationCapability, HoverProviderCapability, OneOf, RenameOptions, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    WorkDoneProgressOptions,
};
use serde::de::DeserializeOwned;

use crate::{compile::CompilerState, context::Context};

macro_rules! match_type {
    ($fn:expr, { $($br:ident: $tr:ty => $rr:expr,)* $(_ => $else:expr,)? }) => {
        if false {
            unreachable!()
        }
        $(else if let Some($br) = {
            type T = $tr;
            $fn
        } {
            $rr
        })*
        $(
            else {
                $else
            }
        )?
    };
}

mod compile;
mod context;
mod diagnostic;
mod symbols;
mod utils;

pub(crate) use rust_sitter::tree_sitter;

fn source_tcx() -> impl TargetContext<Component = DynamicComponent> {
    fir_nvse::Context::default()
}

fn main() -> anyhow::Result<()> {
    env_logger::Builder::from_default_env()
        .format_timestamp(None)
        .filter_level(log::LevelFilter::Trace)
        .target(env_logger::Target::Stderr)
        .try_init()?;

    info!("lsp started!");

    let (connection, _io_threads) = lsp_server::Connection::stdio();

    let server_capabilities = ServerCapabilities {
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".to_string()]),
            ..Default::default()
        }),
        declaration_provider: Some(DeclarationCapability::Simple(true)),
        rename_provider: Some(OneOf::Right(RenameOptions {
            prepare_provider: Some(true),
            work_done_progress_options: WorkDoneProgressOptions::default(),
        })),
        code_action_provider: Some(CodeActionProviderCapability::Options(CodeActionOptions {
            resolve_provider: Some(false),
            code_action_kinds: Some(vec![CodeActionKind::QUICKFIX]),
            work_done_progress_options: WorkDoneProgressOptions::default(),
        })),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                will_save: None,
                will_save_wait_until: None,
                save: None,
            },
        )),
        ..Default::default()
    };

    connection.initialize(serde_json::to_value(server_capabilities)?)?;

    let mut context = Context {
        connection,
        documents: HashMap::default(),
        rid: 0,
        compiler_state: CompilerState::new(source_tcx())?,
        diagnostic_filter: DiagnosticFilter {
            lets_through: DiagnosticKind::Warning(fir::Warning::BadPractice),
            suppressed_warnings: DiagnosticFilter::NO_WARNINGS,
        },
        _marker: std::marker::PhantomData,
    };

    loop {
        let msg = context.connection.receiver.recv()?;
        match msg {
            Message::Request(ref req) => {
                info!("rqst: {req:?}");

                if context.connection.handle_shutdown(req)? {
                    break;
                }

                context.handle_request(req);
            }
            Message::Response(ref res) => {
                info!("resp: {res:?}");
                context.handle_response(res);
            }
            Message::Notification(ref note) => {
                info!("note: {note:?}");
                context.handle_notification(note);
            }
        };
    }

    Ok(())
}

fn as_request<T>(x: &Request) -> Option<T::Params>
where
    T: lsp_types::request::Request,
    T::Params: DeserializeOwned,
{
    if x.method == T::METHOD {
        let params = serde_json::from_value(x.params.clone()).unwrap_or_else(|err| {
            panic!(
                "Invalid request\n  method: {}\n  error: {}\n  request: {:?}\n",
                x.method, err, x,
            )
        });
        Some(params)
    } else {
        None
    }
}

fn as_notification<T>(x: &Notification) -> Option<T::Params>
where
    T: lsp_types::notification::Notification,
    T::Params: DeserializeOwned,
{
    if x.method == T::METHOD {
        let params = serde_json::from_value(x.params.clone()).unwrap_or_else(|err| {
            panic!(
                "Invalid notification\n  method: {}\n  error: {}",
                x.method, err,
            )
        });
        Some(params)
    } else {
        None
    }
}
