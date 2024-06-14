use fir::{Diagnostic, DiagnosticFilter, ResourcesMut};
use hashbrown::HashMap;

use lsp_server::{Message, Notification, Request, RequestId, Response};
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, PublishDiagnostics,
    },
    request::{CodeActionRequest, HoverRequest},
    CodeActionOrCommand, CodeActionParams, Hover, HoverParams, PublishDiagnosticsParams, Url,
};
use rand::SeedableRng;

use crate::{
    as_notification, as_request,
    compile::{AllSources, CompilationInstance, CompilerState},
    diagnostic::{diagnostic_to_lsp, diagnostic_to_lsp_code_actions},
    utils::SpanMap,
};

use crate::tree_sitter;

#[derive(Debug)]
pub enum SymbolKind {
    Form(fir::FormIdx),
    Variable(fir::VariableIdx),
    Function(fir::FunctionIdx),
}

#[derive(Debug)]
pub struct Symbol {
    pub kind: SymbolKind,
}

#[derive(Default)]
pub struct TextDocument {
    pub text: String,
    pub syntax_tree: Option<tree_sitter::Tree>,
    pub fir_script: Option<fir::Script>,
    pub diagnostics: Vec<Box<dyn Diagnostic>>,
    pub version: usize,
    pub diagnostic_version: usize,
    pub line_lengths: Vec<usize>,
    pub symbols: SpanMap<Symbol>,
}

pub struct ConnectionManager {
    pub connection: lsp_server::Connection,
    pub rid: i32,
}

impl ConnectionManager {
    pub fn send(&mut self, message: Message) {
        self.connection
            .sender
            .send(message)
            .unwrap_or_else(|_| panic!("ipc failed"));
    }

    pub fn send_response<T: serde::Serialize>(&mut self, id: RequestId, message: Option<T>) {
        let (result, error) = message.map_or_else(
            || (Some(serde_json::Value::Null), None),
            |message| {
                serde_json::to_value(message).map_or_else(
                    |error| {
                        (
                            None,
                            Some(lsp_server::ResponseError {
                                code: 0,
                                message: error.to_string(),
                                data: None,
                            }),
                        )
                    },
                    |json| (Some(json), None),
                )
            },
        );
        self.send(Message::Response(Response { id, result, error }))
    }

    pub fn send_request<T: lsp_types::request::Request>(&mut self, params: T::Params) {
        let note = lsp_server::Request::new(self.rid.into(), T::METHOD.to_string(), params);
        self.rid += 1;
        self.send(note.into());
    }

    pub fn send_notification<T: lsp_types::notification::Notification>(
        &mut self,
        params: T::Params,
    ) {
        let note = lsp_server::Notification::new(T::METHOD.to_string(), params);
        self.send(note.into());
    }

    pub fn publish_diagnostics<'a>(
        &mut self,
        text: &String,
        uri: &Url,
        version: &mut usize,
        index_offset: &mut usize,
        diagnostics: impl Iterator<Item = &'a dyn Diagnostic>,
        diagnostic_filter: DiagnosticFilter,
    ) {
        // low quality seed. mid quality rng.. who care...
        let mut rng = rand::rngs::StdRng::from_seed([1; 32]);
        self.send_notification::<PublishDiagnostics>(PublishDiagnosticsParams {
            diagnostics: diagnostics
                .flat_map(|rep| {
                    let lsp_diagnostics = diagnostic_to_lsp(
                        rep,
                        *index_offset,
                        &mut rng,
                        text,
                        &uri,
                        diagnostic_filter,
                    );
                    *index_offset += lsp_diagnostics.len();
                    lsp_diagnostics
                })
                .collect(),
            uri: uri.clone(),
            version: Some(*version as i32),
        });
        *version += 1;
    }
}

pub struct Context {
    pub cm: ConnectionManager,
    pub documents: HashMap<Url, TextDocument>,
    pub compiler_state: CompilerState,
    pub diagnostic_filter: DiagnosticFilter,
}

impl Context {
    pub fn document(&self, uri: &Url) -> &TextDocument {
        self.documents.get(uri).unwrap_or_else(|| panic!())
    }

    pub fn update_document(&mut self, uri: Url, text: String) {
        let mut document = self
            .documents
            .remove(&uri)
            .unwrap_or_else(TextDocument::default);
        document.text = text;
        // .or_insert_with(|| TextDocument { text, syntax_tree: , fir_script: , diagnostics: , version: , diagnostic_version: , line_lengths: , symbols:  })
        // .map(|doc| (Some(doc), doc.version + 1, doc.diagnostic_version))
        // .unwrap_or_else(|| (None, 0, 0));

        if let Some(idx) = self.compiler_state.component_to_remove.take() {
            self.compiler_state.resources.remove_component(idx);
        }

        let mut fir_script = None;
        let mut compiler = CompilationInstance::new(
            &mut self.compiler_state,
            AllSources {
                unchanged_documents: self
                    .documents
                    .iter()
                    .filter(|(key, _)| key == &&uri)
                    .map(|(_, value)| value)
                    .collect(),
                changed_documents: vec![&document],
            },
        );

        let component_idx = compiler.preprocess_scripts().ok();
        // {
        //     use std::io::Write as _;
        //     let mut file = std::fs::OpenOptions::new()
        //         .truncate(true)
        //         .create(true)
        //         .write(true)
        //         .open("everything.txt")
        //         .unwrap();
        //     write!(file, "{:#?}", compiler.state.resources).unwrap();
        // }

        let _ = compiler.compile(|sc| {
            fir_script = Some(sc?);
            Ok(())
        });

        let symbols = if let Some(script) = fir_script {
            let symbols = compiler.analyze(&script);
            fir_script = Some(script);
            symbols
        } else {
            SpanMap::default()
        };

        let mut version = document.version;
        let mut diagnostic_version = document.diagnostic_version;
        let (left_ds, right_ds) = compiler.finish();
        // strange quirk of borrowck; we need `left_ds` to be dropped before `right_ds`!
        let left_ds = left_ds;

        self.cm.publish_diagnostics(
            &document.text,
            &uri,
            &mut version,
            &mut diagnostic_version,
            left_ds.chain(right_ds.iter().map(|d| &**d)),
            self.diagnostic_filter,
        );

        document = TextDocument {
            version,
            diagnostic_version,
            line_lengths: document
                .text
                .lines()
                .map(|line| line.encode_utf16().count() + 1)
                .collect(),
            diagnostics: right_ds,
            symbols,
            fir_script,
            syntax_tree: None,
            ..document
        };
        self.documents.insert(uri, document);

        self.compiler_state.component_to_remove = component_idx;
    }

    // pub fn collect_symbols(script: &fir::Script) -> SpanMap<Symbol> {
    //     struct SymbolCollector {
    //         map: SpanMap<Symbol>,
    //     }

    //     impl fir::utils::Visitor for SymbolCollector {
    //         fn visit_expression(&mut self, expression: fir::Spanned<&fir::Expression>) {
    //             let span = expression.to_span();
    //             match *expression.inner() {
    //                 &fir::Expression::FormRef(idx) => {
    //                     self.map.insert(
    //                         span,
    //                         Symbol {
    //                             kind: SymbolKind::Form(idx),
    //                         },
    //                     );
    //                 }
    //                 &fir::Expression::GetVariable(idx) => {
    //                     self.map.insert(
    //                         span,
    //                         Symbol {
    //                             kind: SymbolKind::Variable(idx),
    //                         },
    //                     );
    //                 }
    //                 fir::Expression::Operation(_) => todo!(),
    //                 fir::Expression::PrimitiveValue(_) => (),
    //             }
    //         }
    //     }

    //     let mut collector = SymbolCollector {
    //         map: SpanMap::default(),
    //     };
    //     collector.visit_script(script);
    //     collector.map
    // }

    pub fn publish_code_actions(&mut self, req: CodeActionParams, id: RequestId) {
        let doc = self.document(&req.text_document.uri);
        let response: Vec<_> = req
            .context
            .diagnostics
            .iter()
            .flat_map(|d| {
                d.data
                    .as_ref()
                    .and_then(|value| value.as_u64())
                    .and_then(|i| doc.diagnostics.get(i as usize))
            })
            .flat_map(|diagnostic| diagnostic_to_lsp_code_actions(&**diagnostic))
            .map(CodeActionOrCommand::CodeAction)
            .collect();

        self.cm.send_response(id, Some(response));
    }

    pub fn hover(&mut self, req: HoverParams, id: RequestId) {
        let uri = req.text_document_position_params.text_document.uri;
        let pos = req.text_document_position_params.position;

        let document = self.document(&uri);

        let offset = document.position_to_offset(pos);
        let symbol_got = document.symbols.get_at(offset);

        let response = if let Some((span, symbol)) = symbol_got {
            let contents = symbol.into_hover_contents(&self.compiler_state.resources);

            Some(Hover {
                contents,
                range: Some(document.span_to_range(span)),
            })
        } else {
            None
        };
        self.cm.send_response(id, response);
    }

    pub fn handle_request(&mut self, req: &Request) {
        match_type!(as_request::<T>(req), {
            params: CodeActionRequest => {
                self.publish_code_actions(params, req.id.clone());
            },
            params: HoverRequest => {
                self.hover(params, req.id.clone());
            },
        });
    }

    pub fn handle_response(&mut self, _res: &Response) {
        todo!()
    }

    pub fn handle_notification(&mut self, note: &Notification) {
        match_type!(as_notification::<T>(note), {
            note: DidOpenTextDocument => {
                self.update_document(note.text_document.uri, note.text_document.text);
            },
            note: DidChangeTextDocument => {
                let change = note.content_changes.into_iter().next().unwrap();
                self.update_document(note.text_document.uri, change.text);
            },
            note: DidCloseTextDocument => {
                self.documents.remove(&note.text_document.uri);
            },
        })
    }
}

// use tower_lsp::jsonrpc::Result;

// #[tower_lsp::async_trait]
// impl LanguageServer for Context {
//     async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
//         Ok(InitializeResult {
//             capabilities: ServerCapabilities {
//                 hover_provider: Some(HoverProviderCapability::Simple(true)),
//                 completion_provider: Some(CompletionOptions::default()),
//                 ..Default::default()
//             },
//             ..Default::default()
//         })
//     }

//     async fn shutdown(&self) -> Result<()> {
//         Ok(())
//     }
// }
