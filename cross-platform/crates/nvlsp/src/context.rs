use std::{marker::PhantomData, sync::RwLock};

use fir::{Diagnostic, DiagnosticFilter};
use hashbrown::HashMap;

use lsp_types::{
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, PublishDiagnostics,
    },
    request::{CodeActionRequest, HoverRequest},
    CodeActionOrCommand, CodeActionParams, CompletionOptions, Hover, HoverParams,
    HoverProviderCapability, InitializeParams, InitializeResult, PublishDiagnosticsParams,
    ServerCapabilities, Url,
};
use rand::SeedableRng;
use tower_lsp::LanguageServer;

use crate::{
    as_notification, as_request,
    compile::{Compiler, CompilerState},
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

pub struct TextDocument {
    pub text: String,
    pub syntax_tree: tree_sitter::Tree,
    pub fir_script: Option<fir::Script>,
    pub diagnostics: Vec<Box<dyn Diagnostic>>,
    pub version: usize,
    pub diagnostic_version: usize,
    pub line_lengths: Vec<usize>,
    pub symbols: SpanMap<Symbol>,
}

pub struct Context<'cx> {
    pub documents: HashMap<Url, TextDocument>,
    pub rid: i32,
    pub compiler_state: RwLock<CompilerState>,
    pub diagnostic_filter: DiagnosticFilter,
    pub(crate) _marker: PhantomData<&'cx ()>,
}

// impl<'cx> Context<'cx> {
//     pub fn send(&mut self, message: Message) {
//         self.connection
//             .sender
//             .send(message)
//             .unwrap_or_else(|_| panic!("ipc failed"));
//     }

//     pub fn send_response<T: serde::Serialize>(&mut self, id: RequestId, message: Option<T>) {
//         let (result, error) = message.map_or_else(
//             || (Some(serde_json::Value::Null), None),
//             |message| {
//                 serde_json::to_value(message).map_or_else(
//                     |error| {
//                         (
//                             None,
//                             Some(lsp_server::ResponseError {
//                                 code: 0,
//                                 message: error.to_string(),
//                                 data: None,
//                             }),
//                         )
//                     },
//                     |json| (Some(json), None),
//                 )
//             },
//         );
//         self.send(Message::Response(Response { id, result, error }))
//     }

//     pub fn document(&self, uri: &Url) -> &TextDocument {
//         self.documents.get(uri).unwrap_or_else(|| panic!())
//     }

//     pub fn send_request<T: lsp_types::request::Request>(&mut self, params: T::Params) {
//         let note = lsp_server::Request::new(self.rid.into(), T::METHOD.to_string(), params);
//         self.rid += 1;
//         self.send(note.into());
//     }

//     pub fn send_notification<T: lsp_types::notification::Notification>(
//         &mut self,
//         params: T::Params,
//     ) {
//         let note = lsp_server::Notification::new(T::METHOD.to_string(), params);
//         self.send(note.into());
//     }

//     pub fn publish_diagnostics<'a>(
//         &mut self,
//         text: &String,
//         uri: &Url,
//         version: usize,
//         index_offset: &mut usize,
//         diagnostics: impl Iterator<Item = &'a dyn Diagnostic>,
//     ) {
//         // low quality seed. mid quality rng.. who care...
//         let mut rng = rand::rngs::StdRng::from_seed([1; 32]);
//         self.send_notification::<PublishDiagnostics>(PublishDiagnosticsParams {
//             diagnostics: diagnostics
//                 .flat_map(|rep| {
//                     let lsp_diagnostics = diagnostic_to_lsp(
//                         rep,
//                         *index_offset,
//                         &mut rng,
//                         text,
//                         &uri,
//                         self.diagnostic_filter,
//                     );
//                     *index_offset += lsp_diagnostics.len();
//                     lsp_diagnostics
//                 })
//                 .collect(),
//             uri: uri.clone(),
//             version: Some(version as i32),
//         });
//     }

//     pub fn update_document(&mut self, uri: Url, text: String) {
//         let (version, mut diagnostic_version, _old_syntax_tree) = self
//             .documents
//             .get_mut(&uri)
//             .map(|doc| {
//                 (
//                     doc.version + 1,
//                     doc.diagnostic_version,
//                     Some(&mut doc.syntax_tree),
//                 )
//             })
//             .unwrap_or_else(|| (0, 0, None));

//         let mut fir_script = None;
//         let mut compiler = Compiler::new(&mut self.compiler_state);
//         let (syntax_tree, mut diagnostics, script_result) = compiler.compile(None, &text);

//         let symbols = if let Some((script, internal_variables)) = script_result {
//             let symbols = compiler.analyze(&script, &internal_variables, |d| diagnostics.push(d));
//             fir_script = Some(script);
//             symbols
//         } else {
//             SpanMap::default()
//         };

//         self.publish_diagnostics(
//             &text,
//             &uri,
//             version,
//             &mut diagnostic_version,
//             diagnostics.iter().map(|d| &**d),
//         );

//         let document = TextDocument {
//             version,
//             diagnostic_version,
//             line_lengths: text
//                 .lines()
//                 .map(|line| line.encode_utf16().count() + 1)
//                 .collect(),
//             text,
//             syntax_tree,
//             diagnostics,
//             symbols,
//             fir_script,
//         };
//         self.documents.insert(uri, document);
//     }

//     // pub fn collect_symbols(script: &fir::Script) -> SpanMap<Symbol> {
//     //     struct SymbolCollector {
//     //         map: SpanMap<Symbol>,
//     //     }

//     //     impl fir::utils::Visitor for SymbolCollector {
//     //         fn visit_expression(&mut self, expression: fir::Spanned<&fir::Expression>) {
//     //             let span = expression.to_span();
//     //             match *expression.inner() {
//     //                 &fir::Expression::FormRef(idx) => {
//     //                     self.map.insert(
//     //                         span,
//     //                         Symbol {
//     //                             kind: SymbolKind::Form(idx),
//     //                         },
//     //                     );
//     //                 }
//     //                 &fir::Expression::GetVariable(idx) => {
//     //                     self.map.insert(
//     //                         span,
//     //                         Symbol {
//     //                             kind: SymbolKind::Variable(idx),
//     //                         },
//     //                     );
//     //                 }
//     //                 fir::Expression::Operation(_) => todo!(),
//     //                 fir::Expression::PrimitiveValue(_) => (),
//     //             }
//     //         }
//     //     }

//     //     let mut collector = SymbolCollector {
//     //         map: SpanMap::default(),
//     //     };
//     //     collector.visit_script(script);
//     //     collector.map
//     // }

//     pub fn publish_code_actions(&mut self, req: CodeActionParams, id: RequestId) {
//         let doc = self.document(&req.text_document.uri);
//         let response: Vec<_> = req
//             .context
//             .diagnostics
//             .iter()
//             .flat_map(|d| {
//                 d.data
//                     .as_ref()
//                     .and_then(|value| value.as_u64())
//                     .and_then(|i| doc.diagnostics.get(i as usize))
//             })
//             .flat_map(|diagnostic| diagnostic_to_lsp_code_actions(&**diagnostic))
//             .map(CodeActionOrCommand::CodeAction)
//             .collect();

//         self.send_response(id, Some(response));
//     }

//     pub fn hover(&mut self, req: HoverParams, id: RequestId) {
//         let uri = req.text_document_position_params.text_document.uri;
//         let pos = req.text_document_position_params.position;

//         let document = self.document(&uri);

//         eprintln!("{:#?}", document.symbols);
//         let offset = document.position_to_offset(pos);
//         eprintln!("getting symbol at {offset}");
//         let symbol_got = document.symbols.get_at(offset);
//         eprintln!("{symbol_got:?}");

//         let response = if let Some((span, symbol)) = symbol_got {
//             let contents = self
//                 .compiler_state
//                 .with_resources(|resources| symbol.into_hover_contents(resources));

//             Some(Hover {
//                 contents,
//                 range: Some(document.span_to_range(span)),
//             })
//         } else {
//             None
//         };
//         self.send_response(id, response);
//     }

//     pub fn handle_request(&mut self, req: &Request) {
//         match_type!(as_request::<T>(req), {
//             params: CodeActionRequest => {
//                 self.publish_code_actions(params, req.id.clone());
//             },
//             params: HoverRequest => {
//                 self.hover(params, req.id.clone());
//             },
//         });
//     }

//     pub fn handle_response(&mut self, _res: &Response) {
//         todo!()
//     }

//     pub fn handle_notification(&mut self, note: &Notification) {
//         match_type!(as_notification::<T>(note), {
//             note: DidOpenTextDocument => {
//                 self.update_document(note.text_document.uri, note.text_document.text);
//             },
//             note: DidChangeTextDocument => {
//                 let change = note.content_changes.into_iter().next().unwrap();
//                 self.update_document(note.text_document.uri, change.text);
//             },
//             note: DidCloseTextDocument => {
//                 self.documents.remove(&note.text_document.uri);
//             },
//         })
//     }
// }

use tower_lsp::jsonrpc::Result;

#[tower_lsp::async_trait]
impl<'cx> LanguageServer for Context<'cx> {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions::default()),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}
