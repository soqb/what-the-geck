use miette::GraphicalReportHandler;
use obscript::{
    drive, pipeline,
    reporting::{miettify, DiagnosticFilter, DiagnosticKind},
    stages, wasm, CompilerDriver, CompilerDriverConfig,
};

pub use obscript;
pub use psyker;

use std::{
    collections::{HashMap, HashSet},
    ffi::OsString,
    fmt::Debug,
    fs,
};

use psyker::{
    common::Ident4,
    format::{PluginEntry, Record},
    records::ParsedFields,
};

#[derive(clap::Parser, Debug)]
struct Args {
    path: OsString,
    #[arg(short)]
    json: bool,
    #[arg(long = "dbg")]
    debug: bool,
    #[arg(long)]
    record_names: bool,
}

fn main() -> Result<(), anyhow::Error> {
    unsafe { backtrace_on_stack_overflow::enable() };

    let args: Args = clap::Parser::parse();

    let bytes = fs::read(&args.path)?;
    let parsed = psyker::format::MainFile::parse_from_bytes(&bytes)?;
    println!("a");
    fn walk<'a, F>(entries: impl IntoIterator<Item = &'a PluginEntry>, mut f: F)
    where
        F: FnMut(&'a Record),
    {
        let mut vec: Vec<_> = entries.into_iter().collect();
        while !vec.is_empty() {
            vec = vec
                .into_iter()
                .flat_map(|entry| match entry {
                    PluginEntry::Group(group) => &group.entries[..],
                    PluginEntry::Record(record) => {
                        f(record);
                        &[]
                    }
                })
                .collect();
        }
    }

    let mut total = 0;
    let mut good_tokens = 0;
    let mut good_parse = 0;
    let mut good_ast = 0;
    let mut good_wasm = 0;

    let mut diagnostics_count = 0;

    let report_handler = GraphicalReportHandler::new();

    if args.record_names {
        let mut record_names = HashSet::<Ident4>::new();
        let mut editor_ids: HashMap<Ident4, Vec<&str>> = HashMap::new();

        let commands = obscript::wasm::load_commands()?;

        let wasm_context = wasm::Context {
            defs: wasm::Context::nv_definitions(),
            commands: wasm::Context::map_commands(&commands),
            index: wasm::Context::create_form_index(&parsed),
        };

        let start_time = std::time::Instant::now();

        let mut texts = Vec::new();
        walk(&parsed.top_level_entries, |record| {
            if record.fields.parsed.is_none() {
                record_names.insert(record.name);
            }

            if let Some(id) = &record.fields.common.editor_id {
                editor_ids.entry(record.name).or_default().push(id);
            }

            if let Some(ParsedFields::Script(script)) = &record.fields.parsed {
                texts.push((
                    record.fields.common.editor_id.clone().unwrap_or_default(),
                    script.source_text.clone(),
                ));
            };
        });

        const ERROR_FILTER: DiagnosticFilter = DiagnosticFilter {
            kind: DiagnosticKind::Nothing,
            warnings: DiagnosticFilter::NO_WARNINGS,
        };

        let mut driver = CompilerDriver {
            config: CompilerDriverConfig::default().with_filter(ERROR_FILTER),
            diagnostics: Vec::new(),
            diagnostics_buffer: Vec::new(),
            wasm_context,
        };

        for (_id, script) in &texts {
            let result = drive::<
                _,
                pipeline!(
                    stages::Tokenize,
                    stages::Parse,
                    stages::BuildAst,
                    stages::Compile
                ),
            >(&mut driver, script);
            diagnostics_count += driver.diagnostics.len();

            if result.is_err() {
                for miette in driver.diagnostics.iter().flat_map(|reportable| {
                    miettify(std::ops::Deref::deref(reportable), script, ERROR_FILTER)
                }) {
                    println!("{}", miette.as_display(&report_handler));
                }
            }

            total += 1;
            match result {
                Ok(_) | Err(DiagnosticKind::Warning(_)) => {
                    good_tokens += 1;
                    good_parse += 1;
                    good_ast += 1;
                    good_wasm += 1;
                }
                Err(DiagnosticKind::Syntax) => good_tokens += 1,
                Err(DiagnosticKind::BuildAst) => {
                    good_tokens += 1;
                    good_parse += 1;
                }
                Err(DiagnosticKind::CompileFail) => {
                    good_tokens += 1;
                    good_parse += 1;
                    good_ast += 1;
                }
                Err(a) => println!("{:?}", &a),
            }
        }

        println!(
            "traversal took {}ms",
            start_time.elapsed().as_secs_f32() * 1000.
        );

        // println!("{record_names:?}");
        println!(
            "tokens: {good_tokens}/{total} = {frac}",
            frac = good_tokens as f32 / total as f32
        );
        println!(
            "parsed: {good_parse}/{total} = {frac}",
            frac = good_parse as f32 / total as f32
        );
        println!(
            "astify: {good_ast}/{total} = {frac}",
            frac = good_ast as f32 / total as f32
        );
        println!(
            "wasmed: {good_wasm}/{total} = {frac}",
            frac = good_wasm as f32 / total as f32
        );
        println!(
            "times obsidian & bethesda wrote terrible, bad code that shouldn't compile but has to: {}",
            diagnostics_count,
        );

        for (name, values) in editor_ids {
            fs::write(format!("./.cache/__forms/{name}.txt"), values.join("\n"))?;
        }
    }

    Ok(())
}

// fn print(tree: SyntaxNode<RowanLang<Lang>>, mut depth: usize) {
//     println!(
//         "{}{} @ {:?}",
//         " ".repeat(depth * 2),
//         tree.kind(),
//         tree.text_range()
//     );
//     depth += 1;
//     for child in tree.children_with_tokens() {
//         if !token_is_meaningful(child.kind()) {
//             continue;
//         }
//         match child {
//             NodeOrToken::Node(node) => print(node, depth),
//             NodeOrToken::Token(token) => {
//                 println!(
//                     "{}{} @ {:?} {:?}",
//                     " ".repeat(depth * 2),
//                     token.kind().to_string().to_lowercase(),
//                     Span::from(token.text_range()),
//                     token.text(),
//                 )
//             }
//         }
//     }
// }
