use std::{
    borrow::Cow,
    fs::{self, File},
    io::Write,
    path::PathBuf,
};

use anyhow::anyhow;
use serde_derive::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use tempfile::TempDir;
use thiserror::Error;

#[derive(clap::Args)]
pub struct Args {
    #[arg(long)]
    pub source: PathBuf,
    #[arg(long)]
    pub postprocessing: Option<PathBuf>,
    // #[arg(long, short)]
    // pub output: PathBuf,
    #[arg(long, short)]
    pub build: bool,
    #[arg(long = "grammar")]
    pub grammar_name: String,
}

#[derive(Error, Debug)]
enum Error {
    #[error("io")]
    Io(#[from] std::io::Error),
    #[error("json")]
    Json(#[from] serde_json::Error),
    #[error("regex")]
    Regex(#[from] regex::Error),
    #[error("other")]
    Other(#[from] anyhow::Error),
}

type Result<T> = std::result::Result<T, Error>;

fn merge_json(a: &mut Value, b: Value) {
    match (a, b) {
        (&mut Value::Object(ref mut a), Value::Object(b)) => {
            for (k, v) in b {
                merge_json(a.entry(k.clone()).or_insert(Value::Null), v);
            }
        }
        (a, b) => {
            *a = b;
        }
    }
}

#[derive(Serialize, Deserialize)]
struct Postprocess {
    tokenize: Vec<String>,
    merge: Value,
}

fn postprocess<'a>(
    grammar_name: &'a str,
    grammar: &'a str,
    args: &Args,
) -> Result<Option<Cow<'a, str>>> {
    let cow = if let Some(json_merge) = &args.postprocessing {
        let mut source: Map<String, Value> = serde_json::from_str(grammar)?;
        if source.get("name").and_then(|s| s.as_str()) != Some(grammar_name) {
            return Ok(None);
        }
        let Postprocess { tokenize, merge } =
            serde_json::from_str(&fs::read_to_string(&json_merge)?)?;
        for pat in tokenize {
            let rx = regex::Regex::new(&pat)?;
            for (rule_name, rule) in source["rules"].as_object_mut().unwrap() {
                if rx
                    .find(&rule_name)
                    .is_some_and(|m| m.len() == rule_name.len())
                {
                    let old_rule = rule.take();
                    *rule = json! ({
                        "type": "TOKEN",
                        "content": {
                            "type": "PREC",
                            "value": 1,
                            "content":  old_rule
                        }
                    });
                }
            }
        }
        let mut source = Value::Object(source);
        merge_json(&mut source, merge);
        Cow::Owned(serde_json::to_string_pretty(&source)?)
    } else {
        Cow::Borrowed(grammar)
    };
    Ok(Some(cow))
}

fn build(name: &str, c: &str) -> Result<TempDir> {
    let dir = tempfile::Builder::new()
        .prefix(&format!("grammar-{name}"))
        .tempdir()
        .unwrap();
    let mut parser_file = File::create(dir.path().join("parser.c"))?;
    parser_file.write_all(c.as_bytes())?;
    drop(parser_file);

    let header_dir = dir.path().join("tree_sitter");
    fs::create_dir(&header_dir).unwrap();
    let mut parser_header = File::create(header_dir.join("parser.h"))?;
    parser_header
        .write_all(tree_sitter::PARSER_HEADER.as_bytes())
        .unwrap();
    drop(parser_header);

    cc::Build::new()
        .include(&dir)
        .flag_if_supported("-Wno-everything")
        .file(dir.path().join("parser.c"))
        .compile(&format!("tree-sitter-{name}-cc"));

    Ok(dir)
}

fn run_inner(args: Args) -> Result<()> {
    let grammars = rust_sitter_tool::generate_grammars(&args.source);
    let grammar = grammars
        .iter()
        .find_map(|grammar| postprocess(&args.grammar_name, &grammar, &args).transpose())
        .ok_or_else(|| anyhow!("rust-sitter grammar not found in {}", args.source.display()))??;

    let (name, code) = tree_sitter_cli::generate::generate_parser_for_grammar(&grammar)?;
    // for line in code.lines() {
    //     println!("cargo:warning={line}");
    // }
    let dir = build(&name, &code)?;
    dir.close()?;

    Ok(())
}

pub fn run(args: Args) -> anyhow::Result<()> {
    Ok(run_inner(args)?)
}
