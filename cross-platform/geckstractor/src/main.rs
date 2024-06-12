use std::{borrow::Cow, fs, io};

use parse_wiki_text::{Node, Parameter, Positioned};
use serde::{Deserialize, Serialize};
use thiserror::Error;

pub const WIKI_ROOT: &str = "https://geckwiki.com/";

#[derive(Debug, Serialize, Default)]
pub struct FunctionDefinition<'a> {
    name: &'a str,
    alias: Option<Cow<'a, str>>,
    origin: Option<Cow<'a, str>>,
    version: Option<Cow<'a, str>>,
    syntax: FunctionSyntax<'a>,
    console_only: bool,
    extra_markdown: Cow<'a, str>,
}

#[derive(Debug, Serialize, Default)]
pub struct FunctionSyntax<'a> {
    return_type: Option<Cow<'a, str>>,
    return_val: Option<Cow<'a, str>>,
    reference_type: Option<Cow<'a, str>>,
    arguments: Vec<FunctionArgument<'a>>,
}

#[derive(Debug, Serialize, Default)]
pub struct FunctionArgument<'a> {
    type_: &'a str,
    name: &'a str,
    optional: bool,
    default_value: Option<&'a str>,
}

#[derive(Deserialize)]
struct CachedPage {
    title: String,
    #[serde(rename = "pageid")]
    page_id: usize,
    wikitext: String,
}

pub trait Text {
    fn text<'a>(&self, source: &'a str) -> &'a str;

    fn plaintext<'a>(&self, source: &'a str) -> Result<&'a str, DefinitionError>;

    fn is_plaintext(&self, source: &str, pattern: &str) -> bool {
        self.plaintext(source)
            .map(|text| text == pattern)
            .unwrap_or_default()
    }
}

impl<'b> Text for Node<'b> {
    fn text<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start()..self.end()]
    }

    fn plaintext<'a>(&self, source: &'a str) -> Result<&'a str, DefinitionError> {
        match self {
            Node::Text { start, end, .. } => Ok(&source[*start..*end]),
            _ => Err(DefinitionError::NotPlaintext(self.text(source).to_string())),
        }
    }
}

impl<'b> Text for [Node<'b>] {
    fn text<'a>(&self, source: &'a str) -> &'a str {
        let start = self.first().map(|n| n.start()).unwrap_or(0);
        let end = self.last().map(|n| n.end()).unwrap_or(0);
        &source[start..end]
    }

    fn plaintext<'a>(&self, source: &'a str) -> Result<&'a str, DefinitionError> {
        let text = self.text(source);
        if self.iter().all(|node| matches!(node, Node::Text { .. })) {
            Ok(text)
        } else {
            Err(DefinitionError::NotPlaintext(text.to_string()))
        }
    }
}

#[derive(Debug, Error)]
pub enum DefinitionError {
    #[error("io: {0}")]
    Io(#[from] io::Error),
    #[error("deserialization {0}")]
    SerdeJson(#[from] serde_json::Error),
    #[error("not plaintext: {0}")]
    NotPlaintext(String),
    #[error("no function template")]
    NoFunctionTemplate,
    #[error("invalid arg {0}")]
    InvalidArg(String),
}

fn get_template_parameters<'a>(
    node: &'a Node,
    source: &str,
    name: &str,
) -> Option<&'a Vec<Parameter<'a>>> {
    match node {
        Node::Template {
            name: template_name,
            parameters,
            ..
        } => {
            if template_name.is_plaintext(source, name) {
                Some(parameters)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn match_parameters<'a>(
    source: &str,
    iter: impl IntoIterator<Item = &'a Parameter<'a>>,
    mut matcher: impl FnMut(&str, &Vec<Node<'a>>) -> Result<(), DefinitionError>,
) -> Result<(), DefinitionError> {
    for Parameter {
        value: nodes, name, ..
    } in iter
    {
        let name = name
            .as_ref()
            .and_then(|name| name.plaintext(source).ok())
            .unwrap_or_default();
        matcher(name, nodes)?;
    }

    Ok(())
}

fn bool_parse(input: &str) -> bool {
    let lower = input.trim().to_lowercase();
    lower == "true" || lower == "yes" || lower == "y" || lower == "1"
}

fn parse(cached: &CachedPage) -> Result<FunctionDefinition, DefinitionError> {
    let source = cached.wikitext.as_str();
    let page = parse_wiki_text::Configuration::default().parse(source);

    let template_parameters = page
        .nodes
        .iter()
        .find_map(|node| get_template_parameters(node, source, "Function"))
        .ok_or_else(|| DefinitionError::NoFunctionTemplate)?;

    let mut origin = None;
    let mut version = None;
    let mut name = None;
    let mut alias = None;

    let mut syntax = FunctionSyntax::default();

    match_parameters(source, template_parameters, |param_name, nodes| {
        match param_name {
            "name" => name = Some(nodes.plaintext(source)?),
            "origin" => origin = Some(Cow::Borrowed(nodes.plaintext(source)?)),
            "originVersion" => version = Some(Cow::Borrowed(nodes.plaintext(source)?)),
            "alias" => alias = Some(Cow::Borrowed(nodes.plaintext(source)?)),
            "returnVal" => {
                syntax.return_val = Some(Cow::Borrowed(nodes.plaintext(source)?));
            }
            "returnType" => {
                syntax.return_type = Some(Cow::Borrowed(nodes.plaintext(source)?));
            }
            "arguments" => {
                let templates = nodes.iter().map(|node| {
                    get_template_parameters(node, source, "FunctionArgument")
                        .ok_or_else(|| DefinitionError::InvalidArg(nodes.text(source).to_string()))
                });
                for template in templates {
                    let template = template?;

                    let mut arg = FunctionArgument::default();

                    match_parameters(source, template, |param_name, nodes| {
                        match param_name {
                            "Name" => arg.name = nodes.plaintext(source)?,
                            "Type" => arg.type_ = nodes.plaintext(source)?,
                            "Optional" => arg.optional = bool_parse(nodes.plaintext(source)?),
                            "Value" => arg.default_value = Some(nodes.plaintext(source)?),
                            _ => (),
                        }
                        Ok(())
                    })?;

                    syntax.arguments.push(arg);
                }
            }
            _ => (),
        };

        Ok(())
    })?;

    Ok(FunctionDefinition {
        name: name.unwrap_or(&cached.title),
        origin,
        syntax,
        version,
        alias,
        console_only: false,
        extra_markdown: Cow::Owned(String::new()),
    })
}

fn main() -> Result<(), DefinitionError> {
    let mut total = 0u32;
    let mut good = 0u32;

    for file in fs::read_dir("./geckstractor/.cache")? {
        let contents = fs::read(file?.path())?;
        let cached: CachedPage = serde_json::from_slice(&contents[..])?;

        let func = parse(&cached);

        total += 1;
        good += func.is_ok() as u32;
    }

    println!("parsed: {good}/{total} == {}", good as f32 / total as f32);

    Ok(())
}
