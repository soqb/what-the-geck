use core::fmt;
use std::borrow::Cow;

use fir::{Component, DynamicComponent, FunctionDefinition, Resources, Ty};
use lsp_types::{HoverContents, MarkupContent, MarkupKind};

use crate::context::{Symbol, SymbolKind};

struct DisplaySignature<'a> {
    aliased: bool,
    resources: &'a Resources<DynamicComponent>,
    component_name: &'a str,
    name: &'a str,
    def: &'a FunctionDefinition,
}

impl<'a> fmt::Display for DisplaySignature<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let arg_count = self.def.params.len() + self.def.self_param.is_some() as u8 as usize;

        write!(
            f,
            "[{component}] {paren}{name}",
            name = self.name,
            component = self.component_name,
            paren = (arg_count > 0).then_some("(").unwrap_or(""),
        )?;

        let sep = match arg_count {
            0 | 1 => String::new(),
            _ => {
                let indent_len = self.component_name.len() + 7;
                format!("\n{}", " ".repeat(indent_len))
            }
        };

        if let Some(param) = self.def.self_param {
            write!(
                f,
                "{sep} self: {param}",
                param = self.resources.print_ty(param).as_display_raw(),
                sep = self.def.params.is_empty().then_some("").unwrap_or(&sep),
            )?;
        }

        for (i, param) in self.def.params.iter().enumerate() {
            write!(
                f,
                "{sep} {name}{opt}: {param}",
                name = param
                    .name
                    .as_ref()
                    .map(Cow::Borrowed)
                    .unwrap_or_else(|| Cow::Owned(format!("arg{i}"))),
                param = self.resources.print_ty(param.ty).as_display_raw(),
                opt = param.optional.then_some("?").unwrap_or(""),
                sep = (i < self.def.params.len())
                    .then_some(sep.as_str())
                    .unwrap_or(""),
            )?;
        }

        if arg_count > 0 {
            write!(f, ")")?;
        }

        if self.def.return_ty != Ty::Unit {
            write!(
                f,
                " -> {}",
                self.resources.print_ty(self.def.return_ty).as_display_raw()
            )?;
        }

        write!(f, ";")
    }
}

impl Symbol {
    pub fn into_hover_contents(&self, resources: &Resources<DynamicComponent>) -> HoverContents {
        fn get_func_def(
            resources: &fir::Resources<DynamicComponent>,
            idx: fir::FunctionIdx,
            aliased: bool,
        ) -> Option<(bool, &fir::Name, &fir::FunctionDefinition)> {
            let f = resources.get_function(idx)?;
            match &f.reference {
                fir::FunctionReference::Defined(def) => Some((aliased, &f.name, def)),
                &fir::FunctionReference::Alias { aliased_function } => {
                    get_func_def(resources, aliased_function, true)
                }
            }
        }
        match &self.kind {
            SymbolKind::Form(_) => HoverContents::Markup(MarkupContent {
                kind: MarkupKind::PlainText,
                value: "form".to_owned(),
            }),
            SymbolKind::Variable(_) => HoverContents::Markup(MarkupContent {
                kind: MarkupKind::PlainText,
                value: "variable".to_owned(),
            }),
            &SymbolKind::Function(idx) => {
                // todo: fetch & cache docs from the geck wiki...
                let (aliased, name, def) = get_func_def(resources, idx, false).unwrap();
                let component_name = resources.component(idx.into()).unwrap().identifier();
                let signature = DisplaySignature {
                    aliased,
                    def,
                    resources,
                    component_name,
                    name: &name.ident,
                };
                let value = format!("woah!\n```rust\n{signature}\n```");
                HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::PlainText,
                    value,
                })
            }
        }
    }
}
