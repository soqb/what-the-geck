use anyhow::Result;
use std::path::PathBuf;

fn main() -> Result<()> {
    use xtask::generate_tree_sitter as task;
    println!("cargo:rerun-if-changed=src/grammar.rs");
    println!("cargo:rerun-if-changed=src/grammar-patch.json");

    let source = PathBuf::from("src/grammar.rs");
    let json_merge = PathBuf::from("src/grammar-patch.json");

    task::run(task::Args {
        source,
        postprocessing: Some(json_merge),
        grammar_name: "geckscript".to_owned(),
        build: true,
    })?;

    Ok(())
}
