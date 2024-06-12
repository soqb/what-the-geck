use xtask::*;

#[derive(clap::Parser)]
pub enum Command {
    GenerateTreeSitter(generate_tree_sitter::Args),
}

fn main() -> anyhow::Result<()> {
    match <Command as clap::Parser>::parse() {
        Command::GenerateTreeSitter(args) => generate_tree_sitter::run(args),
    }
}
