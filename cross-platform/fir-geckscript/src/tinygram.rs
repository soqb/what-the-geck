#[rust_sitter::grammar("tiny")]
mod tinygram {
    #[rust_sitter::language]
    #[rust_sitter::leaf(pattern = r"(?i)a")]
    pub struct Lang;
}
