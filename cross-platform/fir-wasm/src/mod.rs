#[macro_use]
mod generate;
pub use generate::*;

mod context;
pub use context::*;

mod data;
pub use data::*;

mod diagnostics;
pub use diagnostics::*;

pub type Result<T> = std::result::Result<T, CompileFail>;
