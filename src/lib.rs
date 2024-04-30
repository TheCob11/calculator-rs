#![warn(clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]

use thiserror::Error;

pub use eval::Context;
pub use eval::Error as EvalError;
pub use eval::Number;
pub use parser::ast::Expr;
pub use parser::Error as ParseError;

pub mod eval;
pub mod lex;
pub mod parser;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    EvalError(#[from] eval::Error),
    #[error(transparent)]
    ParseError(#[from] parser::Error),
}

pub fn calc(s: &str) -> Result<Number, Error> {
    Ok(Context::new().eval(&s.parse::<Expr>()?)?)
}

#[cfg(test)]
mod tests;
