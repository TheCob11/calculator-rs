use thiserror::Error;

pub use token::Token;

use crate::lex::token::LexErrorKind;

pub mod token;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub struct Lexer(String, usize);

impl Lexer {
    #[must_use]
    pub fn new(s: &str) -> Self {
        let mut s = s.to_string();
        s.retain(|c| !c.is_whitespace());
        Self(s, 0)
    }

    #[must_use]
    pub fn pos(&self) -> usize {
        self.1
    }

    #[must_use]
    pub fn src(&self) -> &str {
        &self.0
    }
}

#[derive(Error, Debug, Clone)]
#[error("Lexing error at position {pos} in input '{input}': {kind}")]
pub struct Error {
    pub kind: LexErrorKind,
    pub input: String,
    pub pos: usize,
}

impl Iterator for Lexer {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let s = self.0.get(self.1..)?;
        if s.is_empty() {
            None
        } else {
            Some(match Token::eat(s) {
                Ok((tok, len)) => {
                    self.1 += len;
                    Ok(tok)
                }
                Err(kind) => Err(Error {
                    kind,
                    input: self.0.clone(),
                    pos: self.1,
                }),
            })
        }
    }
}
