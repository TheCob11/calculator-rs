use std::str::FromStr;

use thiserror::Error;

use crate::{ast::*, eval::Number};

#[derive(Debug)]
pub enum TokenKind {
    OpenParen,
    CloseParen,
    Number(Number),
    UnaryOp(UnaryOpKind),
    BinaryOp(BinaryOpKind),
    End,
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    len: usize,
}

#[derive(Error, Debug)]
pub enum TokenParseError {
    #[error("Unknown symbol {0}")]
    Unknown(String),
    #[error("Ambigious symbol {0}")]
    Ambiguous(String),
}

impl FromStr for Token {
    type Err = TokenParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use TokenKind as K;
        let kind = match s {
            "(" => K::OpenParen,
            ")" => K::CloseParen,
            x if x.parse::<Number>().is_ok() => K::Number(x.parse().unwrap()), //TODO: can this be improved despite unstable if let guards?
            "-" => return Err(TokenParseError::Ambiguous(s.to_owned())),
            "+" => K::BinaryOp(BinaryOpKind::Add),
            "*" => K::BinaryOp(BinaryOpKind::Multiply),
            "/" => K::BinaryOp(BinaryOpKind::Divide),
            "**" => K::BinaryOp(BinaryOpKind::Power),
            _ => return Err(TokenParseError::Unknown(s.to_owned())),
        };
        Ok(Token { kind, len: s.len() })
    }
}

#[derive(Debug)]
struct Parser(Vec<Token>);

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Error while parsing token: {0}")]
    TokenParseError(TokenParseError),
}

impl FromStr for Parser {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        todo!()
    }
}

impl BinaryOpKind {
    pub const fn binding_power(self) -> (u8, u8) {
        match self {
            BinaryOpKind::Power => (11, 10),
            BinaryOpKind::Multiply => (8, 9),
            BinaryOpKind::Divide => (8, 9),
            BinaryOpKind::Add => (6, 7),
            BinaryOpKind::Subtract => (6, 7),
        }
    }
}
