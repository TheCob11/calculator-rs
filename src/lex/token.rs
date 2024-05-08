use thiserror::Error;

use crate::eval::Number;
use crate::parser::ast::Identifier;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum GroupEnd {
    Open,
    Close,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum GroupKind {
    Paren,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Group(GroupKind, GroupEnd),
    Comma,
    Pow,
    Mul,
    Div,
    Plus,
    Minus,
    Number(Number),
    Id(Identifier),
    Equals,
}

#[derive(Error, Debug, Copy, Clone)]
pub enum LexErrorKind {
    #[error("Unknown character")]
    Unknown,
    #[error("Malformed number (its probably just a dot?)")]
    BadNumber,
}

impl Token {
    const PREFIXES: [(Self, &'static str); 13] = {
        use Token as T;
        [
            (T::Number(Number::INFINITY), "∞"), // lol
            (T::Group(GroupKind::Paren, GroupEnd::Open), "("),
            (T::Group(GroupKind::Paren, GroupEnd::Close), ")"),
            (T::Equals, "="),
            (T::Comma, ","),
            (T::Pow, "**"),
            (T::Mul, "*"),
            (T::Mul, "×"),
            (T::Mul, "∙"),
            (T::Div, "/"),
            (T::Div, "÷"),
            (T::Plus, "+"),
            (T::Minus, "-"),
        ]
    };
    const NUM_PREFIXES: [char; 11] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.'];
    fn try_prefix_tokens(s: &str) -> Option<(Self, usize)> {
        Self::PREFIXES
            .into_iter()
            .find_map(|(tok, prefix)| s.starts_with(prefix).then_some((tok, prefix.len())))
    }
    pub(super) fn eat(input: &str) -> Result<(Self, usize), LexErrorKind> {
        if let Some(x) = Self::try_prefix_tokens(input) {
            Ok(x)
        } else if input.starts_with(Self::NUM_PREFIXES) {
            (0..input.len())
                .filter_map(|i| {
                    input
                        .get(0..=i)?
                        .parse::<Number>()
                        .map_or(None, |num| Some((Self::Number(num), i + 1)))
                })
                .last()
                .ok_or(LexErrorKind::BadNumber)
        } else if input.starts_with(char::is_alphabetic) {
            let (ident, len) = Identifier::eat(input);
            Ok((Self::Id(ident), len))
        } else {
            Err(LexErrorKind::Unknown)
        }
    }
}

impl Identifier {
    fn eat(input: &str) -> (Self, usize) {
        let ident = input
            .split_once(|c: char| !(c.is_alphanumeric() || c == '_'))
            .map_or(input, |(left, _)| left);
        let len = ident.len();
        (ident.into(), len)
    }
}
