use std::str::FromStr;

use thiserror::Error;

use crate::eval::Number;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    Pow,
    Mul,
    Div,
    Plus,
    Minus,
    Number(Number),
}

#[derive(Error, Debug, Copy, Clone)]
pub enum TokenParseErrorKind {
    #[error("Unknown symbol")]
    Unknown,
    #[error("Malformed number (its probably just a dot?)")]
    BadNumber,
}

#[derive(Error, Debug, Clone)]
#[error("Lexing error at position {pos} in input '{input}': {kind}")]
pub struct TokenParseError {
    kind: TokenParseErrorKind,
    input: String,
    pos: usize,
}

impl Token {
    const PREFIXES: [(Self, &'static str); 11] = {
        use Token as T;
        [
            (T::Number(Number::INFINITY), "∞"), // lol
            (T::LParen, "("),
            (T::RParen, ")"),
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
    fn try_prefix_tokens(s: &str) -> Option<(Self, usize)> {
        Self::PREFIXES
            .into_iter()
            .find_map(|(tok, prefix)| s.starts_with(prefix).then_some((tok, prefix.len())))
    }
    fn eat(s: &str) -> Result<(Self, usize), TokenParseErrorKind> {
        if let Some(x) = Self::try_prefix_tokens(s) {
            Ok(x)
        } else if s.starts_with(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.']) {
            (0..s.len())
                .filter_map(|i| {
                    s.get(0..=i)?
                        .parse::<Number>()
                        .map_or(None, |num| Some((Self::Number(num), i + 1)))
                })
                .last()
                .ok_or(TokenParseErrorKind::BadNumber)
        } else {
            Err(TokenParseErrorKind::Unknown)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer(String, usize);

impl Lexer {
    #[must_use] pub fn new(s: &str) -> Self {
        let mut s = s.to_lowercase();
        s.retain(|c| !c.is_whitespace());
        Self(s, 0)
    }
}

#[derive(Error, Debug)]
pub enum LexerParseError {}

impl FromStr for Lexer {
    type Err = LexerParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::new(s))
    }
}

impl Iterator for Lexer {
    type Item = Result<Token, TokenParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        let s = &self.0[self.1..];
        if s.is_empty() {
            return None;
        }
        Some(match Token::eat(s) {
            Ok((tok, len)) => {
                self.1 += len;
                Ok(tok)
            }
            Err(kind) => Err(TokenParseError {
                kind,
                input: self.0.clone(),
                pos: self.1,
            }),
        })
    }
}

#[cfg(test)]
mod tests {
    use Token::*;

    use super::*;

    fn test_lexing(s: &str, expected_toks: &'static [Token]) {
        let toks = Lexer::new(s).collect::<Result<Vec<_>, _>>();
        // println!("{s} --> {toks:?}");
        match toks {
            Ok(x) => assert_eq!(x, expected_toks),
            Err(x) => panic!("{x}"),
        }
    }

    #[test]
    fn one_plus_one() {
        const TOKS_1P1: &[Token] = &[Number(1.), Plus, Number(1.)];
        test_lexing("1+1", TOKS_1P1);
        test_lexing("1 + 1", TOKS_1P1);
        test_lexing(" 1 + 1", TOKS_1P1);
        test_lexing("1    +1", TOKS_1P1);
        test_lexing(" 1 + 1         ", TOKS_1P1);
    }

    #[test]
    fn two_plus_four_times_eight() {
        test_lexing("2+4*8", &[Number(2.), Plus, Number(4.), Mul, Number(8.)]);
    }

    #[test]
    fn five_point_four_pow_three_times_2_over_9() {
        test_lexing(
            "(5.4**3)*2/9",
            &[
                LParen,
                Number(5.4),
                Pow,
                Number(3.),
                RParen,
                Mul,
                Number(2.),
                Div,
                Number(9.),
            ],
        );
    }

    #[test]
    fn five_cross_three() {
        test_lexing("5×3", &[Number(5.), Mul, Number(3.)]);
    }

    #[test]
    fn five_div_three_pow_two_cross_two_point_two() {
        test_lexing(
            "5÷3**2×2.2",
            &[
                Number(5.),
                Div,
                Number(3.),
                Pow,
                Number(2.),
                Mul,
                Number(2.2),
            ],
        );
    }

    #[test]
    fn five_dot_three_pow_four_cross_two_times_five() {
        test_lexing(
            "5∙3**4×2*5",
            &[
                Number(5.),
                Mul,
                Number(3.),
                Pow,
                Number(4.),
                Mul,
                Number(2.),
                Mul,
                Number(5.),
            ],
        );
    }
}
