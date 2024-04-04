use std::iter::Peekable;
use std::str::FromStr;

use thiserror::Error;

use crate::ast::{BinaryOpKind, Expr, UnaryOpKind};
use crate::lex::{Lexer, LexerParseError, Token, TokenParseError};

#[derive(Error, Debug)]
pub enum ParsingError {
    #[error(transparent)]
    LexerParseError(#[from] LexerParseError),
    #[error(transparent)]
    LexingError(#[from] TokenParseError),
    #[error("Unexpected token '{0:?}'")]
    UnexpectedToken(Token),
    #[error(transparent)]
    BadUnOp(#[from] BadUnOp),
    #[error(transparent)]
    BadBinOp(#[from] BadBinOp),
    #[error("Unclosed grouping '{0:?}'")]
    UnclosedGroup(Token),
    #[error("Expected a prefix operator, found '{0:?}'")]
    NotPrefix(Token),
    #[error("Expected an infix operator, found '{0:?}'")]
    NotInfix(Token),
    #[error("Unexpected end of input")]
    GoneTooSoon,
    #[error("Unexpected trailing input after end of expression: '{0:?}'")]
    TrailingToken(Vec<Result<Token, TokenParseError>>),
}

fn parse_step(
    lex: &mut Peekable<impl Iterator<Item = Result<Token, TokenParseError>>>,
    curr_power: BindingPower,
) -> Result<Expr, ParsingError> {
    use Expr as E;
    use ParsingError as PErr;
    use Token as T;
    let mut lhs = match lex.next().ok_or(PErr::GoneTooSoon)?? {
        T::Number(x) => E::Number(x),
        left @ T::LParen => {
            let lhs = parse_step(lex, 0)?;
            let Some(Ok(T::RParen)) = lex.next() else {
                return Err(PErr::UnclosedGroup(left));
            }; // TODO: better grouping handling -- generic
            lhs
        }
        tok => {
            let pre_power = tok.prefix_power().ok_or(PErr::UnexpectedToken(tok))?;
            let rhs = parse_step(lex, pre_power)?;
            E::UnaryOp(tok.try_into()?, Box::new(rhs))
        }
    };
    while let Some(res) = lex.peek() {
        // gotta do this weird match bc peek returns a ref but only token is copy
        let &tok = match res {
            Ok(x) => x,
            Err(err) => Err(err.clone())?,
        };
        let Some((_, r_power)) = tok.infix_power().filter(|(l, _)| curr_power.lt(l)) else {
            break;
        };
        lex.next();
        lhs = E::BinaryOp(
            Box::new(lhs),
            tok.try_into()?,
            Box::new(parse_step(lex, r_power)?),
        );
    }
    Ok(lhs)
}

impl Expr {
    fn parse(lex: Lexer) -> Result<Self, ParsingError> {
        let mut lex = lex.peekable();
        let expr = parse_step(&mut lex, 0)?;
        // shouldn't have any more tokens after expression
        if lex.peek().is_some() {
            Err(ParsingError::TrailingToken(lex.collect()))
        } else {
            Ok(expr)
        }
    }
}

impl TryFrom<Lexer> for Expr {
    type Error = ParsingError;

    fn try_from(lex: Lexer) -> Result<Self, Self::Error> {
        Self::parse(lex)
    }
}

impl FromStr for Expr {
    type Err = ParsingError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s.parse()?)
    }
}

#[derive(Error, Debug)]
#[error("{0:?} is not a unary operator")]
pub struct BadUnOp(Token);

impl TryFrom<Token> for UnaryOpKind {
    type Error = BadUnOp;
    fn try_from(tok: Token) -> Result<Self, Self::Error> {
        Ok(match tok {
            Token::Plus => UnaryOpKind::Plus,
            Token::Minus => UnaryOpKind::Neg,
            _ => Err(BadUnOp(tok))?,
        })
    }
}

#[derive(Error, Debug)]
#[error("{0:?} is not a binary operator")]
pub struct BadBinOp(Token);

impl TryFrom<Token> for BinaryOpKind {
    type Error = BadBinOp;
    fn try_from(tok: Token) -> Result<Self, Self::Error> {
        use BinaryOpKind as K;
        use Token as T;
        Ok(match tok {
            T::Pow => K::Power,
            T::Mul => K::Multiply,
            T::Div => K::Divide,
            T::Plus => K::Add,
            T::Minus => K::Subtract,
            _ => Err(BadBinOp(tok))?,
        })
    }
}

type BindingPower = u8;
impl Token {
    #[must_use]
    #[allow(clippy::match_same_arms)]
    pub const fn infix_power(self) -> Option<(BindingPower, BindingPower)> {
        use Token as T;
        Some(match self {
            T::Pow => (11, 10),
            T::Mul | T::Div => (8, 9),
            T::Plus | T::Minus => (6, 7),
            T::LParen | T::RParen => return None,
            T::Number(_) => return None,
        })
    }
    #[must_use]
    #[allow(clippy::match_same_arms)]
    pub const fn prefix_power(self) -> Option<BindingPower> {
        use Token as T;
        Some(match self {
            T::Plus | T::Minus => 10,
            T::Number(_) => 0,
            T::LParen => 0,
            T::RParen => return None,
            T::Pow | T::Mul | T::Div => return None,
        })
    }
}
#[cfg(test)]
mod tests {
    use {BinaryOpKind::*, Expr::*, UnaryOpKind::*};

    use super::*;

    #[allow(clippy::unnecessary_box_returns)]
    fn b<T>(x: T) -> Box<T> {
        Box::new(x)
    }
    
    #[allow(clippy::needless_pass_by_value)]
    fn test_parse(s: &str, expected: Expr) {
        match s.parse::<Expr>() {
            Ok(expr) => assert_eq!(expr, expected),
            Err(e) => panic!("{e}"),
        }
    }

    #[test]
    fn one_plus_one() {
        test_parse("1+1", BinaryOp(b(Number(1.)), Add, b(Number(1.))));
    }

    #[test]
    fn two_plus_four_pow_three_div_five() {
        test_parse(
            "     (2+ 4) * * (3  ÷5)",
            BinaryOp(
                b(BinaryOp(b(Number(2.)), Add, b(Number(4.)))),
                Power,
                b(BinaryOp(b(Number(3.)), Divide, b(Number(5.)))),
            ),
        );
    }

    #[test]
    fn neg_whole_two_div_whole_one_plus_one() {
        test_parse(
            "-(2/(1+1))",
            UnaryOp(
                Neg,
                b(BinaryOp(
                    b(Number(2.)),
                    Divide,
                    b(BinaryOp(b(Number(1.)), Add, b(Number(1.)))),
                )),
            ),
        );
    }

    #[test]
    fn neg_one_whole_squared() {
        test_parse(
            "(-1)**2",
            BinaryOp(b(UnaryOp(Neg, b(Number(1.)))), Power, b(Number(2.))),
        );
    }
}
